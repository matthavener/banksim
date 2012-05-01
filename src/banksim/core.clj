(ns banksim.core
  (:import (javax.swing JFrame JPanel))
  (:import (java.awt Graphics Color)))

(def grid-size 10)
(def grid-size-pix 400)
(def block-size-pix (/ grid-size-pix grid-size))
(def truck-count 5)
(def bank-count 10)
(def running true)

(defn random-locations [s]
  (->>
   (repeatedly (fn [] [(rand-int grid-size) (rand-int grid-size)]))
   (distinct)
   (take s)))

(def cells 
  (vec (take grid-size (repeatedly
                        (fn [] (vec (take grid-size (repeatedly (fn [] (ref {}))))))))))

(defn cell-at [[x y]] ((cells x) y))

(def central-bank-loc [(/ grid-size 2) (/ grid-size 2)])

(def bank-locs (filter #(not= central-bank-loc %) (random-locations bank-count)))

(defn setup-world []
  (let [truck-locs (random-locations truck-count)]
    (dosync
     (doseq [b bank-locs]
       (alter (cell-at b) assoc
              :bank 0))
     (doseq [b truck-locs]
       (alter (cell-at b) assoc :truck 0)))
    (dosync (alter (cell-at central-bank-loc) assoc :bank 0))
  truck-locs))

(defn bank-decay [_] 
  (when running (send-off *agent* #'bank-decay))
  (Thread/sleep 1000)
  (doseq [b bank-locs]
    (dosync (alter (cell-at b) update-in [:bank] / 2)))
  (dosync (alter (cell-at central-bank-loc) update-in [:bank] + 10000)))

(defn vector-to [l1 l2] (vec (map - l1 l2)))

(defn distance-sq [l1 l2] (apply + (map #(* (- %1 %2) (- %1 %2)) l1 l2)))

(defn normalize [v]
  (vec (map #(cond
               (> % 0) 1
               (< % 0) -1
               :else 0) v)))

(defn adjacent? [l1 l2]
  (let [v (vector-to l1 l2)]
    (every? #(<= (Math/abs %) 1) v)))

(defn nearest-bank-needing-money [orig]
  (let [banks (map (fn [l]
                     {:loc l
                      :money ((cell-at l) :bank)
                      :dist (distance-sq orig l)}) bank-locs)
        need-money (filter #(== (% :money) 0) banks)]
    (:loc (first (sort #(compare (%1 :dist) (%2 :dist)) need-money)))))

(defn move-towards [from towards]
  (let [[x y] from
        [dx dy] (normalize (vector-to from towards))
        new-loc [(+ dx x) (+ dy y)]]
    new-loc))

(defn truckate [truck-loc]
  (do
    (when running (send-off *agent* truckate))
    (Thread/sleep 50)
    (println "truckate")
    (let [cell (cell-at truck-loc)
          money (cell :truck)
          central-bank (cell-at central-bank-loc)]
      (if (== money 0)
        (when (adjacent? truck-loc central-bank-loc)
          (dosync
           (alter central-bank update-in [:bank] - 100)
           (ref-set cell {:truck 100})))
        (let [near-bank (nearest-bank-needing-money truck-loc)]
          (when (adjacent? truck-loc near-bank)
            (dosync
             (alter (cell-at near-bank) update-in [:bank] + money)
             (ref-set cell {:truck 0})))))
      (let [new-loc
            (if (== money 0)
              (move-towards truck-loc central-bank-loc)
              (move-towards truck-loc (nearest-bank-needing-money truck-loc)))]
        (dosync (alter (cell-at new-loc) @cell
        
            
       

(defn color-for-cell [c]
  (cond
    (c :truck) Color/RED
    (c :bank) Color/GREEN
    :else Color/WHITE))

(defn money-for-cell [c]
  (first (drop-while nil? (list (c :truck) (c :bank) 0))))
    
(defn paint-panel [g]
  (dosync 
   (doseq [x (range grid-size) y (range grid-size)]
     (let [cell (cell-at [x y])
           color (color-for-cell cell)
           money (money-for-cell cell)
           block-x (* x block-size-pix)
           block-y (* y block-size-pix)
           money-pix (* block-size-pix (/ money 100))]
       (doto g
         (.setColor (color-for-cell (cell-at [x y])))
         (.fillRect block-x block-y block-size-pix block-size-pix)
         (.setColor (Color/BLACK))
         (.fillOval block-x block-y money-pix money-pix))))))

(defn redraw-act [f]
  (do
    (when running (send-off *agent* redraw-act))
    (Thread/sleep 50)
    (doto f
      (.invalidate)
      (.repaint)
      (.validate))))

(def frame
  (agent
   (doto (new JFrame "banksim")
     (.setSize grid-size-pix grid-size-pix)
     (.setVisible true)
     (.add (proxy [JPanel] []
             (paintComponent [g]
               (proxy-super paintComponent g)
               (paint-panel g)))))))

(defn start []
  (let [truck-locs (setup-world)]
    (send-off frame redraw-act)
    (doseq [t truck-locs] (send-off (agent t) truckate))
    (send-off (agent nil) bank-decay)))