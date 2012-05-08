(ns banksim.core
  (:import (javax.swing JFrame JPanel))
  (:import (java.awt Graphics Color)))

(def grid-size 10)
(def grid-size-pix 400)
(def block-size-pix (/ grid-size-pix grid-size))
(def truck-count 3)
(def bank-count 10)
(def bank-decay-sleep-ms 1000)
(def truck-move-sleep-ms 100)
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

(def central-bank (cell-at central-bank-loc))

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
  (when running (send-off *agent* bank-decay))
  (Thread/sleep bank-decay-sleep-ms)
  (doseq [b bank-locs]
    (dosync (alter (cell-at b) update-in [:bank] quot 2)))
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
        need-money (filter #(< (% :money) 25) banks)]
    (:loc (first (sort #(compare (%1 :dist) (%2 :dist)) need-money)))))

(defn bound-num [n low high]
  (max (min n high) low))

(defn move-towards [from towards]
  (let [[x y] from
        [dx dy] (normalize (vector-to towards from))
        [xub yub] [(+ dx x) (+ dy y)]
        ]
    [(bound-num xub 0 grid-size) (bound-num yub 0 grid-size)]))

(defn avail-for-truck [loc] (not (@(cell-at loc) :truck)))

(defn drive-truck-towards [truck-loc towards]
  (let [ideal (move-towards truck-loc towards)
        random-move [(- (rand-int 3) 1) (- (rand-int 3) 1)]
        random (move-towards truck-loc random-move)
        next-loc (if (avail-for-truck ideal)
                   ideal
                   (if (avail-for-truck random)
                     random
                     truck-loc))
        old-cell (cell-at truck-loc)
        new-cell (cell-at next-loc)
        money (@old-cell :truck)]
;    (println "move " truck-loc " to " next-loc)
    (alter old-cell dissoc :truck)
    (alter new-cell assoc :truck money)
    next-loc))

(defn behave-truck [truck-loc]
  (dosync   
   (let [cell (cell-at truck-loc)
         money (@cell :truck)
         central-bank (cell-at central-bank-loc)]
;     (println "truckate" truck-loc " money " money)  
     (if (== money 0)
       (when (adjacent? truck-loc central-bank-loc)
         (alter central-bank update-in [:bank] - 100)
         (alter cell assoc :truck 100))
       (when-let [near-bank (nearest-bank-needing-money truck-loc)]
         (when (adjacent? truck-loc near-bank)
           (alter (cell-at near-bank) update-in [:bank] + money)
           (alter cell assoc :truck 0))))
     (if (== money 0)
       (drive-truck-towards truck-loc central-bank-loc)
       (if-let [near-bank (nearest-bank-needing-money truck-loc)]
         (drive-truck-towards truck-loc near-bank)
         truck-loc)))))
  
(defn truckate [truck-loc]
  (do
    (Thread/sleep truck-move-sleep-ms)
    (when running (send-off *agent* truckate))
    (behave-truck truck-loc)))

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
  (let [truck-locs (setup-world)
        truck-agents (map #(agent %) truck-locs)]
    (send-off frame redraw-act)
    (doseq [t truck-agents] (send-off t truckate))
    (send-off (agent nil) bank-decay)
    truck-agents))