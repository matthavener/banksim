(ns banksim.core
  (:import (javax.swing JFrame JPanel))
  (:import (java.awt Graphics Color)))

(def grid-size 50) ; X by X grid
(def grid-size-pix 400) ; X by X pixels
(def block-size-pix (/ grid-size-pix grid-size)) ; size of each block
(def truck-count 50) 
(def bank-count 100)
(def bank-low-water 25) ; when a bank drops below this, it needs refill
(def bank-decay-sleep-ms 5000) ; decay banks every this many ms
(def bank-decay-amt 25) ; banks decay this many $
(def central-bank-inc-amt (* bank-count bank-decay-amt)) ; central inc
(def truck-money-amt 100) ; amt a truck can carry
(def truck-move-sleep-ms 300) ; trucks move every this many ms
(def running true)
(def redraw-delay-ms 100) ; redraw

(defn random-locations
  "Get n distinct locations on the grid"
  [s]
  (->>
   (repeatedly (fn [] [(rand-int grid-size) (rand-int grid-size)]))
   (distinct)
   (take s)))

; the cells that comprise the grid
(def cells 
  (vec (take grid-size (repeatedly
                        (fn [] (vec (take grid-size (repeatedly (fn [] (ref {}))))))))))

(defn cell-at [[x y]] ((cells x) y))

(def central-bank-loc [(/ grid-size 2) (/ grid-size 2)])

(def central-bank (cell-at central-bank-loc))

(def bank-locs (filter #(not= central-bank-loc %) (random-locations bank-count)))


(defn setup-world
  "place all the trucks, banks, central banks and return truck locations"
  []
  (let [truck-locs (random-locations truck-count)]
    (dosync
     (doseq [b bank-locs]
       (alter (cell-at b) assoc
              :bank 0))
     (doseq [b truck-locs]
       (alter (cell-at b) assoc :truck 0)))
    (dosync (alter (cell-at central-bank-loc) assoc :bank 0))
  truck-locs))

(defn bank-decay
  "actor function to decay money from banks and add money to central bank"
  [_]
  (when running (send-off *agent* bank-decay))
  (Thread/sleep bank-decay-sleep-ms)
  (doseq [b bank-locs]
    (dosync (alter (cell-at b) update-in [:bank] - bank-decay-amt)))
  (dosync (alter (cell-at central-bank-loc) update-in [:bank] + central-bank-inc-amt)))

(defn vector-to
  "return a vector from l1 to l2"
  [l1 l2]
  (vec (map - l1 l2)))

(defn distance-sq
  "return the squared distance from l1 to l2"
  [l1 l2]
  (apply + (map #(* % %) (vector-to l1 l2))))

(defn normalize
  "turn a vector into a single grid movement"
  [v]
  (vec (map #(cond
               (> % 0) 1
               (< % 0) -1
               :else 0) v)))

(defn adjacent?
  "true of l1 and l2 are adjacent on the grid"
  [l1 l2]
  (let [v (vector-to l1 l2)]
    (every? #(<= (Math/abs %) 1) v)))

(defn nearest-bank-needing-money
  "returns nearest bank that needs money (can be nil)"
  [orig]
  (let [banks (map (fn [l]
                     {:loc l
                      :money ((cell-at l) :bank)
                      :dist (distance-sq orig l)}) bank-locs)
        need-money (filter #(< (% :money) bank-low-water) banks)]
    (:loc (first (sort #(compare (%1 :dist) (%2 :dist)) need-money)))))

(defn bound-num
  "returns n bounded inclusively by low and high"
  [n low high]
  (max (min n high) low))

(defn move-towards
  "returns a new location (bounded by the grid) for moving to a location"
  [from towards]
  (let [[x y] from
        [dx dy] (normalize (vector-to towards from))
        [xub yub] [(+ dx x) (+ dy y)]
        ]
    [(bound-num xub 0 grid-size) (bound-num yub 0 grid-size)]))

(defn avail-for-truck
  "true if a truck can move to loc"
  [loc]
  (not (@(cell-at loc) :truck)))

(defn drive-truck-towards
  "move a truck towards a location, if its blocked, just move randomly, or not at all"
  [truck-loc towards]
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

(defn behave-truck
  "behave as a truck for truck at truck-loc and return new location"
  [truck-loc]
  (dosync   
   (let [cell (cell-at truck-loc)
         money (@cell :truck)
         central-bank (cell-at central-bank-loc)]
;     (println "truckate" truck-loc " money " money)  
     (if (== money 0)
       (when (adjacent? truck-loc central-bank-loc)
         (alter central-bank update-in [:bank] - truck-money-amt)
         (alter cell assoc :truck truck-money-amt))
       (when-let [near-bank (nearest-bank-needing-money truck-loc)]
         (when (adjacent? truck-loc near-bank)
           (alter (cell-at near-bank) update-in [:bank] + money)
           (alter cell assoc :truck 0))))
     (if (== money 0)
       (drive-truck-towards truck-loc central-bank-loc)
       (if-let [near-bank (nearest-bank-needing-money truck-loc)]
         (drive-truck-towards truck-loc near-bank)
         truck-loc)))))
  
(defn truckate
  "agent fun for trucks"
  [truck-loc]
  (do
    (Thread/sleep truck-move-sleep-ms)
    (when running (send-off *agent* truckate))
    (behave-truck truck-loc)))

(defn color-for-cell
  "color for a cell"
  [c]
  (cond
    (c :truck) Color/RED
    (c :bank) Color/GREEN
    :else Color/WHITE))

(defn money-for-cell
  "money at a cell"
  [c]
  (first (drop-while nil? (list (c :truck) (c :bank) 0))))

(defn paint-panel
  "paint world with g"
  [g]
  (dosync 
   (doseq [x (range grid-size) y (range grid-size)]
     (let [cell (cell-at [x y])
           color (color-for-cell cell)
           money (money-for-cell cell)
           block-x (* x block-size-pix)
           block-y (* y block-size-pix)
           money-pix (* block-size-pix (/ money truck-money-amt))]
       (doto g
         (.setColor (color-for-cell (cell-at [x y])))
         (.fillRect block-x block-y block-size-pix block-size-pix)
         (.setColor (Color/BLACK))
         (.fillOval block-x block-y money-pix money-pix))))))

(defn redraw-act
  "actor fun for redrawing"
  [f]
  (do
    (when running (send-off *agent* redraw-act))
    (Thread/sleep redraw-delay-ms)
    (doto f
      (.invalidate)
      (.repaint)
      (.validate))))

; frame we paint in
(def frame
  (agent
   (doto (new JFrame "banksim")
     (.setSize grid-size-pix grid-size-pix)
     (.setVisible true)
     (.add (proxy [JPanel] []
             (paintComponent [g]
               (proxy-super paintComponent g)
               (paint-panel g)))))))

; start and return truck actors
(defn start []
  (let [truck-locs (setup-world)
        truck-agents (map #(agent %) truck-locs)]
    (send-off frame redraw-act)
    (doseq [t truck-agents] (send-off t truckate))
    (send-off (agent nil) bank-decay)
    truck-agents))