(ns sol.core)

(def sol-layers 
  [[:upper 13]
   [:lower 13]
   [:convective 13]
   [:radiative 8]
   [:core 5]
   [:wormhole 1]])

(def layer-cells
  (into {} sol-layers))

(def layer-names 
  (map first sol-layers))

(def sol-epsilon 0.013)

(defn layer-index
  [layer]
  (.indexOf layer-names layer))

(defn diff-ns
  [ns]
  (loop [prev (first ns) 
         ns (rest ns) 
         diffs []] 
    (if (empty? ns) 
      diffs 
      (recur 
       (first ns) 
       (rest ns) 
       (conj diffs (- (first ns) prev))))))

(defn radial-before
  [n limit]
  (let [before (dec n)]
    (if (zero? before) limit before)))

(defn radial-after
  [n limit]
  (let [after (inc n)]
    (if (> after limit) 1 after)))

(defn cells-range
  [cells]
  (range 1 (inc cells)))

(defn bound-interval
  [interval]
  (if (> 1 interval) (- interval 1)))

(defn overlap?
  [a-start a-interval b-start b-interval epsilon]
  (let [a-end (+ a-start a-interval)
        b-end (+ b-start b-interval)]
    (or (and (<= (+ a-start epsilon) b-start) (< b-start (- a-end epsilon)))
        (and (< (+ a-start epsilon) b-end) (<= b-end (- a-end epsilon)))
        (and (<= (+ b-start epsilon) a-start) (< a-start (- b-end epsilon)))
        (and (< (+ b-start epsilon) a-end) (<= a-end (- b-end epsilon))))))

(defn radial-overlap?
  [a-start a-interval b-start b-interval epsilon]
  (or
   (and (= a-start b-start) (= a-interval b-interval))
   (overlap? a-start a-interval b-start b-interval epsilon)
   (overlap? (+ 1 a-start) (+ 1 a-interval) b-start b-interval epsilon)
   (overlap? a-start a-interval (+ 1 b-start) (+ 1 b-interval) epsilon)))

(defn find-start
  [cell interval]
  (- (* interval (dec cell)) (/ interval 2)))

(defn mediate-phase
  [cell here-key here-cells other-key other-cells epsilon]
  (if (nil? other-key)
    []
    (let [here-interval (/ 1 here-cells)
          other-interval (/ 1 other-cells)
          here-start (find-start cell here-interval)
          overlaps (filter 
                    #(radial-overlap? here-start here-interval (find-start % other-interval) other-interval epsilon)
                    (cells-range other-cells))]
      (map
       (fn [overlapping]
         [other-key overlapping])
       overlaps))))

(defn find-neighbors
  [[above-key above-cells] 
   [here-key here-cells] 
   [below-key below-cells]
   epsilon]
  (if-not here-key
    {}
    (reduce
     (fn [layer cell]
       (let [key [here-key cell]
             here (remove 
                   #(= key %) 
                   (set [[here-key (radial-before cell here-cells)] [here-key (radial-after cell here-cells)]]))
             above (mediate-phase cell here-key here-cells above-key above-cells epsilon)
             below (mediate-phase cell here-key here-cells below-key below-cells epsilon)]
         (assoc layer key (vec (concat above here below)))))
     {} (cells-range here-cells))))

(defn all-neighbors
  [layers]
  (loop [neighbors {}
         layers (cons [] layers)]
    (if (empty? layers)
      neighbors
      (let [[above here below] layers
            layer-neighbors (find-neighbors above here below sol-epsilon)]
        (recur (merge neighbors layer-neighbors) (rest layers))))))

(defn layout-board
  [layers]
  (let [neighbors (all-neighbors sol-layers)]
    (into 
     {}
     (map 
      (fn [[[layer cell] neighborhood]]
        [[layer cell] 
         {:layer layer 
          :cell cell
          :neighbors neighborhood
          :ships {}
          :station nil
          :bridges {}}])
      neighbors))))

(defn add-station-to-board
  [board player at type]
  (if-let [pre-existing (get-in board [at :station])]
    (throw 
     (ex-info 
      (format 
       "%s player cannot add a %s station at %s because there is already a %s %s station there!"
       (:color player) type at (:color pre-existing) (:type pre-existing))))
    (assoc-in 
     board [at :station] 
     {:color (:color player) 
      :type type})))

(defn make-player
  [board color start]
  (let [after (radial-after start 13)
        harvest-start [:upper after]
        build-start [:upper (radial-after after 13)]
        player {:color color
                :energy 0
                :orbit start
                :ships {:reserve 13 :bay 8 :board {}}
                :bridges {:reserve 13 :board {}}
                :build {:reserve 4 :board {build-start true}}
                :harvest {:reserve 4 :board {harvest-start true}}
                :transmit {:reserve 3 :board {}}
                :ark 0}
        board (add-station-to-board board player harvest-start :harvest)
        board (add-station-to-board board player build-start :build)]
    [board player]))

(defn new-game
  [colors]
  (let [starts (range 13 1 (/ -12 (count colors)))
        board (layout-board sol-layers)
        [board players] (reduce
                         (fn [[board players] [color start]]
                           (let [[board player] (make-player board color start)]
                             [board (assoc players (:color player) player)]))
                         [board {}] (map vector colors starts))]
    {:players players
     :board board
     :order colors}))

(defn advance-orbit
  [player]
  (update-in player [:orbit] #(radial-after % 13)))

(defn ships-in-bay?
  [player]
  (> (get-in player [:ships :bay]) 0))

(defn ships-at?
  [player at]
  (let [ships-at (get-in player [:ships :board at])]
    (and ships-at (< 0 ships-at))))

(defn adjacent-cells?
  [board from to]
  (some #{to} (get-in board [from :neighbors])))

(def barrier-layers?
  #{(list :convective :lower)
    (list :convective :radiative)
    (list :core :radiative)})

(defn bridge-required?
  [[from-layer from-cell] [to-layer to-cell]]
  (barrier-layers? (sort [from-layer to-layer])))

(defn find-bridge
  [board from to]
  (get-in board [from :bridges to]))

(defn inc-nil
  [n]
  (if n (inc n) 1))

(defn void?
  [x]
  (or (nil? x) (<= x 0)))

(defn place-ship
  [game color at]
  (let [game (update-in game [:players color :ships :board at] inc-nil)
        game (update-in game [:board at :ships color] inc-nil)]
    game))

(defn remove-ship
  [game color at]
  (if (void? (get-in game [:players color :ships :board at]))
    (throw 
     (ex-info 
      (format "cannot remove %s ship from %s because there are no %s ships there!" color at color) color))
    (let [game (update-in game [:players color :ships :board at] dec)
          game (update-in game [:board at :ships color] dec)]
      game)))

(defn move-ship
  [game color from to]
  (let [game (remove-ship game color from)]
    (place-ship game color to)))

(defn get-player
  [game color]
  (get-in game [:players color]))

(defn launch-ship
  [game color layer]
  (let [player (get-player game color)]
    (if-not (ships-in-bay? player)
      (throw (ex-info (format "%s player has no ships in bay!" color) player))
      (let [at [layer (:orbit player)]
            game (update-in game [:players color :ships :bay] dec)]
        (place-ship game color at)))))

(defn gain-energy
  [game color amount]
  (update-in game [:players color :energy] #(+ % amount)))

(defn move-action
  [game color from to]
  (let [player (get-player game color)]
    (cond 
     (not (ships-at? player from))
     (throw (ex-info (format "%s player has no ships at %s!" color from) player))

     (not (adjacent-cells? (:board game) from to))
     (throw 
      (ex-info 
       (format "%s player cannot move ship from %s to %s because they are not adjacent!" color from to) player))
     
     (bridge-required? from to)
     (if-let [bridge (find-bridge (:board game) from to)]
       (let [game (move-ship game color from to)]
         (if-not (= bridge color)
           (gain-energy game bridge 1)
           game))
       (throw 
        (ex-info 
         (format "%s player cannot move ship from %s to %s because there is no bridge present!" color from to) {})))

     :else (move-ship game color from to))))

(defn harvest-pattern?
  [board cells [layer cell]]
  (and 
   (= 2 (count cells))
   (= 1 (count (set (cons layer (map first cells)))))
   (let [[[_ a-cell] [_ b-cell]] cells
         cells-in-layer (get layer-cells layer)
         before (radial-before cell cells-in-layer)
         after (radial-after cell cells-in-layer)]
     (= (sort [a-cell b-cell]) (sort [before after])))))

(defn build-pattern?
  [board cells [layer cell]]
  (and 
   (= 2 (count cells))
   (= 1 (count (set (cons layer (map first cells)))))
   (let [[[_ a-cell] [_ b-cell]] cells
         cells-in-layer (get layer-cells layer)
         before (radial-before cell cells-in-layer)
         after (radial-after cell cells-in-layer)
         signature (sort [a-cell b-cell])]
     (or 
      (= signature (sort [cell after]))
      (= signature (sort [cell before]))))))

(defn bridge-pattern?
  [board cells to]
  (and
   (= 2 (count cells))
   (let [[a b] (sort-by (comp layer-index first) cells)
         layer-order (map (comp layer-index first) (concat [a b] [to]))
         diffs (diff-ns layer-order)]
     (and 
      (= diffs (list 1 1))
      (adjacent-cells? board a b)
      (adjacent-cells? board b to)))))

(defn transmit-pattern?
  [board cells [layer cell]]
  (and
   (= 3 (count cells))
   (let [[a b c] cells])))

(defn pattern-present?
  [game color at type]
  ())

(defn convert-action
  [game color at type])
