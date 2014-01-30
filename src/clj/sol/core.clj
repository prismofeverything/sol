(ns sol.core)

(def sol-layers
  [{:name :upper :cells 13 :reward 1 :instability 0}
   {:name :lower :cells 13 :reward 2 :instability 0}
   {:name :convective :cells 13 :reward 3 :instability 1}
   {:name :radiative :cells 8 :reward 5 :instability 2}
   {:name :core :cells 5 :reward 8 :instability 3}
   {:name :wormhole :cells 1 :reward 2 :instability 1}])

(defn layer-map
  [layers key]
  (into 
   {} 
   (map 
    (juxt :name key) 
    layers)))

(def layer-names 
  (map :name sol-layers))

(def layer-cells (layer-map sol-layers :cells))
(def layer-reward (layer-map sol-layers :reward))
(def layer-instability (layer-map sol-layers :instability))

(def sol-epsilon 0.013)

(defn layer-index
  [layer]
  (.indexOf layer-names layer))

(def cards-per-suit 15)

(def instability-suits
  [:explosion
   :reverberation 
   :subduction 
   :oscillation 
   :collision 
   :striation
   :reflection
   :tension
   :eruption
   :refraction
   :crystallization
   :condensation
   :convolution
   :dissipation
   :ionization
   :absorption
   :revolution
   :rotation
   :fluctuation
   :friction
   :acceleration])

(def instability-limit 13)

(defn construct-deck
  [suits]
  (shuffle (mapcat (partial repeat cards-per-suit) suits)))

(defn construct-instability-deck
  [colors]
  (let [total-suits (+ 2 (count colors))
        instability-suit (first instability-suits)
        other-suits (take (dec total-suits) (shuffle (rest instability-suits)))]
    (construct-deck (cons instability-suit other-suits))))

(defn diff-ns
  "take the difference between every subsequent number in the list"
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
    (or 
     (and (<= (+ a-start epsilon) b-start) (< b-start (- a-end epsilon)))
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
  (let [neighbors (all-neighbors layers)]
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

(defn place-station
  [board color at type]
  (if-let [pre-existing (get-in board [at :station])]
    (throw 
     (ex-info 
      (format 
       "%s player cannot add a %s station at %s because there is already a %s %s station there!"
       color type at (:color pre-existing) (:type pre-existing))))
    (assoc-in 
     board [at :station] 
     {:color color
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
        board (place-station board color harvest-start :harvest)
        board (place-station board color build-start :build)]
    [board player]))

(defn fresh-instability
  [colors]
  {:level 0
   :deck (construct-instability-deck colors)
   :suit nil})

(defn new-game
  [colors]
  (let [starts (range 13 1 (/ -12 (count colors)))
        board (layout-board (map (juxt :name :cells) sol-layers))
        [board players] (reduce
                         (fn [[board players] [color start]]
                           (let [[board player] (make-player board color start)]
                             [board (assoc players (:color player) player)]))
                         [board {}] (map vector colors starts))]
    {:players players
     :board board
     :order colors
     :instability (fresh-instability colors)}))

(defn advance-orbit
  [player]
  (update-in player [:orbit] #(radial-after % 13)))

(defn pull-instability-cards
  [game number]
  (if (zero? number)
    game
    (let [cards-drawn (take number (get-in game [:instability :deck]))
          explosions (count (filter #(= % (first instability-suits)) cards-drawn))]
      (-> game 
          (update-in [:instability :level] #(+ % explosions))
          (update-in [:instability :deck] (partial drop number))
          (update-in [:instability :suit] (constantly (last cards-drawn)))))))

(defn nova?
  [game]
  (>= (get-in game [:instability :level]) instability-limit))

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
  (-> game 
      (update-in [:players color :ships :board at] inc-nil)
      (update-in [:board at :ships color] inc-nil)))

(defn remove-ship
  [game color at]
  (if (void? (get-in game [:players color :ships :board at]))
    (throw 
     (ex-info 
      (format "cannot remove %s ship from %s because there are no %s ships there!" color at color) color))
    (-> game 
        (update-in [:players color :ships :board at] dec)
        (update-in [:board at :ships color] dec))))

(defn remove-ships
  [game color cells]
  (reduce 
   (fn [game at]
     (remove-ship game color at))
   game cells))

(defn move-ship
  [game color from to]
  (-> game 
      (remove-ship color from)
      (place-ship color to)))

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
  [board cells to]
  (and
   (= 3 (count cells))
   (let [[a b c] (sort-by (comp layer-index first) cells)
         layer-order (map (comp layer-index first) [a b c])
         diffs (diff-ns layer-order)]
     (and
      (= c to)
      (= diffs (list 1 1))
      (adjacent-cells? board a b)
      (adjacent-cells? board b c)))))

(def pattern-present?
  {:harvest harvest-pattern?
   :build build-pattern?
   :bridge bridge-pattern?
   :transmit transmit-pattern?})

(defn return-ships-to-reserve
  [game color cells]
  (-> game
      (remove-ships color cells)
      (update-in [:players color :ships :reserve] #(+ (count cells) %))))

(defn player-station
  [player at type]
  (if (>= 0 (get-in player [type :reserve]))
    (throw
     (ex-info
      (format "%s player has no %s stations left in their reserve!" (:color player) type) player))
    (-> player 
        (update-in [type :reserve] dec)
        (update-in [type :board at] (constantly true)))))

(defn convert-station
  [game color cells at type]
  (-> game 
      (update-in [:players color] #(player-station % at type))
      (update-in [:board] #(place-station % color at type))))

(defn convert-bridge
  [game color cells at]
  (let [[_ source] (sort-by (comp layer-index first) cells)]
    (-> game 
        (update-in [:players color :bridges :reserve] dec)
        (update-in [:players color :bridges :board source] #(conj % at))
        (update-in [:players color :bridges :board at] #(conj % source))
        (update-in [:board source :bridges at] (constantly color))
        (update-in [:board at :bridges source] (constantly color)))))

(defn convert-ships
  [game color cells at type]
  (if (= type :bridge)
    (convert-bridge game color cells at)
    (convert-station game color cells at type)))

(defn convert-action
  [game color cells at type]
  (let [player (get-player game color)]
    (cond
     (not ((get pattern-present? type) (:board game) cells at))
     (throw 
      (ex-info 
       (format "%s player is not making a %s pattern with %s!" color type cells) {}))

     (not (every? #(ships-at? player %) cells))
     (throw (ex-info (format "%s player does not have ships in all of %s!" color cells) {}))

     :else
     (-> game
         (return-ships-to-reserve color cells)
         (convert-ships color cells at type)
         (pull-instability-cards (get layer-instability (first at)))))))
