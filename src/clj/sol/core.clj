(ns sol.core
  (:require [clojure.string :as string]))

;; positions on the game board will be uniquely identified as a pair (layer cell)

(def sol-layers 
  [[:upper 13]
   [:lower 13]
   [:convective 13]
   [:radiative 8]
   [:core 5]])

(def sol-epsilon 0.013)

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
       (let [here [[here-key (radial-before cell here-cells)] [here-key (radial-after cell here-cells)]]
             above (mediate-phase cell here-key here-cells above-key above-cells epsilon)
             below (mediate-phase cell here-key here-cells below-key below-cells epsilon)]
         (assoc layer [here-key cell] (vec (concat above here below)))))
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
        [[layer cell] {:layer layer :cell cell :neighbors neighborhood}])
      neighbors))))
