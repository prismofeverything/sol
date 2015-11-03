(ns sol.flow)

(defn flow
  [{:keys [levels transitions] :as state}]
  (let [intermediate (into {} (map (fn [sym] [sym 0]) (keys levels)))
        delta
        (reduce
         (fn [intermediate transition]
           (let [delta (transition levels)]
             (merge-with + intermediate delta)))
         intermediate transitions)]
    (update state :levels (partial merge-with +) delta)))

(defn support
  [body portion]
  (int (Math/ceil (/ body portion))))

(defn base-flow
  [{:keys [population-per-food birth-rate recycle-rate]}]
  {:levels
   {:population 5
    :food 5
    :waste 5}

   :transitions
   [(fn eat
      [{:keys [food population]}]
      (let [hunger (support population population-per-food)
            eaten (min hunger food)]
        {:waste eaten
         :food (* -1 eaten)}))

    (fn breed
      [{:keys [food population waste]}]
      (let [hunger (support population population-per-food)
            recycled (quot waste recycle-rate)
            fed (* food population-per-food)
            babies (if (<= hunger food)
                     (min
                      (inc (quot population birth-rate))
                      (- waste recycled))
                     (- fed population))]
        {:population babies
         :waste (* -1 babies)}))

    (fn harvest
      [{:keys [waste]}]
      (let [recycled (quot waste recycle-rate)]
        {:food recycled
         :waste (* -1 recycled)}))]})
