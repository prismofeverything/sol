(ns sol.core-test
  (:use clojure.test)
  (:require [sol.core :as sol]))

(def game
  (sol/new-game [:blue :purple :green]))

(deftest instability-test
  (let [total-instability-cards (* (+ 2 (count (:order game))) sol/cards-per-suit)
        initial-pull 12]
    (is (not (sol/nova? game)))
    (is (= total-instability-cards (count (get-in game [:instability :deck]))))
    (let [game (sol/pull-instability-cards game initial-pull)]
      (is (= (- total-instability-cards initial-pull) (count (get-in game [:instability :deck]))))
      (is (get-in game [:instability :suit]))
      (let [game (sol/pull-instability-cards game (- total-instability-cards initial-pull))]
        (is (sol/nova? game))))))

(deftest move-test
  (let [orbit (get-in game [:players :blue :orbit])
        upper-cells (:upper sol/layer-cells)
        after (sol/radial-after orbit upper-cells)
        before (sol/radial-before orbit upper-cells)

        ;; launch some ships
        game (sol/launch-ship game :blue :upper)
        game (sol/launch-ship game :blue :upper)
        game (sol/launch-ship game :blue :lower)

        ;; move them around
        game (sol/move-ship 
              game :blue 
              [:upper orbit] 
              [:upper after])
        game (sol/move-ship 
              game :blue 
              [:upper orbit] 
              [:upper before])
        game (sol/move-ship 
              game :blue 
              [:upper before] 
              [:lower before])
        game (sol/move-ship 
              game :blue 
              [:lower orbit] 
              [:lower before])]

    ;; are they where they are supposed to be?
    (is (= 2 (get-in game [:players :blue :ships :board [:lower before]])))
    (is (= 1 (get-in game [:players :blue :ships :board [:upper after]])))
    (is (= 0 (get-in game [:players :blue :ships :board [:upper orbit]])))
    (is (= 2 (get-in game [:board [:lower before] :ships :blue])))
    (is (= 1 (get-in game [:board [:upper after] :ships :blue])))
    (is (= 0 (get-in game [:board [:upper orbit] :ships :blue])))))

(deftest harvest-pattern-test
  (is (sol/harvest-pattern? 
       (:board game) [[:convective 1] [:convective 12]] [:convective 13]))
  (is (sol/harvest-pattern? 
       (:board game) [[:core 1] [:core 3]] [:core 2]))
  (is (sol/harvest-pattern? 
       (:board game) [[:core 1] [:core 4]] [:core 5]))
  (is (not (sol/harvest-pattern? 
            (:board game) [[:core 1] [:core 4]] [:core 2]))))

(deftest build-pattern-test
  (is (sol/build-pattern? 
       (:board game) [[:convective 1] [:convective 13]] [:convective 13]))
  (is (sol/build-pattern? 
       (:board game) [[:convective 1] [:convective 13]] [:convective 1]))
  (is (not (sol/build-pattern? 
            (:board game) [[:convective 1] [:convective 13]] [:convective 2]))))

(deftest bridge-pattern-test
  (is (sol/bridge-pattern? 
       (:board game) [[:upper 3] [:lower 3]] [:convective 3]))
  (is (sol/bridge-pattern? 
       (:board game) [[:convective 3] [:radiative 2]] [:core 1]))
  (is (not (sol/bridge-pattern? 
            (:board game) [[:convective 3] [:radiative 2]] [:radiative 1])))
  (is (not (sol/bridge-pattern? 
            (:board game) [[:convective 3] [:radiative 2]] [:core 5]))))

(deftest transmit-pattern-test
  (is (sol/transmit-pattern? 
       (:board game) [[:upper 5] [:lower 5] [:convective 5]] [:convective 5]))
  (is (sol/transmit-pattern? 
       (:board game) [[:convective 5] [:radiative 4] [:core 3]] [:core 3]))
  (is (not (sol/transmit-pattern? 
            (:board game) [[:convective 5] [:radiative 4] [:core 3]] [:core 5])))
  (is (not (sol/transmit-pattern? 
            (:board game) [[:convective 5] [:radiative 4] [:core 5]] [:core 5])))
  (is (not (sol/transmit-pattern? 
            (:board game) [[:convective 5] [:radiative 4] [:radiative 5]] [:core 3]))))

(deftest convert-test
  (let [orbit (get-in game [:players :green :orbit])
        after (sol/radial-after orbit (:upper sol/layer-cells))
        game (sol/launch-ship game :green :upper)
        game (sol/launch-ship game :green :lower)
        game (sol/launch-ship game :green :lower)
        game (sol/launch-ship game :green :lower)]
    (try 
      (do
        (sol/move-ship 
         game :green
         [:lower orbit]
         [:convective orbit])
        (is false))
      (catch Exception e (do (println "NO BRIDGE:" (.getMessage e)) (is true))))
    (let [game (sol/convert-action 
                game :green 
                [[:upper orbit] [:lower orbit]] 
                [:convective orbit]
                :bridge)

          game (sol/move-ship
                game :green
                [:lower orbit]
                [:convective orbit])
          game (sol/move-ship
                game :green
                [:lower orbit]
                [:convective orbit])

          game (sol/launch-ship game :green :upper)
          game (sol/launch-ship game :green :lower)

          game (sol/convert-action 
                game :green 
                [[:upper orbit] [:lower orbit] [:convective orbit]] 
                [:convective orbit]
                :transmit)]

      (is (= 1 (get-in game [:players :green :ships :board [:convective orbit]])))
      (is (= 2 (get-in game [:players :green :transmit :reserve])))
      (is (get-in game [:players :green :transmit :board [:convective orbit]]))
      (is (= {:type :transmit :color :green} (get-in game [:board [:convective orbit] :station])))

      (try 
        (do
          (sol/convert-action 
           game :green 
           [[:lower orbit] [:lower after]]
           [:lower orbit]
           :build)
          (is false))
        (catch Exception e (do (println "NO SHIPS:" (.getMessage e)) (is true))))

      (try 
        (do
          (sol/convert-action 
           game :green 
           [[:lower orbit] [:upper after]]
           [:lower orbit]
           :build)
          (is false))
        (catch Exception e (do (println "WRONG PATTERN:" (.getMessage e)) (is true)))))))

(deftest activate-test
  (let [orbit (get-in game [:players :purple :orbit])
        before (sol/radial-before orbit (:upper sol/layer-cells))
        after (sol/radial-after orbit (:upper sol/layer-cells))
        after-after (sol/radial-after after (:upper sol/layer-cells))

        game (update-in game [:players :purple :ships :bay] #(+ % 7))
        game (sol/launch-ship game :purple :upper)
        game (sol/launch-ship game :purple :upper)
        game (sol/launch-ship game :purple :upper)
        game (sol/launch-ship game :purple :upper)
        game (sol/launch-ship game :purple :upper)
        game (sol/launch-ship game :purple :upper)
        game (sol/launch-ship game :purple :upper)
        game (sol/launch-ship game :purple :upper)

        game (sol/launch-ship game :purple :lower)
        game (sol/launch-ship game :purple :lower)
        game (sol/launch-ship game :purple :lower)
        game (sol/launch-ship game :purple :lower)
        game (sol/launch-ship game :purple :lower)
        game (sol/launch-ship game :purple :lower)
        game (sol/launch-ship game :purple :lower)

        game (sol/convert-action
              game :purple
              [[:upper orbit] [:lower orbit]]
              [:convective orbit]
              :bridge)

        ;; move into harvest pattern in :lower
        game (sol/move-ship
              game :purple
              [:lower orbit]
              [:lower after])
        game (sol/move-ship
              game :purple
              [:lower orbit]
              [:lower before])

        ;; complete transmit pattern in :convective
        game (sol/move-ship
              game :purple
              [:lower orbit]
              [:convective orbit])
        game (sol/move-ship
              game :purple
              [:lower orbit]
              [:convective orbit])

        ;; make harvest pattern in :upper
        game (sol/move-ship
              game :purple
              [:upper orbit]
              [:upper after])
        game (sol/move-ship
              game :purple
              [:upper after]
              [:upper after-after])

        ;; make build pattern in :upper
        game (sol/move-ship
              game :purple
              [:upper orbit]
              [:upper after])
        game (sol/move-ship
              game :purple
              [:upper orbit]
              [:upper after])
        game (sol/move-ship
              game :purple
              [:upper after]
              [:upper after-after])

        ;; move ships to :upper build and harvest stations
        game (sol/move-ship
              game :purple
              [:upper orbit]
              [:upper after])
        game (sol/move-ship
              game :purple
              [:upper orbit]
              [:upper after])
        game (sol/move-ship
              game :purple
              [:upper after]
              [:upper after-after])

        game (sol/convert-action
              game :purple
              [[:upper orbit] [:lower orbit] [:convective orbit]]
              [:convective orbit]
              :transmit)
        game (sol/convert-action
              game :purple
              [[:upper after] [:upper after-after]]
              [:upper after-after]
              :build)
        game (sol/convert-action
              game :purple
              [[:upper orbit] [:upper after-after]]
              [:upper after]
              :harvest)
        game (sol/convert-action
              game :purple
              [[:lower before] [:lower after]]
              [:lower orbit]
              :harvest)

        game (sol/activate-action
              game :purple
              [[:lower orbit] [:upper after]])

        _ (is (= 6 (get-in game [:players :purple :energy])))
        _ (is (= 2 (get-in game [:players :purple :ships :bay])))
        _ (is (= 1 (count (get-in game [:players :purple :events]))))

        game (sol/activate-action
              game :purple
              [[:upper after-after]])

        _ (is (= 5 (get-in game [:players :purple :energy])))
        _ (is (= 4 (get-in game [:players :purple :ships :bay])))

        game (sol/launch-ship game :purple :upper)
        game (sol/move-ship
              game :purple
              [:upper orbit]
              [:upper after])

        game (sol/activate-action
              game :purple
              [[:upper after]])

        _ (is (= 6 (get-in game [:players :purple :energy])))
        _ (is (= 4 (get-in game [:players :purple :ships :bay])))

        game (sol/activate-action
              game :purple
              [[:convective orbit]])

        _ (is (= 3 (get-in game [:players :purple :energy])))
        _ (is (= 3 (get-in game [:players :purple :ark])))

        ;; one for bridge conversion, one for transmit conversion and another for transmit activation
        cards-drawn 3
        total-cards (* sol/cards-per-suit (+ 2 (count (get-in game [:players]))))
        cards-left (count (get-in game [:instability :deck]))
        _ (is (= (- total-cards cards-drawn) cards-left))]))

