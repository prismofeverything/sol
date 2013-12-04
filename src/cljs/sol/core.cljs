(ns sol.core
  (:require 
   [clojure.string :as string]
   [cljs.core.async :refer [chan <! >! put!]]
   [cljs.reader :as reader]
   [domina :as dom]
   [domina.css :as css]
   [domina.events :as events]
   [singult.core :as sing]
   [sol.connect :as connect]
   [sol.util :as util])
  (:require-macros 
   [cljs.core.async.macros :refer [go]]))

(def send (chan))
(def receive (chan))

(def ws-url "ws://localhost:21112/async")
(def ws (new js/WebSocket ws-url))

(def two-pi (* 2 js/Math.PI))
(def paper-dimensions {:width 2800 :height 1380})
(def draw (js/Raphael 0 0 (:width paper-dimensions) (:height paper-dimensions)))
(def layer-radius 75)
(def board-radius 400)
(def board-center {:x (* 0.5 (.-innerWidth js/window)) :y (* 0.5 (.-innerHeight js/window))})

(defn log
  [e]
  (.log js/console e))

(defn attr 
  [obj attrs]
  (.attr obj (clj->js attrs)))

(defn move-to
  [point]
  (log (str point))
  (str "M" (js/Math.floor (:x point)) "," (js/Math.floor (:y point))))

(defn line-to
  [point]
  (log (str point))
  (str "L" (js/Math.floor (:x point)) "," (js/Math.floor (:y point))))

(defn line
  [draw {:keys [begin end color width]}]
  (let [l (.path draw (str (move-to begin) (line-to end)))]
    (attr l {:stroke color :stroke-width width})))

(defn circle
  [draw opts]
  (let [{:keys [x y radius]} opts
        attrs (dissoc opts :x :y :radius)]
    (-> (.circle draw x y radius)
        (attr attrs))))

(defn radial-angles
  [divisions]
  (let [interval (/ two-pi divisions)
        half (* 0.5 interval)]
    (range half two-pi interval)))

(defn radial-offset
  [center angle radius]
  {:x (+ (:x center) (* radius (js/Math.cos angle)))
   :y (+ (:y center) (* radius (js/Math.sin angle)))})

(defn radial
  [draw {:keys [center angle begin-radius end-radius color]}]
  (let [begin (radial-offset center angle begin-radius)
        end (radial-offset center angle end-radius)]
    (line 
     draw 
     {:begin begin
      :end end
      :color color
      :width 3})))

(def sol-colors
  [[:outer-orbit "#2255aa" 13]
   [:inner-orbit "#22aa55" 13]
   [:convective-zone "#eecc55" 13]
   [:radiative-zone "#de7f1b" 8]
   [:core "#aa3322" 5]
   [:warp "#000000" 0]])

(defn draw-stars
  []
  (mapv
   (fn [_]
     (circle
      draw
      {:x (* (:width paper-dimensions) (js/Math.random))
       :y (* (:height paper-dimensions) (js/Math.random))
       :radius (* (js/Math.log (+ js/Math.E (* 3 (js/Math.random)))))
       :opacity (js/Math.random)
       :fill "#ffffff"
       :stroke "#ffffff"}))
   (range 1000)))

(defn draw-board
  []
  (let [board (mapv
               (fn [[layer color cells] number]
                 (let [radius (- board-radius (* number layer-radius))]
                   {:layer layer
                    :cells cells
                    :number number
                    :radius radius
                    :color color}))
               sol-colors (range))
        layers (map vector board (concat (rest board) [nil]))]
    (mapv 
     (fn [[outer inner]]
       (let [angles (radial-angles (:cells outer))
             spec (merge
                   board-center
                   {:radius (:radius outer) :stroke-width 3 :fill-opacity (if (< (:number outer) 2) 1 1)
                    :fill (:color outer) :stroke (or (:color inner) (:color outer))})
             zone (circle draw spec)
             radials (if inner 
                       (mapv 
                        (fn [angle]
                          (radial
                           draw
                           {:center board-center
                            :angle angle
                            :begin-radius (:radius outer)
                            :end-radius (:radius inner)
                            :color (:color inner)}))
                        angles))]
         (assoc outer
           :zone zone
           :radials radials)))
     layers)))

(defn init
  [data]
  (draw-stars)
  (draw-board))

(defn dispatch-message
  []
  (go
   (while true
     (let [msg (<! receive)
           raw (.-data msg)
           data (reader/read-string raw)]
       (condp = (:op data)
         :init (init data)
         (log (str "op not supported! " data)))))))

(defn make-sender
  []
  (log "HELLO")
  (util/event-chan send :click js/document.body :click {})
  (go
   (while true
     (let [[id event data] (<! send)]
       (condp = id
         :click (log "click!"))))))

(defn make-receiver []
  (set! 
   (.-onmessage ws)
   (fn [msg]
     (put! receive msg)))
  (set!
   (.-onopen ws)
   (fn [msg] 
     (.send ws {:op :init})))
  (dispatch-message))

(defn init!
  []
  (make-sender)
  (make-receiver))

(def on-load
  (set! (.-onload js/window) init!))

(connect/connect)

