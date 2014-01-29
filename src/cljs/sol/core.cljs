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
(def paper-dimensions {:width 820 :height 820})
;; (def paper-dimensions {:width (.-innerWidth js/window) :height (.-innerHeight js/window)})
;; (def paper-dimensions {:width 2800 :height 1380})
(def draw (js/Snap (:width paper-dimensions) (:height paper-dimensions)))
(def layer-radius 75)
(def board-radius 400)
(def sun-radius (- board-radius layer-radius))
(def orbit-opacity 0.8)
(def zone-opacity 0.5)
(def starmax 1000)
;; (def starmax 2000)
(def board-center 
  {:x (* 0.5 (:width paper-dimensions))
   :y (* 0.5 (:height paper-dimensions))})

(defn log
  [e]
  (.log js/console e))

(defn attr 
  [obj attrs]
  (.attr obj (clj->js attrs)))

(defn move-to
  [point]
  (str 
   "M" (js/Math.floor (:x point)) 
   "," (js/Math.floor (:y point))))

(defn line-to
  [point]
  (str 
   "L" (js/Math.floor (:x point)) 
   "," (js/Math.floor (:y point))))

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
    (range half two-pi interval)
    ;; (range 0 two-pi interval)
))

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
   [:inner-orbit "#227733" 13]
   [:convective-zone "#ffee22" 13]
   [:radiative-zone "#d87f1b" 8]
   [:core "#aa1100" 5]
   [:warp "#000000" 0]])


;; (def membrane-colors ["#ffffff" "#49AD1A" "#ffffff" "#ffffff" "#000000" "#000000"])
(def membrane-colors ["#ffffff" "#fc8a22" "#ffffff" "#ffffff" "#000000" "#000000"])
(def radial-colors ["#ffffff" "#ffffff" "#ffffff" "#ffffff" "#000000" "#000000"])

;; (defn draw-sun
;;   []
;;   (let [inner-radius sun-radius]
;;     (attr 
;;      (.filter
;;       draw 
;;       (str 
;;        "<feImage xlink:href=\"img/SOL.jpg\" result=\"sun\" x=\"" (- (:x board-center) inner-radius) 
;;        "\" y=\"" (- (:y board-center) inner-radius) 
;;        "\" width=\"" (* 2 inner-radius) 
;;        "\" height=\"" (* 2 inner-radius)
;;        "\" /><feBlend mode=\"darken\" in2=\"sun\" />"))
;;      {:id "sun"})))

(defn draw-sun
  []
  (let [inner-radius sun-radius]
    (.image
     draw "img/SOL.jpg"
     (- (:x board-center) inner-radius) 
     (- (:y board-center) inner-radius) (* 2 inner-radius) (* 2 inner-radius))))

(defn distance
  [a b]
  (let [dx (- (:x a) (:x b))
        dy (- (:y a) (:y b))]
    (js/Math.sqrt (+ (* dx dx) (* dy dy)))))

(defn inside-sun?
  [p]
  (let [d (distance p board-center)]
    (< d (- sun-radius layer-radius))))

(defn random-point
  []
  {:x (* (:width paper-dimensions) (js/Math.random))
   :y (* (:height paper-dimensions) (js/Math.random))})

(defn draw-stars
  []
  (mapv
   (fn [_]
     (let [star-position (random-point)]
       (if-not (inside-sun? star-position)
         (circle
          draw
          (merge 
           star-position
           {:radius (* (js/Math.log (+ js/Math.E (* 0.3 (js/Math.random)))))
            :opacity (js/Math.random)
            :fill "#ffffff"
            :stroke "#ffffff"})))))
   (range starmax)))

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
        layers (map vector board (concat (rest board) [nil]))
        sun (draw-sun)
        cells (mapv 
               (fn [[outer inner]]
                 (let [angles (radial-angles (:cells outer))
                       zone-radius (- (:radius outer) (* 0.5 layer-radius))
                       zone (if-not (< zone-radius 0) 
                              (circle
                               draw 
                               (merge
                                board-center
                                {:radius zone-radius
                                 :stroke-width layer-radius
                                 ;; :filter (.filter draw "<feBlend mode=\"multiply\" in=\"SourceGraphic\" in2=\"BackgroundImage\">")
                                 :fill "none" ;; (:color outer)
                                 :fill-opacity 0
                                 :stroke-opacity (if (> 2 (:number outer)) orbit-opacity zone-opacity)
                                 :stroke (:color outer)})))
                       radials (if inner 
                                 (mapv 
                                  (fn [angle]
                                    (radial
                                     draw
                                     {:center board-center
                                      :angle (+ (* 1.0 js/Math.PI) angle)
                                      :begin-radius (:radius outer)
                                      :end-radius (:radius inner)
                                      :color (get radial-colors (:number outer)) ;; (:color inner)
                                      }))
                                  angles))
                       membrane-color (get membrane-colors (:number outer))
                       membrane (circle
                                 draw 
                                 (merge
                                  board-center
                                  {:radius (:radius outer) :stroke-width 3 :fill (:color outer)
                                   :fill-opacity 0 ;; (if (< (:number outer) 2) 0.5 0.5)
                                   :stroke membrane-color}))]
                   (assoc outer
                     :membrane membrane
                     :zone zone
                     :radials radials)))
               layers)
        core-radius (- board-radius (* 5 layer-radius))
        core (circle 
              draw 
              (merge 
               board-center 
               {:radius core-radius 
                :stroke-width 0
                :fill "#000000" 
                :fill-opacity 1 ;; zone-opacity
                }))
        stars (draw-stars)]
    cells))

(defn init
  [data]
  (draw-board)
  (log (.toString draw)))

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

