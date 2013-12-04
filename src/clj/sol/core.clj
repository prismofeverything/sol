(ns sol.core
  (:require [clojure.string :as string]))

(defn uuid
  []
  (str (java.util.UUID/randomUUID)))

(defrecord Cell [zone number])
(defrecord Ship [owner cell])
(defrecord Bridge [owner from to])
(defrecord Station [owner cell type])
(defrecord Player [id ships energy transmit structures moves])
(defrecord Board [cells players instability])

(defn initialize-board
  []
  (let [cells {:core (map )}]))
