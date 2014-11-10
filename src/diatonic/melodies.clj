(ns diatonic.melodies
  (:require [clojure.data.generators :as g]))

(defn melody-notes [context]
  '())

(defn init-melody [context]
  (if-not (:melody-notes context)
    (assoc context :melody-notes (melody-notes context))
    context))

