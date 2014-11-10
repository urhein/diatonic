(ns diatonic.composition
  (:require [clojure.data.generators :as g]
            [diatonic.chords :as c]
            [diatonic.drums :as d]
            [diatonic.midi :as m]
            [diatonic.scales :as s]))

(defmacro with-seed [seed & body]
  "Creates a PRNG with the specified seed and binds it to be used in
  the body."
  `(binding [g/*rnd* (java.util.Random. ~seed)]
     (do ~@body)))

(defn init-context
  "Initializes an empty context with default values."
  ([] (init-context {}))
  ([context] (merge {:notes-per-bar 32 :bars 4} context)))

(defn init-base-scale
  "Initializes the base scale of the specified context, if it's not
  already present."
  [context]
  (if (:base-scale context)
    context
    (assoc context :base-scale
           ((if (g/boolean) s/ionic-scale s/aeolic-scale) 64))))

(defn compose [seed]
  (with-seed (if (string? seed) (.hashCode seed) seed)
    (let [context (init-context {:seed seed})]
      (-> context
          (init-base-scale)
          (c/init-chords)
          (c/init-chord-notes)))))

(defn play [seed]
  (let [composition (compose seed)
        chords {:type :chord :notes (:chord-notes composition)}]
    (m/play-sequence (m/midi-sequence 128 [chords]))
    composition))

; (play (g/long))
; (play "foobar")
