(ns diatonic.composition
  (:require [clojure.data.generators :as g]
            [diatonic.chords :as c]
            [diatonic.drums :as d]
            [diatonic.melodies :as m]
            [diatonic.midi :as midi]
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
           ((if (g/boolean) s/ionic-scale s/aeolic-scale) (g/uniform 59 70)))))

(defn compose [seed]
  (with-seed (if (string? seed) (.hashCode seed) seed)
    (let [context (init-context {:seed seed})]
      (-> context
          (init-base-scale)
          (c/init-chords)
          (c/init-chord-notes)
          (d/init-drums)
          (m/init-melody)))))

(defn play [seed]
  (let [composition (compose seed)
        chords {:type :chord :notes (:chord-notes composition)}
        melody {:type :melody :notes (:melody-notes composition)}
        drums {:type :drums :notes (:drum-notes composition)}]
    (midi/play-sequence (midi/midi-sequence 128 [chords drums]))
    composition))

; (:seed (play (g/long)))
; (play "foobar")
