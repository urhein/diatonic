(ns diatonic.notes)

(defn transpose [amount note]
  (update-in note [:pitch] + amount))
