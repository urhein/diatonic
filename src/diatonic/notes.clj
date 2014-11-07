(ns diatonic.notes)

(defn transpose [note amount]
  (update-in note [:pitch] + amount))
