(ns music-prompt
  (:require [music-theory.note :as note]))

(defn scale
  [starting-note intervals]
  (reductions
    #(note/interval+ %1 %2)
    starting-note
    intervals))

(defn major-scale
  [starting-note]
  (scale starting-note [:M2 :M2 :m2 :M2 :M2 :M2]))

(defn minor-scale
  [starting-note]
  (scale starting-note [:M2 :m2 :M2 :M2 :m2 :M2]))

(defn random-note
  []
  (keyword (str (rand-nth '(A B C D E F G))
                (rand-nth [nil "b" "#"]))))

(defn random-key
  []
  [(random-note)
   (rand-nth [:major :minor])])

(defn chord-numeral
  [scale-degree scale-quality]
  (nth (case scale-quality
         :major ["I" "ii" "iii" "IV" "V" "VI" "vii°"]
         :minor ["i" "ii°" "III" "iv" "V" "vi" "VII"])
       (dec scale-degree)))

(defn chord-quality
  [scale-degree scale-quality]
  (nth (case scale-quality
         :major [:major :minor :minor :major :major :minor :diminished]
         :minor [:minor :diminished :major :minor :major :major :major])
       (dec scale-degree)))

(defn chord-in-scale
  [tonic scale-quality scale-degree]
  (let [scale-notes (->> ((case scale-quality
                            :major major-scale
                            :minor minor-scale)
                          (keyword (str (name tonic) 0)))
                         (map #(keyword (apply str (butlast (name %))))))
        root-note   (nth scale-notes (dec scale-degree))]
    [(chord-numeral scale-degree scale-quality)
     root-note
     (chord-quality scale-degree scale-quality)]))

(defn prompt!
  [& [_cli-arg]]
  (let [[tonic quality]
        (random-key)

        scale-degree
        (inc (rand-int 7))

        [chord-numeral chord-root chord-quality]
        (chord-in-scale tonic quality scale-degree)]
    (println (format "In the key of %s %s..."
                     (name tonic) (name quality)))
    (println (format "Over the %s chord (%s %s)..."
                     chord-numeral
                     (name chord-root)
                     (name chord-quality)))
    (println (format "What is the function of %s?"
                     (name (random-note))))))

(comment
  (random-key)
  (major-scale :F2)
  (major-scale :F#5)
  (minor-scale :F2)
  (prompt!))
