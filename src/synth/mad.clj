(ns synth.mad
  (:use overtone.live
        overtone.inst.synth))

; Adapted from the music as data project, cool stuff!
; http://mad.emotionull.com/

(definst tone [note 60 amp 0.3 dur 0.4]
  (let [snd (sin-osc (midicps note))
        env (env-gen (perc 0.01 dur) :action FREE)]
    (* env snd amp)))

(def ^:private metro (metronome 128))
(def ^:private bpb (atom {metro 4}))

(defn- metro-bpb
  ([m] (@bpb m))
  ([m v] (swap! bpb #(assoc % m %2) v)))

(defn- metro-bar
  ([m] (quot (metro-beat m) (metro-bpb m)))
  ([m bar] (m (* bar (metro-bpb m)))))

;; (defn- metro-play
;;   ([elements] (metrop elements (+ (metro-beat metro) 1)))
;;   ([[{:keys [synth val pitch dur data rhythm-factor]} & elements] beat]
;;      (let [ms-start (metro-beat metro beat)]
;;        (at ms-start)))
;;   )

(defn- p
  ([elements]
   (p elements (now)))
  ([[{:keys [synth vol pitch dur data]} & elements] t]
   (let [next-t (+ t (int (* 1000 dur)))]
     (at t
         (synth pitch vol dur))
     (when elements
       (apply-at next-t #'p elements [next-t])))))

(defn- condition-pattern [pattern]
  (reduce
   (fn
     ([coll] coll)
     ([coll new] (if-let [dur (:sustain new)]
                   (do (update-in coll [(- (count coll) 1) :dur]
                                  #(+ % dur)))
                   (if () (conj coll new)))))
   []
   pattern
   ))

(declare calc-duration)

(defn- pattern
  ([m-element] (pattern m-element 1))
  ([m-element duration]
   (if (= (type []) (type m-element))
     (condition-pattern
      (flatten
       (calc-duration m-element duration (count m-element))))
     (if (= m-element -)
       {:sustain (float duration)}
       (assoc m-element :dur (float duration))))))

(defn- calc-duration
  [elements duration count]
  (map #(pattern % (/ duration count))
       elements))

(defn play! [elements dur inst control]
  (let [pat (map #(assoc % :synth inst) (pattern elements dur))]
    (if-not (empty? control) (ctl inst control))
    (p pat)))

(defn- defnote
  [n-sym pitch]
  (intern *ns* n-sym
          {:synth tone
           :vol 0.2
           :pitch pitch
           :dur 0.1
           :data []}))

(defn- def-notes
  "Define vars for all notes."
  []
  (intern *ns* '_ {:synth tone
                   :vol 0.0
                   :pitch 60
                   :dur 0.1
                   :data []})
  (doseq [octave (range 8)]
    (doseq [n (range 7)]
      (let [n-char (char (+ 65 n))
            n-sym (symbol (str n-char octave))
            note (octave-note octave (get NOTES (keyword (str n-char))))]
        (defnote n-sym note)
        (when-let [sharp (get NOTES (keyword (str n-char "#")))]
          (defnote (symbol (str n-char "#" octave))
                   (octave-note octave sharp)))
        (when-let [flat (get NOTES (keyword (str n-char "b")))]
          (defnote (symbol (str n-char "b" octave))
                   (octave-note octave flat)))))))

(def-notes)



; Bach - Minuet in G Major
; Go here for the sheet music:
;; http://www.sheetmusic1.com/new.great.music/bach.minuet.gmajor/bach.1.demo.gif
;; http://www.sheetmusic1.com/new.great.music/bach.minuet.gmajor/bach.2.demo.gif
(def g-minuet-right-hand [[D5 D5 D5]
                          [B4 [A4 B4] G4]
                          [A4 D5 C5]
                          [B4 - A4] ; NOTE: two B4's should be tied together
                          [D5 [C5 B4] [A4 G4]]
                          [E5 [C5 B4] [A4 G4]]
                          [F#4 [E4 D4] F#4]
                          [G4]
                          [B4 E5 E5]
                          [C#5 [B4 C5] A4]
                          [D5 E5 F5]
                          [[E5 D5] [C#5 B4] A4]

                          [A6 [G5 F#5 E5 D5]]
                          [B6 [G5 F#5 E5 D5]]
                          [C#5 A5 C#5]
                          [D5]
                          [D5 [C5 B5] A5]
                          [B5 [A5 B5] [G4]]
                          [C5 C5 [C5 B5]]
                          [A5]
                          [D5 [C5 B5 A5 G4]]
                          [E5 [C5 B5 A5 G4]]
                          [F#4 [E4 D4] F#4]
                          [G4]])

(def g-minuet-left-hand [[G3 F#3 D3]
                         [G3 D3 G2]
                         [G3 [F#3 E3] [F#3 D3]]
                         [G3 G2 [D3 C3]]
                         [B2]
                         [C3]
                         [D3]
                         [G2]
                         [G3 G3 E3]
                         [A3 E3 A2]
                         [F#3 E3 D3]
                         [A2 E3 [A4 G3]]

                         [F#3]
                         [G3]
                         [A4 A4 A3]
                         [D3 [D4 C4 B4 A4]]
                         [G3 G3 F#3]
                         [G3 D3 G2]
                         [A4 F#3 G3]
                         [D3 D2 [D3 C3]]
                         [B3]
                         [C3]
                         [D3 D3 D2]
                         [G3]])




(def dox-suprano [[G4 G4 F#4 E4 D4 G4 A4  B4 - - _]
                  [B4 B4 B4  A4 G4 C5 B4  A4 - - _]
                  [G4 A4 B4  A4 G4 E4 F#4 G4 - - _]
                  [D5 B4 G4  A4 C5 B4 A4  G4 - - _]])
(def dox-alto [[D4 D4 D4 B4 B4 B3 D4 D4 - - _]
               [D4 D4 G4 F#4 G4 G4 G4 F#4 - - _]
               [G4 F#4 G4 F#4 D4 E4 D4 D4 - - _]
               [D4 D4 G4 F#4 [E4 F#4] G4 F#4 G4 - - _]])
(def dox-tenor [[B3 B3 A3 G3 F#3 G3 F#3 G3 - - _]
                [G3 B3 D4 C4 B3 E4 D4 D4 - - _]
                [B3 D4 D4 D4 D4 [C4 B3] A3 B3 - - _]
                [B3 G3 B3 D4 E4 D4 [D4 C4] B3 - - _]])
(def dox-bass [[G3 G3 D3 E3 B2 E3 D3 G2 - - _]
               [G3 G3 G3 D3 E3 C3 G2 D3 - - _]
               [E3 D3 G3 D3 B2 C3 D3 G2 - - _]
               [G3 G3 E3 D3 A2 [B2 C3] D3 G2 - - _]])

(def beth-5th-intro [[A#3] [- ]])
(defn play-with-inst [[pat inst dur & more]]
  (p (map  #(assoc % :synth inst)
           (pattern pat dur)))
  (if-not (empty? more)
    (recur more)))

(defn play-csv [inst csv]
  (p (csv-pattern)))

(defn- glp
  [t]
  (map
    #(assoc % :synth ks1-demo)
    (pattern g-minuet-left-hand t)))

(defn- grp
  [t]
  (map
    #(assoc % :synth ks1-demo)
    (pattern g-minuet-right-hand t)))

(defn- transpose
  [pat shift]
  (map #(assoc % :pitch (+ (:pitch %) shift)) pat))
