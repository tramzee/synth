(ns synth.violin
  (use overtone.core))


(defn- registered-samples
  "Fetch violin samples from the asset store if they have been manually
  registered"
  []
  (registered-assets ::ViolinArcoVibrato))


;; (reduce #(assoc % (:id %2) (:original_filename %2)) {} (freesound-search :f "pack:\"violin arco vibrato\""))
(def ^:private FREESOUND-VIOLIN-VIB-SAMPLE-IDS
  {55914 "violin arco vib A#2.aif" 55952 "violin arco vib G3.aif" 55930 "violin arco vib C4.aif"
   55919 "violin arco vib A3.aif" 55918 "violin arco vib A2.aif" 55929 "violin arco vib C3.aif"
   55953 "violin arco vib G4.aif" 55920 "violin arco vib A4.aif" 55936 "violin arco vib D4.aif"
   55915 "violin arco vib A#3.aif" 55938 "violin arco vib E3.aif" 55935 "violin arco vib D3.aif"
   55944 "violin arco vib F3.aif" 55941 "violin arco vib F#3.aif" 55939 "violin arco vib E4.aif"
   55926 "violin arco vib C#3.aif" 55923 "violin arco vib B3.aif" 55947 "violin arco vib G#2.aif"
   55954 "violin arco vib G5.aif" 55948 "violin arco vib G#3.aif" 55951 "violin arco vib G2.aif"
   55932 "violin arco vib D#3.aif" 55924 "violin arco vib B4.aif" 55945 "violin arco vib F4.aif"
   55931 "violin arco vib C5.aif" 55922 "violin arco vib B2.aif" 55927 "violin arco vib C#4.aif"
   55949 "violin arco vib G#4.aif" 55946 "violin arco vib F5.aif" 55916 "violin arco vib A#4.aif"
   55950 "violin arco vib G#5.aif" 55921 "violin arco vib A5.aif" 55942 "violin arco vib F#4.aif"
   55937 "violin arco vib D5.aif" 55940 "violin arco vib E5.aif" 55917 "violin arco vib A#5.aif"
   55933 "violin arco vib D#4.aif" 55925 "violin arco vib B5.aif" 55943 "violin arco vib F#5.aif"
   55934 "violin arco vib D#5.aif" 55928 "violin arco vib C#5.aif"})

;; (reduce #(assoc % (:id %2) (:original_filename %2)) {} (freesound-search :f "pack:\"violin arco non vibrato\""))
(def ^:private FREESOUND-VIOLIN-NONVIB-SAMPLE-IDS
  {55873 "violin arco non vib A#2.aif" 55878 "violin arco non vib A3.aif" 55888 "violin arco non vib C3.aif"
   55877 "violin arco non vib A2.aif" 55874 "violin arco non vib A#3.aif" 55889 "violin arco non vib C4.aif"
   55897 "violin arco non vib E3.aif" 55894 "violin arco non vib D3.aif" 55895 "violin arco non vib D4.aif"
   55882 "violin arco non vib B3.aif" 55879 "violin arco non vib A4.aif" 55910 "violin arco non vib G2.aif"
   55911 "violin arco non vib G3.aif" 55880 "violin arco non vib A5.aif" 55881 "violin arco non vib B2.aif"
   55885 "violin arco non vib C#3.aif" 55886 "violin arco non vib C#4.aif" 55890 "violin arco non vib C5.aif"
   55912 "violin arco non vib G4.aif" 55900 "violin arco non vib F#3.aif" 55898 "violin arco non vib E4.aif"
   55875 "violin arco non vib A#4.aif" 55903 "violin arco non vib F3.aif" 55913 "violin arco non vib G5.aif"
   55907 "violin arco non vib G#3.aif" 55891 "violin arco non vib D#3.aif" 55884 "violin arco non vib B5.aif"
   55906 "violin arco non vib G#2.aif" 55883 "violin arco non vib B4.aif" 55904 "violin arco non vib F4.aif"
   55876 "violin arco non vib A#5.aif" 55892 "violin arco non vib D#4.aif" 55887 "violin arco non vib C#5.aif"
   55896 "violin arco non vib D5.aif" 55901 "violin arco non vib F#4.aif" 55899 "violin arco non vib E5.aif"
   55893 "violin arco non vib D#5.aif" 55905 "violin arco non vib F5.aif" 55908 "violin arco non vib G#4.aif"
   55902 "violin arco non vib F#5.aif" 55909 "violin arco non vib G#5.aif"})

(def ^:private FREESOUND-SAMPLES FREESOUND-VIOLIN-VIB-SAMPLE-IDS)

(def ^:private FREESOUND-VIOLIN-SAMPLE-IDS
  "Freesound ids for all the loud samples in the 'violin arco vibrato'"
  [55914 55952 55930 55919 55918 55929 55953 55873 55920 55936 55915 55938 55935 55944 55941 55939 55926 55878 55923 55947 55954 55948 55888 55951 55932 55924 55945 55877 55874 55931 55922 55927 55889 55949 55946 55916 55950 55921 55897 55894 55942 55937 55895 55882 55940 55879 55910 55917 55911 55933 55880 55881 55925 55885 55943 55934 55886 55890 55928 55912 55900 55898 55875 55903 55913 55907 55891 55884 55906 55883 55904 55876 55892 55887 55896 55901 55899 55893 55905 55908 55902 55909])

(defn- download-samples
  "Download violin samples from freesound and store them in the asset
  store."
  []
  (map freesound-path (keys FREESOUND-SAMPLES)))

(defn- get-samples
  "Either fetch samples from the registered store or download them if
  necessary"
  []
  (let [samples (registered-samples)]
    (if (empty? samples)
      (download-samples)
      samples)))

(defonce violin-samples
  (doall (map load-sample (get-samples))))

(defn- note-index
  "Returns a map of midi-note values [0-127] to buffer ids. Uses the
  provided note-fn to determine the midi-note value of a buffer."
  [buffers note-fn]
  (reduce (fn [index buf]
            (let [note (note-fn buf)
                  id   (-> buf :id)]
              (assoc index note id)))
          {}
          buffers))

(defn- violin-note-fn [buf]
  (let [path (:path buf)
        sample-id (read-string ((re-find #"sounds-(\d+)" path) 1))
        name (:name buf)
        note (-> (FREESOUND-SAMPLES sample-id) match-note :midi-note)]
    note))


;; Silent buffer used to fill in the gaps.
(defonce ^:private silent-buffer (buffer 0))

(defonce ^:private index-buffer
  (let [tab (note-index violin-samples violin-note-fn)
        buf (buffer 128)]
    (buffer-fill! buf (:id silent-buffer))
    (doseq [[idx val] tab]
      (buffer-set! buf idx val))
    buf))

(definst violin
  [note 60 level 1 sustain 1
   rate 1 loop? 0
   attack 0 decay 0 release 0.1 curve -4 gate 1 start-pos 0]
  (let [buf (index:kr (:id index-buffer) note)
        sus (- sustain attack release)
        g (env-gen (envelope [1 0] [sus] :linear))
        env (env-gen (asr attack level release level curve)
                     :gate g
                     :action FREE)
        variance 0.04
        section (scaled-play-buf 2 buf :level level :loop loop?
                                 :start-pos start-pos :action FREE)]
    (* env section)))
