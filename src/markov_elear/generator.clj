(ns markov-elear.generator)

(def prefix-list ["No you" "I want" "And so" "You're not"
                  "Uh, we" "I never" "Yeah, I" "Trust me"
                  "Boy, you" "It's not" "Geez, I" "I guess"
                  "Hold on," "It's necessary" "I'm almost"
                  "Yeah, so" "Don't worry" "All right."
                  "What the" "What is" "And away" "Oh shit"
                  "Don't you" "You see" "We're not" "Time is"
                  "You know" "Too late," "Way to" "Oh boy."
                  "Oh yeah," "Watch me," "Can't do" "Two things"
                  "So did" "Get off" "What, so" "Listen, Jerry."
                  "Come on!" "Why does" "They're just" "No idea"
                  "I thought" "I'm a" "Holy crap," "Holy crap!"
                  "Morty, this"])

(defn word-chain [word-transitions]
  (reduce (fn [r t] (merge-with clojure.set/union r
                               (let [[a b c] t]
                                 {[a b] (if c #{c} #{})})))
          {}
          word-transitions))

(defn text->word-chain [s]
  (let [words (clojure.string/split s #"[\s|\n]")
        word-transitions (partition-all 3 1 words)]
    (word-chain word-transitions)))

(defn chain->text [chain]
  (apply str (interpose " " chain)))

(defn walk-chain [prefix chain result]
  (let [suffixes (get chain prefix)]
    (if (empty? suffixes)
      result
      (let [suffix (first (shuffle suffixes))
            new-prefix [(last prefix) suffix]
            result-with-spaces (chain->text result)
            result-char-count (count result-with-spaces)
            suffix-char-count (inc (count suffix))
            new-result-char-count (+ result-char-count suffix-char-count)]
        (if (>= new-result-char-count 75)
          result
          (recur new-prefix chain (conj result suffix)))))))

(defn generate-text
  [start-phrase word-chain]
  (let [prefix (clojure.string/split start-phrase #" ")
        result-chain (walk-chain prefix word-chain prefix)
        result-text (chain->text result-chain)]
    result-text))

(defn process-file [fname]
  (text->word-chain
   (slurp (clojure.java.io/resource fname))))

(defn end-at-last-punctuation [text]
  (let [trimmed-to-last-punct (apply str (re-seq #"[\s\w]+[^.!?,]*[.!?,]" text))
        trimmed-to-last-word (apply str (re-seq #".*[^a-zA-Z]+" text))
        result-text (if (empty? trimmed-to-last-punct)
                      trimmed-to-last-word
                      trimmed-to-last-punct)
        cleaned-text (clojure.string/replace result-text #"[,| ]$" ".")]
    (clojure.string/replace cleaned-text #"\"" "'")))

(defn rick-text []
  (let [text (generate-text (-> prefix-list shuffle first) (process-file "data_imp.txt"))]
    (end-at-last-punctuation text)))
