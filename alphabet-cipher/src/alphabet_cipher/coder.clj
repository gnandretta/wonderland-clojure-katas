(ns alphabet-cipher.coder)

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn- letter->index
  "Returns the letter x position in the alphabet."
  [x]
  (- (int x) (int \a)))

(defn- index->letter
  "Returns the letter in the x position of the alphabet."
  [x]
  (get (vec alphabet) x))

(defn- alphabet-from
  "Returns a rotation of the alphabet starting with the letter x."
  [x]
  (take (count alphabet) (drop (letter->index x) (cycle alphabet))))

(defn- substitute
  "Substitutes each letter of the message by calling (f kl ml) where kl is the
  letter of the keyword occupying the same position as the letter of message ml.
  If the keyword is shorter than the message it will be repeated as many times
  as necessary."
  [f keyword message]
  (apply str (map f (cycle keyword) message)))

(defn encode
  "Returns the enconded message using the alphabet cipher with keyword."
  [keyword message]
  (let [f (fn [kl ml] (first (drop (letter->index kl) (alphabet-from ml))))]
    (substitute f keyword message)))

(defn decode
  "Returns the decoded message using the alphabet cipher with keyword."
  [keyword message]
  (let [f (fn [kl ml] (->> (alphabet-from kl)
                           (take-while (partial not= ml))
                           count
                           index->letter))]
    (substitute f keyword message)))
