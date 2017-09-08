(ns predicates)

(defn sum-f [f g x]
  (+ (f x) (g x)))

(defn less-than [n]
  (fn [k] (< k n)))

(defn equal-to [n]
  (fn [k] (== k n)))

(defn set->predicate [a-set]
  (if (contains? a-set nil)
    (fn [k] (if (nil? k)
              true
              (a-set k)))
    (fn [k] (a-set k))))

(defn pred-and [pred1 pred2]
  (fn [k] (and (pred1 k) (pred2 k))))

(defn pred-or [pred1 pred2]
  (fn [k] (or (pred1 k) (pred2 k))))

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  (every? whitespace? string))

(defn has-award? [book award]
  (contains? (:awards book) award ))

(defn HAS-ALL-THE-AWARDS? [book awards]
  (let [book-has-award (fn [k] (has-award? book k))]
        (every? book-has-award awards)))

(defn my-some [pred a-seq]
  (let [filtered (filter pred a-seq)]
    (if (empty? filtered)
      false
      (pred (first filtered)))))

(defn my-every? [pred a-seq]
  (= (filter pred a-seq) a-seq))

(defn prime? [n]
  (let [pred (fn [divisor] (= 0 (mod n divisor)))]
    (not (some pred (range 2 n)))))
;^^
