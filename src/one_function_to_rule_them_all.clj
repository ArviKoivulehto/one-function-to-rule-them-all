(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (letfn [(space [x y] (str x " " y))]
      (reduce space a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (letfn [(conjx [a b] (conj a b x))]
      (drop 1 (reduce conjx '() (reverse a-seq))))))

(defn my-count [a-seq]
  (letfn [(counter [init _] (inc init))]
  (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (loop [complete '() i sorted-seq index 0 doublecheck false]
    (cond
      (empty? sorted-seq)
      (cons n complete)
      (or (empty? i) doublecheck)
      (if (not doublecheck) (concat (concat complete [n]) i) (concat complete i)) 
      :else
      ;(let [now (if (< n (apply min i)) (cons n (cons (first i) complete)) (cons (first i) complete))]
      (let [second (first (rest i))]
        (let [check (and (< n (first i)))] ;(nth i index)
	        (let [now (if check (concat (concat complete [n]) [(first i)]) (concat complete [(first i)]))]
	          (recur now (rest i) (inc index) check)))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  [:-])

(defn minus [x]
  :-)

(defn count-params [x]
  :-)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])