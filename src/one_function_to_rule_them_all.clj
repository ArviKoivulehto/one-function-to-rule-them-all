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
  (let [freqs (frequencies a-seq)]
    (let [all (loop [complete '() current a-seq]
                (cond
                  (empty? current)
                  complete
                  :else
                  (let [now (if (odd? (get freqs (first current))) (cons (first current) complete) (cons nil complete))]
                    (recur now (rest current)))))]
  (into #{} (reverse (filter (complement nil?) all))))))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))
  
(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p0 p1] (fn [x] (and (p0 x) (p1 x))))
  ([p0 p1 & more] (reduce pred-and (pred-and p0 p1) more)))

(defn my-map
  ([f] (fn [x y] (my-map f x y)))
  ([f a-seq] (loop [complete '() i a-seq]
               (cond
                 (empty? i)
                 complete
                 :else
                 (recur (cons (f (first i)) complete ) (rest i)))))
  ([f a-seq b-seq] (loop [complete '() i a-seq j b-seq]
                     (cond
                       (or (empty? i))
                       complete
                       :else
                       (recur (cons (f (first i) (first j)) complete) (rest i) (rest j)))))
  ([f a-seq b-seq & more] (reduce (my-map f) (my-map f a-seq b-seq) more)))