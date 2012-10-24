(ns looping-is-recursion)

(defn power [base exp]
  (let [helper
        (fn [acc base exp]
          (cond
           (= exp 0)
           acc
           :else
           (recur (* base acc) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2))
   true
   (or (empty? seq1) (empty? seq2) (not (= (first seq1) (first seq2))))
   false
   :else
   (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [a-seq a-seq
         index 0]
    (cond
     (empty? a-seq)
     nil
     (pred (first a-seq))
     index
     :else
     (recur (rest a-seq) (inc index)))))

(defn avg [a-seq]
  (loop [acc 0
         a-seq a-seq
         n 0]
    (if (empty? a-seq)
      (/ acc n)
      (recur (+ acc (first a-seq)) (rest a-seq) (inc n)))))

(defn parity [a-seq]
  (let [toggle
        (fn [a-set elem]
          (if (contains? a-set elem)
            (disj a-set elem)
            (conj a-set elem)))]
    (loop [a-seq a-seq
           a-set #{}]
      (if (empty? a-seq)
        a-set
        (recur (rest a-seq) (toggle a-set (first a-seq)))))))

(defn fast-fibo [n]
  (loop [fibo1 0
         fibo2 1
         m 0]
    (if (= m n)
      fibo1
      (recur fibo2 (+ fibo2 fibo1) (inc m)))))

(defn cut-at-repetition [a-seq]
  (loop [a-seq a-seq
         res-seq []]
    (cond
     (empty? a-seq)
     res-seq
     (some (fn [elem] (= (first a-seq) elem)) res-seq)
     res-seq
     :else
     (recur (rest a-seq) (conj res-seq (first a-seq))))))