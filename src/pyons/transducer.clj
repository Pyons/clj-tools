(ns pyons.transducer)

(defn prn-tr 
  "Print transducer"
  [rf]
  (let [cnt (volatile! 0)]
    (fn
      ([]
       (println "init")
       (rf))
      ([result]
       (println "reduced" @cnt)
       (rf result))
      ([result input]
       (vswap! cnt inc)
       (println input)
       (rf result input)))))

(defn log-tr 
  "Log transducer"
  [rf]
  (let [cnt (volatile! 0)]
    (fn
      ([]
       (println "init")
       (rf))
      ([result]
       (println "Reduced after: " @cnt " steps")
       (rf result))
      ([result input]
       (vswap! cnt inc)
       (println (str "Step: " @cnt " input: " input))
       (rf result input)))))

(defn lazy-sequence
  "Returns a lazy sequence of applying the transformation to
  the items in the coll. Lazier than sequence, does not use
  chunked sequences
  Threads each item from the collection through the xforms till either 
  the collection is exhausted or the xforms terminate e.g. `(take 3)`"
  {:arglists '([xform* coll])}
  ([& xforms]
   (let [rf (fn
              ([result]
               (unreduced result))
              ([result input]
               (conj (vec (unreduced result)) input)))
         reducer-fn ((apply comp (butlast xforms)) rf)
         reduce-seq (fn reduce-seq [coll]
                      (if-let [fs (first coll)] 
                        (lazy-seq
                          (if-let [res (reducer-fn nil fs)] ;; short circuit nil, to avoid a useless concat
                            (concat (unreduced res)
                                    (when-not (reduced? res)
                                      (reduce-seq (rest coll))))
                            (reduce-seq (rest coll))))
                            (reducer-fn (reduced nil))))]
     (reduce-seq (last xforms)))))

(comment
  (time (transduce (comp (dedupe) (map inc)) conj (repeat 10000000 1)))
  (time (vec (sequence (comp (dedupe) (map inc)) (repeat 10000000 1))))
  (time  (vec (lazy-sequence (dedupe) (map inc) (repeat 10000000 1))))
  (time  (->> (repeat 10000000 1) dedupe (mapv inc)))
  (time (do
          (vec (lazy-sequence (comp (filter odd?) (map inc)) (range 10000000)))
          nil))
  (time (do
          (vec (map inc (filter odd? (range 10000000))))
          nil))
  (take 2 (eduction (comp cat prn-tr) (repeat 34 [1 2])))
  (take 2 (lazy-sequence (comp cat prn-tr) (repeat 34 [1 2])))
  (take 2 (lazy-sequence (comp cat log-tr) (repeat 34 [1 2])))
  (take 2 (sequence (comp cat prn-tr) (repeat 36 [1 2])))
  (take 2 (sequence (comp cat prn-tr) (repeat 60 [1 2])))
  (take 4 (sequence (comp (partition-all 2) prn-tr) (range 3)))
  (take 2 (eduction prn-tr (range 50))))
