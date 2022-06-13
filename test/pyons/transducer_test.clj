(ns pyons.transducer-test
  (:require [clojure.test :refer [deftest is]]
            [pyons.transducer :refer [lazy-sequence]]))

(deftest lazy-sequence-test
  (is (= '(0 1 2 3)
         (lazy-sequence (range 4))))
  (is (= '([101 102 103] [104 105 106] [107 108 109] [110 111 112])
         (take 4 (lazy-sequence (filter #(> % 100)) (partition-all 3) (range)))))
  (is (= '([0 1] [2 3] [4])
         (lazy-sequence (partition-all 2) (range 5))))
  (is (= '(0 1 2 3)
         (take 4 (lazy-sequence (map identity) (iterate inc 0)))))
  (is (= '(2 3 4 5)
         (take 4 (lazy-sequence (map inc) (take 4) (map inc) (range 100)))))
  (let [items 12 pgsize 5
        src (vec (repeatedly items random-uuid))
        api (fn [tok]
              (let [tok (or tok 0)]
                (when (< tok items)
                  {:tok (+ tok pgsize)
                   :ret (subvec src tok (min (+ tok pgsize) items))})))]
    (is (= src
           (mapcat identity (iteration api :kf :tok :vf :ret))
           (transduce cat conj (iteration api :kf :tok :vf :ret))
           (lazy-sequence cat (iteration api :kf :tok :vf :ret))
           (sequence cat (iteration api :kf :tok :vf :ret))
           (into [] cat (iteration api :kf :tok :vf :ret))))))
