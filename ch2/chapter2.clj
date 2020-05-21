;; Tree Stuff

(def entry first)
(def left-branch second)
(def right-branch (comp second rest))
(def make-tree vector)

(def ex-1
  [7
   [3
    [1 [] []]
    [5 [] []]]
   [9 [] [11 [] []]]])

(defn tree->list-1 [tree]
  (if (empty? tree)
    []
    (concat (tree->list-1 (left-branch tree))
            [(entry tree)]
            (tree->list-1 (right-branch tree)))))

;; This first guy grows as nlogn. Each step takes n, logn deep.

(defn tree->list-2 [tree]
  (letfn [(copy-to-list [tree result-list]
            (if (empty? tree)
              result-list
              (copy-to-list (left-branch tree)
                            (concat [(entry tree)]
                                    (copy-to-list (right-branch tree)
                                                  result-list)))))]
    (copy-to-list tree [])))

(defn partial-tree [elts n]
  (if (zero? n)
    [[] elts]
    (let [left-size         (quot (dec n) 2)
          right-size        (- n (inc left-size))
          [l [x & remaining]] (partial-tree elts left-size)
          [r   [& remaining]] (partial-tree remaining right-size)]
      (assert (= n (+ left-size 1 right-size))
              "The returned tree must have consumed n elements.")
      [(make-tree x l r) remaining])))

(defn list->tree [elements]
  (first (partial-tree elements (count elements))))

(defn element-of-set? [x set]
  (cond (empty? set) false
        (= x (entry set)) true
        (< x (entry set)) (element-of-set? x (left-branch set))
        :else (element-of-set? x (right-branch set))))

(defn adjoin-set [x set]
  (cond (empty? set) (make-tree x [] [])
        (= x (entry set)) set
        (< x (entry set)) (make-tree (entry set)
                                     (adjoin-set x (left-branch set))
                                     (right-branch set))
        :else (make-tree (entry set)
                         (left-branch set)
                         (adjoin-set x (right-branch set)))))

(defn union-set [l r]
  (letfn [(union [l r]
            (cond (empty? l) r
                  (empty? r) l
                  :else (let [x (first l)
                              y (first r)]
                          (cond (< x y) (cons x (union (rest l) r))
                                (= x y) (cons x (union (rest l) (rest r)))
                                :else   (cons y (union l (rest r)))))))]
    (list->tree
     (union (tree->list-2 l)
            (tree->list-2 r)))))

(defn intersection-set [l r]
  (letfn [(intersection [l r]
            (if (or (empty? l) (empty? r))
              []
              (let [x (first l)
                    y (first r)]
                (cond (= x y) (cons x (intersection (rest l) (rest r)))
                      (< x y) (intersection (rest l) r)
                      :else (intersection l (rest r))))))]
    (list->tree
     (intersection (tree->list-1 l)
                   (tree->list-1 r)))))

;; 2.66

(defn lookup
  "I can tell that I really want this bitch to return an Option a."
  [keyfn x set]
  (cond (empty? set) []
        (= x (keyfn (entry set))) [(entry set)]
        (< x (keyfn (entry set))) (lookup x (left-branch set))
        :else (lookup keyfn x (right-branch set))))

;; Huffman Encoding

(defrecord Leaf [sym weight])

(defn make-leaf [sym weight]
  (->Leaf sym weight))

(defn leaf? [x]
  (instance? Leaf x))

(defn symbol-leaf [l]
  (:sym l))

(defn weight-leaf [l]
  (:weight l))


(declare symbols weight)
(defn make-code-tree [l r]
  [l r (concat (symbols l)
               (symbols r))
   (+ (weight l) (weight r))])

(defn left-branch [tree]
  (first tree))

(defn right-branch [tree]
  (second tree))

(defn symbols [tree]
  (if (leaf? tree)
    [(symbol-leaf tree)]
    (nth tree 2)))

(defn weight [tree]
  (if (leaf? tree)
    (weight-leaf tree)
    (nth tree 3)))

(defn choose-branch [bit branch]
  (cond (= 0 bit) (left-branch branch)
        (= 1 bit) (right-branch branch)
        :else (throw (Exception. (str "Bad bit - " bit)))))

(defn decode-tree [bits tree]
  (letfn [(decode-1 [bits current-node]
            (if (empty? bits)
              []
              (let [next-branch (choose-branch (first bits) current-node)]
                (if (leaf? next-branch)
                  (cons (symbol-leaf next-branch)
                        (decode-1 (rest bits) tree))
                  (decode-1 (rest bits) next-branch)))))]
    (decode-1 bits tree)))

(def sample-tree
  (make-code-tree
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

;; ### Encoding

(defn in-symbol-set? [c tree]
  (some #{c} (symbols tree)))

(defn assert-in-tree! [c tree]
  (assert (in-symbol-set? c tree)
 (throw (Exception. (str "Error! Sym not in tree - " c)))))

(defn encode-symbol [c tree]
  (assert-in-tree! c tree)
  (if (leaf? tree)
    []
    (if (in-symbol-set? c (left-branch tree))
      (cons 0 (encode-symbol c (left-branch tree)))
      (cons 1 (encode-symbol c (right-branch tree))))))

(defn encode
  "Encodes the supplied message using the supplied huffman encoding
  tree."
  [message tree]
  (if (empty? message)
    []
    (concat (encode-symbol (first message) tree)
            (encode (rest message) tree))))

;; ## Generating Huffman Trees

(defn adjoin-set [x set]
  (cond (empty? set) [x]
        (= x (first set)) set
        (< (weight x) (weight (first set))) (cons x set)
        :else (cons (first set) (adjoin-set x (rest set)))))

(defn make-leaf-set
  "Takes in pairs of symbol and weight."
  [pairs]
  (if (empty? pairs)
    []
    (let [[sym weight] (first pairs)]
      (adjoin-set (make-leaf sym weight)
                  (make-leaf-set (rest pairs))))))

(defn successive-merge [[l r & more]]
  (cond (nil? l) []
        (nil? r) l
        :else (-> (make-code-tree l r)
                  (adjoin-set more)
                  (successive-merge))))

(defn generate-huffman-tree
  [pairs]
  {:pre [(< 1 (count pairs))]}
  (successive-merge
   (make-leaf-set pairs)))

(def pairs-example
  [[':a 4] [:b 2] [:d 1] [:c 1]])

;; 2.70

(def rock-pairs
  [[:a 2]
   [:na 16]
   [:boom 1]
   [:sha 3]
   [:get 2]
   [:yip 9]
   [:job 2]
   [:wah 1]])

(def rock-message
  [:get :a :job
   :sha :na :na :na :na :na :na :na :na
   :get :a :job
   :sha :na :na :na :na :na :na :na :na
   :wah :yip :yip :yip :yip :yip :yip :yip :yip :yip
   :sha :boom])

;; 84 bits needed to encode this business.
;;
;; If we used a fixed length code for the 8 symbol alphabet, we could
;; get away with three bits per "symbol", so 36 * 3 = 108
;; bits. Winning!

;; 2.71

;; To note about the huffman tree:
;;
;; - super fucking optimal way of doing it!
