(ns maze-gen.core
    (:require [quil.core :as q]
              [quil.middleware :as m])
    (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; dimensions of the grid graph in terms of vertices
(def gridgraph-m 20)
(def gridgraph-n 20)

(def cell-size 25)

(defn grid-edges [m n]
    (let [
        horz-edges
        (set (for [i (range m) j (range (dec n))] 
                #{[i, j], [i, (inc j)]}
            )
        )
        vert-edges
        (set (for [i (range (dec m)) j (range n)] 
                #{[i, j], [(inc i), j]}
            )
        )
    ] (into horz-edges vert-edges))
)

(defn rand-edge [grid]
    (rand-nth (vec grid))
)

(defn every-same? [collection]
    (every? (partial = (first collection)) (rest collection))
)

;; gridpoint to vertexnumber: used for which-tree map
(defn vertexnumber [[p q] n]
    (+ (* n p) q)
)

(defn which-tree-init [vertexcount]
    (zipmap (range vertexcount) (range vertexcount))
)

(defn cycle-edge [edge which-tree n]
    (let [
        a (first edge)
        b (last edge)
        vn-a (vertexnumber a n)
        vn-b (vertexnumber b n)
        tn-a (get which-tree vn-a)
        tn-b (get which-tree vn-b)
    ] (= tn-a tn-b))
)

(defn which-tree-update [which-tree chosen-edge n]
    (let [
        a (first chosen-edge) 
        b (last chosen-edge)
        vn-a (vertexnumber a n)
        vn-b (vertexnumber b n)
        tn-a (get which-tree vn-a)
        tn-b (get which-tree vn-b)
        new-vals (map (fn [x] (if (= tn-b x) tn-a x)) (vals which-tree))
    ] (zipmap (keys which-tree) new-vals))
)

(defn random-spanning-tree [m n]
    (let [grid (grid-edges m n) which-tree (which-tree-init (* m n))]
        (letfn [(f [grid-remaining which-tree spanning-tree] ;; grid-remaining, spanning-tree: sets of sets of vectors; which-tree: map
            (if (every-same? (vals which-tree))
                spanning-tree
                ;; find some edge that has endpoints in different trees
                (let [
                    noncycle-edges (filter (fn [edge] (not (cycle-edge edge which-tree n))) grid)
                    chosen-edge (rand-edge noncycle-edges)
                ] (f 
                    (disj grid-remaining chosen-edge) 
                    (which-tree-update which-tree chosen-edge n)
                    (conj spanning-tree chosen-edge)
                ))
            )
        )] (f grid which-tree #{}))
    )
)

;;;;;;;;;;; EDGES-SPANNINGTREE TO CELLARRAY ;;;;;;;;;;;

;; interpolating horizontally or vertically
(defn interpolate [[p1 q1] [p2 q2]]
    (if (= p1 p2)
        [p1 (/ (+ q1 q2) 2)]
        [(/ (+ p1 p2) 2) q1]
    )
)

(defn times2 [[p q]] [(* p 2) (* q 2)])

(defn edge-times2-interpolate [edge]
    (let [vert-a (times2 (first edge)) vert-c (times2 (last edge)) vert-b (interpolate vert-a vert-c)]
        #{vert-a vert-b vert-c}
    )
)

(defn path-cells [m n]
    (reduce into (map edge-times2-interpolate (random-spanning-tree m n))))

;;;;;;;;;;;;;;;;;;;;;;

(defn draw-cell [[i j]]
    (q/rect (* i cell-size) (* j cell-size) cell-size cell-size))

(defn setup []
    (q/background 255)
    (q/frame-rate 30)
    (q/fill 0)
    (q/no-stroke)
    (dorun (map draw-cell (path-cells gridgraph-m gridgraph-n))))

(q/defsketch maze-gen
    :title "MAZE"
    :size (letfn [(pixelcount [x] (* cell-size (dec (* 2 x))))] 
              [(pixelcount gridgraph-m) (pixelcount gridgraph-n)])
    :setup setup
    :features [:keep-on-top]
    :middleware [m/fun-mode])

(defn -main [& args])
