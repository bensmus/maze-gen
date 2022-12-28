(ns maze-gen.core
    (:require [quil.core :as q]
              [quil.middleware :as m])
    (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; ;; turn spanning tree edges into 2d 1/0 array signifying path cell or block cell
;; (defn edges-to-cellarray [edges [gridm gridn]]
;;     ;; (reduce into #{#{1 2 3} #{4 5 6} #{8 9 10}})
;;     (let [path-cells (reduce into (map edge-times2-interpolate edges))]
;;         (vec (for [i (range gridm)]
;;             (vec (for [j (range gridn)]
;;                 (if (contains? path-cells [i j]) 1 0)
;;             ))
;;         ))
;;     )
;; )

;; (defn maze [m n]
;;     (let [edges-st (random-spanning-tree m n)]
;;         (edges-to-cellarray edges-st [(dec (* 2 m)) (dec (* 2 n))])
;;     )
;; )

(defn path-cells [m n]
    (reduce into (map edge-times2-interpolate (random-spanning-tree m n))))

;;;;;;;;;;;;;;;;;;;;;;

(defn draw-cell [[i j]]
    (q/rect (* i 25) (* j 25) 25 25))

(defn setup []
    ; Set frame rate to 30 frames per second.
    (q/background 255)
    (q/frame-rate 30)
    (q/fill 0)
    (q/no-stroke)
    ; setup function returns initial state. It could contain
    ; circle color and position, or any other data.

    (dorun (map draw-cell (path-cells 10 10))))

;; (defn update-state [state]
;;   ; Update sketch state by changing circle color and position.
;;   {:color (mod (+ (:color state) 0.7) 255)
;;    :angle (+ (:angle state) 0.1)})

;; (defn draw-maze [maze]
;;   ; Clear the sketch by filling it with light-grey color.
;;     (q/background 255)
;;   ; Set rect color.
;;     (q/fill 0)
;;   ;; ; Calculate x and y coordinates of the circle.
;;   ;; (let [angle (:angle state)
;;   ;;       x (* 150 (q/cos angle))
;;   ;;       y (* 150 (q/sin angle))]
;;   ;;   ; Move origin point to the center of the sketch.
;;   ;;   (q/with-translation [(/ (q/width) 2)
;;   ;;                        (/ (q/height) 2)]
;;   ;;     ; Draw the circle.
;;   ;;     (q/ellipse x y 100 100))))
;;     ;; (println maze)
;;     (for [i (range 10) j (range 10)]
;;         (when (= (get-in maze [i j]) 1)
;;             (q/rect (* i 50) (* j 50) 50 50))))

(q/defsketch maze-gen
  :title "MAZE"
  :size [475 475]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  ;; :update update-state
;;   :draw draw-maze
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

(defn -main [& args])
