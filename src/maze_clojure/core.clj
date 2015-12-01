(ns maze-clojure.core
  (:gen-class))


(def size 10)                                               ;grid size is 10

(defn create-rooms []
  (vec (for [row (range 0 size)]                            ;create a vector inside a vector
     (vec (for [col (range 0 size)]
            {:row row,
             :col col,
             :visited? false,
             :bottom? true,
             :right? true}))))
  )                                                         ;end of create-rooms

(defn possible-neighbors [rooms row col]
 [(get-in rooms [(- row 1) col])                            ;the room on the top
  (get-in rooms [(+ row 1) col])                            ;the room on the bottom
  (get-in rooms [row ( - col 1)])                           ;the room to the left
  (get-in rooms [row (+ col 1)])                            ;the room to the right
  ])


(defn random-neighbor [rooms row col]
  (let [neighbors (possible-neighbors rooms row col)
        neighbors (filter (fn [room]                        ;filter through valid and non-visited neighbors
                            (and room
                                 (not (:visited? room))))
                          neighbors)
        ]
    (if (pos? (count neighbors))
      (rand-nth neighbors)
      nil)
    )                                                       ;end of let
  )                                                         ;end of random-neighbor


(defn tear-down-wall [rooms old-row old-col new-row new-col]
  (cond
    ;going up
    (< new-row old-row)                                     ;comparing the new room row to the old room row
    (assoc-in rooms [new-row new-col :bottom?] false)       ;Look at rooms.Go to that row, then that wall, then remove wall
    ;going down
    (> new-row old-row)                                     ;comparing new row to old row
    (assoc-in rooms [old-row old-col :bottom?] false)      ;removes the bottom wall of the current room/the top wall of next room
    ;going left
    (< new-col old-col)
    (assoc-in rooms [new-row new-col :right?] false)        ;remove right wall of room to the left
    ;going right
    (> new-col old-col)
    (assoc-in rooms [old-row old-col :right?] false)
    )                                                       ;end of condition
  )                                                         ;end of tear-down-wall

(defn create-maze [rooms row col]
  (let [rooms (assoc-in rooms [row col :visited?] true)
        next-room (random-neighbor rooms row col)]
    (if next-room
      (let [rooms (tear-down-wall rooms row col (:row next-room) (:col next-room))]
        (loop [old-rooms rooms]
          (let [new-rooms (create-maze old-rooms (:row next-room) (:col next-room))]
            (if (= old-rooms new-rooms)
              old-rooms
              (recur new-rooms)
              )                                             ;end of if
            )
          )                                                 ;end of loop
        )
      rooms
      )                                                     ;end of if
    )                                                       ;end of let
  )                                                         ;end of create-maze


(defn -main [& args]
  (let [rooms (create-rooms)
        rooms (create-maze rooms 0 0)]
    (doseq [row rooms]                                      ;print top walls of underscores for top row
      (print " _"))
      (println)
      (doseq [row rooms]                                    ;for each list inside rooms, loop over it
        (print "|")                                         ;print all left side walls at beginnig of row
        (doseq [room row]                                   ;looping over each room in each row
               (print (str (if (:bottom? room) "_" " ")
                           (if (:right? room) "|" " "))))                                ;print grid
        (println)
        )
    )
  )
