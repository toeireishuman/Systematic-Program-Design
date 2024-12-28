;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tents_and_trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Tents and Trees (TAT) Puzzle Solver!
;; Rules (Taken from brainbashers [https://www.brainbashers.com/tents.asp])
;; - Find all of the tents in the forest.
;; - Every tent is attached to exactly one tree, and every tree is attached to exactly one tent.
;; - The number of tents is the same as the number of trees.
;; - The clues tell you how many tents are in that row or column.
;; - A tent can only be found horizontally or vertically next to a tree.
;; - Tents are never next to each other, neither vertically, horizontally, nor diagonally.
;; - A tree might be next to two tents but is only connected to one.
;; Sample puzzles are taken from brainbashers and puzzle-tents [https://www.puzzle-tents.com/]

;; ---- Constant Definitions ----

(define TAT6EASY-P1
  (list 1 0 1 0 0 0
        0 0 0 0 1 0
        0 0 1 0 0 0
        1 0 0 1 0 0
        0 0 0 0 0 0
        0 0 0 0 1 0))

(define TAT6EASY-COL1
  (list 1 2 0 2 1 1))

(define TAT6EASY-ROW1
  (list 2 0 2 0 2 1))

(define TAT6EASY-S1
  (list 1 2 1 2 0 0
        0 0 0 0 1 0
        0 2 1 0 2 0
        1 0 0 1 0 0
        2 0 0 2 0 0
        0 0 0 0 1 2))

;; ---- Data Definitions ----

;; Value is one of:
;;  - 0
;;  - 1
;;  - 2
;; interp. an element in a TAT board, where
;;          - 0 represents a vacant space,
;;          - 1 represents a tree, and
;;          - 2 represents a tent.


;; Position is Natural[0, (lengthof board) - 1]
;; interp. a 0-based indexed position on a TAT board


(define-struct numtents (ntent_col ntent_row))
;; NumTents is (make-numtents (listof Natural[0, (board-side-length board))
;;                            (listof Natural[0, (board-side-length board)))
;; interp. (make-numtents ntent-row ntent-col) is a specification for the number
;;          of tents that a column or row should have. NumTents is better understood
;;          visually like this:
;;          (make-numtents (list     c_0 c_1 ... c_n)
;;                         (list r_0
;;                               r_1
;;                               ...
;;                               r_n))
;;          with i as the column number and j as the row number,
;;           - c_i represents the number of tents in column i, and
;;           - r_j represents the number of tents in row j.


(define-struct board (sl lop lov nt))
;; Board is (make-board Natural[5, 20] (listof Position) (listof Value) NumTents)
;; interp. (make-board sl lop lov nt) is a TAT board, with the properties:
;;         - sl is the length of the board's side,
;;         - lop is the flat list of all positions,
;;         - lov is the arrangement of all values, provided as a flat list, corresponding with lop, and
;;         - nt is the specification of the numer of tents each row and column must have.


;; ---- Function Defintions ----

;; Board -> Board or false
;; Given an unsolved TAT board, return either the solved TAT board
;; or false if the given board is unsolvable.
(check-expect (solve (make-board 6
                                 (build-list (sqr 6) identity)
                                 TAT6EASY-P1
                                 (make-numtents TAT6EASY-COL1
                                                TAT6EASY-ROW1)))
              (make-board 6
                          (build-list (sqr 6) identity)
                          TAT6EASY-S1
                          (make-numtents TAT6EASY-COL1
                                         TAT6EASY-ROW1)))

;(define (solve b) false)


(define (solve b)
  
  (local [(define (solve--board b)
            (if (solved? b)
                b
                (solve--lob (next-boards b))))

          (define (solve--lob lob)
            (cond [(empty? lob) false]
                  [else
                   (local [(define try (solve--board (first lob)))]
                     (if (not (false? try))
                         try
                         (solve--lob (rest lob))))]))]
    
    (solve--board b)))


;; Board -> Boolean
;; Given a TAT board, return true if it is solved.
;; A tents-and-trees puzzle is solved iff:
;;     - the number of tents correspond with the number of tents specified in numtents,
;;     - for every tree, there is a corresponding tent positioned either horizontally or vertically,
;;     - a tent cannot be adjacent to another tent vertically, horizontally, or diagonally.
;; !!!
(define (solved? b) false)


;; Board -> (listof Board)
;; Given a TAT board, return a list of all possible next board.
;; Among the next boards, keep only those that are valid.
;; A tents-and-trees puzzle is valid iff:
;;     - the number of tents correspond with the number of tents specified in numtents,
;;     - for every tree, there is a corresponding tent positioned either horizontally or vertically,
;;     - a tent cannot be adjacent to another tent vertically, horizontally, or diagonally.
(check-expect (next-boards (make-board 6
                                       (build-list (sqr 6) identity)
                                       (list 1 0 1 0 0 0
                                             0 0 0 0 1 0
                                             0 0 1 0 0 0
                                             1 0 0 1 0 0
                                             0 0 0 0 0 0
                                             0 0 0 0 1 0)
                                       (make-numtents (list 1 2 0 2 1 1)
                                                      (list 2 0 2 0 2 1))))
              (list (make-board 6
                                (build-list (sqr 6) identity)
                                (list 1 2 1 0 0 0
                                      0 0 0 0 1 0
                                      0 0 1 0 0 0
                                      1 0 0 1 0 0
                                      0 0 0 0 0 0
                                      0 0 0 0 1 0)
                                (make-numtents (list 1 2 0 2 1 1)
                                               (list 2 0 2 0 2 1)))))
(check-expect (next-boards (make-board 6
                                       (build-list (sqr 6) identity)
                                       (list 1 2 1 2 0 0
                                             0 0 0 0 1 0
                                             0 2 1 0 2 0
                                             1 0 0 1 0 0
                                             2 0 0 0 0 0
                                             0 0 0 0 1 0)
                                       (make-numtents (list 1 2 0 2 1 1)
                                                      (list 2 0 2 0 2 1))))
              (list (make-board 6
                                (build-list (sqr 6) identity)
                                (list 1 2 1 2 0 0
                                      0 0 0 0 1 0
                                      0 2 1 0 2 0
                                      1 0 0 1 0 0
                                      2 0 0 2 0 0
                                      0 0 0 0 1 0)
                                (make-numtents (list 1 2 0 2 1 1)
                                               (list 2 0 2 0 2 1)))))

;(define (next-boards b) empty)

(define (next-boards b)
  (only-valid-boards (next-possible-boards b)))


;; Board -> (listof Board)
;; Given a TAT board, return a list of all possible next board.
(check-expect (next-possible-boards (make-board 6
                                                (build-list (sqr 6) identity)
                                                (list 1 0 1 0 0 0
                                                      0 0 0 0 1 0
                                                      0 0 1 0 0 0
                                                      1 0 0 1 0 0
                                                      0 0 0 0 0 0
                                                      0 0 0 0 1 0)
                                                (make-numtents (list 1 2 0 2 1 1)
                                                               (list 2 0 2 0 2 1))))
              (list (make-board 6
                                (build-list (sqr 6) identity)
                                (list 1 2 1 0 0 0
                                      0 0 0 0 1 0
                                      0 0 1 0 0 0
                                      1 0 0 1 0 0
                                      0 0 0 0 0 0
                                      0 0 0 0 1 0)
                                (make-numtents (list 1 2 0 2 1 1)
                                               (list 2 0 2 0 2 1)))
                    (make-board 6
                                (build-list (sqr 6) identity)
                                (list 1 0 1 0 0 0
                                      2 0 0 0 1 0
                                      0 0 1 0 0 0
                                      1 0 0 1 0 0
                                      0 0 0 0 0 0
                                      0 0 0 0 1 0)
                                (make-numtents (list 1 2 0 2 1 1)
                                               (list 2 0 2 0 2 1)))))
(check-expect (next-possible-boards (make-board 6
                                                (build-list (sqr 6) identity)
                                                (list 1 2 1 2 0 0
                                                      0 0 0 0 1 0
                                                      0 2 1 0 2 0
                                                      1 0 0 1 0 0
                                                      2 0 0 0 0 0
                                                      0 0 0 0 1 0)
                                                (make-numtents (list 1 2 0 2 1 1)
                                                               (list 2 0 2 0 2 1))))
              (list (make-board 6
                                (build-list (sqr 6) identity)
                                (list 1 2 1 2 0 0
                                      0 0 0 0 1 0
                                      0 2 1 2 2 0
                                      1 0 0 1 0 0
                                      2 0 0 0 0 0
                                      0 0 0 0 1 0)
                                (make-numtents (list 1 2 0 2 1 1)
                                               (list 2 0 2 0 2 1)))
                    (make-board 6
                                (build-list (sqr 6) identity)
                                (list 1 2 1 2 0 0
                                      0 0 0 0 1 0
                                      0 2 1 0 2 0
                                      1 0 2 1 0 0
                                      2 0 0 0 0 0
                                      0 0 0 0 1 0)
                                (make-numtents (list 1 2 0 2 1 1)
                                               (list 2 0 2 0 2 1)))
                    (make-board 6
                                (build-list (sqr 6) identity)
                                (list 1 2 1 2 0 0
                                      0 0 0 0 1 0
                                      0 2 1 0 2 0
                                      1 0 0 1 0 0
                                      2 0 0 2 0 0
                                      0 0 0 0 1 0)
                                (make-numtents (list 1 2 0 2 1 1)
                                               (list 2 0 2 0 2 1)))
                    (make-board 6
                                (build-list (sqr 6) identity)
                                (list 1 2 1 2 0 0
                                      0 0 0 0 1 0
                                      0 2 1 0 2 0
                                      1 0 0 1 2 0
                                      2 0 0 0 0 0
                                      0 0 0 0 1 0)
                                (make-numtents (list 1 2 0 2 1 1)
                                               (list 2 0 2 0 2 1)))))

(define (next-possible-boards b) empty)

#;
(define (next-possible-boards b)
  (local []
    ()))

;; (listof Board) -> (listof Board)
;; Given a list of boards, return a list of only valid boards.
;; !!!
(define (only-valid-boards lob) empty)

;; ---- Board Methods ----

;; Position Natural -> Natural
;; Given a position and the board's side length, return the position's row.
(check-expect (position->row 11 5) 2)
(check-expect (position->row 64 10) 6)

;(define (position->row p s) 0)

(define (position->row p s)
  (quotient p s))

;; Position Natural -> Natural
;; Given a position and the board's side length, return the position's column.
(check-expect (position->column 11 5) 1)
(check-expect (position->column 64 10) 4)

;(define (position->column p s) 0)

(define (position->column p s)
  (remainder p s))

;; Natural Natural Natural -> Position
;; Given a row, a column, and the board's side length, return the position.
(check-expect (rc->position 1 1 11) 12)
(check-expect (rc->position 2 1 5) 11)
(check-expect (rc->position 6 4 10) 64)

;(define (rc->position r c s) 0)

(define (rc->position r c s)
  (+ (* r s) c))

;; Natural Natural Natural (listof X) -> X or false
;; Given a row r, column c, side length n, and a list of elements (where this
;; list can be visualized as an n-by-n array of elements, return the element
;; at row position r and column position c in this array.
(check-expect (read-list 1 1 4 (list 0 0 0 0
                                     0 1 0 0
                                     0 0 0 0
                                     0 0 0 0))
              1)

;(define (read-list r c n lox) 0)

(define (read-list r c n lox)
  (local [(define position
           (rc->position r c n))

          (define (read-list p lox)
            (cond [(empty? lox) false]
                  [else
                   (if (= p position)
                       (first lox)
                       (read-list (add1 p) (rest lox)))]))]
    (read-list 0 lox)))

;; Natural Natural Natural X (listof X) -> (listof X)
;; Given a row r, column c, side length n, a value k, and a list of elements
;; (where this list can be visualized as an n-by-n array of elements), return
;; the given list with the value k inserted at row position r and column position c.
(check-expect (write-list 1 1 7 1 (build-list (* 7 7) (λ (p) 0)))
              (list 0 0 0 0 0 0 0
                    0 1 0 0 0 0 0
                    0 0 0 0 0 0 0
                    0 0 0 0 0 0 0
                    0 0 0 0 0 0 0
                    0 0 0 0 0 0 0
                    0 0 0 0 0 0 0))

;(define (write-list r c n k lox) lox)

(define (write-list r c n k lox)
  (local [(define position
            (rc->position r c n))

          (define (write-list p k lox)
            (cond [(empty? lox) empty]
                  [else
                   (if (zero? p)
                       (cons k (rest lox))
                       (cons (first lox)
                             (write-list (sub1 p) k (rest lox))))]))]
    (write-list position k lox)))

;; https://www.brainbashers.com/tents.asp
;; https://www.puzzle-tents.com/