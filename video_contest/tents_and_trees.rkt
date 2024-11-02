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
  (list 0 0 0 1 0 0
        1 0 0 1 0 0
        0 0 0 0 1 0
        0 1 0 0 0 0
        0 0 0 0 0 1
        0 0 0 1 0 0))

(define TAT6EASY-S1
  (list 2 0 0 1 2 0
        1 0 0 1 0 0
        0 0 0 2 1 2
        2 1 0 0 0 0
        0 0 0 0 0 1
        0 0 2 1 0 2))

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


(define-struct numtents (ntent_row ntent_col))
;; NumTents is (make-numtents (listof Natural[0, (board-side-length board))
;;                            (listof Natural[0, (board-side-length board)))
;; interp. (make-numtents ntent-row ntent-col) is a specification for the number
;;          of tents that a column or row should have. Numtrees is better understood
;;          visually like this:
;;          (make-numtents (list     r_0 r_1 ... r_n)
;;                         (list c_0
;;                               c_1
;;                               ...
;;                               c_n))
;;          with i as the row number and j as the column number,
;;           - r_i represents the number of tents in row i, and
;;           - c_j represents the number of tents in column j.


(define-struct board (side-length lop lov nt))
;; Board is (make-board Natural[5, 20] (listof Position) (listof Value) NumTents)
;; interp. (make-board side-length lop lov nt) is a TAT board, with the properties:
;;         - side-length is the length of the board's side,
;;         - lop is the flat list of all positions,
;;         - lov is the arrangement of all values, provided as a flat list, corresponding with lop, and
;;         - nt is the specification of the numer of tents each row and column must have.


;; ---- Function Defintions ----





;; https://www.brainbashers.com/tents.asp
;; https://www.puzzle-tents.com/