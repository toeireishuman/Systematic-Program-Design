;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname parameterization-ms) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; parameterization-starter.rkt

(* pi (sqr 4)) ;area of circle radius 4
(* pi (sqr 6)) ;area of circle radius 6


;; ====================

;; ListOfString -> Boolean
;; produce true if los includes "UBC"
(check-expect (contains-ubc? empty) false)
(check-expect (contains-ubc? (cons "McGill" empty)) false)
(check-expect (contains-ubc? (cons "UBC" empty)) true)
(check-expect (contains-ubc? (cons "McGill" (cons "UBC" empty))) true)

;(define (contains-ubc? los) false) ;stub

;<template from ListOfString>

#;
(define (contains-ubc? los)
  (cond [(empty? los) false]
        [else
         (if (string=? (first los) "UBC")
             true
             (contains-ubc? (rest los)))]))

(define (contains-ubc? los)
  (contains? "UBC" los))

;; ListOfString -> Boolean
;; produce true if los includes "McGill"
(check-expect (contains-mcgill? empty) false)
(check-expect (contains-mcgill? (cons "UBC" empty)) false)
(check-expect (contains-mcgill? (cons "McGill" empty)) true)
(check-expect (contains-mcgill? (cons "UBC" (cons "McGill" empty))) true)

;(define (contains-mcgill? los) false) ;stub

;<template from ListOfString>
#;
(define (contains-mcgill? los)
  (cond [(empty? los) false]
        [else
         (if (string=? (first los) "McGill")
             true
             (contains-mcgill? (rest los)))]))

(define (contains-mcgill? los)
  (contains? "McGill" los))


;; String (listof String) -> Boolean
;; produce true if los includes string s
(check-expect (contains? "UBC" empty) false)
(check-expect (contains? "UBC" (cons "McGill" empty)) false)
(check-expect (contains? "UBC" (cons "UBC" empty)) true)
(check-expect (contains? "UBC" (cons "McGill" (cons "UBC" empty))) true)
(check-expect (contains? "UBC" (cons "UBC" (cons "McGill" empty))) true)
(check-expect (contains? "McGill" (cons "UBC" (cons "McGill" empty))) true)
(check-expect (contains? "Toronto" (cons "UBC" (cons "McGill" empty))) false)

(define (contains? s los)
  (cond [(empty? los) false]
        [else
         (if (string=? (first los) s)
             true
             (contains? s (rest los)))]))



;; ====================

;; ListOfNumber -> ListOfNumber
;; produce list of sqr of every number in lon
(check-expect (squares empty) empty)
(check-expect (squares (list 3 4)) (list 9 16))

;(define (squares lon) empty) ;stub

;<template from ListOfNumber>
#;
(define (squares lon)
  (cond [(empty? lon) empty]
        [else
         (cons (sqr (first lon))
               (squares (rest lon)))]))
(define (squares lon)
  (map2 sqr lon))


;; ListOfNumber -> ListOfNumber
;; produce list of sqrt of every number in lon
(check-expect (square-roots empty) empty)
(check-expect (square-roots (list 9 16)) (list 3 4))

;(define (square-roots lon) empty) ;stub

;<template from ListOfNumber>
#;
(define (square-roots lon)
  (cond [(empty? lon) empty]
        [else
         (cons (sqrt (first lon))
               (square-roots (rest lon)))]))
(define (square-roots lon)
  (map2 sqrt lon))


;; (X -> Y) (listof X) -> (listof Y)
;; given opr and (list n0 n1 ...), produce (list (opr n0) (opr n1) ...)
(check-expect (map2 sqr empty) empty)
(check-expect (map2 sqr (list 4 8)) (list 16 64))
(check-expect (map2 sqrt (list 25 16 9)) (list 5 4 3))
(check-expect (map2 abs (list 1 -4 9 -11)) (list 1 4 9 11))
(check-expect (map2 sub1 (list 1 -4 9 -11)) (list 0 -5 8 -12))
(check-expect (map2 string-length (list "hello"
                                        ""
                                        "how"
                                        "hi"))
              (list 5 0 3 2))

(define (map2 opr lon)
  (cond [(empty? lon) empty]
        [else
         (cons (opr (first lon))
               (map2 opr (rest lon)))]))

;; ====================

;; ListOfNumber -> ListOfNumber
;; produce list with only positive? elements of lon
(check-expect (positive-only empty) empty)
(check-expect (positive-only (list 1 -2 3 -4)) (list 1 3))

;(define (positive-only lon) empty) ;stub

;<template from ListOfNumber>
#;
(define (positive-only lon)
  (cond [(empty? lon) empty]
        [else
         (if (positive? (first lon))
             (cons (first lon)
                   (positive-only (rest lon)))
             (positive-only (rest lon)))]))
(define (positive-only lon)
  (filter2 positive? lon))


;; ListOfNumber -> ListOfNumber
;; produce list with only negative? elements of lon
(check-expect (negative-only empty) empty)
(check-expect (negative-only (list 1 -2 3 -4)) (list -2 -4))

;(define (negative-only lon) empty) ;stub

;<template from ListOfNumber>
#;
(define (negative-only lon)
  (cond [(empty? lon) empty]
        [else
         (if (negative? (first lon))
             (cons (first lon)
                   (negative-only (rest lon)))
             (negative-only (rest lon)))]))
(define (negative-only lon)
  (filter2 negative? lon))



;; My solution

;; (X -> Boolean) (listof X) -> (listof X)
;; filter list to contain only those elements
;; for which test produces true
(check-expect (filter2 positive? empty) empty)
(check-expect (filter2 positive? (list -1)) empty)
(check-expect (filter2 negative? (list -1)) (list -1))
(check-expect (filter2 positive? (list 4 -2 0 10)) (list 4 10))
(check-expect (filter2 zero? (list (- 4 4) -11 0 63 (sqr 0)))
              (list 0 0 0))
(check-expect (filter2 empty? (list (list 1) empty (list 2 4) empty))
              (list empty empty))

(define (filter2 test lon)
  (cond [(empty? lon) empty]
        [else
         (if (test (first lon))
             (cons (first lon)
                   (filter2 test (rest lon)))
             (filter2 test (rest lon)))]))