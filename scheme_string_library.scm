; returns:
; ... #t ... on success
; ... #f ... on failure
;
(define (str-starts-with? str needle)
  (let
    (
      (len_str (string-length str))
      (len_needle (string-length needle))
      (matched #t)
    )
      (if (or (< len_str len_needle) (zero? len_needle))
        #f
        (do
          (
            (i 0 (+ i 1))
          )
          ((>= i len_needle) matched)
          (unless (char=? (string-ref str i) (string-ref needle i))
            (set! matched #f)
            (set! i len_needle)
          )
        )
      )
  )
)

; returns:
; ... #t ... on success
; ... #f ... on failure
;
(define (str-ends-with? str needle)
  (let
    (
      (len_str (string-length str))
      (len_needle (string-length needle))
      (matched #t)
    )
      (if (or (< len_str len_needle) (zero? len_needle))
        #f
        (do
          (
            (i (- len_needle 1) (- i 1))
            (j (- len_str 1) (- j 1))
          )
          ((< i 0) matched)
          (unless (char=? (string-ref str j) (string-ref needle i))
            (set! matched #f)
            (set! i 0)
          )
        )
      )
  )
)

; returns:
; ... #t ... on success
; ... #f ... on failure
;
(define (str-exists? str needle)
  (let
    (
      (len_str (string-length str))
      (len_needle (string-length needle))
      (matched #t)
      (delta 0)
    )
      (if (or (< len_str len_needle) (zero? len_needle))
        #f
        (do
          (
            (i 0 (+ i 1))
          )
          ((>= i len_needle) matched)
          (unless (char=? (string-ref str (+ i delta)) (string-ref needle i))
            (set! delta (+ delta 1))
            (set! i -1)
            (when (or (> len_needle (- len_str delta)) (zero? (- len_str delta)))
              (set! i len_needle)
              (set! matched #f)
            )
          )
        )
      )
  )
)

; returns:
; ... index ... on success
; ... -1 ... on failure
;
(define (str-find str needle)
  (let
    (
      (len_str (string-length str))
      (len_needle (string-length needle))
      (delta 0)
    )
      (if (or (< len_str len_needle) (zero? len_needle))
        -1
        (do
          (
            (i 0 (+ i 1))
          )
          ((>= i len_needle) delta)
          (unless (char=? (string-ref str (+ i delta)) (string-ref needle i))
            (set! delta (+ delta 1))
            (set! i -1)
            (when (or (> len_needle (- len_str delta)) (zero? (- len_str delta)))
              (set! i len_needle)
              (set! delta -1)
            )
          )
        )
      )
  )
)

; returns:
; ... (list of indices) ... on success
; ... () ... on failure
;
(define (str-find-all str needle)
  (let*
    (
      (len_str (string-length str))
      (len_needle (string-length needle))
      (len_str_bbb len_str)
      (result '())
    )
      (if (or (< len_str len_needle) (zero? len_needle))
        result
        (do
          (
            (inx (str-find str needle) (str-find (substring str (+ (car result) len_needle) len_str) needle))
          )
          ((= inx -1) (reverse result))
          (set! result (cons (+ inx (- len_str len_str_bbb)) result))
          (set! len_str_bbb (- len_str_bbb (+ inx len_needle)))
        )
      )
  )
)

; returns:
; ... (list of sub_strings) ... on success
; ... () ... on failure
;
(define (str-split str needle)
  (let
    (
      (the_inxs (str-find-all str needle))
      (result '())
      (len_needle (string-length needle))
      (start 0)
    )
      (if (null? the_inxs)
        result
        (do
          (
            (inxs the_inxs (cdr inxs))
          )
          ((null? inxs) (reverse (cons (substring str start (string-length str)) result)))
          (set! result (cons (substring str start (car inxs)) result))
          (set! start (+ (car inxs) len_needle))
        )
      )
  )
)

(define str-tokenize str-split) ; alias

; Copyright (c) 2020 civAnimal ... Email: civanimal@gmail.com ... Twitter: civAnimal
; Released under ... Creative Commons License (CC BY-SA)
