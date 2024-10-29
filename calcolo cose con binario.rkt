;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |calcolo cose con binario|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define bin-val
  (lambda (bin)
    (let (
          (k (- (string-length bin) 1))
          )
      
      (if (= k 0)
          (bit-val (string-ref bin k))
          (+ (* 2 (bin-val (substring bin 0 k)))
          (bit-val (string-ref bin k)))
      ))
 ))

(define bit-val
  (lambda (bit)
    ;(if (char=? #\0)
    (- (char->integer bit) ascii-0)
    ))
(define ascii-0 (char->integer #\0))

(define bin-rep
  (lambda (n)
    (let (
          (q (quotient  n 2))
          (r (remainder n 2))
          )
      (if (= q 0)
          (bit-rep r) ;casi base
          (string-append
           (bin-rep q)
           (bin-rep r)
           ))
          )
      )
    )

(define bit-rep   ;val: stringa "0" oppure "1"
  (lambda (v)     ;v: 0 oopure 1
    (if (= v 0)
        "0"
        "1"
        )
    ))

(define num-val
  (lambda (num b)
    (let (
          (k (- (string-length num) 1))
          )
      
      (if (= k 0)
          (dgt-val (string-ref num k))
          (+ (* b (num-val (substring num 0 k)b))
          (dgt-val (string-ref  num k)))
      ))
 ))

(define dgt-val
  (lambda (dgt)
    (- (char->integer dgt) ascii-0)
    ))