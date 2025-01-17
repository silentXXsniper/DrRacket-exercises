;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |es 2|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss.txt" "installed-teachpacks") (lib "hanoi.ss.txt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss.txt" "installed-teachpacks") (lib "hanoi.ss.txt" "installed-teachpacks")) #f)))
#; (2. Procedure in Scheme
Completa il programma increment, che calcola l’incremento di un numero naturale rappresentato come stringa di
cifre in una base compresa fra 2 e 10. Gli argomenti sono num, la stringa numerica, e base, di tipo intero; il valore
restituito è una stringa numerica. Per esempio, il valore dell’espressione (increment "1011" 2) è "1100", dove
le stringhe rappresentano rispettivamente 11 e 12 in base 2.
(define offset (char->integer #\0))
(define last-digit
(lambda (base) (integer->char (+ (- base 1) offset)) ))
(define next-digit
(lambda (dgt) ( (integer->char (+ (char->integer dgt) 1))) ))
(define increment
(lambda (num base) ; 2 <= base <= 10
(let ((digits (string-length num)))
(if (= digits 0)
"1"
(let ((dgt ))
(if (char=? dgt (last-digit base))
(string-append
"0")
(string-append (substring num 0 (- digits 1)) )
))
))))
)


(define offset (char->integer #\0))
(define last-digit
(lambda (base) (integer->char (+ (- base 1) offset)) ))
(define next-digit
(lambda (dgt) ( .............(integer->char (+ (char->integer dgt) 1))) ))
(define increment
(lambda (num base) ; 2 <= base <= 10
(let ((digits (string-length num)))
(if (= digits 0)
"1"
(let ((dgt ..................))
(if (char=? dgt (last-digit base))
(string-append ....................
"0")
(string-append (substring num 0 (- digits 1))................... )
))
))))