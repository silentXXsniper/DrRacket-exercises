;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |esercizio 5|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss.txt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss.txt" "installed-teachpacks")) #f)))
(define L-tessellation
  (lambda (n)
    (if (= n 1)
        L-tile
        (glue-tiles
         (L-tessellation (/ n 2))
         (glue-tiles
          (up-right (L-tessellation (/ n 2))n)
          (glue-tiles
           (down-left (L-tessellation (/ n 2)) n)
           (move-down (L-tessellation (/ n 2))(/ n 2))
           )
          )
         )
        )
    ))

(define move-down
  (lambda (fig n)
    (shift-right (shift-down fig n) n)
    ))

(define down-left
  (lambda (fig n)
    (shift-down (quarter-turn-left fig ) n)
    ))

(define up-right
  (lambda (fig n)
    (shift-right (quarter-turn-right fig ) n)
    ))

(set-tessellation-shift-step!)


;per eseguire rapidamente tutti gli esempi usare il comando esempi
(define esempi
  (list
   "esempi funzionamento"
(L-tessellation 1)
"--------------------------------------"
(L-tessellation 2)
"--------------------------------------"
(L-tessellation 4)
"--------------------------------------"
(L-tessellation 8)
"--------------------------------------"
(L-tessellation 16)
"--------------------------------------"
(L-tessellation 32)
))