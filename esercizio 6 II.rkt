;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |esercizio 6 II|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss.txt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss.txt" "installed-teachpacks")) #f)))
(define (diff text1 text2)
  (letrec
      ;; Funzione principale per calcolare le differenze
      ((compute-diff
        (lambda (t1 t2 index1 index2 acc)
          (cond
            ;; Caso base: entrambi i testi sono vuoti
            [(and (null? t1) (null? t2))
             (reverse acc)]
            ;; Caso: prima lista esaurita, righe rimanenti da aggiungere
            [(null? t1)
             (compute-diff t1 (cdr t2) index1 (+ index2 1)
                           (cons (list index1 'a index2 (car t2)) acc))]
            ;; Caso: seconda lista esaurita, righe da eliminare
            [(null? t2)
             (compute-diff (cdr t1) t2 (+ index1 1) index2
                           (cons (list index1 'd index2 (car t1)) acc))]
            ;; Caso: le righe sono uguali, passiamo oltre
            [(string=? (car t1) (car t2))
             (compute-diff (cdr t1) (cdr t2) (+ index1 1) (+ index2 1) acc)]
            ;; Caso: le righe sono diverse, segnala differenze
            [else
             (compute-diff (cdr t1) t2 (+ index1 1) index2
                           (cons (list index1 'd index2 (car t1)) acc))
             (compute-diff t1 (cdr t2) index1 (+ index2 1)
                           (cons (list index1 'a index2 (car t2)) acc))]))))
    ;; Avvio della funzione ricorsiva con indici iniziali
    (compute-diff text1 text2 1 1 '())))