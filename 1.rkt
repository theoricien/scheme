;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1|) (read-case-sensitive #t) (teachpacks ((lib "valrose.rkt" "installed-teachpacks"))) (htdp-settings #(#t write mixed-fraction #f #t none #f ((lib "valrose.rkt" "installed-teachpacks")) #f)))
(define (s x)
  (cond
    ((< x -3) 0)
    ((<= x -1) 1)
    ((<= x 2) 0)
    ((<= x 4) 2)
    (else 0)
    )
  )

(define (distance x1 y1 x2 y2)
  (sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1)))))
 
(define (point-interieur? x y a b r)
  (<= (distance x y a b) r))

(define (air_triangle a b c)
  (local [(define p (/ (+ a b c) 2))]
    (sqrt (* (* p (- p a) (- p b) (- p c))))))

(define (triangle? a b c)
  (real? (air_triangle a b c)))

; don't work
(define (tranche s b h p)
  (* s (if (> 0 (- s h)) 2 (if (> b (- h s)) 1 0)) (/ p 100)) )

(define (impot s)
  (+ (tranche s 8000 25000 10) (tranche s 25000 +inf.0 20)))

(define (s_c_m a b c)
  (max ))

(define (sum x)
  (/ (* n (+ n 1)) 2))