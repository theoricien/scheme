;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |2|) (read-case-sensitive #t) (teachpacks ((lib "valrose.rkt" "installed-teachpacks"))) (htdp-settings #(#t write mixed-fraction #f #t none #f ((lib "valrose.rkt" "installed-teachpacks")) #f)))
(define (einstein u v)
  (local [(define c (#i3000000))]
  (/ (+ u v) (+ 1 (/ (* u v) (sqr c))))))

(define (stirling n)
  (* (sqrt (* 2 n pi)) (expt (/ n e) n)))

(define (rand-impair)
  (local [(define r (random 101))]
    (if (= (modulo r 2) 0) (if (= r 100) 99 (+ r 1)) r)))

(define (rand-between a b)
  (local [(define grand (random (- b a)))
          (define petit (random 1))]
    (+ grand a (/ petit (modulo (numerator (- b a)) (denominator (- b a)))))))

(rand-between e pi)

(define (derivee f x)
  (local [(define b #i0.001)]
    (/ (- (f (+ x h)) (f x)) h)))

(define (deriv f n x)
  (if (= n 0) f (deriv (lambda (x) (derivee f x)) (- n 1) x)))

(define (bar n)
  (lambda (n) (* 2 n)))

((bar 5))