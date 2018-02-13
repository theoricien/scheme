;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname tp5) (read-case-sensitive #t) (teachpacks ((lib "valrose.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "valrose.rkt" "installed-teachpacks")) #f)))
(define (ex1_a start limit padding)
  (if (< start limit)
      (+ (sqr start) (ex1_a (+ start padding) limit padding))
      (sqr start)))

(define (sum start lim)
  (local [(define (f_sum a) (/ (* a (+ a 1)) 2))]
    (- (f_sum lim) (f_sum (- start 1)))))

(define (interfac a b)
  (if (equal? a b)
      a
      (* a (interfac (+ a 1) b))))

(define (fac n)
  (interfac 2 n))

(define (nb_parmi_nb nb2 nb1)
  (/ (fac nb2) (* (fac nb2) (fac (- nb2 nb1)))))

(define (integrale f a b h)
  (local [(define HEIGHT (f a))
          (define WIDTH h)
          (define AIR (* WIDTH HEIGHT))]
    (if (> a b)
        0
        (+ AIR (integrale f (+ a h) b h)))))

;(define (f x) (exp (- (sqr x))))
;(integrale f (- 1) 1 0.01)
 
(define (somme-chiffres n)
  (if (equal? (modulo n 10) n)
      n
      (+ (modulo n 10) (somme-chiffres (/ (- n (modulo n 10)) 10)))))

(define (nb1 n digit base)
  (local [(define n_based
            (string->number (number->string n base)))]
    (/ (somme-chiffres n_based) digit)))
