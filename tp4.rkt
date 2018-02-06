;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname tp4) (read-case-sensitive #t) (teachpacks ((lib "valrose.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "valrose.rkt" "installed-teachpacks")) #f)))
; un 'x' qui de décrémentera usqu'à 0
(define (EX_1 lim)
  (local [(define RECT (rectangle 300 200 'outline "black"))
          (define (TEXT t) (text (number->string t) 100 "black"))
          (define (NEXT x) (+ x 1))
          (define (DRAW x) (underlay RECT (TEXT x)))]
    (big-bang 0
      (on-tick NEXT 1 lim)
      (on-draw DRAW))))


(define (EX_2 n)
  (local [(define WIDTH 800)
          (define HEIGHT WIDTH)
          (define speed 10)
          (define F (/ WIDTH 2))
          (define-struct monde (x y img n))
          (define INIT (make-monde (/ WIDTH 2) (/ HEIGHT 2) (rectangle WIDTH HEIGHT 'solid "yellow") 0))
          (define D 20)
          (define (NEXT s)
            (local [(match-define (monde x y img n) s)
                    (define r (* D (random)))
                    (define t (* 2 pi (random)))
                    (define x2 (+ x (* r (cos t))))
                    (define y2 (+ y (* r (sin t))))
                    (define img2 (scene+line img x y x2 y2 "black"))] 
              (make-monde x2 y2 img2 (+ n 1))))
          (define (DRAW s)
            (local [(match-define (monde x y img n) s)]
             (place-image (circle 1 'solid "black") x y img)))]
    (big-bang INIT
      (on-tick NEXT 1/60)
      (on-draw DRAW))))

(define EX_3
  (local [(define INIT 0)
          (define WIDTH 500)
          (define HEIGHT WIDTH)])
  (big-bang INIT
    (on-tick next 1/28)
    (on-tick draw)))