;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Drawing) (read-case-sensitive #t) (teachpacks ((lib "valrose.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "valrose.rkt" "installed-teachpacks")) #f)))
(define PIXEL_RATIO 0.5)
(define BACKGROUND_COLOR "white")
(define DEFAULT_COLOR "black")
(define NOTHING (rectangle 0 0 'solid BACKGROUND_COLOR))

(define HEAD
  (local [
          (define EYE
            (circle (* 26 PIXEL_RATIO) 'solid DEFAULT_COLOR))

          (define EYEBROW
            (rectangle (* 92 PIXEL_RATIO) (* 13 PIXEL_RATIO) 'solid DEFAULT_COLOR))

          (define EYEBROW2
            (rectangle (* 65 PIXEL_RATIO) (* 13 PIXEL_RATIO) 'solid DEFAULT_COLOR))

          (define FACE_OUTLINE
            (circle (* 171 PIXEL_RATIO) 'outline DEFAULT_COLOR))

          (define EATING
            (local [(define visible
                      (ellipse (* 300 PIXEL_RATIO) (* 165 PIXEL_RATIO) 'outline DEFAULT_COLOR))
                    (define invisible
                      (rectangle (* 250 PIXEL_RATIO) (* 166 PIXEL_RATIO) 'solid BACKGROUND_COLOR))]
              (underlay/xy visible (* 64 PIXEL_RATIO) (* 0 PIXEL_RATIO) invisible)))

          (define REST
            (local [(define first (rectangle (* 12 PIXEL_RATIO) (* 25 PIXEL_RATIO) 'solid DEFAULT_COLOR))
                    (define second (circle (* 5 PIXEL_RATIO) 'solid DEFAULT_COLOR))]
              (underlay/xy first (* 31 PIXEL_RATIO) (* 25 PIXEL_RATIO) second)))

          (define MOUTH
            (local [(define LIPS (ellipse (* 180 PIXEL_RATIO) (* 92 PIXEL_RATIO) 'solid DEFAULT_COLOR))
                    (define TONGUE (rotate 350 (ellipse (* 50 PIXEL_RATIO) (* 35 PIXEL_RATIO) 'solid BACKGROUND_COLOR)))]
              (overlay/xy TONGUE (- (* 17 PIXEL_RATIO)) (- (* 42 PIXEL_RATIO)) LIPS)))]

    ; LET'S DRAW THIS !
    (underlay/xy
         FACE_OUTLINE
         (- 0 (* 5 PIXEL_RATIO)) (* 75 PIXEL_RATIO)
         (underlay/xy
              EATING
              (* 50 PIXEL_RATIO) (- (* 33 PIXEL_RATIO))
              (underlay/xy
                   MOUTH
                   (- 0 (* 18 PIXEL_RATIO)) (- (* 75 PIXEL_RATIO))
                   (underlay/xy
                        REST
                        (* 40 PIXEL_RATIO) (* (- 0 133) PIXEL_RATIO)
                        (underlay/xy
                             EYE
                             (- 0 (* 30 PIXEL_RATIO)) (- 0 (* 15 PIXEL_RATIO))
                             (underlay/xy
                                  EYEBROW
                                  (* 170 PIXEL_RATIO) (- (* 10 PIXEL_RATIO))
                                  (underlay/xy
                                       EYE
                                       (* 0 PIXEL_RATIO) (- 0 (* 20 PIXEL_RATIO))
                                       EYEBROW2)))))))
    )
)

(define BODY
  (local [
          (define MAIN
            (local [
                    (define VISIBLE (circle (* (/ 372 2) PIXEL_RATIO) 'outline DEFAULT_COLOR))
                    (define INVISIBLE (rectangle (* 271 PIXEL_RATIO) (* 308 PIXEL_RATIO) 'solid BACKGROUND_COLOR))]
              (underlay/xy
               INVISIBLE
               (- (* 150 PIXEL_RATIO)) (* 28 PIXEL_RATIO)
               VISIBLE)))

          (define LEFT_ARM
            (local [
                    (define VISIBLE (ellipse (* 513 PIXEL_RATIO) (* 228 PIXEL_RATIO) 'outline DEFAULT_COLOR))
                    (define INVISIBLE (rectangle (* 520 PIXEL_RATIO) (* 250 PIXEL_RATIO) 'solid BACKGROUND_COLOR))]
              (overlay/xy
               INVISIBLE
               (* 0 PIXEL_RATIO) (* 70 PIXEL_RATIO)
               VISIBLE)))

          (define RIGHT_ARM
            (ellipse (* 265 PIXEL_RATIO) (* 154 PIXEL_RATIO) 'outline DEFAULT_COLOR))]

    ;DRAW THE BODY
    (overlay/xy
     LEFT_ARM
     0 0
     MAIN)))


(underlay/xy
 BODY
 52 (- 150)
 HEAD)

