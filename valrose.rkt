;;; teachpack valrose.rkt - Livre "Premiers Cours de Programmation avec Scheme" (Ellipses ed, 2010)
;;; Doit etre sauvegarde comme <<< langage determine par le code source >>> !
;;; Racket v6.6 - Janvier 2017

#lang racket

(require 2htdp/image 2htdp/universe net/url)              ; images et animations

(provide 
 (all-from-out 2htdp/image 2htdp/universe)
 show srandom reduce pair? match-define                   ; quelques utilitaires manquants pour l'enseignement
 read-from-url read-from-file print-to-file               ; i/o minimales
 arbre racine fg fd feuille? operateur?                   ; les arbres 2-3 d'expressions algebriques
 pile-vide pile-vide? empiler depiler sommet)             ; les piles fonctionnelles
; atome? make-neg make-fbf2 connecteur arg1 arg2)         ; les FBF de la Logique d'ordre 0 (pour le livre PCPS)

(define-struct posn (x y) #:transparent)

; petit utilitaire pour avoir les tests dans l'editeur avec echo au toplevel
(define-syntax show
  (syntax-rules ()
    ((show e) (begin (printf "? ~s\n" 'e) (printf "--> ~s\n" e)))))

; pour le MOOC "Programmation Recursive" de Christian Queinnec

(define (pair? x)
  (and (list? x) (not (empty? x))))      ; list? et pair? sont O(1) en langage d'enseignement !

(define (reduce f e L)                   ; abstraction du parcours de liste recursif
  (foldr f e L))

; (read-from-file f) retourne la premiere expression Scheme disponible dans le fichier f
; (print-to-file x f) cree un nouveau fichier f et y depose la valeur de x, aucun resultat.

(define (read-from-url url)   ; retourne le contenu de url comme liste de string
  (call/input-url (string->url url) get-pure-port
                  (lambda (p-in)
                    (let loop ((str (read-line p-in)) (L empty))
                      (if (eof-object? str)
                          (reverse L)
                          (loop (read-line p-in) (cons str L)))))))

;(define PRIMES (map string->number (read-from-url "http://nombrespremiersliste.free.fr/listes/1-1000000.txt")))

(define (read-from-file f)    ; retourne la premiere expression Scheme du fichier f
  (call-with-input-file f read))

;(define PRIMES (read-from-file "../data/1-1000000.txt")    ; par exemple

(define (print-to-file f x mode)     ; mode = 'replace ou 'append (si f existe deja)
  (call-with-output-file f
    (lambda (p-out)
      (fprintf p-out "~s\n" x))
    #:exists mode))

;(print-to-file "numbers.txt" L 'replace)     ; par exemple

; srandom pour generer de tres grands entiers aleatoires, par Dorai Sitaram. Ne pas lire ;-)
; (srandom 7228478649364893479832423478428746746786487465751325787485368725465124)

(define (current-time)
  (sqr (current-milliseconds)))

(define srandom:initialize-state #f)
(define srandom:seed-state #f)
(define srandom #f)

(let ((ij #f)
      (kl #f)
      (u #f)
      (c #f)
      (cd #f)
      (cm #f)
      (i97 #f)
      (j97 #f))
  
  (set! srandom:initialize-state
        (lambda ()
          (set! ij 1802)
          (set! kl 9373)
          (set! u (make-vector 97 0))
          (set! c (/ 362436.0 16777216.0))
          (set! cd (/ 7654321.0 16777216.0))
          (set! cm (/ 16777213.0 16777216.0))
          (set! i97 96)
          (set! j97 32)))
  
  (srandom:initialize-state)
  
  (set! srandom:seed-state
        (lambda z
          (let ((z-len (length z))
                (tt (current-time)))
            (set! ij
                  (if (>= z-len 1) (car z)
                      (modulo tt 31329)))
            (set! kl
                  (if (>= z-len 2) (cadr z)
                      (modulo tt 30082)))
            (let ((i (+ (modulo (inexact->exact (floor (/ ij 177)))
                                177) 2))
                  (j (+ (modulo ij 177) 2))
                  (k (+ (modulo (inexact->exact (floor (/ kl 169)))
                                178) 1))
                  (l (modulo kl 169))
                  (ii 0))
              (let loop ((ii ii))
                (when (< ii 97)
                  (let ((s 0.0)
                        (t1 0.5)
                        (jj 0))
                    (let loop ((jj jj))
                      (when (< jj 24)
                        (let ((m (modulo (* (modulo (* i j) 179) k) 179)))
                          (set! i j)
                          (set! j k)
                          (set! k m)
                          (set! l (modulo (+ (* 53 l) 1) 169))
                          (when (not (< (modulo (* l m) 64) 32))
                            (set! s (+ s t1)))
                          (set! t1 (* 0.5 t1))
                          (loop (+ jj 1)))))
                    (vector-set! u ii s)
                    (loop (+ ii 1)))))))))
  
  (srandom:seed-state)
  
  (let ((srandom-one
         (lambda ()
           (let ((uni (- (vector-ref u i97)
                         (vector-ref u j97))))
             (when (< uni 0)
               (set! uni (+ uni 1)))
             (vector-set! u i97 uni)
             (set! i97 (- i97 1))
             (when (< i97 0)
               (set! i97 96))
             (set! j97 (- j97 1))
             (when (< j97 0) (set! j97 96))
             (set! c (- c cd))
             (when (< c 0) (set! c (+ c cm)))
             (set! uni (- uni c))
             (when (< uni 0) (set! uni (+ uni 1)))
             uni))))
    
    (set! srandom
          (lambda (n)
            ;; n > 0
            (let ((r (* n (srandom-one))))
              (if (and (integer? n) (exact? n))
                  (inexact->exact (floor r))
                  r))))))

; le type abstrait "arbre 2-3 d'expression algebrique". Toutes les operations sont O(1)
(define (arbre r Ag . Lfils)    ; au moins un fils !
  (cons r (cons Ag Lfils)))

(define (racine A)
  (if (feuille? A)
      (error (format "pas de racine pour une feuille : ~a" A))
      (first A)))

(define (fg A)
  (if (feuille? A)
      (error (format "pas de fg pour une feuille : ~a" A))
      (second A)))

(define (fd A)
  (if (feuille? A)
      (error (format "pas de fd pour une feuille : ~a" A))
      (third A)))

(define (fdd A)
  (if (or (feuille? A) (empty? (rest (rest (rest A)))))
      (error (format "le fdd n'existe pas : ~a" A))
      (fourth A)))

(define (feuille? obj)
  (or (number? obj) 
      (boolean? obj)
      (and (symbol? obj) (not (operateur? obj)))))

(define (operateur? obj)
  (if (member obj '(+ * - / < > <= >= =)) #t #f))

; le type abstrait "pile fonctionnelle". Toutes les operations sont O(1)
(define (pile-vide) 
  empty)

(define (pile-vide? pile) 
  (empty? pile))

(define (empiler x pile)
  (cons x pile)) 

(define (sommet pile)
  (if (empty? pile)
      (error "Pile vide !")
      (first pile)))

(define (depiler pile)
  (if (empty? pile)
      (error "Pile vide !")
      (rest pile)))

#|
; le type abstrait "FBF en Logique d'ordre 0"
; un parametre F denote une fbf
(define (atome? F)            ; le reconnaisseur d'atomes [symboles p, q, r...]
  (symbol? F))

(define (make-neg F)          ; le constructeur de molecule unaire (negation)
  (cond ((atome? F) (list 'non F))
        ((equal? (connecteur F) 'non) (arg1 F))   ; petite simplification au passage...
        (else (list 'non F))))

(define (make-fbf2 r Fg Fd)   ; le constructeur de molecule binaire (et, ou, =>)
  (if (not (member r '(et ou =>)))
      (error "Mauvais connecteur" r)
      (list Fg r Fd)))        ; representation interne infixee

(define (connecteur mol)      ; on suppose que mol est une molecule
  (if (= (length mol) 2)
      (first mol)             ; non
      (second mol)))          ; et, ou, =>

(define (arg1 mol)            ; mol est une molecule
  (if (= (length mol) 2)
      (second mol)
      (first mol)))

(define (arg2 mol)            ; mol est une molecule
  (if (= (length mol) 2)
      (error "Molecule unaire" mol)
      (third mol)))
|#

(printf "Module valrose : 
(match-define pattern expr), (show expr), (srandom n), (reduce f e L), (pair? x),
(read-from-url url), (read-from-file f), (print-to-file f x mode),
(arbre r Ag Ad), (racine A), (fg A), (fd A), (feuille? A), (operateur? obj), 
(pile-vide? P), (pile-vide), (empiler x P), (sommet P), (depiler P)\n")
;(atome? F), (make-neg F), (make-fbf2 r Fg Fd), (connecteur mol), (arg1 mol), (arg2 mol)\n")  ; pour le livre PCPS