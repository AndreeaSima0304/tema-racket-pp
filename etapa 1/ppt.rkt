#lang racket

(provide (all-defined-out))

;; Un triplet pitagoreic primitiv (TPP) este format din 
;; 3 numere naturale nenule a, b, c cu proprietățile:
;;    a^2 + b^2 = c^2
;;    a, b, c prime între ele
;;
;; TPP pot fi generate sub formă de arbore (infinit) cu
;; rădăcina (3,4,5), pe baza a 3 transformări matriciale:
;;
;;      |-1 2 2|        |1 2 2|        |1 -2 2|
;; T1 = |-2 1 2|   T2 = |2 1 2|   T3 = |2 -1 2|
;;      |-2 2 3|        |2 2 3|        |2 -2 3|
;;
;;                         (3,4,5)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)      (21,20,29)     (5,12,13)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37) ..........................................
;;
;; unde:
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.
;;
;; În această reprezentare, TPP sunt indexate "de sus în jos",
;; respectiv "de la stânga la dreapta", rezultând ordinea:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) (35,12,37) ... etc.

;; Reprezentăm matricile T1, T2, T3 ca liste de liste:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Implementați o funcție care calculează produsul scalar
; a doi vectori X și Y (reprezentați ca liste).
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
; Utilizați recursivitate pe stivă.
(define (dot-product X Y)
  (if (null? X)
      0
      (+ (* (car X) (car Y)) (dot-product (cdr X) (cdr Y)))))


; TODO
; Implementați o funcție care calculează produsul dintre
; o matrice M și un vector V (puneți V "pe verticală").
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
; Utilizați recursivitate pe coadă.
(define (multiply-helper M V acc)
  (if (null? M)
      acc
      (multiply-helper (cdr M) V (append acc (list (dot-product (car M) V))))))

(define (multiply M V)
  (multiply-helper M V null))


; TODO
; Implementați o funcție care primește un număr n și
; întoarce o listă numerică (unde elementele au valoarea
; 1, 2 sau 3), reprezentând secvența de transformări prin
; care se obține, plecând de la (3,4,5), al n-lea TPP
; din arbore.
; Ex: (get-transformations 8) întoarce '(2 1), adică
; al 8-lea TPP din arbore se obține din T1·T2·(3,4,5).
; Sunteți încurajați să folosiți funcții ajutătoare
; (de exemplu pentru determinarea nivelului din arbore 
; pe care se află n, sau a indexului minim/maxim de pe 
; nivelul respectiv, etc.)
(define (pow-sum p)
  (if (zero? p)
      1
      (+ (pow-sum (sub1 p)) (expt 3 p))))

(define (line n p)
  (if (<= n (pow-sum p))
      p
      (line n (add1 p))))

(define (get-transf-helper n interval-size left right res)
  (define T1-L left)
  (define T1-R (sub1 (+ T1-L interval-size)))
  (define T2-L (add1 T1-R))
  (define T2-R (sub1 (+ T2-L interval-size)))
  (define T3-L (add1 T2-R))
  (define T3-R right)  
  (cond
    [(< interval-size 1) res]
    [else (cond
            [(<= n T1-R)
             (get-transf-helper n (/ interval-size 3) T1-L T1-R (append res (list 1)))]
            [(<= n T2-R)
             (get-transf-helper n (/ interval-size 3) T2-L T2-R (append res (list 2)))]
            [(<= n T3-R)
             (get-transf-helper n (/ interval-size 3) T3-L T3-R (append res (list 3)))])]))

(define (get-transformations n)
  (cond
    [(equal? n 1) null]
    [else (define interval-size (/ (expt 3 (line n 0)) 3))
          (define left (add1 (pow-sum (sub1 (line n 0)))))
          (define right (+ left (* 3 interval-size)))
          (get-transf-helper n interval-size left right null)]))


; TODO
; Implementați o funcție care primește o listă Ts de 
; tipul celei întoarsă de get-transformations, respectiv 
; un triplet de start ppt și întoarce tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Utilizați recursivitate pe coadă.
(define (apply-transf-helper Ts ppt res)
  (if (null? Ts)
      res
      (cond
        [(= (car Ts) 1) (apply-transf-helper (cdr Ts) ppt (multiply T1 res))]
        [(= (car Ts) 2) (apply-transf-helper (cdr Ts) ppt (multiply T2 res))]
        [(= (car Ts) 3) (apply-transf-helper (cdr Ts) ppt (multiply T3 res))])))

(define (apply-matrix-transformations Ts ppt)
  (apply-transf-helper Ts ppt ppt))


; TODO
; Implementați o funcție care calculează al n-lea TPP
; din arbore, folosind funcțiile anterioare.
(define (get-nth-ppt-from-matrix-transformations n)
  (apply-matrix-transformations (get-transformations n) '(3 4 5)))
