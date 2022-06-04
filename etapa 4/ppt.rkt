#lang racket

(provide (all-defined-out))

(define naturals
  (let nat ([seed 1])
    (stream-cons seed (nat (add1 seed)))))

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

(define (apply-transf-helper Ts ppt res)
  (if (null? Ts)
      res
      (cond
        [(= (car Ts) 1) (apply-transf-helper (cdr Ts) ppt (multiply T1 res))]
        [(= (car Ts) 2) (apply-transf-helper (cdr Ts) ppt (multiply T2 res))]
        [(= (car Ts) 3) (apply-transf-helper (cdr Ts) ppt (multiply T3 res))])))

(define (apply-matrix-transformations Ts ppt)
  (apply-transf-helper Ts ppt ppt))

(define (get-nth-ppt-from-matrix-transformations n)
  (apply-matrix-transformations (get-transformations n) '(3 4 5)))

;; Dacă ne interesează doar al n-lea TPP din arbore, este
;; convenabil să determinăm secvența de transformări care
;; conduce la acest TPP, așa cum am procedat până acum.
;;
;; În schimb, dacă ne interesează primele n TPP (sau în
;; general o secvență mai lungă de TPP) ar fi de preferat
;; crearea unui flux infinit care să le conțină pe toate
;; în ordine.
;;
;; Observăm că această ordine corespunde unei parcurgeri
;; BFS a arborelui infinit. Acesta este un BFS mai simplu
;; decât BFS-ul uzual
;; (https://en.wikipedia.org/wiki/Breadth-first_search),
;; întrucât succesorii unui TPP sunt automat triplete noi,
;; deci nu este necesar să verificăm dacă un nod a mai
;; fost sau nu vizitat.
;; 
;; Schema acestui BFS simplificat este:
;;  1. inițializăm coada de noduri care trebuie vizitate cu
;;     rădăcina arborelui (tripletul (3,4,5))
;;  2. adăugăm primul nod din coadă în rezultat
;;  3. adăugăm cei 3 succesori ai săi în coada de noduri
;;     care trebuie vizitate
;;  4. revenim la pasul 2 (întrucât construim un flux
;;     infinit, nu există condiție de oprire, și toate
;;     structurile sunt fluxuri: atât coada cât și
;;     rezultatul funcției BFS)

;; Vom refolosi matricile T1, T2, T3:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Aduceți aici (nu sunt necesare modificări) implementările
; funcțiilor dot-product și multiply din etapa 1 sau 2.
; Cele două funcții nu sunt re-punctate de checker, însă 
; sunt necesare generării succesorilor unui nod.
(define (dot-product X Y)
  (apply + (map * X Y)))

(define (multiply M V)
  (map (λ (M-line) (dot-product M-line V)) M))


; TODO
; Definiți fluxul infinit de TPP folosind algoritmul descris
; (parcurgerea BFS a arborelui infinit).
; Funcție utilă: stream-append
; Folosiți cel puțin o formă de let.
(define ppt-stream-in-tree-order
  (letrec ([make-ppt-stream (λ (n) (stream-cons (get-nth-ppt-from-matrix-transformations n) (make-ppt-stream (add1 n))))])
    (make-ppt-stream 1)))


;; Un alt mod de a genera TPP se folosește de perechi (g, h)
;; care indeplinesc condițiile:
;;    g, h impare
;;    g < h
;;    g, h prime între ele
;;
;; Nu întâmplător am ales aceste notații, teoria este aceeași
;; cu cea din spatele cvartetelor (g, e, f, h), pe care le
;; putem exprima și ca (g, (h-g)/2, (h+g)/2, h).
;;
;; Pentru a obține un TPP dintr-o pereche (g, h) se aplică
;; aceleași formule (dar le vom exprima în funcție de g și h):
;;    a = gh
;;    b = 2ef = (h - g)(h + g) / 2
;;      = (h^2 - g^2) / 2
;;    c = e^2 + f^2 = (h - g)^2 / 4 + (h + g)^2 / 4
;;      = (h^2 + g^2) / 2
;;
;; Acest mod de generare ne furnizează TPP în altă ordine
;; decât cea dată de parcurgerea în lățime a arborelui TPP.
;;
;; Noua ordine se obține parcurgând pe coloane diagrama:
;;                        h      
;;         3     5     7     9     11   .  .  .
;;    1  (1,3) (1,5) (1,7) (1,9) (1,11) .  .  .
;;    3        (3,5) (3,7)   -   (3,11) .  .  .
;;    5              (5,7) (5,9) (5,11) .  .  .
;; g  7                    (7,9) (7,11) .  .  .
;;    9                          (9,11) .  .  .
;;    .                                 .  .  .
;;    .                                    .  .
;;    .                                       .
;; (lipsește perechea (3,9), 3 și 9 nefiind prime între ele)
;;
;; Folosind această indexare, primele 6 TPP sunt:
;;    (3,4,5)                           - din perechea (1,3)
;;    (5,12,13), (15,8,17)              - din (1,5), (3,5)
;;    (7,24,25), (21,20,29), (35,12,37) - din (1,7), (3,7), (5,7)
;;
;; Ne propunem să definim fluxul infinit de TPP în ordinea de
;; mai sus. Acesta se bazează pe fluxul corespunzător de 
;; perechi (g, h), pe care îl generăm astfel:
;;  - pornim cu 2 fluxuri infinite:
;;    * G = 1, 3, 5, 7 ...
;;    * H = 3, 5, 7, 9 ... (întrucât g < h)
;;  - fluxul ordonat pe coloane va conține:
;;    * perechea compusă din cele mai mici numere din G și H
;;      (ex: (1,3))
;;    * apoi interclasarea (conform ordinii "pe coloane") între:
;;      - perechile compuse dintre minimul din G și restul din H
;;        (ex: (1,5), (1,7), (1,9) ...)
;;      - fluxul ordonat generat de restul lui G și restul lui H
;;        (ex: (3,5), (3,7), (5,7) ...)
;; Aceasta este abordarea generală, în urma căreia generăm toate
;; perechile, inclusiv pe cele de numere care nu sunt prime  
;; între ele. Perechile neconforme trebuie înlăturate ulterior
;; (utilizând funcția de bibliotecă gcd).


; TODO
; Definiți o funcție care primește 2 fluxuri numerice infinite
; G și H, și generează fluxul de perechi de câte un element 
; din G și unul din H ordonate conform metodei de mai sus.
; Condițiile ca g și h să fie impare, prime între ele, respectiv
; menținerea restricției g < h (cât timp urmați algoritmul) nu
; trebuie impuse în implementarea funcției pairs.
; Ele vor fi asigurate de definirea fluxurilor de mai jos prin:
;  - apelarea lui pairs exclusiv pe fluxurile
;    G = 1, 3, 5, 7 ... și H = 3, 5, 7, 9 ...
;  - eliminarea perechilor de numere neprime între ele (care 
;    există în rezultatul funcției pairs, dar nu vor mai exista
;    în fluxul gh-pairs-stream)
(define (pairs-helper G H init nat curr)
    (if (equal? curr 0) (pairs-helper init (stream-rest H) init (stream-rest nat) (stream-first (stream-rest nat)))
                       (stream-cons (cons (stream-first G) (stream-first H)) (pairs-helper (stream-rest G) H init nat (sub1 curr)))))

(define (pairs G H)
  (pairs-helper G H G naturals 1))

; TODO
; Definiți fluxul de perechi (g, h) pe care se bazează noua
; indexare a TPP.
; Nu folosiți recursivitate explicită (decât pentru a genera
; fluxurile de pornire - G și H).
(define G
  (let i ([seed 1])
    (stream-cons seed (i (+ seed 2)))))
(define H
  (let i ([seed 3])
    (stream-cons seed (i (+ seed 2)))))

(define gh-pairs-stream
  (stream-filter (λ (pair) (and (< (car pair) (cdr pair))
                                (odd? (car pair)) (odd? (cdr pair))
                                (equal? (gcd (car pair) (cdr pair)) 1)))
                 (pairs G H)))


; TODO
; Definiți fluxul de TPP corespunzător fluxului anterior de
; perechi (g, h).
(define ppt-stream-in-pair-order
  (stream-map (λ (pair) (append (list (* (car pair) (cdr pair)))
                                (list (/ (- (expt (cdr pair) 2) (expt (car pair) 2)) 2))
                                (list (/ (+ (expt (cdr pair) 2) (expt (car pair) 2)) 2))))
              gh-pairs-stream))


