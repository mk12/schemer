;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.
;;; (exercises from The Reasoned Schemer)

;;;;; Chapter 1: Playthings

;;; there are two types of goals
#s ; => success
#u ; => failure

(run* (q) #u)           ; => ()
(run* (q) (== #t q))    ; => (#t)
(run* (q) #u (== #t q)) ; => ()
(run* (q) #s (== #t q)) ; => (#t)
(run* (q) #u (== 'a q)) ; => ()
(run* (q) #s (== 'a q)) ; => (a)

(let ((x #f)) (== x #f)) ; => #s
(let ((x #f)) (== x #t)) ; => #u

;;; fresh makes new variables
;;; == unifies two things, suceeding if either
;;; 1. they are already the same
;;; 2. at least one is fresh
;;; (order does not matter)

(run* (q) #s) ; => (_0)

(run* (q)
  (fresh (x)
    (== #t x)
    (== #t q)))
;; => (#t)

(run* (x)
  (let ((x #f))
    (fresh (x)
      (== #t x))))
;; => (_0)

(run* (r)
  (fresh (x y)
    (== (list x y) r)))
;; => ((_0 _1))

(run* (r)
  (fresh (x)
    (let ((y x))
      (fresh (x)
        (== (list y x y) r)))))
;; => ((_0 _1 _0))

(run* (q) (== #f q) (== #t q))     ; => ()
(run* (q) (== #f q) (== #f q))     ; => (#f)
(run* (q) (let ((x q)) (== #t x))) ; => (#t)
(run* (q) (fresh (x) (== x q)))    ; => (_0)

(run* (q)
  (fresh (x)
    (== #t x)
    (== x q)))
;; => (#t)

(run* (q)
  (fresh (x)
    (== x q)
    (== #t x)))
;; => (#t)

(run* (q)
  (fresh (x)
    (== (eq? x q) q)))
;; => (#f)

;;; conde tries every path
;;; and finds all successful ones

(conde (#u #s) (else #u)) ; => #u
(conde (#u #u) (else #s)) ; => #s
(conde (#s #s) (else #u)) ; => #s

(run* (x)
  (conde
    ((== 'olive x) #s)
    ((== 'oil x) #s)
    (else #u)))
;; => (olive oil)

(run 1 (x)
  (conde
    ((== 'olive x) #s)
    ((== 'oil x) #s)
    (else #u)))
;; => (olive)

(run* (x)
  (conde
    ((== 'virgin x) #u)
    ((== 'olive x) #s)
    (#s #s)
    ((== 'oil x) #s)
    (else #u)))
;; => (olive _0 oil)

(run* (r)
  (fresh (x y)
    (== 'split x)
    (== 'pea y)
    (== (list x y) r)))
;; => ((split pea))

(run* (r)
  (fresh (x y)
    (conde
      ((== 'split x) (== 'pea y))
      ((== 'navy x) (== 'bean y))
      (else #u))
    (== (list x y 'soup) r)))
;; => ((split pea soup) (navy bean soup))

(define teacupo
  (lambda (x)
    (conde
      ((== 'tea x) #s)
      ((== 'cup x) #s)
      (else #u))))

(run* (x) (teacupo x)) ; => (tea cup)

(run* (r)
  (fresh (x y)
    (conde
      ((teacupo x) (== #t y) #s)
      ((== #f x) (== #t y))
      (else #u))
    (== (list x y) r)))
;; => ((tea #t) (cup #t) (#f #t))

(run* (r)
  (fresh (x y z)
    (conde
      ((== y x) (fresh (x) (== z x)))
      ((fresh (x) (== y x)) (== z x))
      (else #u))
    (== (list y z) r)))
;; => ((_0 _1) (_0 _1))

(run* (r)
  (fresh (x y z)
    (conde
      ((== y x) (fresh (x) (== z x)))
      ((fresh (x) (== y x) (== z x)))
      (else #u))
    (== #f x)
    (== (list y z) r)))
;; => ((#f _0) (_0 #f))

(run* (q)
  (let ((a (== #t q))
        (b (== #f q)))
    b))
;; => (#f)

(run* (q)
  (let ((a (== #t q))
        (b (fresh (x)
             (== x q)
             (== #f x)))
        (c (conde
             ((== #t q) #s)
             (else (== #f q)))))
    b))
;; => (#f)

;;;;; Chapter 2: Teaching Old Toys New Tricks

(let ((x (lambda (a) a))
      (y 'c))
  (x y))
;; => c

(run* (r)
  (fresh (x y)
    (== (list x y) r)))
;; => ((_0 _1))

(run* (r)
  (fresh (v w)
    (== (let ((x v) (y w))
          (list x y))
        r)))
;; => ((_0 _1))

(run* (r) (caro '(a c o r n) r))            ; => (a)
(run* (q) (caro '(a c o r n) 'a) (== #t q)) ; => (#t)

(run* (r)
  (fresh (x y)
    (caro (list r y) x)
    (== 'pear x)))
;; => (pear)

(define caro
  (lambda (p a)
    (fresh (d)
      (== (cons a d) p))))

(run* (r)
  (fresh (x y)
    (caro '(grape raisin pear) x)
    (caro '((a) (b) (c)) y)
    (== (cons x y) r)))
;; => ((grape a))

(run* (r)
  (fresh (v)
    (cdro '(a c o r n) v)
    (caro v r)))
;; => (c)

(define cdro
  (lambda (p d)
    (fresh (a)
      (== (cons a d) p))))

(run* (r)
  (fresh (x y)
    (cdro '(grape raisin pear) x)
    (caro '((a) (b) (c)) y)
    (== (cons x y) r)))
;; => (((raisin pear) a))

(run* (q)
  (cdro '(a c o r n) '(c o r n))
  (== #t q))
;; => (#t)

(run* (x) (cdro '(c o r n) (list x 'r 'n))) ; => (o)

(run* (l)
  (fresh (x)
    (cdro l '(c o r n))
    (caro l x)
    (== 'a x)))
;; => ((a c o r n))

(run* (l) (conso '(a b c) '(d e) l))     ; => (((a b c) d e))
(run* (x) (conso x '(a b c) '(d a b c))) ; => (d)

(run* (r)
  (fresh (x y z)
    (== (list 'e 'a 'd x) r)
    (conso y (list 'a z 'c) r)))
;; => ((e a d c))

(run* (x) (conso x (list 'a x 'c) (list 'd 'a x 'c))) ; => (d)

(run* (l)
  (fresh (x)
    (== (list 'd 'a x 'c) l)
    (conso x (list 'a x 'c) l)))
;; => ((d a d c))

(run* (l)
  (fresh (x)
    (conso x (list 'a x 'c) l)
    (== (list 'd 'a x 'c) l)))
;; => ((d a d c))

(define conso
  (lambda (a d p)
    (== (cons a d) p)))

(run* (l)
  (fresh (d x y w s)
    (conso w '(a n s) s)
    (cdro l s)
    (caro l x)
    (== 'b x)
    (cdro l d)
    (caro d y)
    (== 'e y)))
;; => ((b e a n s))

(run* (q)
  (nullo '(grape raisin pear))
  (== #t q))
;; => ()

(run* (q) (nullo '()) (== #t q)) ; => (#t)
(run* (x) (nullo x)) ; => (())

(define nullo
  (lambda (x)
    (== '() x)))

(run* (q) (eqo 'pear 'plum) (== #t q)) ; => ()
(run* (q) (eqo 'plum 'plum) (== #t q)) ; => (#t)

(define eqo
  (lambda (x y)
    (== x y)))

(run* (r)
  (fresh (x y)
    (== (cons x (cons y 'salad)) r)))
;; => ((_0 _1 . salad))

(define pairo
  (lambda (p)
    (fresh (a d)
      (conso a d p))))

(run* (q) (pairo (cons q q)) (== #t q)) ; => (#t)
(run* (q) (pairo ()) (== #t q))         ; => ()
(run* (q) (pairo 'pair) (== #t q))      ; => ()

(run* (x) (pairo x))              ; => ((_0 . _1))
(run* (r) (pairo (cons r 'pear))) ; => (_0)

;;;;; Chapter 3: Seeing Old Friends in New Ways

(define list?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((pair? l) (list? (cdr l)))
      (else #f))))

(list? ()) ; => #t
(list? 's) ; => #f
(list? '(d a t e . s)) ; => #f
(list? '(d a t e s))   ; => #t

(define listo
  (lambda (l)
    (conde
      ((nullo l) #s)
      ((pairo l)
       (fresh (d)
         (cdro l d)
         (listo d)))
      (else #u))))

(run* (x) (listo `(a b ,x d)))   ; => (_0)
(run1 (x) (listo `(a b c . ,x))) ; => (())
(run* (x) (listo `(a b c . ,x))) ; => no value
(run4 (x) (listo `(a b c . ,x))) ; => (() (_0) (_0 _1) (_0 _1 _2))

(define lol?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((list? (car l)) (lol? (cdr l)))
      (else #f))))

(define lolo
  (lambda (l)
    (conde
      ((nullo l) #s)
      ((fresh (a)
         (caro l a)
         (listo a))
       (fresh (d)
         (cdro l d)
         (lolo d)))
      (else #u))))

(run1 (l) (lolo l)) ; => (())

(run* (q)
  (fresh (x y)
    (lolo `((a b) (,x c) (d ,y)))
    (== #t q)))
;; => (#t)

(run1 (x) (lolo `((a b) (c d) . ,x))) ; => (())
(run4 (x) (lolo `((a b) (c d) . ,x))) ; => (() (()) (() ()) (() () ()))

(define twinso
  (lambda (s)
    (fresh (x y)
      (conso x y s)
      (conso x '() y))))o

(run* (q) (twinso '(tofu tofu)) (== #t q)) ; => (#t)
(run* (z) (twinso `(,z tofu)))             ; => (tofu)

(define twinso
  (lambda (s)
    (fresh (x)
      (== (list x x) s))))

(define loto
  (lambda (l)
    (conde
      ((nullo l) #s)
      ((fresh (a)
         (caro l a)
         (twinso a))
       (fresh (d)
         (cdro l d)
         (loto d)))
      (else #u))))

(run1 (z) (loto `((g g) . ,z))) ; => (())
(run3 (z) (loto `((g g) . ,z))) ; => (() ((_0 _0)) ((_0 _0) (_1 _1)))

(run3 (r)
  (fresh (w x y z)
    (loto `((g g) (e ,w) (,x ,y) . ,z))
    (== (list w (list x y) z) r)))
;; => ((e (_0 _0) ())
;;     (e (_0 _0) ((_1 _1)))
;;     (e (_0 _0) ((_1 _1) (_2 _2))))

(run3 (out)
  (fresh (w x y z)
    (== `((g g) (e ,w) (,x ,y) . ,z) out)
    (loto out)))
;; => (((g g) (e e) (_0 _0))
;;     ((g g) (e e) (_0 _0) (_1 _1))
;;     ((g g) (e e) (_0 _0) (_1 _1) (_2 _2)))

(define listofo
  (lambda (predo l)
    (conde
      ((nullo l) #s)
      ((fresh (a)
         (caro la )
         (predo a))
       (fresh (d)
         (cdro l d)
         (listofo predo d)))
      (else #u))))

(run3 (out)
  (fresh (w x y z)
    (== `((g g) (e ,w) (,x ,y) . ,z) out)
    (listofo twinso out)))
;; => (((g g) (e e) (_0 _0))
;;     ((g g) (e e) (_0 _0) (_1 _1))
;;     ((g g) (e e) (_0 _0) (_1 _1) (_2 _2)))

(define loto
  (lambda (l)
    (listofo twinso l)))

(define member?
  (lambda (x l)
    (cond
      ((null? l) #f)
      ((eq-car? l x) #t)
      (else (member? x (cdr l))))))

(define eq-car?
  (lambda (l x)
    (eq? (car l) x)))

(define eq-caro
  (lambda (l x)
    (caro l x)))

(define membero
  (lambda (x l)
    (conde
      ;; nullo guaranteed to fail: unnecessary
      ((nullo l) #u)
      ((eq-caro l x) #s)
      (else
        (fresh (d)
          (cdro l d)
          (membero x d))))))

(run* (q)
  (membero 'olive '(virgin olive oil))
  (== #t q))
;; => (#t)

(run1 (y) (membero y '(hummus with pita))) ; => (humus)
(run1 (y) (membero y '(with pita)))        ; => (with)
(run1 (y) (membero y '(pita)))             ; => (pita)
(run1 (y) (membero y '()))                 ; => ()
(run* (y) (membero y '(hummus with pita))) ; => (hummus with pita)

(define identity
  (lambda (l)
    (run* (y)
      (membero y l))))

(run* (x) (membero 'e `(pasta ,x fagioli)))   ; => (e)
(run1 (x) (membero 'e `(pasta e ,x fagioli))) ; => (_0)
(run1 (x) (membero 'e `(pasta ,x e fagioli))) ; => (e)

(run* (r)
  (fresh (x y)
    (membero 'e `(pasta ,x fagioli ,y))
    (== (list x y) r)))
;; => ((e _0) (_0 e))

(run1 (l) (membero 'tofu l)) ; => ((tofu . _0))
(run* (l) (membero 'tofu l)) ; => no value
(run3 (l) (membero 'tofu l)) ; => ((tofu . _0) (_0 tofu . _1) (_0 _1 tofu . _2))

(define pmembero
  (lambda (x l)
    (conde
      ((nullo l) #u)
      ((eq-caro l x) (cdro l '()))
      (else
        (fresh (d)
          (cdro l d)
          (pmembero x d))))))

(run3 (l) (pmembero 'tofu l)) ; => ((tofu) (_0 tofu) (_0 _1 tofu))

(run* (q)
  (pmembero 'tofu '(a b tofu d tofu))
  (== #t q))
;; => (#t)

(define pmembero
  (lambda (x l)
    (conde
      ((nullo l) #u)
      ((eq-caro l x) (cdro l '()))
      ((eq-caro l x) #s)
      (else
        (fresh (d)
          (cdro l d)
          (pmembero x d))))))

(run* (q)
  (pmembero 'tofu '(a b tofu d tofu))
  (== #t q))
;; => (#t #t #t)

(define pmembero
  (lambda (x l)
    (conde
      ((nullo l) #u)
      ((eq-caro l x) (cdro l '()))
      ((eq-caro l x)
       (fresh (a d)
         (cdro l (cons a d))))
      (else
        (fresh (d)
          (cdro l d)
          (pmembero x d))))))

(run* (q)
  (pmembero 'tofu '(a b tofu d tofu))
  (== #t q))
;; => (#t #t)

(run3 (l) (pmembero 'tofu l)) ; => ((tofu) (tofu _0 . _1) (_0 tofu))

(define pmembero
  (lambda (x l)
    (conde
      ((nullo l) #u)
      ((eq-caro l x)
       (fresh (a d)
         (cdro l (cons a d))))
      ((eq-caro l x) (cdro l '()))
      (else
        (fresh (d)
          (cdro l d)
          (pmembero x d))))))

(run3 (l) (pmembero 'tofu l)) ; => ((tofu _0 . _1) (tofu) (_0 tofu _1 . _2))

(define first-value
  (lambda (l)
    (run1 (y)
      (membero y l))))

(first-value '(pasta e fagioli)) ; => (pasta)

(define memberrevo
  (lambda (x l)
    (conde
      ((nullo l) #u)
      (#s (fresh (d)
            (cdro l d)
            (memberrevo x d)))
      (else (eq-caro l x)))))

(run* (x) (memberrevo x '(pasta e fagioli))) ; => (fagioli e pasta)

(define reverse-list
  (lambda (l)
    (run* (y)
      (memberrevo y l))))

;;;;; Chapter 4: Members Only

(define mem
  (lambda (x l)
    (cond
      ((null? l) #f)
      ((eq-car? l x) l)
      (else (mem x (cdr l))))))

(run* (out)
  (== (mem 'tofu '(a b tofu d peas e))
      out))
;; => ((tofu d peas e))

(define memo
  (lambda (x l out)
    (conde
      ((nullo l) #u)
      ((eq-caro l x) (== l out))
      (else
        (fresh (d)
          (cdro l d)
          (memo x d out))))))

(run1 (out) (memo 'tofu '(a b tofu d tofu e) out)) ; => ((tofu d tofu e))

(run1 (out)
  (fresh (x)
    (memo 'tofu `(a b ,x d tofu e) out)))
;; => ((tofu d tofu e))

(run* (r)
  (memo r
        '(a b tofu d tofu e)
        '(tofu d tofu e)))
;; => (tofu)

(run* (q)
  (memo 'tofu '(tofu e) '(tofu e))
  (== #t q))
;; => (#t)

(run* (q)
  (memo 'tofu '(tofu e) '(tofu))
  (== #t q))
;; => ()

(run* (x) (memo 'tofu '(tofu e) `(,x e)))    ; => (tofu)
(run* (x) (memo 'tofu '(tofu e) `(peas ,x))) ; => ())

(run* (out)
  (fresh (x)
    (memo 'tofu `(a b ,x d tofu e) out)))
;; => ((tofu d tofu e) (tofu e))

(run5 (z)
  (fresh (u)
    (memo 'tofu `(a b tofu d tofu e . ,z) u)))
;; => (_0 _0 (tofu . _0) (_0 tofu . _1) (_0 _1 tofu . _2))

(define memo
  (lambda (x l out)
    (conde
      ((eq-caro l x) (== l out))
      (else
        (fresh (d)
          (cdro l d)
          (memo x d out))))))

(define rembero
  (lambda (x l out)
    (conde
      ((nullo l) (== '() out))
      ((eq-caro l x) (cdro l out))
      (else
        (fresh (a d res)
          (conso a d l)
          (rembero x d res)
          (conso a res out))))))

(run1 (out)
  (fresh (y)
    (rembero 'peas `(a b ,y d peas e) out)))
;; => ((a b d peas e))

(run* (out)
  (fresh (y z)
    (rembero y `(a b ,y d ,z e) out)))
;; => ((b a d _0 e)
;;     (a b d _0 e)
;;     (a b d _0 e)
;;     (a b d _0 e)
;;     (a b _0 d e)
;;     (a b e d _0)
;;     (a b _0 d _1 e))

(run* (r)
  (fresh (y z)
    (rembero y `(,y d ,z e) `(,y d e))
    (== (list y z) r)))
;; => ((d d) (d d) (_0 _0) (e e))

(run10 (w)
  (fresh (y z out)
    (rembero y `(a b ,y d ,z . ,w) out)))
;; => (_0 _0 _0 _0 _0 () (_0 . _1) (_0) (_0 _1 . _2) (_0 _1 _2))

;; This succeeds for all values of s other than a, b, and c.
;; Or does it?
(define surpriseo
  (lambda (s)
    (rembero s '(a b c) '(a b c))))

(run* (r) (== 'd r) (surpriseo r)) ; => (d)
(run* (r) (surpriseo r))           ; => (_0)
(run* (r) (== 'b r) (surpriseo r)) ; => (b)

;;;;; Chapter 5: Double Your Fun

(define append
  (lambda (l s)
    (cond
      ((null? l) s)
      (else (cons (car l)
                  (append (cdr l) s))))))

(define appendo
  (lambda (l s out)
    (conde
      ((nullo l) (== s out))
      (else
        (fresh (a d res)
          (conso a d l)
          (appendo d s res)
          (conso a res out))))))

(run* (x) (appendo '(cake) '(tastes yummy) x)) ; => ((cake tastes yummy))

(run* (x)
  (fresh (y)
    (appendo `(cake with ice ,y)
             '(tastes yummy)
             x)))
;; => ((cake with ice _0 tastes yummy))

(run* (x)
  (fresh (y)
    (appendo '(cake with ice cream) y x)))
;; => ((cake with ice cream . _0))

(run1 (x)
  (fresh (y)
    (appendo `(cake with ice . ,y) '(d t) x)))
;; => ((cake with ice d t))

(run3 (x)
  (fresh (y)
    (appendo `(cake with ice . ,y) '(d t) x)))
;; => ((cake with ice d t) (cake with ice _0 d t) (cake with ice _0 _1 d t))
j
(run5 (y)
  (fresh (x)
    (appendo `(cake with ice . ,y) '(d t) x)))
;; => (() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3))

(run2 (x)
  (fresh (y)
    (appendo `(cake with ice . ,y)
             `(d t . y)
             x)))
;; => ((cake with ice d t) (cake with ice _0 d t _0))

(run* (x)
  (fresh (z)
    (appendo '(cake with ice cream)
             `(d t . ,z)
             x)))
;; => ((cake with ice cream d t))

(run5 (x)
  (fresh (y)
    (appendo x y '(cake with ice d t))))
;; => (() (cake) (cake with) (cake with ice) (cake with ice d))

(run6 (y)
  (fresh (x)
    (appendo x y '(cake iwth ice d t))))
;; => ((cake with ice d t) (with ice d t) (ice dt) (d t) (t) ())

(run6 (r)
  (fresh (x y)
    (appendo x y '(cake with ice d t))
    (== (list x y) r)))
;; => ((() (cake with ice d t))
;;     ((cake) (with ice d t))
;;     ((cake with) (ice d t))
;;     ((cake with ice) (d t))
;;     ((cake with ice d) (t))
;;     ((cake with ice d t) ()))

;; run7 the above => no value

(define appendo
  (lambda (l s out)
    (conde
      ((nullo l) (== s out))
      (else
        (fresh (a d res)
          (conso a d l)
          (conso a res out)
          (appendo d s res))))))

;; now, run7 the above => same as run6

(run4 (x) (fresh (y z) (appendo x y z))) ; => (() (_0) (_0 _1) (_0 _1 _2))
(run7 (y) (fresh (x z) (appendo x y z))) ; => (_0 _0 _0 _0 _0 _0 _0)
(run3 (z) (fresh (x y) (appendo x y z))) ; => (_0 (_0 . _1) (_0 _1 . _2))

(run4 (r)
  (fresh (x y z)
    (appendo x y z)
    (== (list x y z) r)))
;; => ((() _0 _0)
;;     ((_0) _1 (_0 . _1))
;;     ((_0 _1) _2 (_0 _1 . _2))
;;     ((_0 _1) _3 (_0 _1 _2 . _3)))

(define swappendo
  (lambda (l s out)
    (conde
      (#s (fresh (a d res)
            (conso a d l)
            (conso a res out)
            (swappendo d s res)))
      (else (nullo l) (== s out)))))

(run1 (z) (fresh (x y) (swappendo x y z))) ; => no value

(define unwrap
  (lambda (x)
    (cond
      ((pair? x) (unwrap (car x)))
      (else x))))

(define unwrapo
  (lambda (x out)
    (conde
      ((pairo x)
       (fresh (a)
         (cdro x a)
         (unwrapo a out)))
      (else (== x out)))))

(run* (x) (unwrapo '(((pizza))) x)) ; => (pizza (pizza) ((pizza)) (((pizza))))
(run1 (x) (unwrapo x 'pizza))       ; => no value
(run1 (x) (unwrapo `((,x)) 'pizza)) ; => no value

(define unwrapo
  (lambda (x out)
    (conde
      (#s (== x out))
      (else
        (fresh (a)
          (caro x a)
          (unwrapo a out))))))

(run3 (x) (unwrapo x 'pizza)) ; => (pizza (pizza . _0) ((pizza . _0) . _1))

(define flatten
  (lambda (s)
    (cond
      ((null? s) '())
      ((pair? s)
       (append (flatten (car s))
               (flatten (cdr s))))
      (else (cons s '())))))

(define flatteno
  (lambda (s out)
    (conde
      ((nullo s) (== '() out))
      ((pairo s)
       (fresh (a d fa fd)
         (conso a d s)
         (flatteno a fa)
         (flatteno d fd)
         (appendo fa fd out)))
      (else (conso s '() out)))))

(run1 (x) (flatteno '((a b) c) x)) ; => ((a b c))
(run1 (x) (flatteno '(a (b c)) x)) ; => ((a b c))
(run* (x) (flatteno '(a) x))       ; => ((a) (a ()) ((a)))

(define flattenrevo
  (lambda (s out)
    (conde
      (#s (conso s '() out))
      ((nullo s) (== '() out))
      (else
       (fresh (a d fa fd)
         (conso a d s)
         (flatteno a fa)
         (flatteno d fd)
         (appendo fa fd out))))))

;;;;; Chapter 6: The Fun Never Ends...

(define anyo
  (lambda (g)
    (conde
      (g #s)
      (else (anyo g)))))

(define nevero (anyo #u))

(run1 (q) nevero (== #t q)) ; => no value
(run1 (q) #u nevero)        ; => ()

(define alwayso (anyo #s))

(run1 (q) alwayso (== #t q)) ; => (#t)
(run* (q) alwayso (== #t q)) ; => no value
(run5 (q) alwayso (== #t q)) ; => (#t #t #t #t #t)

;; succeed at least once
(define salo
  (lambda (g)
    (conde
      (#s #s)
      (else g))))

(run1 (q) (salo alwayso) (== #t q)) ; => (#t)
(run1 (q) (salo nevero) (== #t q))  ; => (#t)
(run* (q) (salo nevero) (== #t q))  ; => no value
(run1 (q) alwayso #u (== #t q))     ; => no value

(run1 (q)
  (conde
    ((== #f q) alwayso)
    (else (anyo (== #t q))))
  (== #t q))
;; => no value

;;; condi is like conde, but interleaved

(run1 (q)
  (condi
    ((== #f q) alwayso)
    (else (== #t q)))
  (== #t q))
;; => (#t)

(run2 (q)
  (condi
    ((== #f q) alwayso)
    (else (== #t q)))
  (== #t q))
;; => no value

(run5 (q)
  (condi
    ((== #f q) alwayso)
    (else (anyo (== #t q))))
  (== #t q))
;; => (#t #t #t #t #t)

(run5 (r)
  (condi
    ((teacupo r) #s)
    ((== #f r) #s)
    (else #u)))
;; => (tea #f cup)

(run5 (q)
  (condi
    ((== #f q) alwayso)
    ((== #t q) alwayso)
    (else #u))
  (== #t q))
;; => (#t #t #t #t #t)

(run5 (q)
  (conde
    ((== #f q) alwayso)
    ((== #t q) alwayso)
    (else #u))
  (== #t q))
;; => no value

(run5 (q)
  (conde
    (alwayso #s)
    (else nevero))
  (== #t q))
;; => (#t #t #t #t #t)

(run5 (q)
  (condi
    (alwayso #s)
    (else nevero))
  (== #t q))
;; => no value

(run1 (q)
  (all
    (conde
      ((== #f q) #s)
      (else (== #t q)))
    alwayso)
  (== #t q))
;; => no value

(run1 (q)
  (alli
    (conde
      ((== #f q) #s)
      (else (== #t q)))
    alwayso)
  (== #t q))
;; => (#t)

(run5 (q)
  (alli
    (conde
      ((== #f q) #s)
      (else (== #t q)))
    alwayso)
  (== #t q))
;; => (#t #t #t #t #t)

(run5 (q)
  (all
    (conde
      (#s #s)
      (else nevero))
    alwayso)
  (== #t q))
;; => (#t #t #t #t #t)

(run5 (q)
  (alli
    (conde
      (#s #s)
      (else nevero))
    alwayso)
  (== #t q))
;; => no value

;;;;; Chapter 7: A Bit Too Much

;;; a bit is either 0 or 1

(define bit-xoro
  (lambda (x y r)
    (conde
      ((== 0 x) (== 0 y) (== 0 r))
      ((== 1 x) (== 0 y) (== 1 r))
      ((== 0 x) (== 1 y) (== 1 r))
      ((== 1 x) (== 1 y) (== 0 r))
      (else #u))))

(run* (s)
  (fresh (x y)
    (bit-xoro x y 0)
    (== (list x y) s)))
;; => ((0 0) (1 1))

(run* (s)
  (fresh (x y)
    (bit-xoro x y 1)
    (== (list x y) s)))
;; => ((1 0) (0 1))

(run* (s)
  (fresh (x y r)
    (bit-xoro x y r)
    (== (list x y r) s)))
;; => ((0 0 0) (1 0 1) (0 1 1) (1 1 0))

(define bit-ando
  (lambda (x y r)
    (conde
      ((== 0 x) (== 0 y) (== 0 r))
      ((== 1 x) (== 0 y) (== 0 r))
      ((== 0 x) (== 1 y) (== 0 r))
      ((== 1 x) (== 1 y) (== 1 r))
      (else #u))))

(run* (s)
  (fresh (x y)
    (bit-ando x y 1)
    (== (list x y) s)))
;; => ((1 1))

(define half-addero
  (lambda (x y r c)
    (all (bit-xoro x y r)
         (bit-ando x y c))))

(run* (r) (half-addero 1 1 r 1)) ; => (0)

(run* (s)
  (fresh (x y r c)
    (half-addero x y r c)
    (== (list x y r c) s)))
;; => ((0 0 0 0) (1 0 1 0) (0 1 1 0) (1 1 0 1))

(define full-addero
  (lambda (b x y r c)
    (fresh (w xy wz)
      (half-addero x y w xy)
      (half-addero w b r wz)
      (bit-xoro xy wz c))))

(run* (s)
  (fresh (r c)
    (full-addero 0 1 1 r c)
    (== (list r c) s)))
;; => ((0 1))

(run* (s)
  (fresh (r c)
    (full-addero 1 1 1 r c)
    (== (list r c) s)))
;; => ((1 1))

(run* (s)
  (fresh (b x y r c)
    (full-addero b x y r c)
    (== (list b x y r c) s)))
;; => ((0 0 0 0 0)
;;     (1 0 0 1 0)
;;     (0 1 0 1 0)
;;     (1 1 0 0 1)
;;     (0 0 1 1 0)
;;     (1 0 1 0 1)
;;     (0 1 1 0 1)
;;     (1 1 1 1 1))

;;; represent numbers using lists of bits
;;; each number has a unique representation
;;; bits are ordered by little endian
;;; there is no zero padding on the right
;;; the number zero is represented by ()
;;; all other numbers end with a 1

;;; (0 . n) => 2n
;;; (1 . n) => 2n + 1

(define build-num
  (lambda (n)
    (cond
      ((and (not zero? n) (even? n))
       (cons 0 (build-num (/ n 2))))
      ((odd? n)
       (cons 1 (build-num (/ (- n 1) 2))))
      ((zero? n) '()))))

;;; only one of the cond-lines is every true:
;;; non-overlapping property

(define poso
  (lambda (n)
    (fresh (a d)
      (conso a d n))))

(run* (q) (poso '(0 1 1)) (== #t q)) ; => (#t)
(run* (q) (poso '(1)) (== #t q))     ; => (#t)

(run3 (s)
  (fresh (x y r)
    (addero 0 x y r)
    (== (list x y r) s)))
;; => ((_0 () _0)
;;     (() (_0 . _1) (_0 . _1))
;;     ((1) (1) (0 1)))

(define addero
  (lambda (d n m r)
    (condi
      ((== 0 d) (== '() m) (== n r))
      ((== 0 d) (== '() n) (== m r) (poso m))
      ((== 1 d) (== '() m) (addero 0 n '(1) r))
      ((== 1 d) (== '() n) (addero 0 '(1) m r))
      ((== '(1) n) (== '(1) m)
                   (fresh (a c)
                     (== (list a c) r)
                     (full-addero d 1 1 a c)))
      ((== '(1) n) (gen-addero d n m r))
      ((== '(1) m) (>1o n) (>1o r) (addero d '(1) n r))
      ((>1o n) (gen-addero d n m r))
      (else #u))))

(define gen-addero
  (lambda (d n m r)
    (fresh (a b c e x y z)
      (conso a x n)
      (conso b y m)
      (poso y)
      (conso c z r)
      (poso z)
      (alli (full-addero d a b c e)
            (addero e x y z)))))

(run* (s) (gen-addero 1 '(0 1 1) '(1 1) s)) ; => ((0 1 0 1))

(run* (s)
  (fresh (x y)
    (addero 0 x y '(1 0 1))
    (== (list x y) s)))
;; => (((1 0 1) ())
;;     (() (1 0 1))
;;     ((1) (0 0 1))
;;     ((0 0 1) (1))
;;     ((1 1) (0 1))
;;     ((0 1) (1 1)))

(define +o
  (lambda (n m k)
    (addero 0 n m k)))

(run* (s)
  (fresh (x y)
    (+o x y '(1 0 1))
    (== (list x y) s)))
;; => same as before

(define -o
  (lambda (n m k)
    (+o m k n)))

(run* (q) (-o '(0 0 0 1) '(1 0 1) q)) ; => ((1 1))
(run* (q) (-o '(0 1 1) '(0 1 1) q))   ; => (())
(run* (q) (-o '(0 1 1) '(0 0 0 1) q)) ; => ()

;;;;; Chapter 8: Just a Bit More

(define *o
  (lambda (n m p)
    (condi
      ((== '() n) (== '() p))
      ((poso n) (== '() m) (== '() p))
      ((== '(1) n) (poso m) (== m p))
      ((>1o n) (== '(1) m) (== n p))
      ((fresh (x z)
         (conso 0 x n)
         (poso x)
         (conso 0 z p)
         (poso z)
         (>1o m)
         (*o x m z)))
      ((fresh (x y)
         (conso 1 x n)
         (poso x)
         (conso 1 y m)
         (poso y)
         (*o m n p)))
      ((fresh (x y)
         (conso 1 x n)
         (poso x)
         (conso 1 y m)
         (poso y)
         (odd-*o x n m p)))
      (else #u))))

(define odd-*o
  (lambda (x n m p)
    (fresh (q)
      (bound-*o q p n m)
      (*o x m q)
      (+o (cons 0 q) m p))))

(define bound-*o
  (lambda (q p n m)
    (conde
      ((nullo q) (pairo p))
      (else
        (fresh (x y z)
          (cdro q x)
          (cdro p y)
          (condi
            ((nullo n)
             (cdro m z)
             (bound-*o x y z '()))
            (else (cdro n z)
                  (bound-*o x y z m))))))))

(run2 (t)
  (fresh (n m)
    (*o n m '(1))
    (= (list n m) t)))
;; => (((1) (1)))

(run* (p)
  (*o '(1 1 1) '(1 1 1 1 1 1) p))
;; => ((1 0 0 1 1 1 0 1 1))

(define =lo
  (lambda (n m)
    (conde
      ((== '() n) (== '() m))
      ((== '(1) n) (== '(1) m))
      (else
        (fresh (a x b y)
          (conso a x n)
          (poso x)
          (conso b y m)
          (poso y)
          (=lo x y))))))

(run* (t)
  (fresh (w x y)
    (=lo `(1 ,w ,x . ,y) '(0 1 1 0 1))
    (== (list w x y) t)))
;; => ((_0 _1 (_2 1)))

(run* (b) (=lo '(1) (list b)))              ; => (1)
(run* (n) (=lo `(1 0 1 . ,n) '(0 1 1 0 1))) ; => ((_0 1))

(run4 (t)
  (fresh (y z)
    (=lo (cons 1 y) (cons 1 z))
    (== (list y z) t)))
;; => ((() ()) ((1) (1)) ((_0 1) (_1 1)) ((_0 _1 1) (_2 _3 1)))

(run3 (t)
  (fresh (y z)
    (=lo (cons 1 y) (cons 0 z))
    (== (list y z) t)))
;; => (((1) (1)) ((_0 1) (_1 1)) ((_0 _1 1) (_2 _3 1)))

(run3 (t)
  (fresh (y z)
    (=lo (cons 1 y) `(0 1 1 0 1 . ,z))
    (== (list y z) t)))
;; => (((_0 _1 _2 1) ()) ((_0 _1 _2 _3 1) (1)) ((_0 _1 _2 _3 _4 1) (_5 1)))

(define <lo
  (lambda (n m)
    (conde
      ((== '() n) (poso m))
      ((== '(1) n) (>1o m))
      (else
        (fresh (a x b y)
          (conso a x n)
          (poso x)
          (conso b y m)
          (poso y)
          (<lo x y))))))

(run5 (t)
  (fresh (y z)
    (<lo (cons 1 y) `(0 1 1 0 1 . ,z))
    (== (list y z) t)))
;; => ((() _0) ((1) _0) ((_0 1) _1) ((_0 _1 1) _2) ((_0 _1 _2 1) (_3 . _4)))

(run1 (n) (<lo n n)) ; => no value

(define <=lo
  (lambda (n m)
    (conde
      ((=lo n m) #s)
      ((<lo n m) #s)
      (else #u))))

(run4 (t)
  (fresh (n m)
    (<=lo n m)
    (== (list n m) t)))
;; => ((() ()) ((1) (1)) ((_0 1) (_1 1)) ((_0 _1 1) (_2 _3 1)))

(run1 (t)
  (fresh (n m)
    (<=lo n m)
    (*o n '(0 1) m)
    (== (list n m) t)))
;; => ((() ()))
;; run2 => no value

(define <=lo
  (lambda (n m)
    (condi
      ((=lo n m) #s)
      ((<lo n m) #s)
      (else #u))))

(run5 (t)
  (fresh (n m)
    (<=lo n m)
    (*o n '(0 1) m)
    (== (list n m) t)))
;; => ((() ()) ((1) (0 1)) ((0 1) (0 0 1)) ((1 1) (0 1 1)) ((0 0 1) (0 0 0 1)))

(define <o
  (lambda (n m)
    (condi
      ((<lo n m) #s)
      ((=lo n m)
       (fresh (x)
         (poso x)
         (+o n x m)))
      (else #u))))

(define <=o
  (lambda (n m)
    (condi
      ((== n m) #s)
      ((<o n m) #s)
      (else #u))))

(run* (q) (<o '(1 0 1) '(1 1 1)) (== #t q)) ; => (#t)
(run* (q) (<o '(1 1 1) '(1 0 1)) (== #t q)) ; => ()
(run* (q) (<o '(1 0 1) '(1 0 1)) (== #t q)) ; => ()

(run6 (n) (<o n '(1 0 1))) ; => (() (0 0 1) (1) (_0 1))
(run6 (m) (<o '(1 0 1) m)) ; => ((_0 _1 _2 _3 . _4) (0 1 1) (1 1 1))

(run* (n) (<o n n)) ; => no value

(define ÷o
  (lambda (n m q r)
    (condi
      ((== '() q) (== n r) (<o n m))
      ((== '(1) q) (== '() r) (== n m) (<o r m))
      ((<o m n) (<o r m)
                (fresh (mq)
                  (<=lo mq n)
                  (*o m q mq)
                  (+o mq r n)))
      (else #u))))

(run* (m)
  (fresh (r)
    (÷o '(1 0 1) m '(1 1 1) r)))
;; => ()

;;; pages 125 to 129 are too complicated

;;;;; Chapter 9: Under the Hood

(define u (var 'u))
(define v (var 'v))
(define w (var 'w))
(define x (var 'x))
(define y (var 'y))
(define z (var 'z))

;;; a cons pair is an association
;;; it has lhs (car) and rhs (cdr)
;;; the lhs must be a variable
;;; the rhs can be any value (except the lhs variable)

(rhs `(,z . b))         ; => b
(rhs `(,z. ,w))         ; => var-x
(rhs `(,z . (,x e ,y))) ; => (var-x e var-y)

;;; a list of associations is a substitution

(define empty-s '())

(walk z `((,z . a) (,x . ,w) (,y . ,z))) ; => a
(walk y `((,z . a) (,x . ,w) (,y . ,z))) ; => a
(walk x `((,z . a) (,x . ,w) (,y . ,z))) ; => var-w
(walk w `((,z . a) (,x . ,w) (,y . ,z))) ; => var-w

(walk x `((,x . ,y) (,z . ,x) (,y . ,z)))          ; => no value (circular)
(walk w `((,x . ,y) (,w . b) (,z . ,x) (,y . ,z))) ; => b
(walk u `((,x . b) (,w . (,x e ,x)) (,u . ,w)))    ; => (var-x e var-x)

(define walk
  (lambda (v s)
    (cond
      ((var? v)
       (let ((a (assq v s)))
         (cond
           (a (walk (rhs a) s))
           (else v))))
      (else v))))

(define ext-s
  (lambda (x v s)
    (cons (cons x v) s)))

(walk x (ext-s x y `((,z . ,x) (,y . ,z)))) ; => no value

(walk y `((,x . e)))                         ; => var-y
(walk y (ext-s y x `((,x . e))))             ; => e
(walk x `((,y . ,z) (,x . ,y)))              ; => var-z
(walk x (ext-s z 'b `((,y . ,z) (,x . ,y)))) ; => b
(walk x (ext-s z w `((,y . ,z) (,x . ,y))))  ; => var-w

(unify v w s) ; => either #f or new substitution

(define unify
  (lambda (v w s)
    (let ((v (walk v s))
          (w (walk w s)))
      (cond
        ((eq? v w) s)
        ((var? v) (ext-s v w s))
        ((var? w) (ext-s w v s))
        ((and (pair? v) (pair? w))
         (let ((a (unify (car v) (car w) s)))
           (cond
             (a (unify (cdr v) (cdr w) a))
             (else #f))))
        ((equal? v w) s)
        (else #f)))))

(walk* x `((,y . (a ,z c)) (,x . ,y) (,z . a)))  ; => (a a c)
(walk* x `((,y . (,z ,w c)) (,x . ,y) (,z . a))) ; => (a var-w c)

(walk* y `((,y . (,w ,z c)) (,v . b) (,x . ,v) (,z . ,x))) ; => (var-w b c)

(define walk*
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) v)
        ((pair? v)
         (cons (walk* (car v) s)
               (walk* (cdr v) s)))
        (else v)))))

(define reify-s
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        ((var? v)
         (ext-s v (reify-name (size-s s)) s))
        ((pair? v) (reify-s (cdr v)
                            (reify-s (car v) s)))
        (else s)))))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

(let ((r (list w x y)))
  (walk* r (reify-s r empty-s)))
;; => (_0 _1 _2)

(let ((r (walk* (list x y z) empty-s)))
  (walk* r (reify-s r empty-s)))
;; => (_0 _1 _2)

(let ((r `(,u (,v (,w ,x) ,y) ,x)))
  (walk* r (reify-s r empty-s)))
;; => (_0 (_1 (_2 _3) _4) _3)

(let ((s `((,y . (,z ,w c ,w)) (,x . ,y) (,z . a))))
  (let ((r (walk* x s)))
    (walk* r (reify-s r empty-s))))
;; => (a _0 c _0)

(define reify
  (lambda (v)
    (walk* v (reify-s v empty-s))))

(let ((s `((,y . (,z ,w c ,w)) (,x . ,y) (,z . a))))
  (reify (walk* x s)))
;; => (a _0 c _0)

(define ext-sv
  (lambda (x v s)
    (cond
      ((occursv x v s) #f)
      (else (ext-s x v s)))))

(define occursv
  (lambda (x v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) (eq? v x))
        ((pair? v)
         (or (occursv x (car v) s)
             (occursv x (cdr v) s)))
        (else #f)))))

(define unifyv
  (lambda (v w s)
    (let ((v (walk v s))
          (w (walk w s)))
      (cond
        ((eq? v w) s)
        ((var? v) (ext-sv v w s))
        ((var? w) (ext-sv w v s))
        ((and (pair? v) (pair? w))
         (let ((a (unifyv (car v) (car w) s)))
           (cond
             (a (unifyv (cdr v) (cdr w) a))
             (else #f))))
        ((equal? v w) s)
        (else #f)))))

(run1 (x) (== (list x) x)) ; => no value

(run1 (q)
  (fresh (x)
    (== (list x) x)
    (== #t q)))
;; => (#t)

(run1 (q)
  (fresh (x y)
    (== (list x) y)
    (== (list y) x)
    (== #t q)))
;; => (#t)

(run1 (x) (==v (list x) x)) ; => ()

(run1 (x)
  (fresh (y z)
    (== x z)
    (== `(a b ,z) y)
    (== x y)))
;; => no value

(run1 (x)
  (fresh (y z)
    (== x z)
    (== `(a b ,z) y)
    (==v x y)))
;; => ()

;;;;; Chapter 10: Thin Ice

;;; conda is like conde, but only one line can succeed

(run* (x)
  (conda
    ((== 'olive x) #s)
    ((== 'oil x) #s)
    (else #u)))
;; => (olive)

(run* (x)
  (conda
    ((== 'virgin x) #u)
    ((== 'olive x) #s)
    ((== 'oil x) #s)
    (else #u)))
;; => ()

(run* (q)
  (fresh (x y)
    (== 'split x)
    (== 'pea y)
    (conda
      ((== 'split x) (== x y))
      (else #s)))
  (== #t q))
;; => ()

(run* (q)
  (fresh (x y)
    (== 'split x)
    (== 'pea y)
    (conda
      ((== x y) (== 'split x))
      (else #s)))
  (== #t q))
;; => (#t)

(define not-pastao
  (lambda (x)
    (conda
      ((== 'pasta x) #u)
      (else #s))))

(run* (x)
  (conda
    ((not-pastao x) #u)
    (else (== 'spaghetti x))))
;; => (spaghetti)

(run* (x)
  (== 'spaghetti x)
  (conda
    ((not-pastao x) #u)
    (else (== 'spaghetti x))))
;; => ()

(run* (q)
  (conda
    (alwayso #s)
    (else #u))
  (== #t q))
;; => no value

;;; condu is like conda, but the successful question can only succeed once

(run* (q)
  (condu
    (alwayso #s)
    (else #u))
  (== #t q))
;; => (#t)

(run* (q)
  (condu
    (#s alwayso)
    (else #u))
  (== #t q))
;; => no value

(run1 (q)
  (conda
    (alwayso #s)
    (else #u))
  #u
  (== #t q))
;; => no value

(run1 (q)
  (condu
    (alwayso #s)
    (else #u))
  #u
  (== #t q))
;; => ()

(define onceo
  (lambda (g)
    (condu
      (g #s)
      (else #u))))

(run* (x) (onceo (teacupo x)))      ; => (tea)
(run1 (q) (onceo (salo nevero)) #u) ; => ()

(run* (r)
  (conde
    ((teacupo r) #s)
    ((== #f r) #s)
    (else #u)))
;; => (tea cup #f)

(run* (r)
  (conda
    ((teacupo r) #s)
    ((== #f r) #s)
    (else #u)))
;; => (tea cup)

(run* (r)
  (== #f r)
  (conda
    ((teacupo r) #s)
    ((== #f r) #s)
    (else #u)))
;; => (#f)

(run* (r)
  (== #f r)
  (condu
    ((teacupo r) #s)
    ((== #f r) #s)
    (else #u)))
;; => (#f)

(define bumpo
  (lambda (n x)
    (conde
      ((== n x) #s)
      (else
        (fresh (m)
          (-o n '(1) m)
          (bumpo m x))))))

(run* (x)
  (bumpo '(1 1 1) x))
;; => ((1 1 1) (0 1 1) (1 0 1) (0 0 1) (1 1) (0 1) (1) ())

(define gen&testo
  (lambda (op i j k)
    (onceo
      (fresh (x y z)
        (op x y z)
        (== i x)
        (== j y)
        (== k z)))))

(run* (q)
  (gen&testo +o '(0 0 1) '(1 1) '(1 1 1))
  (== #t q))
;; => (#t)

(run1 (q)
  (gen&testo +o '(0 0 1) '(1 1) '(0 1 1)))
;; => no value

(define enumerateo
  (lambda (op r n)
    (fresh (i j k)
      (bumpo n i)
      (bumpo n j)
      (op i j k)
      (gen&testo op i j k)
      (== (list i j k) r))))

(run* (s)
  (enumerateo +o s '(1 1)))
;; => long list

(run1 (s)
  (enumerateo +o s '(1 1 1)))
;; => (((1 1 1) (1 1 1) (0 1 1 1)))
