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
