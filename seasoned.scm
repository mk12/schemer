;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.
;;; (exercises from The Seasoned Schemer)

(define (atom? x)
  (not (or (pair? x) (null? x))))

(define (member? a lat)
  (and (not (null? lat))
       (or (eq? (car lat) a)
           (member? a (cdr lat)))))

;;;; Chapter 11: Welcome Back to the Show

;;; two in a row

(define (two-in-a-row? lat)
  (cond
    ((null? lat) #f)
    ((null? (cdr lat)) #f)
    ((eq? (car lat) (cadr lat)) #t)
    (else (two-in-a-row? (cdr lat)))))

(define (two-in-a-row-a? preceding lat)
  (and (not (null? lat))
       (or (eq? (car lat) preceding)
           (two-in-a-row-a? (car lat) (cdr lat)))))

(define (two-in-a-row-aa? lat)
  (and (not (null? lat))
       (two-in-a-row-a? (car lat) (cdr lat))))

;;; cumulative sum

(define (cumsum-h preceding tup)
  (cond
    ((null? tup) '())
    (else (let ((x (+ (car tup) preceding)))
            (cons x (cumsum-h x (cdr tup)))))))

(define (cumsum tup)
  (cumsum-h 0 tup))

;;; scramble

(define (pick n lat)
  (cond
    ((one? n) (car lat))
    (else (pick (dec n) (cdr lat)))))

(define (scramble-h tup rev-pre)
  (cond
    ((null? tup) '())
    (else (cons (pick (car tup) rev-pre)
                (scramble-h (cdr tup)
                            (cons (car tup) rev-pre))))))

(define (scramble tup)
  (scramble-h tup '()))

;;;;; Chapter 12: Take Cover

;;; Y combinator

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

;;; multirember

(define (multirember a lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) a) (multirember a (cdr lat)))
    (else (cons (car lat) (multirember a (cdr lat))))))

(define (multirember-a a lat)
  ((letrec
     ((mr
        (lambda (lat)
          (cond
            ((null? lat) '())
            ((eq? (car lat) a) (mr (cdr lat)))
            (else (cons (car lat) (mr (cdr lat))))))))
     mr)
   lat))

(define (multirember-b a lat)
  (letrec
    ((mr
       (lambda (lat)
         (cond (null? lat) '())
         ((eq? (car lat) a) (mr (cdr lat))))))
    (mr lat)))


(define (multirember-f test?)
  (lambda (a lat)
    (letrec
      ((mr
        (lambda (lat)
          (cond (null? lat) '())
          ((test? (car lat) a) (mr (cdr lat))))))
      (mr lat))))

;;; union

(define (union set1 set2)
  (cond
    ((null? set1) set2)
    ((member? (car set1) set2) (union (cdr set1) set2))
    (else (cons (car set1) (union (cdr set1) set2)))))

(define (union-a set1 set2)
  ((letrec
     ((u
        (lambda (set)
          (cond
            ((null? set) set2)
            ((member? (car set) set2) (u (cdr set)))
            (else (cons (car set) (u (cdr set))))))))
     u)
   set1))

(define (union-b set1 set2)
  (letrec
    ((u
       (lambda (set)
         (cond
          ((null? set) set2)
          ((member? (car set) set2) (u (cdr set)))
          (else (cons (car set) (u (cdr set))))))))
    (u set1)))

(define (union-c set1 set2)
  (letrec
    ((u (lambda (set)
          (cond
            ((null? set) set2)
            ((m? (car set) set2) (u (cdr set)))
            (else (cons (car set) (u (cdr set)))))))
     (m? (lambda (a lat)
           (cond
             ((null? lat) #f)
             ((eq? (car lat) a) #t)
             (else (m? a (cdr lat)))))))
    (u set1)))

(define (union-d set1 set2)
  (letrec
    ((u (lambda (set)
          (cond
            ((null? set) set2)
            ((m? (car set) set2) (u (cdr set)))
            (else (cons (car set) (u (cdr set)))))))
     (m? (lambda (a lat)
           (letrec
             ((N? (lambda (lat)
                    (cond
                      ((null? lat) #f)
                      ((eq? (car lat) a) #t)
                      (else (N? (cdr lat)))))))
             (N? lat)))))
    (u set1)))

;;; two in a row

(define (two-in-a-row-h? lat)
  (letrec
    ((W (lambda (a lat)
          (cond
            ((null? lat) #f)
            ((eq? (car lat) a) #t)
            (else (W (car lat) (cdr lat)))))))
    (cond
      ((null? lat) #f)
      (else (W (car lat) (cdr lat))))))

(define two-in-a-row-hb?
  (letrec
    ((W (lambda (a lat)
          (cond
            ((null? lat) #f)
            ((eq? (car lat) a) #t)
            (else (W (car lat) (cdr lat)))))))
    (lambda (lat)
      (cond
        ((null? lat) #f)
        (else (W (car lat) (cdr lat)))))))

;;; cumulative sum

(define (cumsum-p tup)
  (letrec
    ((S (lambda (sss tup)
          (cond
            ((null? tup) '())
            (else (cons (+ (car tup) sss)
                        (S (+ (car tup) sss)
                           (cdr tup))))))))
    (S 0 tup)))

;;; scramble

(define scramble-p
  (letrec
    ((P (lambda tup rev-pre
          (cond
            ((null? tup) '())
            (else (cons (pick (car tup) rev-pre)
                        (P (cdr tup)
                           (cons (car tup) rev-pre))))))))
    (lambda (tup)
      (P tup '()))))

;;;;; Chapter 13: Hop, Skip, and Jump

;;; intersect

(define (intersect a b)
  (cond
    ((null? a) '())
    ((member? (car a) b) (cons (car a) (intersect (cdr a) b)))
    (else (intersect (cdr a) b))))

(define (intersect-h a b)
  (letrec
    ((I (lambda (s)
          (cond
            ((null? s) '())
            ((member? (car s) b) (cons (car s) (I (cdr s))))
            (else (I (cdr s)))))))
    (I a)))

;;; intersect all

(define (intersectall lset)
  (cond
    ((null? lset) '())
    ((null? (cdr lset)) (car lset))
    (else (intersect-h (car lset)
                       (intersectall (cdr lset))))))

(define intersectall-h
  (letrec
    ((A (lambda (lset)
          (cond
            ((null? (cdr lset)) (car lset))
            (else (intersect-h (car lset)
                               (A (cdr lset))))))))
    (lambda (lset)
      (cond
        ((null? lset) '())
        (else (A lset))))))

(define (intersectall-cc lset)
  (letcc hop
    (letrec
      ((A (lambda (lset)
            (cond
              ((null? (car lset)) (hop '()))
              ((null? (cdr lset)) (car lset))
              (else (intersect-h (car lset)
                                 (A (cdr lset))))))))
      (cond
        ((null? lset) '())
        (else (A lset))))))

(define (intersectall-ccc lset)
  (letcc hop
    (letrec
      ((A (lambda (lset)
            (cond
              ((null? (car lset)) (hop '()))
              ((null? (cdr lset)) (car lset))
              (else (I (car lset)
                       (A (cdr lset)))))))
       (I (lambda (s1 s2)
            (letrec
              ((J (lambda (s)
                    (cond
                      ((null? s) '())
                      ((member? (car s) s2) (cons (car s)
                                                  (J (cdr s))))
                      (else (J (cdr s)))))))
              (cond
                ((null? s2) (hop '()))
                (else (J s1)))))))
      (cond
        ((null? lset) '())
        (else (A lset))))))

;;; rember

(define (rember-h a lat)
  (letrec
    ((R (lambda (lat)
          (cond
            ((null? lat) '())
            ((eq? (car lat) a) (cdr lat))
            (else (cons (car lat) (R (cdr lat))))))))
    (R lat)))

(define (rember-beyond-first a lat)
  (letrec
    ((R (lambda (lat)
          (cond
            ((null? lat) '())
            ((eq? (car lat) a) '())
            (else (cons (car lat) (R (cdr lat))))))))
    (R a lat)))

(define (rember-upto-last a lat)
  (letcc skip
    (letrec
      ((R (lambda (lat)
            (cond
              ((null? lat) '())
              ((eq? (car lat) a) (skip (R (cdr lat))))
              (else (cons (car lat) (R (cdr lat)))))))))
    (R a lat)))

;;;;; Chapter 14: Let There Be Names

;;; leftmost

(define (leftmost l)
  (cond
    ((null? l) '())
    ((atom? (car l)) (car l))
    (else
      (let ((a (leftmost (car l))))
        (cond
          ((atom? a) a)
          (else (leftmost (cdr l))))))))

;;; rember* one

(define (rember1* a l)
  (letrec
    ((R (lambda (l)
          (cond
            ((null? l) '())
            ((atom? (car l))
             (cond
               ((eq? (car l) a) (cdr l))
               (else (cons (car l) (R (cdr l))))))
            (else
              (let ((rc (R (car l))))
                (cond
                  ((eqlist? (car l) rc) (cons rc (R (cdr l))))
                  (else (cons rc (cdr l)))))))))
    (R l))))

;;; depth

(define (depth* l)
  (cond
    ((null? l) 1)
    ((atom? (car l)) (depth* (cdr l)))
    (else
      (let ((a (add1 (depth* (car l))))
            (d (depth* (cdr l))))
        (cond
          ((> a d) a)
          (else d))))))

(define (depth*-m l)
  (cond
    ((null? l) 1)
    ((atom? (car l)) (depth* (cdr l)))
    (else (max (add1 (depth* (car l)))
               (depth* (cdr l))))))

;;; scramble

(define (scramble-l tup)
  (letrec
    ((P (lambda (tup rp)
          (cond
            ((null? tup) '())
            (else
              (let ((rp (cons (car tup) rp)))
                (cons (pick (car tup) rp)
                      (P (cdr tup) rp))))))))
    (P tup '())))

;;; leftmost

(define (leftmost-cc l)
  (letcc skip
    (letrec
      ((L (lambda (l)
            (cond
              ((null? l) '())
              ((atom? (car l)) (skip (car l)))
              (else
                (begin
                  (L (car l))
                  (L (cdr l))))))))
      (L l))))

;;; rember* one

(define (rember1*-rm a l)
  (letrec
    ((R (lambda (l cont)
          (cond
            ((null? l) (cont 'fail))
            ((atom? (car l))
             (if (eq? (car l) a)
               (cdr l)
               (cons (car l) (R (cdr l) cont))))
            (else
              (let ((rcar (letcc cont (R (car l) cont))))
                (if (atom? rcar)
                  (cons (car l) (R (cdr l) cont))
                  (cons rcar (cdr l)))))))))
    (let ((nl (letcc cont (R l cont))))
      (if (atom? nl) l nl))))

(define (rember1*-rmt a l)
  (letrec
    ((R (lambda (l cont)
          (cond
            ((null? l) (cont 'fail))
            ((atom? (car l))
             (if (eq? (car l) a)
               (cdr l)
               (cons (car l) (R (cdr l) cont))))
            (else
              (try cont2
                (cons (R (car l) cont2) (cdr l))
                (cons (car l) (R (cdr l) cont))))))))))

;;;;; Chapter 15: The Difference Between Men and Boys ...

(define omnivore
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food (cons x '())))))

(define gobbler
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food (cons x '())))))

(define (chez-nous)
  (let ((temp food))
    (set! food x)
    (set! x temp)))

;;;;; Chapter 16: Ready, Set, Bang!

;;; sweet tooth

(define (sweet-toothL food)
  (set! last food)
  (cons food (cons 'cake '())))

(define (sweet-toothR food)
  (set! ingredients
    (cons food ingredients))
  (cons food (cons 'cake '())))

;;; Deep

(define (deep n)
  (if (zero? n)
    'pizza
    (cons (deepM (sub1 n)) '())))

(define (mfind n ns rs)
  (letrec
    ((F (lambda (ns rs)
          (cond
            ((null? ns) #f)
            ((eq? (car ns) n)) (car rs)
            (else (F (cdr ns) (cdr rs)))))))
    (F ns rs)))

(define deepM
  (let ((ns '())
        (rs '()))
    (lambda (n)
      (let ((cached (mfind n ns rs)))
        (if (atom? cached)
          (let ((result (deep n)))
            (set! ns (cons n ns))
            (set! rs (cons result rs))
            result)
          cached)))))

;;; length

(define mlength
  (let ((h (lambda (l) 0)))
    (set! h
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (h (cdr l)))))))
    h))

(define (L recur)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (recur (cdr l)))))))

(define mlenth-b
  (let ((h (lambda (l) 0)))
    (set! h
      (L (lambda (arg) (h arg))))
    h))

;;; applicative-order imperative Y combinator

(define Y!
  (lambda (f)
    (let ((h (lambda (l) '())))
      (set! h
        (L (lambda (arg) (h arg))))
      h)))

(define Y-bang
  (lambda (f)
    (letrec
      ((h (f (lambda (arg) (h arg)))))
      h)))

(define length-y (Y! L))

(define (D recur)
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (recur (cdr l)))
      (else (max (add1 (recur (car l)))
                 (recur (cdr l)))))))

(define depth* (Y! D))

;;; bizzare

(define biz
  (let ((x 0))
    (lambda (f)
      (set! x (add1 x))
      (lambda (a)
        (if (= a x)
          0
          (f a))))))

;;;;; Chapter 17: We Change, Therefore We Are!

;;; deep (memory)

(define deepM
  (let ((ns '())
        (rs '())) 
    (lambda (n)
      (let ((cached (find n ns rs)))
        (if (atom? cached)
          (let ((result (if (zero? n)
                          'pizza
                          (cons (deepM (sub1 n)) '()))))
            (set! ns (cons n ns))
            (set! rs (cons result rs))
            result)
          cached)))))

;;; cons (count)

(define counter)
(define set-counter)

(define consC
  (let ((n 0))
    (set! counter (lambda () n))
    (set! set-counter (lambda (x) (set! n x)))
    (lambda (x xs)
      (set! n (add1 n))
      (cons x xs))))

(define (supercounter f)
  (letrec
    ((S (lambda (n)
          (if (zero? n)
            (f n)
            (let ()
              (f n)
              (S (sub1 n)))))))
    (S 1000)
    (counter)))

;;;;; Chapter 18: We Change, Therefore We Are the Same!

;;; lots

(define (lots m)
  (cond
    ((zero? m) '())
    (else (kons 'egg (lots (sub1 m))))))

(define (lenkth l)
  (cond
    ((null? l) 0)
    (else (add1 (lenkth (kdr l))))))

;;; add at end

(define (add-at-end l)
  (cond
    ((null? l) (cons 'egg '()))
    (else (kons (kar l)
                (add-at-end (kdr l))))))

(define (add-at-end-too l)
  (letrec
    ((A (lambda (ls)
          (cond
            ((null? (kdr ls))
             (set-kdr ls
                      (kons 'egg '())))
            (else (A (kdr ls)))))))
    (A l)
    l))

;;; kons, kar, kdr

;; λa.λd.λs.((s a) d)
(define kons
  (lambda (kar kdr)
    (lambda (selector)
      (selector kar kdr))))

;; λk.λa.λd.(k a)
(define kar
  (lambda (c)
    (c (lambda (a d) a))))

;; λk.λa.λd.(k d)
(define kdr
  (lambda (c)
    (c (lambda (a d) d))))

;;; bons, bar, bdr

(define bons
  (lambda (bar)
    (let ((bdr '()))
      (lambda (selector)
        (selector
          (lambda (x) (set! bdr x))
          bar
          bdr)))))

(define bar
  (lambda (c)
    (c (lambda (s a d) a))))

(define bdr
  (lambda (c)
    (c (lambda (s a d) d))))

(define set-bdr
  (lambda (c x)
    ((c (lambda (s a d) s)) x)))

(define bkons
  (lambda (a d)
    (let ((c (bons a)))
      (set-bdr c d)
      c)))

;;; eklist?, same

(define (eklist? ls1 ls2)
  (cond
    ((null? ls1) (null? ls2))
    ((null? ls2) #f)
    (else (and (eq? (kar ls1) (kar ls2))
               (eklist? (kdr ls1) (kdr ls2))))))

(define (same c1 c2)
  (let ((t1 (kdr c1))
        (t2 (kdr c2)))
    (set-kdr c1 1)
    (set-kdr c2 2)
    (let ((v (= (kdr c1) (kdr c2))))
      (set-kdr c1 t1)
      (set-kdr c2 t2)
      v)))

;;; last kons

(define (last-kons ls)
  (cond
    ((null? (kdr ls)) ls)
    (else (last-kons (kdr ls)))))

(define long (lots 12))
(set-kdr (last-kons long) long)
(lenkth long) ;; never returns -- it's circular!

;;; finite lenkth

(define (finite-lenkth p)
  (letcc infinite
    (letrec
      ((C (lambda (p q)
            (cond
              ((same? p q) (infinite #f))
              ((null? q) 0)
              ((null? (kdr q)) 1)
              (else (+ (C (sl p) (qk q))
                       2)))))
       (qk (lambda (x) (kdr (kdr x))))
       (sl (lambda (x) (kdr x))))
      (cond
        ((null? p) 0)
        (else (add1 (C p (kdr p))))))))

;;;;; Chapter 19: Absconding with the Jewels

;;; deep

(define toppings)
(define (deepB m)
  (cond
    ((zero? m)
     (letcc jump
       (set! toppings jump)
       (quote pizza)))
    (else (cons (deepB (sub1 m)) '()))))

(define (deep&co m k)
  (cond
    ((zero? m) (k 'pizza))
    (else
      (deep&co
        (sub1 m)
        (lambda (x)
          (k (cons x '())))))))

(define (two-layers p)
  (cons (cons p '()) '()))

(define (deep&coB m k)
  (cond
    ((zero? m)
     (let ()
       (set! toppings k)
       (k 'pizza)))
    (else
      (deep&coB
        (sub1 m)
        (lambda (x)
          (k (cons x '())))))))

;;; walk

(define leave)
(define (walk l)
  (cond
    ((null? l) '())
    ((atom? (car l)) (leave (car l)))
    (else (let ()
            (walk (car l))
            (walk (cdr l))))))

(define (start-it l)
  (letcc here
    (set! leave here)
    (walk l)))

;;; waddle

(define fill)
(define (waddle l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (let ()
       (letcc rest
         (set! fill rest)
         (leave (car l)))
       (waddle (cdr l))))
    (else (let ()
            (waddle (car l))
            (waddle (cdr l))))))

(define (start-it2 l)
  (letcc here
    (set! leave here)
    (waddle l)))

(define (get-next x)
  (letcc here-again
    (set! leave here-again)
    (fill 'go)))

(define (get-first l)
  (letcc here
    (set! leave here)
    (waddle l)
    (leave '())))

;;; two in a row

(define (two-in-a-row-a*? l)
  (let ((fst (get-first l)))
    (if (atom? fst)
      (two-in-a-row-b*? fst)
      #f)))

(define (two-in-a-row-b*? a)
  (let ((n (get-next 'go)))
    (if (atom? n)
      (or (eq? n a)
          (two-in-a-row-b*? n))
      #f)))

(define two-in-a-row*?
  (letrec ((T? (lambda (a)
                 (let ((n (get-next 0)))
                   (if (atom? n)
                     (or (eq? n a)
                         (T? n))
                     #f))))
           (get-next
             (lambda (x)
               (letcc here-again
                 (set! leave here-again)
                 (fill 'go))))
           (fill (lambda (x) x))
           (waddle
             (lambda (l)
               (cond
                 ((null? l) '())
                 ((atom? (car l))
                  (let ()
                    (letcc rest
                      (set! fill rest)
                      (leave (car l)))
                    (waddle (cdr l))))
                 (else (let ()
                         (waddle (car l))
                         (waddle (cdr l)))))))
           (leave (lambda (x) x)))
    (lambda (l)
      (let ((fst (letcc here
                   (set! leave here)
                   (waddle l)
                   (leave '()))))
        (if (atom? fst)
          (T? fst)
          #f)))))

;;;;; Chapter 20: What's in Store?

(define (the-empty-table name)
  (abort (cons 'no-answer
               (cons name '()))))

(define (lookup table name)
  (table name))

(define (extend name1 value table)
  (lambda (name2)
    (cond
      ((eq? name2 name1) value)
      (else (table name2)))))

(define abort)
(define (value e)
  (letcc the-end
    (set! abort the-end)
    (cond
      ((define? e) (*define e))
      (else (the-meaning e)))) ...)

(define (define? e)
  (cond
    ((atom? e) #f)
    ((atom? (car e))
     (eq? (car e) 'define))
    (else #f)))

(define global-table
  ... the-empty-table ...)

(define (*define e)
  (set! global-table
    (extend
      (name-of e)
      (box (the-meaning (right-side-of e)))
      global-table)))

(define (box it)
  (lambda (sel)
    (sel it (lambda (new)
              (set! it new)))))

(define (setbox b new)
  (b (lambda (it set) (set new))))

(define (unbox b)
  (b (lambda (it set) it)))

(define (the-meaning e)
  (meaning e lookup-in-global-table))

(define (lookup-in-global-table name)
  (lookup global-table name))

(define (meaning e table)
  ((expression-to-action e) e table))

(define (*quote e table)
  (text-of e))

(define (*identifier e table)
  (unbox (lookup table e)))

(define (*set e table)
  (setbox (lookup table (name-of e))
          (meaning (right-side-of e) table)))

(define (*lambda e table)
  (lambda (args)
    (beglis (body-of e)
            (multi-extend
              (formals-of e)
              (box-all args)
              table))))

(define (beglis es table)
  (cond
    ((null? (cdr es)) (meaning (car es) table))
    (else ((lambda (val)
             (beglis (cdr es) table))
           (meaning (car es) table)))))

(define (box-all vals)
  (cond
    ((null? vals) '())
    (else (cons (box (car vals))
                (box-all (cdr vals))))))

(define (multi-extend names vals table)
  (cond
    ((null? names) table)
    (else (extend (car names)
                  (car vals)
                  (multi-extend (cdr names)
                                (cdr vals)
                                table)))))

(define (odd? n)
  (cond
    ((zero? n) #f)
    (else (even? (sub1 n)))))

(define (even? n)
  (cond
    ((zero? n) #t)
    (else (odd? (sub1 n)))))

(define (*application e table)
  ((meaning (function-of e) table)
   (evlis (arguments-of e) table)))

(define (evlist args table)
  (cond
    ((null? args) '())
    (else ((lambda (val)
             (cons val
                   (evlist (cdr args) table)))
           (meaning (car args) table)))))

(define (a-prim p)
  (lambda (args-in-a-list)
    (p (car args-in-a-list))))

(define (b-prim p)
  (lambda (args-in-a-list)
    (p (car args-in-a-list) (car (cdr args-in-a-list)))))

(define (*const e table)
  (cond
    ((number? e) e)
    ((eq? e #t) #t)
    ((eq? e #f) #f)
    ((eq? e 'cons) (b-prim cons))
    ((eq? e 'car) (a-prim car))
    ((eq? e 'cdr) (a-prim cdr))
    ((eq? e 'eq?) (b-prim eq?))
    ((eq? e 'atom?) (a-prim atom?))
    ((eq? e 'null?) (a-prim null?))
    ((eq? e 'zero?) (a-prim zero?))
    ((eq? e 'add1) (a-prim add1))
    ((eq? e 'sub1) (a-prim sub1))
    ((eq? e 'number?) (a-prim number?))))

(define (*cond e table)
  (evcon (cond-lines-of e) table))

(define (evcon lines table)
  (cond
    ((else? (question-of (car lines)))
     (meaning (answer-of (car lines)) table))
    ((meaning (question-of (car lines)) table)
     (meaning (answer-of (car lines)) table))
    (else (evcon (cdr lines) table))))

(define (*letcc e table)
  (letcc skip
    (beglis (ccbody-of e)
            (extend (name-of e)
                    (box (a-prim skip))
                    table))))

(define (expression-to-action e)
  (cond
    ((atom? e) (atom-to-action e))
    (else (list-to-action e))))

(define (atom-to-action e)
  (cond
    ((number? e) *const)
    ((eq? e #t) *const)
    ((eq? e #f) *const)
    ((eq? e 'cons) *const)
    ((eq? e 'car) *const)
    ((eq? e 'cdr) *const)
    ((eq? e 'null?) *const)
    ((eq? e 'eq?) *const)
    ((eq? e 'atom?) *const)
    ((eq? e 'zero?) *const)
    ((eq? e 'add1) *const)
    ((eq? e 'sub1) *const)
    ((eq? e 'number?) *const)
    (else *identifier)))

(define (list-to-action e)
  (cond
    ((atom? (car e))
     (cond
       ((eq? (car e) 'quote) *quote)
       ((eq? (car e) 'lambda) *lambda)
       ((eq? (car e) 'letcc) *letcc)
       ((eq? (car e) 'set!) *set)
       ((eq? (car e) 'cond) *cond)
       (else *application)))
    (else *application)))

(define (text-of x) (car (cdr x)))
(define (formals-of x) (car (cdr x)))
(define (body-of x) (cdr (cdr x)))
(define (ccbody-of x) (cdr (cdr x)))
(define (name-of x) (car (cdr x)))
(define (right-side-of x)
  (cond
    ((null? (cdr (cdr x))) 0)
    (else (car (cdr (cdr x))))))
(define (cond-lines-of x) (cdr x))
(define (else? x)
  (cond
    ((atom? x) (eq? x 'else))
    (else #f)))
(define (question-of x) (car x))
(define (answer-of x) (car (cdr x)))
(define (function-of x) (car x))
(define (arguments-of x) (cdr x))
