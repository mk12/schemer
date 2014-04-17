;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.
;;; (exercises from The Little Schemer)

;;;;; Chapter 1: Toys

(define (atom? x) (not (list? x)))

;;;;; Chapter 2: Do It, Do It Again, and Again, and Again ...

(define (lat? ls)
  (cond
    ((null? ls) #t)
    ((atom? (car ls)) (lat? (cdr ls)))
    (else #f)))

(define (member? x ls)
  (cond
    ((null? ls) #f)
    (else (or (equal? (car ls) x)
              (member? x (cdr ls))))))

;;;;; Chapter 3: Cons the Magnificent

(define (rember x ls)
  (cond
    ((null? ls) '())
    ((eq? (car ls) x) (cdr ls))
    (else (cons (car ls)
                (rember x (cdr ls))))))

(define (firsts ls)
  (cond
    ((null? ls) '())
    (else (cons (caar ls)
                (firsts (cdr ls))))))

(define (seconds ls)
  (cond
    ((null? ls) '())
    (else (cons (cadar ls)
                (seconds (cdr ls))))))

(define (insertR x after ls)
  (cond
    ((null? ls) '())
    ((eq? (car ls) after)
     (cons after (cons x (cdr ls))))
    (else (cons (car ls)
                (insertR x after (cdr ls))))))

(define (insertL x before ls)
  (cond
    ((null? ls) '())
    ((eq? (car ls) before)
     (cons x ls))
    (else (cons (car ls)
                (insertL x before (cdr ls))))))

(define (subst new old ls)
  (cond
    ((null? ls) '())
    ((eq? (car ls) old)
     (cons new (cdr ls)))
    (else (cons (car ls)
                (subst new old (cdr ls))))))

(define (subst2 new o1 o2 ls)
  (cond
    ((null? ls) '())
    ((or (eq? (car ls) o1)
         (eq? (car ls) o2))
     (cons new (cdr ls)))
    (else (cons (car ls)
                (subst2 new o1 o2 (cdr ls))))))

(define (multirember x ls)
  (cond
    ((null? ls) '())
    ((equal? (car ls) x) (multirember x (cdr ls)))
    (else (cons (car ls)
                (multirember x (cdr ls))))))

(define (multiinsertR x after ls)
  (cond
    ((null? ls) '())
    ((eq? (car ls) after)
     (cons after
           (cons x (multiInsertR x after (cdr ls)))))
    (else (cons (car ls)
                (multiInsertR x after (cdr ls))))))

(define (multiinsertL x before ls)
  (cond
    ((null? ls) '())
    ((eq? (car ls) before)
     (cons x (cons before
                   (multiInsertL x before (cdr ls)))))
    (else (cons (car ls)
                (multiInsertL x before (cdr ls))))))

(define (multisubst new old ls)
  (cond
    ((null? ls) '())
    ((eq? (car ls) old)
     (cons new (multisubst new old (cdr ls))))
    (else (cons (car ls)
                (multisubst new old (cdr ls))))))

;;;;; Chapter 4: Numbers Games

(define (add1 n) (+ n 1))
(define (sub1 n) (- n 1))

(define (o+recur n m)
  (cond
    ((zero? m) n)
    (else (add1 (o+recur n (sub1 m))))))

(define (o+ n m)
  (cond
    ((zero? m) n)
    (else (o+ (add1 n) (sub1 m)))))

(define (o-recur n m)
  (cond
    ((zero? m) n)
    (else (sub1 (o- n (sub1 m))))))

(define (o- n m)
  (cond
    ((zero? m) n)
    (else (o- (sub1 n) (sub1 m)))))

(define (addtup-recur tup)
  (cond
    ((null? tup) 0)
    (else (o+ (car tup)
              (addtup (cdr tup))))))

(define (addtup tup)
  (define (helper tup total)
    (cond
      ((null? tup) total)
      (else (helper (cdr tup)
                    (o+ total (car tup))))))
  (helper tup 0))

(define (o*recur n m)
  (cond
    ((zero? m) 0)
    (else (o+ n (o* n (sub1 m))))))

(define (o* n m)
  (define (helper x product)
    (cond
      ((zero? x) product)
      (else (helper (sub1 x) (o+ product n)))))
  (helper m 0))

(define (tup+ tup1 tup2)
  (cond
    ((null? tup1) tup2)
    ((null? tup2) tup1)
    (else (cons (o+ (car tup1)
                    (car tup2))
                (tup+ (cdr tup1) (cdr tup2))))))

(define (o> n m)
  (cond
    ((zero? n) #f)
    ((zero? m) #t)
    (else (o> (sub1 n) (sub1 m)))))

(define (o< n m)
  (cond
    ((zero? m) #f)
    ((zero? n) #t)
    (else (o< (sub1 n) (sub1 m)))))

(define (o= n m)
  (cond
    ((or (> n m) (< n m)) #f)
    (else #t)))

(define (^recur n m)
  (cond
    ((zero? m) 1)
    (else (o* n (^recur n (sub1 m))))))

(define (^ n m)
  (define (helper x power)
    (cond
      ((zero? x) power)
      (else (helper (sub1 x) (o* power n)))))
  (helper m 1))

(define (oquotient-recur n m)
  (cond
    ((o< n m) 0)
    (else (add1 (oquotient-recur (o- n m) m)))))

(define (oquotient n m)
  (define (helper x quot)
    (cond
      ((o< x m) quot)
      (else (helper (o- x m) (add1 quot)))))
  (helper n 0))

(define (olength-recur ls)
  (cond
    ((null? ls) 0)
    (else (add1 (olength-recur (cdr ls))))))

(define (olength ls)
  (define (helper sls len)
    (cond
      ((null? sls) len)
      (else (helper (cdr sls) (add1 len)))))
  (helper ls 0))

(define (pick n ls)
  (cond
    ((zero? n) (car ls))
    (else (pick (sub1 n) (cdr ls)))))

(define (rempick n ls)
  (cond
    ((zero? n) (cdr ls))
    (else (cons (car ls)
                (rempick (sub1 n) (cdr ls))))))

(define (no-nums ls)
  (cond
    ((null? ls) '())
    ((number? (car ls)) (no-nums (cdr ls)))
    (else (cons (car ls)
                (no-nums (cdr ls))))))

(define (all-nums ls)
  (cond
    ((null? ls) '())
    ((number? (car ls))
     (cons (car ls) (all-nums (cdr ls))))
    (else (all-nums (cdr ls)))))

(define (equan? a b)
  (cond
    ((and (number? a) (number? b)) (o= a b))
    ((or (number? a) (number? b)) #f)
    (else (eq? a b))))

(define (occur-recur a ls)
  (cond
    ((null? ls) 0)
    ((eq? (car ls) a)
     (add1 (occur-recur a (cdr ls))))
    (else (occur-recur a (cdr ls)))))

(define (occur a ls)
  (define (helper sls total)
    (cond
      ((null? sls) total)
      ((eq? (car sls) a)
       (helper (cdr sls) (add1 total)))
      (else (helper (cdr sls) total))))
  (helper ls 0))

;;;;; Chapter 5: *Oh My Gawd*: It's Full of Stars

(define (rember* a ls)
  (cond
    ((null? ls) '())
    ((atom? (car ls))
     (cond
       ((eq? (car ls) a)
        (rember* a (cdr ls)))
       (else (cons (car ls)
                   (rember* a (cdr ls))))))
    (else (cons (rember* a (car ls))
                (rember* a (cdr ls))))))

(define (insertR* x after ls)
  (cond
    ((null? ls) '())
    ((atom? (car ls))
     (cond
       ((eq? (car ls) after)
        (cons after
              (cons x (insertR* x after (cdr ls)))))
       (else (cons (car ls)
                   (insertR* x after (cdr ls))))))
    (else (cons (insertR* x after (car ls))
                (insertR* x after (cdr ls))))))

(define (occur* a ls)
  (cond
    ((null? ls) 0)
    ((atom? (car ls))
     (cond
       ((eq? (car ls) a)
        (add1 (occur* a (cdr ls))))
       (else (occur* a (cdr ls)))))
    (else (o+ (occur* a (car ls))
              (occur* a (cdr ls))))))

(define (subst* new old ls)
  (cond
    ((null? ls) '())
    ((atom? (car ls))
     (cond
       ((eq? (car ls) old)
        (cons new (subst* new old (cdr ls))))
       (else (cons (car ls)
                   (subst* new old (cdr ls))))))
    (else (cons (subst* new old (car ls))
                (subst* new old (cdr ls))))))

(define (insertL* x before ls)
  (cond
    ((null? ls) '())
    ((atom? (car ls))
     (cond
       ((eq? (car ls) before)
        (cons x (cons before
                      (insertL* x before (cdr ls)))))
       (else (cons (car ls)
                   (insertL* x before (cdr ls))))))
    (else (cons (insertL* x before (car ls))
                (insertL* x before (cdr ls))))))

(define (member*? a ls)
  (cond
    ((null? ls) #f)
    ((atom? (car ls))
     (or (eq? (car ls) a)
         (member* a (cdr ls))))
    (else (or (member* a (car ls))
              (member* a (cdr ls))))))

(define (leftmost ls)
  (cond
    ((atom? (car ls)) (car ls))
    (else (leftmost (car ls)))))

(define (eqlist1? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    ((and (atom? (car l1)) (atom? (car l2)))
     (and (equan? (car l1) (car l2))
          (eqlist1? (cdr l1) (cdr l2))))
    ((or (atom? (car l1)) (atom? (car l2))) #f)
    (else (and (eqlist1? (car l1) (car l2))
               (eqlist1? (cdr l1) (cdr l2))))))

(define (oequal? a b)
  (cond
    ((and (atom? a) (atom? b))
     (equan? a b))
    ((or (atom? a) (atom? b)) #f)
    (else (eqlist? a b))))

(define (eqlist? l1 l2)
  (cond
    ((and (null? l2) (null? l2)) #t)
    ((or (null? l2) (null? l2)) #f)
    (else (and (oequal? (car l1) (car l2))
               (eqlist? (cdr l1) (cdr l2))))))

(define (rember2 a ls)
  (cond
    ((null? ls) '())
    ((oequal? (car ls) a) (cdr ls))
    (else (cons (car ls)
                (rember2 a (cdr ls))))))

;;;;; Chapter 6: Shadows

(define (numbered? expr)
  (cond
    ((atom? expr) (number? expr))
    (else (and (number? (car expr))
               (number? (caddr expr))))))

(define (operator expr) (car expr))
(define (subexpr1 expr) (cadr expr))
(define (subexpr2 expr) (caddr expr))

(define (value expr)
  (cond
    ((atom? expr) expr)
    ((eq? (operator expr) '+)
     (o+ (value (subexpr1 expr))
         (value (subexpr2 expr))))
    ((eq? (operator expr) '*)
     (o* (value (subexpr1 expr))
         (value (subexpr2 expr))))
    (else (^ (value (subexpr1 expr))
             (value (subexpr2 expr))))))

(define (sero? n) (null? n))
(define (edd1 n) (cons '() n))
(define (zub1 n) (cdr n))

(define (e+ n m)
  (cond
    ((sero? m) n)
    (else (e+ (edd1 n) (zub1 m)))))

;;;;; Chapter 7: Friends and Relations

(define (set? ls)
  (cond
    ((null? ls) #t)
    ((member? (car ls) (cdr ls)) #f)
    (else (set? (cdr ls)))))

(define (makeset ls)
  (cond
    ((null? ls) '())
    (else (cons (car ls)
                (makeset (multirember (car ls) (cdr ls)))))))

(define (subset? s1 s2)
  (cond
    ((null? s1) #t)
    (else (and (member? (car s1) s2)
               (subset? (cdr s1) s2)))))

(define (eqset? s1 s2)
  (and (subset? s1 s2) (subset? s2 s1)))

(define (intersect? s1 s2)
  (cond
    ((null? s1) #f)
    (else (or (member? (car s1) s2)
              (intersect? (cdr s1) s2)))))

(define (intersect s1 s2)
  (cond
    ((null? s1) '())
    ((member? (car s1) s2)
     (cons (car s1) (intersect (cdr s1) s2)))
    (else (intersect (cdr s1) s2))))

(define (union s1 s2)
  (cond
    ((null? s1) s2))
    ((member? (car s1) s2)
     (union (cdr set1) s2))
    (else (cons (car s1)
                (union (cdr s1) s2))))

(define (difference s1 s2)
  (cond
    ((null? s1) '())
    ((member? (car s1) s2)
     (difference (cdr s1) s2))
    (else (cons (car s1)
                (difference (cdr s1) s2)))))

(define (intersectall lset)
  (cond
    ((null? (cdr lset)) (car lset))
    (else (intersect (car lset)
                     (intersectall (cdr lset))))))

(define (a-pair? x)
  (and (not (or (atom? x)
                (null? x)
                (null? (cdr x))))
       (null? (cddr x))))

(define (first p) (car p))
(define (second p) (cadr p))
(define (build a b) (cons a (cons b '())))

(define (fun? rel) (set? (firsts rel)))
(define (revpair p) (build (second p) (first p)))

(define (revrel rel)
  (cond
    ((null? rel) '())
    (else (cons (revpair (car rel))
                (revrel (cdr rel))))))

(define (one-to-one fun) (fun? (revrel fun)))

;;;;; Chapter 8: Lambda the Ultimate

(define (rember-f test?)
  (lambda (a ls)
    (cond
      ((null? ls) '())
      ((test? (car ls) a) (cdr ls))
      (else (cons (car ls)
                  ((rember-f test?) a (cdr ls)))))))

(define (eq?-c a) (lambda (x) (eq? x a)))

(define (insertL-f test?)
  (lambda (x before ls)
    (cond
      ((null? ls) '())
      ((test? (car ls) before)
       (cons x ls))
      (else (cons (car ls)
                  ((insertL-f test?) x before (cdr ls)))))))

(define (insertR-f test?)
  (lambda (x after ls)
    (cond
      ((null? ls) '())
      ((test? (car ls) after)
       (cons after (cons x (cdr ls))))
      (else (cons (car ls)
                  ((insertR-f test?) x after (cdr ls)))))))

(define (seqL x y ls) (cons x (cons y ls)))
(define (seqR x y ls) (cons y (cons x ls)))

(define (insert-g seq)
  (lambda (x old ls)
    (cond
      ((null? ls) '())
      ((eq? (car ls) old)
       (seq x old (cdr ls)))
      (else (cons (car ls)
                  ((insert-g seq) x old (cdr ls)))))))

(define insertL-new
  (insert-g
    (lambda (x y ls) (cons x (cons y ls)))))

(define subst-new
  (insert-g
    (lambda (x _ ls) (cons x ls))))

(define (atom->function x)
  (cond
    ((eq? x '+) o+)
    ((eq? x '*) o*)
    (else ^)))

(define (value-new expr)
  (cond
    ((atom? expr) expr)
    (else ((atom->function (operator expr))
           (value-new (subexpr1 expr))
           (value-new (subexpr2 expr))))))

(define (multirember-f test?)
  (lambda (a ls)
    (cond
      ((null? ls) '())
      ((test? (car ls) a)
       ((multirember-f test?) a (cdr ls)))
      (else (cons (car ls)
                  ((multirember-f test?) a (cdr ls)))))))

(define (multiremberT test? ls)
  (cond
    ((null? ls) '())
    ((test? (car ls))
     (multiremberT test? (cdr ls)))
    (else (cons (car ls)
                (multiremberT test? (cdr ls))))))

(define (multirember&co a lat col)
  (cond
    ((null? lat) (col '() '()))
    ((eq? (car lat) a)
     (multirember&co
       a
       (cdr lat)
       (lambda (newlat seen)
         (col newlat (cons (car lat) seen)))))
    (else
      (multirember&co
        a
        (cdr lat)
        (lambda (newlat seen)
          (col (cons (car lat) newlat) seen))))))

(define (multiinsertLR&co new oldL oldR lat col)
  (cond
    ((null? lat) (col '() 0 0))
    ((eq? (car lat) oldL)
     (multiinsertLR&co
       new
       oldL
       oldR
       (cdr lat)
       (lambda (newlat nleft nright)
         (col (cons new (cons oldL newlat)) (add1 nleft) nright))))
    ((eq? (car lat) oldR)
     (multiinsertLR&co
       new
       oldL
       oldR
       (cdr lat)
       (lambda (newlat nleft nright)
         (col (cons oldR (cons new newlat)) nleft (add1 nright)))))
    (else
      (multiinsertLR&co
        new
        oldL
        oldR
        (cdr lat)
        (lambda (newlat nleft nright)
          (col (cons (car lat) newlat) nleft nright))))))

(define (evens-only* l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((even? (car l))
        (cons (car l) (evens-only* (cdr l))))
       (else (evens-only* (cdr l)))))
    (else (cons (evens-only* (car l))
                (evens-only* (cdr l))))))

(define (evens-only*&co l col)
  (cond
    ((null? l) (col '() 1 0))
    ((atom? (car l))
     (cond
       ((even? (car l))
        (evens-only*&co
          (cdr l)
          (lambda (newl p s)
            (col (cons (car l) newl) (* (car l) p) s))))
       (else
         (evens-only*&co
           (cdr l)
           (lambda (newl p s)
             (col newl p (+ (car l) s)))))))
    (else
      (evens-only*&co
        (cdr l)
        (lambda (dl dp ds)
          (evens-only*&co
            (car l)
            (lambda (al ap as)
              (col (cons al dl) (* ap dp) (+ as ds)))))))))

(define (looking a lat)
  (keep-looking a (pick 1 lat) lat))

(define (keep-looking a sorn lat)
  (cond
    ((number? sorn) (keep-looking a (pick sorn lat) lat))
    (else (eq? sorn a))))

(define (shift pair)
  (build (first (first pair))
         (build (second (first pair))
                (second pair))))

(define (align pora)
  (cond
    ((atom? pora) pora)
    ((a-pair? (first pora))
     (align (shift pora)))
    (else (build (first pora)
                 (align (second pora))))))

(define (Y le)
  ((lambda (f) (f f))
   (lambda (f)
     (le (lambda (x) ((f f) x))))))

(define new-entry build)

(define (lookup-in-entry name entry entry-f)
  (let recur ((name name)
              (keys (first entry))
              (vals (second entry))
              (entry-f entry-f))
    (cond
      ((null? keys) (entry-f name))
      ((eq? (car keys) name) (car vals))
      (else (recur name (cdr keys) (cdr vals) entry-f)))))

(define extend-table cons)

(define (lookup-in-table name table table-f)
  (cond
    ((null? table)
     (table-f name))
    (else
      (lookup-in-entry
        name
        (car table)
        (lambda (name)
        (lookup-in-table name (cdr table) table-f))))))

(define (expression->action e)
  (cond
    ((atom? e) (atom->action e))
    (else (list->action e))))

(define (atom->action e)
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

(define (list->action e)
  (cond
    ((atom? (car e))
     (cond
       ((eq? (car e) 'quote) *quote)
       ((eq? (car e) 'lambda) *lambda)
       ((eq? (car e) 'cond) *cond)
       (else *application)))
     (else *application)))

(define (evalue e)
  (meaning e '()))

(define (meaning e table)
  ((expression->action e) e table))

(define (*const e table)
  (cond
    ((number? e) e)
    ((eq? e #t) #t)
    ((eq? e #f) #f)
    (else (build 'primitive e))))

(define text-of second)
(define (*quote e table)
  (text-of e))

(define (*identifier e table)
  (lookup-in-table e table (lambda (name) (car '()))))

(define table-of car)
(define formals-of cadr)
(define body-of caddr)
(define (*lambda e table)
  (build 'non-primitive
         (cons table (cdr e))))

(define cond-lines-of cdr)
(define (*cond e table)
  (evcon (cond-lines-of e) table))

(define question-of first)
(define answer-of second)
(define (evcon lines table)
  (cond
    ((else? (question-of (car lines)))
     (meaning (answer-of (car lines)) table))
    ((meaning (question-of (car lines)) table)
     (meaning (answer-of (car lines)) table))
    (else (evcon (cdr lines) table))))

(define (else? x)
  (and (atom? x) (eq? x 'else)))

(define function-of car)
(define arguments-of cdr)
(define (*application e table)
  (applyy (meaning (function-of e) table)
         (evlis (arguments-of e) table)))

(define (evlis args table)
  (cond
    ((null? args) '())
    (else (cons (meaning (car args) table)
                (evlis (cdr args) table)))))

(define (primitive? l) (eq? (first l) 'primitive)) 
(define (applyy fun vals)
  (cond
    ((primitive? fun)
     (apply-primitive (second fun) vals))
    (else
      (apply-closure (second fun) vals))))

;; Didn't feel like copying the entire thing.
(define (apply-primitive name vals)
  (apply (eval name) vals))

(define (apply-closure closure vals)
  (meaning (body-of closure)
           (extend-table
             (new-entry
               (formals-of closure)
               vals)
             (table-of closure))))
