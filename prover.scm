;;; Copyright 2016 Mitchell Kember. Subject to the MIT License.
;;; (exercises from The Little Prover)

;;;;; Chapter 1: Old Games, New Rules

;;; The Axioms of Cons (initial)
(dethm atom/cons (x y)
  (equal (atom (cons x y)) 'nil))
(dethm car/cons (x y)
  (equal (car (cons x y)) x))
(dethm cdr/cons (x y)
  (equal (cdr (cons x y)) y))

;;; The Axioms of Equal (initial)
(dethm equal-same (x)
  (equal (equal x x) 't))
(dethm equal-swap (x y)
  (equal (equal x y) (equal y x)))

;;; The Law of Dethm (initial)
;; For any theorem (dethm name (x1 ... xn) bodyx), the variables x1, ..., xn in
;; bodyx can be replaced with any corresponding expressions e1, ..., en. The
;; result, bodye, can be used to rewrite a focus p to become q provided bodye is
;; either (equal p q) or (equal q p).

(atom (car (cons (car a) (cdr b))))
;; => (atom (car a))

;;;;; Chapter 2: Even Older Games

;;; The Axioms of If (initial)
(dethm if-true (x y)
  (equal (if 't x y) x))
(dethm if-false (x y)
  (equal (if 'f x y) y))
(dethm if-same (x y)
  (equal (if x y y) y))

;;; The Axioms of Equal (final)
(dethm equal-same (x)
  (equal (equal x x) 't))
(dethm equal-swap (x y)
  (equal (equal x y) (equal y x)))
(dethm equal-if (x y)
  (if (equal x y) (equal x y) 't))

;;; The Law of Dethm (final)
;; For any theorem (dethm name (x1 ... xn) bodyx), the variables x1, ..., xn in
;; bodyx can be replaced with any corresponding expressions e1, ..., en. The
;; result, bodye, can be used to rewrite a focus as follows:
;;
;; 1. bodye must contain the conclusion (equal p q) or (equal q p),
;; 2. the conclusion must not be found in the question of any "if" or in the
;;    argument of any function application,
;; 3. and if the conclusion ca nbe found in an "if" answer (respectively else),
;;    then the focus must be found in an "if" answer (respectively else) with
;;    the same question.

(if (if (equal a 't)
        a
        (equal 'or '(black coffee)))
    c
    c)
;; => (if (if (equal a 't)
;;            't
;;            (equal 'or '(black coffee)))
;;        c
;;        c)

;;; The Axioms of Cons (final)
(dethm atom/cons (x y)
  (equal (atom (cons x y)) 'nil))
(dethm car/cons (x y)
  (equal (car (cons x y)) x))
(dethm cdr/cons (x y)
  (equal (cdr (cons x y)) y))
(dethm cons/car+cdr (x)
  (if (atom x)
    't
    (equal (cons (car x) (cdr x)) x)))

;;; The Axioms of If (final)
(dethm if-true (x y)
  (equal (if 't x y) x))
(dethm if-false (x y)
  (equal (if 'f x y) y))
(dethm if-same (x y)
  (equal (if x y y) y))
(dethm if-nest-A (x y z)
  (if x (equal (if x y z) y) 't))
(dethm if-nest-E (x y z)
  (if x 't (equal (if x y z) z)))

;;;;; Chapter 3: What's in a Name?
