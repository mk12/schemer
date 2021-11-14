;;; Copyright 2016 Mitchell Kember. Subject to the MIT License.
;;; (exercises from The Little Prover)

(load "../j-bob/scheme/j-bob-lang.scm")
(load "../j-bob/scheme/j-bob.scm")

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

(defun chapter1.frame62 ()
  (J-Bob/step (prelude)
    '(atom (car (cons (car a) (cdr b))))
    '(((1) (car/cons (car a) (cdr b))))))
;; => (atom (car a))

;;;;; Chapter 2: Even Older Games
