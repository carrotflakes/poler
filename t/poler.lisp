(in-package :cl-user)
(defpackage poler-test
  (:use :cl
        :poler
        :prove))
(in-package :poler-test)

;; NOTE: To run this test file, execute `(asdf:test-system :poler)' in your Lisp.

(plan 107)


;;; infix | infixl | infixr

(is (polish '(1 + 2 + 3)
      (+ :infix 1))
    '(+ 1 2 3)
    :test #'equal)

(is (polish '(1 + 2 + 3)
      (+ :infixl 1))
    '(+ (+ 1 2) 3)
    :test #'equal)

(is (polish '(1 + 2 + 3)
      (+ :infixr 1))
    '(+ 1 (+ 2 3))
    :test #'equal)


;;; infix | infixl | infixr & parenthesis

(is (polish '(1 + (2 + 3) + 4)
      (+ :infix 1))
    '(+ 1 (+ 2 3) 4)
    :test #'equal)

(is (polish '(1 + (2 + 3) + 4)
      (+ :infixl 1))
    '(+ (+ 1 (+ 2 3)) 4)
    :test #'equal)

(is (polish '(1 + (2 + 3) + 4)
      (+ :infixr 1))
    '(+ 1 (+ (+ 2 3) 4))
    :test #'equal)


;;; infix | infixl | infixr & different precedence

(is (polish '(1 + 2 * 3 + 4)
      (+ :infix 1)
      (* :infix 2))
    '(+ 1 (* 2 3) 4)
    :test #'equal)

(is (polish '(1 + 2 * 3 + 4)
      (+ :infixl 1)
      (* :infix 2))
    '(+ (+ 1 (* 2 3)) 4)
    :test #'equal)

(is (polish '(1 + 2 * 3 + 4)
      (+ :infixr 1)
      (* :infix 2))
    '(+ 1 (+ (* 2 3) 4))
    :test #'equal)

(is (polish '(1 * 2 + 3 * 4)
      (+ :infix 1)
      (* :infix 2))
    '(+ (* 1 2) (* 3 4))
    :test #'equal)


;;; infix & infixl & same precedence (behavior is undefined by the specification)

(is (polish '(1 + 2 * 3 * 4 + 5)
      (+ :infix 1)
      (* :infixl 1))
    '(+ (* (* (+ 1 2) 3) 4) 5)
    :test #'equal)

(is (polish '(1 + 2 * 3 * 4 + 5)
      (+ :infixl 1)
      (* :infix 1))
    '(+ (* (+ 1 2) 3 4) 5)
    :test #'equal)


;;; infix & infixr & same precedence (behavior is undefined by the specification)

(is (polish '(1 + 2 * 3 * 4 + 5)
      (+ :infix 1)
      (* :infixr 1))
    '(+ (* (+ 1 2) (* 3 4)) 5)
    :test #'equal)

(is (polish '(1 + 2 * 3 * 4 + 5)
      (+ :infixr 1)
      (* :infix 1))
    '(+ (* (+ 1 2) 3 4) 5)
    :test #'equal)


;;; infixl & infixr & same precedence (behavior is undefined by the specification)

(is (polish '(1 + 2 * 3 * 4 + 5)
      (+ :infixl 1)
      (* :infixr 1))
    '(+ (* (+ 1 2) (* 3 4)) 5)
    :test #'equal)

(is (polish '(1 + 2 * 3 * 4 + 5)
      (+ :infixr 1)
      (* :infixl 1))
    '(+ (* (* (+ 1 2) 3) 4) 5)
    :test #'equal)


;;; infix & prefix

(is (polish '(pre1 1 * 2 + pre2 3 * 4 5 * 6)
      (+ :infix 1)
      (* :infix 3)
      (pre1 :prefix-1 2)
      (pre2 :prefix-2 2))
    '(+ (PRE1 (* 1 2)) (PRE2 (* 3 4) (* 5 6)))
    :test #'equal)


;;; infix & postfix

(is (polish '(1 * 2 post + 3 * 4 post*)
      (+ :infix 1)
      (* :infix 3)
      (post :postfix 2)
      (post* :postfix 4))
    '(+ (POST (* 1 2)) (* 3 (POST* 4)))
    :test #'equal)


;;; prefix & postfix

(is (polish '(pre1 pre2 pre3 1 * 2 + 3 post2 post1)
      (+ :infix 3)
      (* :infix 6)
      (pre1 :prefix-1 2)
      (pre2 :prefix-1 4)
      (pre3 :prefix-1 7)
      (post1 :postfix 1)
      (post2 :postfix 5))
    '(POST1 (PRE1 (+ (PRE2 (* (PRE3 1) 2)) (POST2 3))))
    :test #'equal)


;;; complex

(defun all-combi (lists)
  (if lists
      (loop
         for rest in (all-combi (cdr lists))
         append (loop
                   for x in (car lists)
                   collect (cons x rest)))
      '(())))

(defun make-forms (n ops)
  (if (zerop n)
      (list 1)
      (let ((forms (make-forms (1- n) ops)))
        (loop
           for op in ops
           append (all-combi (list* (list op)
                                    (loop
                                       repeat (second op)
                                       collect forms)))))))

(defun build-prefix (form)
  (if (numberp form)
      form
      (list* (first (first form))
             (loop
                for operand in (cdr form)
                collect (build-prefix operand)))))

(defun build-infix (form &optional (priority -1) pop)
  (if (numberp form)
      (list form)
      (destructuring-bind (op-name op-arity op-type op-priority) (first form)
        (declare (ignore op-arity))
        (let ((ret
               (case op-type
                 (:infix
                  (cdr (loop
                          for f in (cdr form)
                          collect op-name
                          append (build-infix f op-priority))))
                 (:infixl
                  `(,@(build-infix (second form) op-priority (first form))
                      ,op-name
                      ,@(build-infix (third form) op-priority)))
                 (:infixr
                  `(,@(build-infix (second form) op-priority)
                      ,op-name
                      ,@(build-infix (third form) op-priority (first form))))
                 ((:prefix-n :prefix-*)
                  `(,op-name
                    ,@(loop
                         for operand in (cdr form)
                         append (build-infix operand op-priority))))
                 (:postfix
                  `(,@(build-infix (first (cdr form)) op-priority (first form))
                      ,op-name)))))
          (if (or (eq pop (first form)) (< priority op-priority))
              ret
              (list ret))))))

(loop
   for form in (make-forms 2 '((+ 3 :infix 1) (- 2 :infixl 2) (* 2 :infixr 3)))
   do (is (polish (build-infix form)
                  (+ :infix 1)
                  (- :infixl 2)
                  (* :infixr 3))
          (build-prefix form)
          :test #'equal))

(loop
   for form in (make-forms 2 '((+ 2 :infixl 1) (- 3 :prefix-n 2) (* 1 :postfix 3)))
   do (is (polish (build-infix form)
                  (+ :infixl 1)
                  (- :prefix-3 2)
                  (* :postfix 3))
          (build-prefix form)
          :test #'equal))


;;; recursive option

(is (polish '(1 + (2 * 3) + '(hello) + 'world + !)
      (+ :infix 1)
      (* :infix 2)
      :recursive t)
    '(+ 1 (* 2 3) (HELLO) WORLD !)
    :test #'equal)

(is (polish '(1 + (2 * 3) + '(hello) + 'world + !)
      (+ :infix 1)
      (* :infix 2)
      :recursive nil)
    '(+ 1 (2 * 3) '(HELLO) 'WORLD !)
    :test #'equal)


;;; decorate option

(is (polish '(1 + 2)
      (+ :infix 1)
      (* :infix 2)
      :decorate print)
    '(PRINT (+ 1 2))
    :test #'equal)


;;; operator-prefix option

(is (polish '(1 + 2)
      (+ :infix 1)
      (* :infix 2)
      :operator-prefix great-)
    '(great-+ 1 2)
    :test #'equal)

(finalize)
