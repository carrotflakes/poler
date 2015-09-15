(in-package :cl-user)

(defpackage :poler
  (:use :cl)
  (:export :define-poler
           :polish))

(in-package :poler)


;;; structures
(defstruct operator
  symbol
  associativity
  precedence
  replaced-symbol)


;;; special variables
(defparameter *operators* nil)
(defparameter *recursive* t)
(defparameter *decorator* nil)


;;; auxiliary functions
(defun fix (precedence part-list)
  (if (cdr part-list)
      (let ((first (first part-list))
            (second (second part-list)))
        (cond
          ((<= precedence (operator-precedence (first second)))
           (rplacd (last second) (list first))
           (fix precedence (cdr part-list)))
          (t
           part-list)))
      part-list))

(defun lookup (symbol)
  (find symbol *operators* :key #'operator-symbol :test #'eq))

;; transform an infix-form into a tree
(defun build-tree (tokens &optional part-list)
  (if tokens
      (let* ((token (pop tokens))
             (operator (lookup token)))
        (build-tree
         tokens
         (cond
           ((null operator)
            (list* (cond
                     ((not (and *recursive*
                                (consp token)))
                      token)
                     ((eq (car token) 'quote)
                      (second token))
                     (t
                      (build-tree token)))
                   part-list))
           ((eq (operator-associativity operator) :pre)
            (list* (list operator) part-list))
           (t
            (setf part-list (fix (operator-precedence operator) part-list))
            (if (and (member (operator-associativity operator) '(:non :left :right))
                     (consp (first part-list))
                     (eq (first (first part-list)) operator))
                part-list
                (list* (list operator (first part-list)) (cdr part-list)))))))
      (first (fix -1 part-list))))

;; (left-associate '+ '(1 2 3)) => (+ (+ 1 2) 3)
(defun left-associate (symbol arguments &optional associated)
  (cond
    ((null arguments)
     associated)
    (associated
     (left-associate symbol (cdr arguments) (list symbol associated (first arguments))))
    (t
     (left-associate symbol (cddr arguments) (list symbol (first arguments) (second arguments))))))

;; (right-associate '+ '(1 2 3)) => (+ 1 (+ 2 3))
(defun right-associate (symbol arguments)
  (if (cddr arguments)
      (list symbol (first arguments) (right-associate symbol (cdr arguments)))
      (list symbol (first arguments) (second arguments))))

;; compose prefix form
(defun apply-operator (tree)
  (if (and (consp tree)
           (operator-p (first tree)))
      (case (operator-associativity (first tree))
        ((:left)
         (left-associate (operator-replaced-symbol (first tree))
                         (mapcar #'apply-operator (cdr tree))))
        ((:right)
         (right-associate (operator-replaced-symbol (first tree))
                          (mapcar #'apply-operator (cdr tree))))
        ((:non :pre :post)
         (list* (operator-replaced-symbol (first tree))
                (mapcar #'apply-operator (cdr tree)))))
      tree))

(defun parse-arguments (args)
  (let ((operators '()))
    (loop
       for operator-source in args
       while (listp operator-source)
       do (push (make-operator
                 :symbol (first operator-source)
                 :associativity (or (second operator-source) :left)
                 :precedence (or (third operator-source) most-positive-fixnum)
                 :replaced-symbol (or (fourth operator-source)
                                      (first operator-source)))
                operators)
       (pop args))
    (values operators
            (getf args :recursive t)
            (getf args :decorate nil))))

;;; main macros
(defmacro define-poler (name &body args)
  (multiple-value-bind (operators recursive decorator) (parse-arguments args)
    `(defmacro ,name (&body infix-form)
       (let ((*operators* ',operators)
             (*recursive* ',recursive))
         ,(if decorator
              `(list ,decorator (apply-operator (build-tree infix-form)))
              `(apply-operator (build-tree infix-form)))))))

(defmacro polish (infix-form &body args)
  (multiple-value-bind (operators recursive decorator) (parse-arguments args)
    `(let ((*operators* ',operators)
           (*recursive* ',recursive))
       ,(if decorator
            `(list ,decorator (apply-operator (build-tree ,infix-form)))
            `(apply-operator (build-tree ,infix-form))))))
