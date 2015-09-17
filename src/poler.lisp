(in-package :cl-user)

(defpackage :poler
  (:use :cl)
  (:export :define-poler
           :polish))

(in-package :poler)


;;; structures
(defstruct operator
  symbol
  fix
  associativity
  arity
  precedence
  replaced-symbol)

(defmethod print-object ((object operator) stream)
  (format stream "#< ~a >" (operator-symbol object)))


;;; special variables
(defparameter *operators* nil)
(defparameter *recursive* t)
(defparameter *decorator* nil)


;;; auxiliary functions
(defun part-close (part)
  (cons 'closed part))

(defun part-closed-p (part)
  (eq (first part) 'closed))

(defun fix (precedence part-list)
  (if (cdr part-list)
      (let ((part1 (first part-list))
            (part2 (second part-list)))
        (when (part-closed-p part2) ; part2 is closed
          (rplacd part-list (fix precedence (cdr part-list)))
          (setf part2 (second part-list))
          (when (part-closed-p part2)
            (return-from fix part-list)))
        (cond
          ((and (<= precedence (operator-precedence (first part2))) ; part2 is an operator can takes part1
                (not (part-closed-p part2))
                (part-closed-p part1))
           (rplacd (last part2) (list (cdr part1)))
           ;; close
           (unless (and (eq (operator-fix (first part2)) :prefix)
                        (< (- (length part2) 1)
                           (or (operator-arity (first part2)) most-positive-fixnum)))
             (setf (second part-list) (part-close (second part-list))))
           (fix precedence (cdr part-list)))
          (t ; part2 is an operator cannot takes part1
           part-list)))
      part-list))

(defun lookup (symbol)
  (and (symbolp symbol)
       (find symbol *operators* :key #'operator-symbol :test #'eq)))

(defun part-close-prefix-* (part)
  (if (and (operator-p (first part))
           (eq (operator-fix (first part)) :prefix)
           (null (operator-arity (first part))))
      (part-close part)
      part))

;; transform an infix-form into a tree
(defun build-tree (tokens &optional part-list)
  (if
   tokens
   (let* ((token (car tokens))
          (operator (lookup token)))
     (build-tree
      (cdr tokens)
      (cond
        ((null operator) ; token is not an operator
         (cons (cond
                 ((or (not *recursive*) (not (consp token))) ; operand
                  (part-close token))
                 ((eq (car token) 'quote) ; operand by quote
                  (part-close (second token)))
                 (t ; paren
                  (build-tree token)))
               part-list))
        ((eq (operator-fix operator) :prefix)  ; prefix
         (cons (list operator) part-list))
        (t  ; infix or postfix
         (setf part-list (fix (operator-precedence operator) part-list))
         (let ((part1 (first part-list)))
           (cond
             ((and (eq (operator-fix operator) :infix) ; infix union
                   (part-closed-p part1)
                   (consp (cdr part1))
                   (eq (second part1) operator)
                   (not (eq (operator-arity operator) (- (length part1) 2))))
              (setf (first part-list) (cdr part1)) ; unclose
              part-list)
             ((eq (operator-fix operator) :postfix) ; postfix
              (cons (part-close (list operator (cdr part1))) (cdr part-list)))
             ((eq (operator-fix operator) :infix) ; infix
              (when (null part1)
                (error "No operand at left of an infix operator."))
              (cons (list operator (cdr part1)) (cdr part-list)))))))))
   (let ((part-list (fix -1 part-list)))
     (when (/= 1 (length part-list))
       (error "Root is must only one."))
     (unless (part-closed-p (first part-list))
       (error "Invalcid infix form"))
     (part-close-prefix-* (first part-list)))))

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
        (otherwise
         (list* (operator-replaced-symbol (first tree))
                (mapcar #'apply-operator (cdr tree)))))
      tree))

(defun new-operator (source)
  (multiple-value-bind (fix associativity arity)
      (case (second source)
        (:infix-l    (values :infix :left nil))
        (:infix-r    (values :infix :right nil))
        (:infix-2    (values :infix nil 2))
        (:infix-*    (values :infix nil nil))
        (:prefix-1   (values :prefix nil 1))
        (:prefix-*   (values :prefix nil nil))
        (:postfix-1  (values :postfix nil 1))
        (otherwise   (let ((symbol-name (symbol-name (second source))))
                       (if (string= symbol-name "PREFIX-" :end1 7)
                           (values :prefix nil (parse-integer symbol-name :start 7))
                           (error "Bad operator type.")))))
    (make-operator :symbol (first source)
                   :fix fix
                   :associativity associativity
                   :arity arity
                   :precedence (or (third source) most-positive-fixnum)
                   :replaced-symbol (or (fourth source)
                                        (first source)))))

(defun parse-arguments (args)
  (let ((operators '()))
    (loop
       for operator-source in args
       while (listp operator-source)
       do (push (new-operator operator-source) operators)
       (pop args))
    (values operators
            (getf args :recursive t)
            (getf args :decorate nil))))


;;; main macros
(defmacro polish (infix-form &body args)
  (multiple-value-bind (operators recursive decorator) (parse-arguments args)
    `(let ((*operators* ',operators)
           (*recursive* ',recursive))
       ,(if decorator
            `(list ,decorator (apply-operator (cdr (build-tree ,infix-form))))
            `(apply-operator (cdr (build-tree ,infix-form)))))))

(defmacro define-poler (name &body args)
  `(defmacro ,name (&body infix-form)
     (polish infix-form ,@args)))
