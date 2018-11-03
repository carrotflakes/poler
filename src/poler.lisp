(in-package :cl-user)

(defpackage :poler
  (:use :cl)
  (:export :define-poler
           :polish
           :define-operator
           :*format-parameter-prefix*
           :*format-parameter-whole*))

(in-package :poler)


;;; structures
(defstruct operator
  name
  fix
  arity
  precedence
  replace-name
  format)

(defmethod print-object ((object operator) stream)
  (format stream "#<~a>" (operator-name object)))

(defmethod make-load-form ((self operator) &optional environment)
  (declare (ignore environment))
  `(make-operator
    :name ',(operator-name self)
    :fix ',(operator-fix self)
    :arity ',(operator-arity self)
    :precedence ',(operator-precedence self)
    :replace-name ',(operator-replace-name self)
    :format ',(operator-format self)))


;;; special variables
(defparameter *operators* nil)
(defparameter *recursive* t)
(defparameter *decorator* nil)
(defparameter *format-parameter-prefix* "$")
(defparameter *format-parameter-whole*  "$WHOLE")


;;; generic functions
(defgeneric %define-operator
    (poler-name operator-source))


;;; auxiliary functions

(defun lookup-operator (name)
  (and (symbolp name)
       (gethash name *operators*)))

(defun part-closed-p (part)
  (= (car part) 0))

(defun part-associativity (part)
  (unless (zerop (first part))
    (let ((args-num (length (cddr part))))
      (case (operator-fix (second part))
        ((:infixl)  (if (= args-num 0) :left :right))
        ((:infixr)  (if (= args-num 0) :left :right))
        ((:infix)   (if (= args-num 0) :left :right))
        ((:prefix)  :right)
        ((:postfix) :left)))))

(defun part-precedence (part)
  (if (zerop (first part))
      most-positive-fixnum
      (operator-precedence (second part))))

(defun part-union (part argument)
  (list* (1- (first part)) (second part) (nconc (cddr part) (list argument))))

(defun merge-part (part1 part2)
  (cond
    ((and (consp (cdr part1))           ; merge same infix operators each other
          (consp (cdr part2))
          (= (first part1) 1)
          (= (first part2) 0)
          (eq (second part1) (second part2))
          (operator-p (second part1))
          (member (operator-fix (second part1)) '(:infixl :infixr :infix)))
     (list* 0 (second part1) (nconc (cddr part1) (cddr part2))))

    ((and (consp (cdr part1))          ; merge different infix operators in left-associative
          (consp (cdr part2))
          (= (first part1) 1)
          (= (first part2) 0)
          (operator-p (second part1))
          (operator-p (second part2))
          (= (operator-precedence (second part1)) (operator-precedence (second part2)))
          (member (operator-fix (second part1)) '(:infixl :infixr :infix))
          (eq (operator-fix (second part2)) :infixl))
     (list 0
           (second part2)
           (list (second part1) (third part1) (third part2))
           (fourth part2)))

    ((and (not (part-closed-p part1))   ; merge part2 into part1
          (part-closed-p part2)
          (eq (part-associativity part1) :right))
     (part-union part1 (cdr part2)))

    ((and (not (part-closed-p part2))   ; merge infix operators
          (part-closed-p part1)
          (eq (and (consp (cdr part1)) (second part1))
              (and (consp (cdr part2)) (second part2)))
          (member (operator-fix (second part2)) '(:infixl :infixr :infix)))
     (incf (first part1))
     part1)

    ((and (not (part-closed-p part2))   ; merge part1 into part2
          (part-closed-p part1)
          (eq (part-associativity part2) :left))
     (part-union part2 (cdr part1)))))

(defun build-tree (part-list precedence)
  (loop
     for part1 = (first part-list)
     for part2 = (second part-list)
     for part3 = (third part-list)

     when (null part2)
     return part-list

     do (cond
          ((not (part-closed-p part1))
           (let ((part-precedence (part-precedence part1)))
             (cond
               ((and (< part-precedence precedence)
                     (= (operator-arity (second part1)) most-positive-fixnum)) ; for :prefix-* operator
                (return (cons (cons 0 (cdr part1)) (cdr part-list))))
               ((< precedence part-precedence)
                (setf part-list
                      (build-tree part-list part-precedence)))
               ((not (part-closed-p part2))
                (setf part-list
                      (cons part1
                            (build-tree (cdr part-list) part-precedence))))
               ((or (null part3)
                    (part-closed-p part3)
                    (<= (part-precedence part3) precedence))
                (setf part-list
                      (cons (merge-part part1 part2) (nthcdr 2 part-list))))
               (t
                (setf part-list
                      (cons part1
                            (build-tree (cdr part-list)
                                        (part-precedence part3))))))))

          ((not (part-closed-p part2))
           (let ((part-precedence (part-precedence part2)))
             (cond
               ((< part-precedence precedence)
                (return part-list))
               ((< precedence part-precedence)
                (setf part-list
                      (build-tree part-list part-precedence)))
               (t
                (setf part-list
                      (let ((merged-part (merge-part part1 part2)))
                        (if merged-part
                            (cons merged-part (nthcdr 2 part-list))
                            (cons part1
                                  (build-tree (cdr part-list)
                                              (part-precedence part2))))))))))

          (t
           (return part-list)))))


(defun apply-format (operator arguments)
  (if (operator-format operator)
      (sublis (cons (cons *format-parameter-whole* arguments)
                    (loop
                       for i from 1
                       for argument in arguments
                       collect (cons (concatenate 'string
                                                  *format-parameter-prefix*
                                                  (write-to-string i))
                                     argument)))
              (operator-format operator)
              :test (lambda (x y) (and (symbolp x) (string= x y))))
      (list* (operator-replace-name operator) arguments)))

;; (left-associate '+ '(1 2 3)) => (+ (+ 1 2) 3)
(defun left-associate (operator arguments &optional associated)
  (cond
    ((null arguments)
     associated)
    (associated
     (left-associate operator
                     (cdr arguments)
                     (apply-format operator (list associated (first arguments)))))
    (t
     (left-associate operator
                     (cddr arguments)
                     (apply-format operator (list (first arguments) (second arguments)))))))

;; (right-associate '+ '(1 2 3)) => (+ 1 (+ 2 3))
(defun right-associate (operator arguments)
  (if (cddr arguments)
      (apply-format operator
                    (list (first arguments)
                          (right-associate operator (cdr arguments))))
      (apply-format operator
                    (list (first arguments)
                          (second arguments)))))

;; compose prefix form
(defun apply-operator (tree)
  (if (and (consp tree)
           (operator-p (first tree)))
      (case (operator-fix (first tree))
        ((:infixl)
         (left-associate  (first tree)
                          (mapcar #'apply-operator (cdr tree))))
        ((:infixr)
         (right-associate (first tree)
                          (mapcar #'apply-operator (cdr tree))))
        (otherwise
         (apply-format    (first tree)
                          (mapcar #'apply-operator (cdr tree)))))
      tree))


(defun build (infix-form)
  (let ((parts (build-tree
                (mapcar (lambda (token)
                          (let ((operator (lookup-operator token)))
                            (cond
                              (operator ; operator
                               (list (operator-arity operator) operator))
                              ((or (atom token) (not *recursive*)) ; operand
                               (cons 0 token))
                              ((eq (car token) 'quote) ; operand by quote
                               (cons 0 (second token)))
                              (t        ; paren
                               (cons 0 (apply-operator (build token)))))))
                        infix-form)
                -1)))
    (if (= (length parts) 1)
        (cdr (first parts))
        (error "Invalid form."))))


(defun %%define-operator (*operators* operator-source operator-prefix)
  (if (second operator-source)
      (setf (gethash (first operator-source) *operators*)
            (new-operator (first operator-source)
                          (second operator-source)
                          (third operator-source)
                          (fourth operator-source)
                          operator-prefix))
      (remhash (first operator-source) *operators*))
  t)

(defun new-operator (name type precedence replace-name-or-format operator-prefix)
  (multiple-value-bind (fix arity)
      (case type
        (:infixl     (values :infixl 2))
        (:infixr     (values :infixr 2))
        (:infix      (values :infix  2))
        (:prefix     (values :prefix 1))
        (:prefix-*   (values :prefix most-positive-fixnum))
        (:postfix    (values :postfix 1))
        (otherwise   (let ((symbol-name (symbol-name type)))
                       (if (and (< 7 (length symbol-name))
                                (string= symbol-name "PREFIX-" :end1 7))
                           (values :prefix (parse-integer symbol-name :start 7))
                           (error "Invalid operator type ~s." type)))))

    (make-operator
     :name name
     :fix fix
     :arity arity
     :precedence (or precedence most-positive-fixnum)
     :replace-name (cond
                     ((and replace-name-or-format (symbolp replace-name-or-format))
                      replace-name-or-format)
                     (operator-prefix
                      (intern (format nil "~a~a" operator-prefix name)
                              (symbol-package operator-prefix)))
                     (t
                      name))
     :format (when (consp replace-name-or-format) replace-name-or-format))))

(defun parse-arguments (args)
  (let ((operator-sources (loop
                             for operator-source in args
                             while (listp operator-source)
                             collect operator-source
                             do (pop args)))
        (recursive (getf args :recursive t))
        (decorate  (getf args :decorate nil))
        (operator-prefix (getf args :operator-prefix nil))
        (operators (make-hash-table)))

    (mapc (lambda (os)
            (setf (gethash (first os) operators)
                  (new-operator (first os)
                                (second os)
                                (third os)
                                (fourth os)
                                operator-prefix)))
          operator-sources)

    (values operators
            recursive
            decorate
            operator-prefix)))


;;; main macros
(defmacro polish (infix-form &body args)
  (multiple-value-bind (operators recursive decorator operator-prefix)
      (parse-arguments args)
    (declare (ignore operator-prefix))

    `(let ((*operators* ',operators)
           (*recursive* ',recursive)
           (*format-parameter-prefix* ,*format-parameter-prefix*)
           (*format-parameter-whole*  ,*format-parameter-whole*))

       ,(if decorator
            `(list ',decorator (apply-operator (build ,infix-form)))
            `(apply-operator (build ,infix-form))))))

(defmacro define-poler (name &body args)
  (multiple-value-bind (operators recursive decorator operator-prefix)
      (parse-arguments args)

    `(progn
       (defmethod %define-operator
           ((poler-name (eql ',name)) (operator-source list))
         (%%define-operator ,operators operator-source ',operator-prefix))

       (defmacro ,name (&body infix-form)
         (let ((*operators* ',operators)
               (*recursive* ',recursive)
               (*format-parameter-prefix* ,*format-parameter-prefix*)
               (*format-parameter-whole*  ,*format-parameter-whole*))

           ,(if decorator
                `(list ',decorator (apply-operator (build infix-form)))
                `(apply-operator (build infix-form))))))))


(defmacro define-operator
    (poler-name operator-name type
     &optional (precedence most-positive-fixnum) replace-name-or-format)
  `(%define-operator ',poler-name
                     ',(list operator-name
                             type
                             precedence
                             replace-name-or-format)))
