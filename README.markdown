# Poler

Poler can generate macros which convert infix notation to prefix notation.
Poler aims to easily build DSL on Common Lisp.
We call the conversion **polish** in Poler.


## Usage

``` lisp
;; Define a polish macro named 'arithmetic'.
(poler:define-poler arithmetic
  (+ :infixl 1)
  (- :infixl 1)
  (* :infixl 2)
  (/ :infixl 2))

;; Then, we can use 'arithmetic' macro.
(arithmetic 1 + 2 * 3)
; => 7
(macroexpand '(arithmetic 1 * 2 / (3 + 4 + 5)))
; => (/ (* 1 2) (+ (+ 3 4) 5))
```


## Operator definition
A polish needs operator definitions. An operator definition is represented as following:

	(name type precedence [replace-name | format])

`name` is a symbol as the operator name.  
`type` is a keyword represents the operator type. See [Operator types](#operator-types).  
`precedence` is a fixnum over 0 represents the operator precedence. The high precedence of operator is, the more prior association of the operators.  
`replace-name` is a symbol. See [Name replacement](#name-replacement).  
`format` is a form. See [Format of form](#format-of-form).

### Operator types
Poler supports following operator types.

#### `:infixl`
Left-associative infix operator.

	(1 + 2 + 3) => (+ (+ 1 2) 3)

#### `:infixr`
Right-associative infix operator.

	(1 + 2 + 3) => (+ 1 (+ 2 3))

#### `:infix`
Non-associative infix operator.

	(1 + 2 + 3) => (+ 1 2 3)

#### `:prefix-n` (`n` is a natural number)
`n` operands prefix operator.

	; In the case of :prefix-3.
	(+ 1 2 3) => (+ 1 2 3)
	(+ 1 2)   => Illegal.

#### `:prefix-*`
Variable arity prefix operator.

	(+ 1 2 3) => (+ 1 2 3)
	(+ 1 2)   => (+ 1 2)

#### `:postfix`
Unary postfix operator.

	(1 +) => (+ 1)

### Name replacement
If you gave `replace-name` to an operator, the operator name is replaced by `replace-name` while polishing.

For example:

``` lisp
(poler:define-poler arithmetic
  (+ :infixl 1 add)
  (* :infixl 2 mul))

(macroexpand '(arithmetic 1 + 2 * 3)) ; => (add 1 (mul 2 3))
```

Also, if you gave `operator-prefix` to the polish macro, the operator name is appended `operator-prefix`.

For example:

``` lisp
(poler:define-poler arithmetic
  (+ :infixl 1)
  (* :infixl 2)
	:operator-prefix foo-)

(macroexpand '(arithmetic 1 + 2 * 3)) ; => (foo-+ 1 (foo-* 2 3))
```

### Format of form
Usually a polished form shapes such as `(operator operand1 operand2 ...)`, but we can transform the form to any shape also.
The shape is specified by `format` in the operator definition.
In form `format`, symbol `$1`, `$2`, ... will be replaced with `operand1`, `operand2`, ... , symbol `$whole` will be replaced with `(operand1 operand2 ...)`.

For example:

``` lisp
(poler:define-poler combine-string
  (+ :infix    1 (concatenate 'string . $whole))
  (* :infixl   2 (apply #'concatenate 'string (make-list $2 :initial-element $1))))

(macroexpand '(combine-string "Ha" + " ha" * 3))
; => (CONCATENATE 'STRING "Ha" (APPLY #'CONCATENATE 'STRING (MAKE-LIST 3 :INITIAL-ELEMENT " ha")))
```


## APIs

### Macro: `define-poler`

    (define-poler macro-name [operator-definition ...] &key (recursive t) decorate (operator-prefix nil))

Defines a polish macro.

The keyword parameter `:recursive` enables recursive application to nested form.
The default is `t`.
Quoted forms are exempted from recursive application.

``` lisp
(poler:define-poler foo
  (+ :infixl 1)
  :recursive nil)

(macroexpand '(foo 1 + (2 + 3) + '(+ 4 5)))
; => (+ (+ 1 (2 + 3)) '(+ 4 5))


(poler:define-poler bar
  (+ :infixl 1)
  :recursive t)

(macroexpand '(bar 1 + (2 + 3) + '(+ 4 5)))
; => (+ (+ 1 (+ 2 3)) (+ 4 5))
```

The keyword parameter `:decorate` takes a symbol, if the symbol is non-nil, a result of the polish is decorated with the symbol.

``` lisp
(poler:define-poler foo
  (+ :infixl 1)
  :decorate quote)

(macroexpand '(foo 1 + 2))
; => '(+ 1 2)
; i.e. (quote (+ 1 2))
```

The keyword parameter `:operator-prefix` takes a symbol. See [Name replacement](#name-replacement).

``` lisp
(poler:define-poler foo
  (+ :infixl 1)
  (* :infixl 2 *)
  :operator-prefix foo-)

(macroexpand '(foo 1 + 2 * 3))
; => '(foo-+ 1 (* 2 3))
```


### Macro: `polish`

    (polish infix-form [operator-definition ...] &key (recursive t) decorate (operator-prefix nil))

`polish` macro look like `define-poler`, but this macro does not define a polish macro, does polish given infix form once.

``` lisp
(poler:polish '(1 + 2 * 3)
  (+ :infixl 1)
  (- :infixl 1)
  (* :infixl 2)
  (/ :infixl 2))
; => (+ 1 (* 2 3))
```

### Macro: `define-operator`

    (define-operator macro-name operator-name type precedence [replace-name | format])

`define-operator` macro adds an operator into the polish macro which is named `macro-name`.
If `type` is `nil`, the macro removes operator `operator-name`.

``` lisp
(poler:define-poler arithmetic
  (+ :infix  1)
  (* :infixl 2)
  (% :infixl 3))

(poler:define-operator arithmetic - :infixl 1) ; add operator -
(poler:define-operator arithmetic + :infixl 1) ; change operator +
(poler:define-operator arithmetic % nil)       ; remove operator %
```


## Example

``` lisp
(defun fact (n)
  (if (< 0 n)
      (* n (fact (1- n)))
      1))

(poler::define-poler arithmetic
  (+   :infix   1)
  (-   :infix   1)
  (*   :infix   2)
  (/   :infix   2)
  (%   :infixl  2 mod)
  (^   :infixr  3 expt)
  (!   :postfix 4 fact))

(arithmetic 3 ! ^ 2 / 3 - 2)
; => 10
```


## Installation

1. Git clone the repository into `~/quicklisp/local-projects/`
2. `(ql:quickload :poler)`

## Author

* carrotflakes (carrotflakes@gmail.com)

## Copyright

Copyright (c) 2015 carrotflakes (carrotflakes@gmail.com)

## License

Licensed under the LLGPL License.
