# Poler

Poler is an infix notation macro generator, quite simple.

## Usage

``` lisp
;; Define a polish macro named 'arithmetic'.
(poler::define-poler arithmetic
  (+ :left 1)
  (- :left 1)
  (* :left 2)
  (/ :left 2))
  
;; Then, we can use 'arithmetic' macro.
(arithmetic 1 + 2 * 3)
; => 7
(macroexpand '(arithmetic 1 * 2 / (3 + 4 + 5)))
; => (/ (* 1 2) (+ (+ 3 4) 5))
```

## Operators
Polish macro can transform a form composed of **binary infix operators** and **unary operators** into a prefix form.

We can specify operators such as:
``` lisp
(+ :left 1)
```
means the operator symbol is `+`, associativity is left-associative and precedence is `1`.

If you want to replace operator symbols after polish, specify the operator such as:
``` lisp
(poler::define-poler foo
  (+ :left 1 add))

(macroexpand '(foo 1 + 2))
; => (add 1 2)
```

### Operator associativity
Poler comprehends following associativities.
- `:left` : left-associative binary infix operator.
- `:right` : right-associative binary infix operator.
- `:non` : non-associative binary infix operator.
- `:pre` : unary prefix operator.
- `:post` : unary postfix operator.

### Operator precedence
The high precedence of operator is, the more prior association of the operators.
An operator precedence must be a fixnum of 0 or more.

## APIs

### Macro: `define-poler`
Defines a polish macro as the usage.

This macro takes a keyword parameter `:recursive`.
The default is `t`.
``` lisp
(poler::define-poler foo
  (+ :left 1)
  :recursive nil)

(macroexpand '(foo 1 + (2 + 3)))
; => (+ 1 (2 + 3))
```

This macro takes a keyword parameter `:decorate`.
``` lisp
(poler::define-poler foo
  (+ :left 1)
  :decorate 'quote)

(macroexpand '(foo 1 + 2))
; => '(+ 1 2)
; i.e. (quote (+ 1 2))
```

### Macro: `polish`
`polish` macros look like `define-poler`, but this macro does not define a polish macro, does polish given infixed-form only once.
``` lisp
(poler::polish '(1 + 2 * 3)
  (+ :left 1)
  (- :left 1)
  (* :left 2)
  (/ :left 2))
; => (+ 1 (* 2 3))
```

## Example
``` lisp
(defun fact (n)
  (if (< 0 n)
      (* n (fact (1- n)))
      1))

(poler::define-poler arithmetic
  (+   :non   1)
  (-   :non   1)
  (*   :non   2)
  (/   :non   2)
  (%   :left  2 mod)
  (^   :right 3 expt)
  (!   :post  4 fact))

(arithmetic 3 ! ^ 2 / 3 - 2)
; => 10
```

## Installation

1. Git clone this repository into `~/quicklisp/local-projects/`
2. `(ql:quickload :poler)`

## Author

* carrotflakes (carrotflakes@gmail.com)

## Copyright

Copyright (c) 2015 carrotflakes (carrotflakes@gmail.com)

## License

Licensed under the LLGPL License.
