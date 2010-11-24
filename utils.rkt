#lang racket

(require mzlib/defmacro)

(define-macro (aif test then else)
  `(let ([it ,test])
     (if it ,then ,else)))

(provide (all-defined-out))
