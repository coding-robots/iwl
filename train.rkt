#lang racket

(require "bayes.rkt")
(train-on-authors! (file->value "authors.rkt"))
(dump-data)