#lang racket

(require "bayes.rkt")
(require "authors.rkt")
(train-on-authors! authors)
(dump-data)