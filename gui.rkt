#lang racket/gui

(require "bayes.rkt")

(define frame (new frame% [label "I Write Like"]
                   [spacing 4]
                   [border 10]
                   [min-width 400]
                   [min-height 400]))

(define loading-msg (new message%
                     [parent frame]
                     [label "Loading data..."]))

(send frame show #t)
(load-data!)
(send frame delete-child loading-msg)

(define text-field (new text-field% [parent frame]
                          [label "Text"]
                          [style '(multiple vertical-label)]))

(new button% [parent frame]
     [label "Analyze"]
     (callback (lambda (button event)
                   (send result-field set-value (get-category (send text-field get-value)))
                   (or (member result-field (send frame get-children))
                       (send frame add-child result-field)))))

(define result-field (new text-field% [parent frame]
                            [label "Written like"]
                            [style '(single deleted vertical-label)]))
