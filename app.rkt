#lang racket

(require web-server/servlet
         web-server/servlet-env
         srfi/43)

(require "bayes.rkt"
         "crc32.rkt")

(load-data!)

; hash with crc32 mappings of author names
(define authors-hash
  (for/hash ([author categories*])
    (values (string-crc32-hex author) author)))

;(require web-server/dispatch)
(define interface-version 'stateless)
;(no-web-browser)

(define-values (app-dispatch req)
  (dispatch-rules
   [("") index]
   [("b" (string-arg)) show-badge]
   [("archive" (integer-arg) (integer-arg)) show-shared]
   [else not-found]))

(define (start request)
  (app-dispatch request))

(define (index request)
  `(html (body (p "This is index"))))

(define (not-found request)
  `(html (body (p "Not found"))))

(define (show-badge request crc)
  `(html (body (p "Show Badge: " ,(hash-ref authors-hash crc)))))

(define (show-shared request crc)
  `(html (body (p "Show Shared"))))


(serve/servlet start
               #:servlet-path "" ; default URL
               #:port 8080
               #:servlet-regexp #rx"")
