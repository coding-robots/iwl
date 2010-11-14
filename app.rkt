#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/templates
         srfi/43
         racket/serialize ; temp
         net/uri-codec)

(require ;"bayes.rkt" ; temp
 "crc32.rkt")

;(load-data!)
(define categories* (deserialize (file->value "data/categories.dat"))) ; temp

; Templates
(define (base-template title body)  
  (include-template "templates/base.html"))

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
   [("s" (string-arg)) show-shared]
   [("newsletter") show-newsletter]
   [("newsletter" (string-arg)) (lambda (r a) (redirect-to "/newsletter"))]
   [else not-found]))

(define (start request)
  (app-dispatch request))

(define (index request)
  `(html (body (p "This is index"))))

(define (not-found request)
 (make-response/full
   404 #"Not Found"
   (current-seconds) 
   TEXT/HTML-MIME-TYPE
   empty
   (list #"not found")))

(define (show-badge request crc)
  (let* ([writer (hash-ref authors-hash crc)]
         [badge  (include-template "templates/badge.html")]
         [body   (include-template "templates/show-badge.html")])
    (list TEXT/HTML-MIME-TYPE (base-template writer body))))

(define (show-shared request crc)
  (let* ([writer (hash-ref authors-hash crc)]
         [badge  (include-template "templates/badge.html")]
         [body   (include-template "templates/show-shared.html")])
    (list TEXT/HTML-MIME-TYPE (base-template writer body))))

(define (show-newsletter request)
  (list TEXT/HTML-MIME-TYPE
        (base-template "Newsletter" 
                       (include-template "templates/show-newsletter.html"))))

(serve/servlet start
               #:servlet-path "" ; default URL
               #:port 8080
               #:servlet-regexp #rx"")
