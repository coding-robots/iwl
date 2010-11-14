#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/templates
         srfi/43
         net/uri-codec)

(require "bayes.rkt"
         "crc32.rkt")

(load-data!)

; Templates
(define (base-template title body)  
  (include-template "templates/base.html"))

; hash with crc32 mappings of author names
(define authors-hash
  (for/hash ([author categories*])
    (values (string-crc32-hex author) author)))

(define interface-version 'stateless)

(define-values (app-dispatch req)
  (dispatch-rules
   [("") index]
   [("b" (string-arg)) show-badge]
   [("s" (string-arg)) show-shared]
   [("w" (string-arg)) show-writer]
   [("newsletter") show-newsletter]
   [("newsletter" (string-arg)) (lambda (r a) (redirect-to "/newsletter"))]
   [else not-found]))

(define (index request)
  (define (index-template short?)
    (list TEXT/HTML-MIME-TYPE 
          (base-template "" (include-template "templates/index.html"))))
  (let ([text-binding (bindings-assq #"text" (request-bindings/raw request))])
    (if text-binding
        (let ([text (bytes->string/utf-8 (binding:form-value text-binding))])
          (if (> 30 (string-length text))
              (index-template #t)
              (redirect-to 
               (string-append "/b/" (string-crc32-hex (get-category text))))))
        (index-template #f))))

(define (not-found request)
  (make-response/full
   404 #"Not Found"
   (current-seconds)
   TEXT/HTML-MIME-TYPE
   empty
   (list #"not found")))

(define (show-badge request crc)
  (let ([writer (hash-ref authors-hash crc)])
    (list TEXT/HTML-MIME-TYPE 
          (base-template writer 
                         (include-template "templates/show-badge.html")))))

(define (show-shared request crc)
  (let ([writer (hash-ref authors-hash crc)])
    (list TEXT/HTML-MIME-TYPE 
          (base-template writer 
                         (include-template "templates/show-shared.html")))))

(define (show-writer request crc)
  (redirect-to (format "http://www.amazon.com/gp/search?ie=UTF8&keywords=~a"
                       "&tag=blogjetblog-20&index=books&linkCode=ur2&camp=1789"
                       "&creative=9325" 

(define (show-newsletter request)
  (list TEXT/HTML-MIME-TYPE
        (base-template "Newsletter"
                       (include-template "templates/show-newsletter.html"))))


(define (start request)
  (app-dispatch request))

(serve/servlet start
               #:servlet-path "" ; default URL
               #:port 8080
               #:servlet-regexp #rx"")
