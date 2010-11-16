#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/templates
         srfi/43
         net/uri-codec
         racket/runtime-path)

(require "bayes.rkt"
         "crc32.rkt")

(define app-version* 8)
(define app-date* "November 2010")

(load-data!)

; Templates
(define (base-template title menu body)
  (include-template "templates/base.html"))

; hash with crc32 mappings of author names
(define authors-hash
  (for/hash ([author categories*])
    (values (string-crc32-hex author) author)))

(define-values (app-dispatch req)
  (dispatch-rules
   [("") index]
   [("b" (string-arg)) show-badge]
   [("s" (string-arg)) show-shared]
   [("w" (string-arg)) show-writer]
   [("api")            api]
   [("newsletter")     show-newsletter]
   [("newsletter" (string-arg)) (lambda (r a) (redirect-to "/newsletter"))]
   [else not-found]))

(define (limited-text s)
  (safe-substring s 0 3000))

(define (index req)
  (define (index-template short?)
    (list TEXT/HTML-MIME-TYPE 
          (base-template "" "analyzer"
                         (include-template "templates/index.html"))))
  (let ([text (dict-ref (request-bindings req) 'text #f)])
    (if text
        (if (> 30 (string-length text))
            (index-template #t)
            (redirect-to 
             (string-append "/b/" (string-crc32-hex
                                    (get-category (limited-text text))))))
        (index-template #f))))

(define (api req)
  (let* ([bindings (request-bindings req)]
         [text (dict-ref bindings 'text #f)]
         [wrapper (dict-ref bindings 'function "")]
         [client-id (dict-ref bindings 'client_id #f)]  ; unused, but required
         [permalink (dict-ref bindings 'permalink #f)]) ; -"-
    (if (and text client-id permalink)
        (let* ([writer (get-category (limited-text text))]
               [crc    (string-crc32-hex writer)])
          (list #"text/plain; charset=utf-8"
                (string->bytes/utf-8
                 (format "~a{\"share_link\": \"http://iwl.me/s/~a\", 
                             \"writer_link\": \"http://iwl.me/w/~a\",
                             \"writer\": \"~a\", 
                             \"id\": \"~a\", 
                             \"badge_link\": \"http://iwl.me/b/~a\"}"
                         wrapper crc crc writer crc crc))))
        (list #"text/plain; charset=utf-8"
              #"{\"error\": \"not enough arguments\"}"))))

(define (not-found req)
  (make-response/full 404 #"Not Found" (current-seconds)
                      TEXT/HTML-MIME-TYPE empty (list #"not found")))

(define (show-badge req crc)
  (let ([writer (hash-ref authors-hash crc)])
    (list TEXT/HTML-MIME-TYPE 
          (base-template writer ""
                         (include-template "templates/show-badge.html")))))

(define (show-shared req crc)
  (let ([writer (hash-ref authors-hash crc)])
    (list TEXT/HTML-MIME-TYPE 
          (base-template writer ""
                         (include-template "templates/show-shared.html")))))

(define (show-writer req crc)
  (redirect-to 
   (format (string-append
             "http://www.amazon.com/gp/search?ie=UTF8&keywords=~a"
             "&tag=blogjetblog-20&index=books&linkCode=ur2"
             "&camp=1789&creative=9325") (hash-ref authors-hash crc))))

(define (show-newsletter req)
  (list TEXT/HTML-MIME-TYPE
        (base-template "Newsletter" "newsletter"
                       (include-template "templates/show-newsletter.html"))))

(define (start req)
  (app-dispatch req))

(define-runtime-path srvpath ".")

(serve/servlet start
               #:servlet-path ""
               #:port 8080
               #:servlet-regexp #rx"^((?!/static/).)*$"
               #:extra-files-paths (list srvpath)
               #:launch-browser? #f
               #:stateless? #t)
