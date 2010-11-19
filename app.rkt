#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/templates
         srfi/43
         net/uri-codec
         racket/runtime-path)

(require "bayes.rkt"
         "crc32.rkt")

(define *app-version* 8)
(define *app-date* "November 2010")

(load-data!)

; Hash with crc32 mappings to author names
(define *authors-hash*
  (for/hash ([author *categories*])
    (values (string->crc32/hex author) author)))

(define-values (app-dispatch req)
  (dispatch-rules
   [("") show-index]
   [("b" (string-arg)) show-badge]
   [("s" (string-arg)) show-shared]
   [("w" (string-arg)) show-writer]
   [("api")            api]
   [("newsletter")     show-newsletter]
   [("newsletter" (string-arg)) (lambda (r a) (redirect-to "/newsletter"))]
   [else not-found]))

(define (base-template title menu body)
  (include-template "templates/base.html"))

(define (get-author s)
  (and (> (string-length s) 30)
       (get-category (safe-substring s 0 3000))))

(define (index-template short?)
  (list TEXT/HTML-MIME-TYPE
        (base-template "" "analyzer"
                       (include-template "templates/index.html"))))

(define (badge-url author)
  (string-append "/b/" (string->crc32/hex author)))

(define (show-index req)
  (let ([text (dict-ref (request-bindings req) 'text #f)])
    (if text
        (cond
          [(get-author text) => (lambda (x) (redirect-to (badge-url x)))]
          [else (index-template #t)])
        (index-template #f))))

(define (json-out s)
  (list #"application/json; charset=utf-8" (string->bytes/utf-8 s)))

(define (json-error desc)
  (json-out (format "{\"error\": \"~a\"}" desc)))

(define (json-result wrapper author)
  (let ([crc (string->crc32/hex author)])
    (json-out (format "~a{\"share_link\": \"http://iwl.me/s/~a\",
                       \"writer_link\": \"http://iwl.me/w/~a\",
                       \"writer\": \"~a\",
                       \"id\": \"~a\",
                       \"badge_link\": \"http://iwl.me/b/~a\"}"
                      wrapper crc crc author crc crc))))

(define (api req)
  (let* ([bindings (request-bindings req)]
         [text (dict-ref bindings 'text #f)]
         [wrapper (dict-ref bindings 'function "")]
         [client-id (dict-ref bindings 'client_id #f)]  ; unused, but required
         [permalink (dict-ref bindings 'permalink #f)]) ; -"-
    (if (and text client-id permalink)
        (cond
          ([get-author text] => (lambda (x) (json-result wrapper x)))
          (else (json-error "text is too short or doesn't have words")))
        (json-error "not enough arguments"))))

(define (not-found req)
  (make-response/full 404 #"Not Found" (current-seconds)
                      TEXT/HTML-MIME-TYPE null (list #"not found")))

(define (crc->author crc)
  (hash-ref *authors-hash* crc #f))

(define (badge-template req crc shared?)
  (let ([writer (crc->author crc)])
    (if writer
        (list TEXT/HTML-MIME-TYPE
              (base-template
               writer ""
               (if shared?
                   (include-template "templates/show-shared.html")
                   (include-template "templates/show-badge.html"))))
        (not-found req))))

(define (show-badge req crc)
  (badge-template req crc #f))

(define (show-shared req crc)
  (badge-template req crc #t))

(define (show-writer req crc)
  (cond
    [(crc->author crc)
     => (lambda (w)
          (redirect-to
           (format (string-append
                    "http://www.amazon.com/gp/search?ie=UTF8&keywords=~a"
                    "&tag=blogjetblog-20&index=books&linkCode=ur2"
                    "&camp=1789&creative=9325") w)))]
    [else (not-found req)]))

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
