#lang racket

(require srfi/1
         srfi/13
         racket/vector
         racket/serialize)

(define words-re
  (pregexp "(?>\\$?\\d*(?:[.,]\\d+)+|\\p{L}+-\\p{L}+|\\p{L}+)")) ; p{L} matches Unicode word

(define sentences-re
  (pregexp "(?>[\\.\\?!]\\\"?(?:\\s|--)+?)"))

(define syllables-re
  (pregexp "(?i:[AEIOUÄÖÜ]+)"))

(define (get-words s)
  (map (lambda (x) 
         (string-trim-both (bytes->string/utf-8 x) #\_)) 
       (regexp-match* words-re (string->bytes/utf-8 s)))) ; regexp-match works like x100 faster with bytes

(define (get-sentences s)
  (map bytes->string/utf-8 
       (regexp-split sentences-re (string->bytes/utf-8 s))))

(define (word-count s)
  (length (get-words s)))

(define (substring-count sub s)
  (length (regexp-match* (regexp-quote sub) s)))

(define (comma-count s)
  (substring-count "," s))

(define (semicolon-count s)
  (substring-count ":" s))

(define (has-substring sub s)
  (regexp-match? (regexp-quote sub) s))

(define (has-quote s)
  (has-substring "\"" s))

(define (has-dashes s)
  (or (has-substring "- " s)
      (has-substring "--" s)))

(define (word-count-token s)
  (let ([wc (word-count s)])
    (and (> wc 4) 
         (format "**S_WC=~a" wc))))

(define (comma-count-token s)
  (let ([cc (comma-count s)])
    (and (> cc 0) 
         (format "**S_CC=~a" cc))))

(define (semicolon-count-token s)
  (let ([sc (semicolon-count s)])
    (and (> sc 0) 
         (format "**S_SCC=~a" sc))))

(define (quote-token s)
  (and (has-quote s) 
       "**S_HQ"))

(define (dashes-token s)
  (and (has-dashes s) 
       "**S_HD"))

(define (get-special-tokens msg)
  (remq* '(#f) 
         (foldl (lambda (s lst)
                  (append lst
                          (list (word-count-token s)
                                (semicolon-count-token s)
                                (quote-token s)
                                (dashes-token s))))
                empty (get-sentences msg))))

(define (safe-substring s start end)
  (substring s start (min end (string-length s))))

(define (get-tokens msg)
  (append (map (lambda (x) 
                 (string-upcase (safe-substring x 0 26)))
               (get-words msg))
          (get-special-tokens msg)))

(define (syllables-count s)
  (length (regexp-match* syllables-re s)))

(define (sum lst)
  (foldl + 0 lst))

(define (readability-score msg)
  (let* ([wl (get-words msg)]
         [words (length wl)]
         [sentences (length (get-sentences msg))]
         [syllables (sum (map syllables-count wl))])
    (- (- 206.876 (* 1.015 (/ words sentences)))
       (* 84.6 (/ syllables words)))))


; Data stuctures

(define categories    (make-vector 0))
(define totals        (make-hash))
(define tokens        (make-hash))
(define readabilities (make-hash))

(define (hash-inc! hash key)
  (hash-set! hash key (+ 1 (hash-ref! hash key 0))))

(define (train! msg cat)
  ; Tokens
  (for-each (lambda (w) 
              (let ([idx (or (vector-member cat categories)
                             (begin
                               (set! categories 
                                     (vector-append categories (vector cat)))
                               (- (vector-length categories) 1)))])
                (hash-inc! totals idx)
                (let ([tok-hash (hash-ref! tokens w (make-hash))])
                  (hash-inc! tok-hash idx))))
            (get-tokens msg))
  ; Readabilities
  (let ([cur-rdb (readability-score msg)])
    (hash-set! readabilities cat 
               (/ (+ cur-rdb (hash-ref! readabilities cat cur-rdb)) 2))))

(define (hash-sum hash)
  (sum (hash-values hash)))


(define (list-top-bottom num lst) ; return list with only num top and num bottom elements of list
  (if (> (length lst) (* 2 num))
      (let ([slst (sort lst <)])
        (append (take slst num) (take-right slst num)))
      lst))

(define (limit-fr x)
  (max (min x 0.99) 0.01))

(define (get-ratings msg)
  (let ([all-totals (hash-sum totals)]
        [ratings    (make-hash)])
    (for-each 
     (lambda (w)
       (let* ([tk (hash-ref tokens w (make-hash))]
              [all-count (hash-sum tk)])
         (hash-for-each totals
                        (lambda (cat cat-total)
                          (let* ([cnt (hash-ref tk cat 0)]
                                 [this-prob (/ cnt cat-total)]
                                 [other-prob (/ (- all-count cnt) 
                                                (- all-totals cat-total))]
                                 [rating (if (> all-count 0)
                                             (limit-fr (/ this-prob (+ this-prob other-prob)))
                                             0.4)])
                            (hash-set! ratings cat
                                       (cons rating (hash-ref! ratings cat empty))))))))
     (get-tokens msg))
    (let ([max-read (reduce max 0 (hash-values readabilities))]
          [cur-read (readability-score msg)])
      (for-each 
       (lambda (cat)
         (let* ([read-prob (limit-fr (/ (- max-read 
                                           (abs (- cur-read (hash-ref readabilities cat 0)))) 
                                        max-read))]
                [probs (append (list-top-bottom 10 (hash-ref ratings cat)) 
                               (make-list 3 read-prob))]
                [fr (/ 1 (length probs))]
                [P (- 1.0 (expt (reduce * 1.0 (map (lambda (p) (- 1.0 p)) probs)) fr))]
                [Q (- 1.0 (expt (reduce * 1.0 probs) fr))]
                [S (/ (- P Q) (+ P Q))])
           (hash-set! ratings cat (/ (+ 1 S) 2))))
       (hash-keys ratings)))
    ratings))

(define (get-category msg)
  (vector-ref categories (car (argmax cdr (hash->list (get-ratings msg))))))

; Data saving and loading

(define categories-file    "categories.dat")
(define totals-file        "totals.dat")
(define tokens-file        "tokens.dat")
(define readabilities-file "readabilities.dat")

(define (data-path file)
  (build-path (current-directory) "data" file))

(define (dump-data)
  (define (dump-var var file)
    (call-with-output-file (data-path file)
      (lambda (out)
        (write (serialize var) out))
       #:exists 'replace))
  (dump-var categories    categories-file)
  (dump-var totals        totals-file)
  (dump-var tokens        tokens-file)
  (dump-var readabilities readabilities-file))

(define (load-data!)
  (define (load-var file)
    (call-with-input-file (data-path file)
      (lambda (in)
        (deserialize (read in)))))
  (set! categories    (load-var categories-file))
  (set! totals        (load-var totals-file))
  (set! tokens        (load-var tokens-file))
  (set! readabilities (load-var readabilities-file))
  (collect-garbage)
  (collect-garbage))

; File trainer

(define (file->string file)
  (call-with-input-file file
    (lambda (in)
      (read-string (file-size file) in))))

(define (train-on-file! file category)
  (train! (file->string file) category))

(define (train-path file)
  (build-path (current-directory) "train-data" file))

(define (train-on-authors! lst)
  (displayln "Training...")
  (for-each (lambda (x) 
              (train-on-file! (train-path (cdr x)) (car x))
              (displayln (car x)))
            lst))

(provide (all-defined-out))
