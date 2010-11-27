#lang racket

(require srfi/1
         srfi/13
         racket/serialize
         racket/runtime-path)

(define *categories*    (make-vector 0))
(define *totals*        (make-hash))
(define *tokens*        (make-hash))
(define *readabilities* (make-hash))

(define words-re     (pregexp "(?>\\p{L}+-\\p{L}+|\\p{L}+)"))
(define sentences-re (pregexp "(?>[\\.\\?!]\\\"?(?:\\s|--)+?)"))
(define syllables-re (pregexp "(?i:[AEIOUÄÖÜ]+)"))

(define (get-words s)
  (map bytes->string/utf-8
       (regexp-match* words-re (string->bytes/utf-8 s)))) 

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

(define (get-special-tokens text)
  (foldl (lambda (s lst)
           (append lst
                   (remq* '(#f)
                          (list 
                           (word-count-token s)
                           (comma-count-token s)
                           (semicolon-count-token s)
                           (quote-token s)
                           (dashes-token s)))))
         null (get-sentences text)))

(define (safe-substring s start end)
  (substring s start (min end (string-length s))))

(define (get-tokens text)
  (append (map (lambda (x) (string-upcase (safe-substring x 0 26)))
               (get-words text))
          (get-special-tokens text)))

(define (syllables-count s)
  (length (regexp-match* syllables-re s)))

(define (sum lst)
  (foldl + 0 lst))

(define (readability-score text)
  ; Flesch Reading Ease
  (let* ([wl (get-words text)]
         [words (length wl)]
         [sentences (length (get-sentences text))]
         [syllables (sum (map syllables-count wl))])
    (- 206.876
       (* 1.015 (/ words sentences))
       (* 84.6 (/ syllables words)))))

(define (hash-inc! hash key)
  (hash-update! hash key add1 0))

(define-syntax-rule (vector-expand! vec val)
  ; Expands vector with value val and returns its index
  (begin
    (set! vec (vector-append vec (vector val)))
    (sub1 (vector-length vec))))

(define (train! text cat)
  ; Tokens
  (let ([idx (or (vector-member cat *categories*)
                 (vector-expand! *categories* cat))])
    (for-each (lambda (w)
                (hash-inc! *totals* idx)
                (hash-inc! (hash-ref! *tokens* w (make-hasheqv)) idx))
              (get-tokens text))
    ; Readabilities
    (let ([cur-rdb (readability-score text)])
      (hash-update! *readabilities* idx
                    (lambda (x) (/ (+ cur-rdb x) 2)) cur-rdb))))

(define (hash-sum hash)
  (sum (hash-values hash)))

(define (list-top-bottom num lst) 
  ; return list with only num top and num bottom elements of sorted lst
  (if (> (length lst) (* 2 num))
      (let ([slst (sort lst <)])
        (append (take slst num) (take-right slst num)))
      lst))

(define (lim-frac x)
  (max (min x 0.99) 0.01))

(define (fold-ratings probs)
  (let* ([fr (/ 1 (length probs))]
         [P (- 1 (expt (reduce * 1 (map (lambda (p) (- 1 p)) probs)) fr))]
         [Q (- 1 (expt (reduce * 1 probs) fr))]
         [S (/ (- P Q) (+ P Q))])
    (/ (+ 1 S) 2)))

(define (readability-prob maxr cat current)
  (lim-frac (/ (- maxr (abs (- current cat))) maxr)))

(define (get-ratings text)
  (let ([ratings    (make-hash)]
        [all-totals (hash-sum *totals*)])
    ; Generate list of probabilities per category for each token in text
    (for-each
     (lambda (w)
       (let* ([token-counts (hash-ref *tokens* w (make-hash))]
              [all-count (hash-sum token-counts)])
         (hash-for-each
          *totals*
          (lambda (cat cat-total)
            (let* ([cnt (hash-ref token-counts cat 0)]
                   [this-prob (/ cnt cat-total)]
                   [other-prob (/ (- all-count cnt)
                                  (- all-totals cat-total))]
                   [rating (if (> all-count 0)
                               (lim-frac (/ this-prob (+ this-prob other-prob)))
                               0.4)])
              (hash-update! ratings cat (lambda (x) (cons rating x)) null))))))
     (get-tokens text))
    ; Calculate single "rating" value from list of probabilities (including
    ; readabilities) for each category for which we generated probabilities
    (let ([cur-readability (readability-score text)]
          [max-readability (reduce max 0 (hash-values *readabilities*))])
      (for/hash ([cat (hash-keys ratings)])
        (values
         cat
         (fold-ratings (cons
                        (readability-prob max-readability
                                          (hash-ref *readabilities* cat 0)
                                          cur-readability)
                        (list-top-bottom 10 (hash-ref ratings cat)))))))))

(define (get-category text)
  (with-handlers ([exn:fail:contract:divide-by-zero? (lambda (_) #f)])
    (vector-ref *categories*
                (car (argmax cdr (hash->list (get-ratings text)))))))

; Data saving and loading

(define-runtime-paths
  (categories-file
   totals-file
   tokens-file
   readabilities-file)
  (values
   "data/categories.dat"
   "data/totals.dat"
   "data/tokens.dat"
   "data/readabilities.dat"))

; I tried fasl and just plain dumping of s-exprs, but while it's
; faster to dump and load, for some reason it uses a lot more memory
; after loading, plus it's less portable, so stick with serialize.

(define (dump-data)
  (define (dump-var var file)
    (write-to-file (serialize var) file #:exists 'replace))
  (dump-var *categories*    categories-file)
  (dump-var *totals*        totals-file)
  (dump-var *tokens*        tokens-file)
  (dump-var *readabilities* readabilities-file))

(define (load-data!)
  (define (load-var file)
    (deserialize (file->value file)))
  (set! *categories*    (load-var categories-file))
  (set! *totals*        (load-var totals-file))
  (set! *tokens*        (load-var tokens-file))
  (set! *readabilities* (load-var readabilities-file))
  (collect-garbage)
  (collect-garbage)) ; collects better when used two times

; File trainer

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
