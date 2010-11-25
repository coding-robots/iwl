#lang racket

; -- Taken from unlib/crc, under LGPL
; http://planet.racket-lang.org/display.ss?package=unlib.plt&owner=untyped

(define (crc32 data)
  (bitwise-xor
   (for/fold ([accum #xFFFFFFFF])
     ([byte  (in-bytes data)])
     (for/fold ([accum (bitwise-xor accum byte)])
       ([num (in-range 0 8)])
       (bitwise-xor (quotient accum 2)
                    (* #xEDB88320 (bitwise-and accum 1)))))
   #xFFFFFFFF))

(define (string->crc32 s)
  (crc32 (string->bytes/utf-8 s)))

(define (number->hex-string n)
  (format "~x" n))

(define (string->crc32/hex s)
  (number->hex-string (string->crc32 s)))

(provide/contract
 [crc32 (-> bytes? natural-number/c)]
 [string->crc32 (-> string? natural-number/c)]
 [number->hex-string (-> number? any/c)]
 [string->crc32/hex (-> string? any/c)])
