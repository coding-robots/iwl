#lang racket

(require (planet untyped/unlib/crc))

(define (string-crc32 s)
  (crc32 (string->bytes/utf-8 s)))

(define (number->hex-string n)
  (format "~x" n))

(define (string-crc32-hex s)
  (number->hex-string (string-crc32 s)))

(provide/contract
 [string-crc32 (-> string? natural-number/c)]
 [number->hex-string (-> number? any/c)]
 [string-crc32-hex (-> string? any/c)])
