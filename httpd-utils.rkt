#lang racket

(require srfi/13)

(provide string-split
         flatten-iolist
         interleave-element)

(define string->utf-8 string->bytes/utf-8)

(include "httpd-utils.scm")
