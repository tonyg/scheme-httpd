#lang racket

(require srfi/1)
(require srfi/2)
(require srfi/9)
(require srfi/13)
(require srfi/34)

(require "httpd-utils.rkt")

(provide httpd
         stop-httpd
         
         make-http-request
         http-request?
         http-request-headers
         http-request-method
         http-request-path
         http-request-parsed-path
         http-request-http-version
         http-request-body
         
         make-parsed-path
         parsed-path?
         parsed-path-query-string
         parsed-path-query
         parsed-path-params
         parsed-path-path-string
         parsed-path-pieces
         
         make-redirect-response
         
         make-http-response
         http-response?
         http-response-code
         http-response-message
         http-response-headers
         http-response-body)

(define make-byte-vector make-bytes)

(define byte-vector-length bytes-length)

(define (read-block buf start count port)
  (read-bytes! buf port start (+ start count)))

(define (write-block buf start count port)
  (write-bytes buf port start (+ start count)))

(define (open-socket port)
  (tcp-listen port 4 #t))

(define (socket-accept listener)
  (tcp-accept listener))

(define (close-socket listener)
  (tcp-close listener))

(define spawn thread)

(include "httpd.scm")