#lang racket

(require srfi/2)
(require srfi/9)
(require srfi/13)

(require "httpd-utils.rkt")
(require "httpd.rkt")

(provide make-httpd-servlet-handler
         
         make-servlet-index
         servlet-index?
         reset-servlet-index!
         publish-pattern!
         invoke-handler
         
         make-publication-pattern
         instantiate-publication-pattern
         extract-publication-pattern-binding
         
         define-publication-patterns
         binding-let
         publication-pattern-handler
         publish-pattern)

(include "../portable/httpd-servlet.scm")
