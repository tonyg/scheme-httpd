#!/bin/sh # -*- scheme -*-
cd $(dirname "$0"); tail -n +3 $(basename "$0") | scheme48; exit

,set batch
,config ,load "./httpd-config.scm"
,config ,load "../../xxexpr/xxexpr-s48.scm"
,open httpd
,open httpd-utils
,open httpd-servlet
,open threads
,open xxexpr
,open pp

;; Parameters:
,open srfi-39

(load "../portable/example.scm")
