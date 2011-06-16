#!/bin/sh # -*- scheme -*-
tail -n +3 "$0" | scheme48; exit

,set batch
,config ,load httpd-config-s48.scm
,config ,load "../xxexpr/xxexpr-s48.scm"
,open httpd
,open httpd-servlet
,open threads
,open xxexpr

;; Parameters:
,open srfi-39

(load "example.scm")
