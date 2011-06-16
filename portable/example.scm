(define current-request (make-parameter #f))
(define index (make-servlet-index))

(define (respond title body-parts)
  (make-http-response 200 "OK"
		      '(("Awesome" "oh yes"))
		      (parameterize ((xml-empty-tags-mode #f))
			(xxexpr->string
			 `((html (head (title ,title))
				 (body (h1 "Test")
				       ,@body-parts
				       (p "Return to the "(a ((href "/")) "main page")"."))))))))

(publish-pattern index
    (make-publication-pattern '())
    ()
  (respond "Test"
	   `((p "See "(a ((href "/other")) "here")".")
	     (p "Alternatively, "(a ((href "/stop")) "stop")" the server."))))

(publish-pattern index
    (make-publication-pattern '(other))
    ()
  (respond "Other"
	   `((form ((action "/post-target")
		    (method "POST"))
		   (input ((name "foo") (value "bar"))) (br)
		   (input ((name "zot") (value "quux"))) (br)
		   (input ((type "submit") (value "Do it")))))))

(publish-pattern index
    (make-publication-pattern '(post-target))
    ()
  (respond "Target"
	   `((p "You posted:")
	     (ul
	      ,@(map (lambda (piece)
		       `(li ,(symbol->string (car piece))" = ",@(cdr piece)))
		     (parse-query (http-request-body-string (current-request))))))))

(publish-pattern index
    (make-publication-pattern '(stop))
    ()
  (stop-http-daemon daemon)
  (respond "Stopped"
	   `((p "It's stopped now."))))

(define daemon
  (make-http-daemon 8000
		    (make-httpd-servlet-handler current-request index)))

(run-http-daemon daemon)
