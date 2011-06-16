(define current-request (make-parameter #f))
(define index (make-servlet-index))

(define (respond title body-parts)
  (make-http-response 200 "OK"
		      '(("Awesome" "oh yes"))
		      (parameterize ((xml-empty-tags-mode #f))
			(xxexpr->string
			 `((html (head (title ,title))
				 (body (h1 "Test")
				       ,@body-parts)))))))

(publish-pattern index
    (make-publication-pattern '())
    ()
  (respond "Test" `((p "See " (a ((href "/other")) "here") "."))))

(publish-pattern index
    (make-publication-pattern '(other))
    ()
  (respond "Other" `((p "Go " (a ((href "/")) "home") "."))))

(httpd 8000 (make-httpd-servlet-handler current-request index))

(do () (#f)
  (sleep 1000))
