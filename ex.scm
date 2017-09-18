(use-modules (web client)
             (web uri)
             (web response)
             (json)
             (srfi srfi-19)
             (srfi srfi-64))

(define http-res-stdin
  (lambda ()
    (display "Insert web address OR type quit to exit: ") (newline)
    (let* ((stdin ((compose symbol->string read))))
           (try-http-get-json stdin))))

(define try-http-get-json
  (lambda (x)
    (cond ((eq? x "quit") (exit)
      (else (http-get-json x))))))

(define http-res-arg (lambda (x) ((compose http-get string->uri) x)))

(define http-get-json
  (lambda (url)
    (let* ((res (http-res-arg url))
           (res-code (response-code res))
           (res-length (response-content-length res))
           (res-date-string (date->string (response-date res)))
           (res-json-str (scm->json-string `(("Url" ,@url)
                        ("Status_code" ,@res-code)
                        ("Content_length" ,@res-length)
                        ("Date" ,@res-date-string)) #:pretty #t)))
           (cons res-json-str
             (cons (json-string->scm res-json-str) (quote ()))))))

(define get-json-bbc (lambda () (http-get-json "http://www.bbc.co.uk/iplayer")))

(define get-json-pretty (lambda (l) (car l)))
(define get-json-object (lambda (l) (car (cdr l))))

(define bbc-json-response (get-json-bbc))
(define bbc-json-pretty (get-json-pretty bbc-json-response))
(define bbc-json-object (get-json-object bbc-json-response))

(display bbc-json-pretty) (newline)
(display bbc-json-object) (newline)

(define get-keys-hash
  (lambda (h) (hash-fold (lambda (k v s) (cons k s)) '() h)))

(test-begin "bbc-programming-exam-tests" 1)

(test-equal "json-success-keys-interface"
  '("Date" "Content_length" "Status_code" "Url")
  (get-keys-hash bbc-json-object))
