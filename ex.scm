(use-modules (web client)
             (web uri)
             (web response)
             (json)
             (gnutls)
             (srfi srfi-19)
             (srfi srfi-64))

(define http-res-stdin
  (lambda (results)
    (display "Insert web address OR type quit to exit: ") (newline)
    (let* ((stdin ((compose symbol->string read))))
      (cond ((eq? stdin "quit") (quit))
            (else (http-res-stdin (cons (http-get-json stdin) results)))))))

(define http-res-arg
  (lambda (x)
    (catch #t
      (lambda ()
        ((compose http-get string->uri) x))
      (lambda (key . parameters)
        (cond ((eq? key 'getaddrinfo-error) "bad address info")
              (else (symbol->string key)))))))

(define http-get-json
  (lambda (url)
    (let* ((res (http-res-arg url)))
      (http-res-json url res))))

(define http-res-json
  (lambda (url res)
    (let*
      ((res-code (try-res-prop response-code res))
      (res-length (try-res-prop response-content-length res))
      (res-m-date (try-res-prop response-date res))
      (res-date (cond ((date? res-m-date) (date->string res-m-date)) (else #f)))
      (res-json-str
      (cond
        ((or (eq? #f res-code) (eq? #f res-length) (eq? #f res-date))
          (scm->json-string `(("Url" ,@url) ("Error" ,@res))))
        (else (scm->json-string `(("Url" ,@url) ("Status_code" ,@res-code)
          ("Content_length" ,@res-length) ("Date" ,@res-date)) #:pretty #t)))))
      (display res-json-str) (newline)
      (cons res-json-str (cons (json-string->scm res-json-str) (quote ()))))))

(define try-res-prop
  (lambda (f x)
    (cond ((response? x) (f x))
          (else #f))))

(define init
  (lambda ()
    ;; Uncomment this and comment lines below for stdin
    ;; (http-res-stdin '())

    (http-get-json "http://www.bbc.co.uk/iplayer")
    (http-get-json "https://google.com")
    (http-get-json "bad://address")
    (http-get-json "http://www.bbc.co.uk/missing/thing")
    (http-get-json "http://not.exists.bbc.co.uk/")
    (http-get-json "https://www.oracle.com/technetwork/java/javase/downloads/index.html")
    (http-get-json "https://www.pets4home.co.uk/images/articles/1646/large/kitten-emergencies-signs-to-look-out-for-537479947ec1c.jpg")
    (http-get-json "http://site.mockito.org")))

(init)

(define get-json-pretty (lambda (l) (car l)))
(define get-json-object (lambda (l) (car (cdr l))))
(define get-keys-hash (lambda (h) (hash-fold (lambda (k v s) (cons k s)) '() h)))

(define mock-success-res-hash-keys
  (lambda ()
    (let* ((res (build-response))
           (keys (get-keys-hash (http-res-json res "test_url"))))
      (display "Built response in unit test: ") (display res) (newline)
      (display "Keys: ") (display keys) (newline)
      keys)))

(mock-success-res-hash-keys)


(test-begin "bbc-programming-exam-tests" 1)

(test-equal "json-success-keys-interface"
  '("Date" "Content_length" "Status_code" "Url")
  (mock-success-res-hash-keys))
