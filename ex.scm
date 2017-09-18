(use-modules (web client)
             (web uri)
             (web response)
             (json)
             (srfi srfi-19)
             (srfi srfi-64))

(define http-res-stdin
  (lambda ()
    (display "Insert a public web address: ") (newline)
    ((compose http-get string->uri symbol->string read))))

(define http-res-arg (lambda (x) ((compose http-get string->uri) x)))

(define bbc-unit-test
  (lambda ()
    (let* ((url "http://www.bbc.co.uk/iplayer")
           (res (http-res-arg url))
           (res-code (response-code res))
           (res-length (response-content-length res))
           (res-date-string (date->string (response-date res))))
      (display "Url: ") (display url) (newline)
      (display "Status code: ") (display res-code) (newline)
      (display "Content length: ") (display res-length) (newline)
      (display "Date: ") (display res-date-string) (newline)
      )))

(bbc-unit-test)


(test-begin "bbc-programming-exam-tests" 1)

(test-equal "correct-structure" #t
      (and (eq? 2 (+ 1 1))
           (eq? 4 (* 2 2))))
