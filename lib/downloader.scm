(define-module (downloader)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (web response)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:export (get-day)
  #:export (get-example))

(define get-cookie
  (call-with-input-file ".aoccookie.txt"
    (lambda (port)
      (read-line port))))

(define (write-file filename content)
  (call-with-output-file filename
    (lambda (port)
      (display content port))))

(define (read-all-lines filename)
  (call-with-input-file filename
    (lambda (port)
      (let ((raw (string-split (get-string-all port) #\newline)))
        (remove string-null? raw)))))

(define (download url)
  (let ((cookie-str (string-append "session=" get-cookie)))
    (call-with-values 
      (lambda () 
        (http-get url #:headers `((cookie . ,cookie-str))))
      (lambda (response body)
        (if (= (response-code response) 200)
            body
            (error "Download failed with code" (response-code response)))))))

(define (get-day n)
  (let ((filename (format #f "inputs/day~d" n))
        (urif (format #f "https://adventofcode.com/2025/day/~d/input" n)))
    (unless (file-exists? filename)
        (write-file filename
                    (download urif)))
    (read-all-lines filename)))

(define (get-example n m)
  (let ((filename (format #f "inputs/example~d_~d" n m)))
    (read-all-lines filename)))
