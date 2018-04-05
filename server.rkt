(require xml net/url net/cookie)

;; Simplified http server for CS 1102 by C. Rich, WPI

;; server : number -> void
;; Starts server listening on given port number (e.g., 8088)
(define (server port)
  (printf "Press red Stop button to stop server (and press Run before restarting)...")
  ;; Run button press needed to close open tcp listener after user break
  (listen-forever (tcp-listen port 10 true)))

;; listen-forever : tcp-listener -> void
;; Runs forever (stop with user break Stop button in DrRacket)
(define (listen-forever listener)
   (let-values ([(in out) (tcp-accept listener)])
     (handle in out)
     (close-input-port in)
     (close-output-port out)
     (listen-forever listener)))
  
;; handle : port port -> void
;; Handles one request on given socket input and output streams
(define (handle in out)
  (let ([request (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                               (try-read-line in))])
    (when request
      (let* ([header (parse-header in)]
             [cookie-header (assoc "Cookie" header)])
        (let-values ([(reply cookie) 
                      (dispatch (second request)
                                (if cookie-header 
                                    (second cookie-header) ""))])
          ;; send reply
          (display "HTTP/1.0 200 Okay\r\n" out)
          (display "Server: k\r\nContent-Type: text/html\r\n" out)
          (when cookie
            (display (format "Set-Cookie: ~a\r\n" 
                             (print-cookie cookie)) 
                     out))
          (display "\r\n" out)
          (display (xexpr->string reply) out))))))

;; try-read-line : port -> string 
;; Return result of read-line or empty string if client disconnected
(define (try-read-line in) 
  (with-handlers ([exn:fail? (lambda (e) (printf "~nReset") "")]) 
    (read-line in)))

;; parse-header : port -> list[list[string]]
;; Reads remaining lines on input port and parses them into 
;;   a list of title/value lists
(define (parse-header in)
  (let ([line (read-line in)])
    (if (eof-object? line) empty
        (let ([match (regexp-match "^([^:]+): (.*)" line)])
          (if match (cons (rest match) (parse-header in)) empty)))))

;; list[script]
(define SCRIPTS empty)

;; (make-script string 
;;              (list[pair] string -> xexpr [or cookie false])))
(define-struct script (name fn))

;; dispatch : string string -> xexpr cookie
;; Invoke the script named by the first element of the path,
;;   giving it the path and the cookies as input, 
;;   and return the reply page and cookie returned by the script
(define (dispatch path cookies)
  (let ([url (string->url path)])
    (invoke (path/param-path (first (url-path url)))
            (url-query url)
            cookies)))

;; invoke : string list[pair] string -> xexpr [or cookie false]
;; Invokes script with given name with given form and cookies
;;   and return the reply page and cookie returned by the script
(define (invoke name form cookies)
  (let ([found (filter (lambda (s) (string=? (script-name s) name)) 
                       SCRIPTS)])
    (if (empty? found)
        (values (html-page (string-append "Error! Script Not Found: " name)
                           empty)
                false)
        ((script-fn (first found)) form cookies))))
  
;; html-page : string . list[xexpr] -> xexpr
;; Produces html page with given title containing body
(define (html-page title . body)
  (list 'html
        (list 'head (list 'title title))
        (cons 'body body)))
;; Defines a script with given name and function that consumes a list
;;   of form field/value pairs and a cookies string, and 
;;   produces a reply page and cookie (or false)
(define-syntax define-script
  (syntax-rules ()
    [(define-script (name f c) body ...)
     (set! SCRIPTS (cons (make-script (symbol->string 'name)
                                      (lambda (f c) body ...)) 
                         SCRIPTS))]))

;;;;;;;;;;;;;;; EXAMPLE SCRIPTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Example script that replies hello to typed in first name,
;;   and ignores cookies.  See hello.html example form.
(define-script (hello form cookies)
  (values (html-page "Reply"
           (string-append "Hello " (cdr (assoc 'firstName form))))
          ;; no cookie
          false))

;; Example script that increments value of "CS1102" cookie.
;;   Assumes zero value if no or non-number cookie
;;   Assumes browser visiting http://localhost:8088/bump
(define-script (bump form cookies)
  (let* ([cookie (get-cookie/single "CS1102" cookies)]
         [old (if (and cookie (string->number cookie))
                  (string->number cookie) 
                  0)]
         [query (assoc 'increment form)]
         [incr (and query (string->number (cdr query)))]
         ;; ignore non-number increment
         [new (and incr (number->string (+ incr old)))])
    (values
     (html-page "CS1102 Cookie"
                (string-append "Current: " 
                               (or new (number->string old)))
                (list 'form 
                      '([action "http://localhost:8088/bump"])
                      '(input ([type "submit"] [value "Add"]))
                      (list 'input 
                            (list* '[type "text"] '[name "increment"] 
                                   (if query
                                       (list [list 'value (cdr query)])
                                       empty)))))
     ;; send cookie if updated
     (and new (set-cookie "CS1102" new)))))