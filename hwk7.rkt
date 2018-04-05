;; Joseph Petitti & Peter Maida
;;Starting URL: http://localhost:8088/forum-main-page

(load "server.rkt")
(require racket/list)

;;====================
;;  Data Definitions
;;====================
;; A post is (make-post string string)
(define-struct post (author body))

;; Global list of posts, displayed on main page
(define ALL-POSTS empty)

(define post1 (make-post "Steve" "hello world"))
(define post2 (make-post "John" "asdf"))
(define post3 (make-post "Paul" "abcdefghijklmnop"))

;;===========
;;  Scripts
;;===========
(define-script (forum-main-page form cookies) ;args ignored
  (values 
   (html-page "Forum Main Page"
              "Welcome! Displaying all current posts"
              (append (list 'form 
                            (list (list 'action "http://localhost:8088/authoring")
                                  (list 'target "_blank"))
                            (list 'input (list (list 'type "submit")
                                               (list 'value "New Post")))) ;in new window/tab
                      (map html-format (format-posts ALL-POSTS))))
   ;; no cookie
   false))

(define-script (authoring form cookies)
  (values
   (html-page "New Post"
              (list 'form
                    (list (list 'action "http://localhost:8088/preview")
                          (list 'target "_blank"))
                    "Name: "
                    (list 'input (list (list 'type "text")
                                       (list 'name "author")))
                    (list 'br)
                    "Text: "
                    (list 'input (list (list 'type "text")
                                       (list 'name "body")))
                    (list 'br)
                    (list 'input (list (list 'type "submit")
                                       (list 'value "Preview")))))
   ;; send cookie
   false))

(define-script (preview form cookies)
  (let ([author-preview (cdr (assoc 'author form))]
        [body-preview (cdr (assoc 'body form))])
  (values
   (html-page "Preview"
              "Name: "
              author-preview
              (list 'br)
              "Post body: "
               (html-format body-preview)
              (list 'form
                    (list (list 'action "http://localhost:8088/accept"))
                    (list 'input (list (list 'type "submit")
                                       (list 'value "Accept")))
                    (list 'input (list (list 'type "hidden")
                                       (list 'name "author")
                                       (list 'value author-preview)))
                    (list 'input (list (list 'type "hidden")
                                       (list 'name "body")
                                       (list 'value body-preview))))
              (list 'form
                    (list (list 'action "http://localhost:8088/authoring"))
                    (list 'input (list (list 'type "submit")
                                       (list 'value "Cancel")))))
   false)))

(define-script (accept form cookies)
  (begin
    (set! ALL-POSTS (cons (make-post (cdr (assoc 'author form)) (cdr (assoc 'body form)))
                          ALL-POSTS))
    (invoke "forum-main-page" empty cookies)))
  
;;===========
;;  Helpers
;;===========
;; format-posts : list[post] -> list[string]
;; produces numbered list of posts
(define (format-posts data)
  (append-map (lambda (numstr apost) 
                (list (format "  ~a ~a: ~a" numstr (post-author apost) (post-body apost))
                      ))
              (build-list (length data) number->string)
              data))

;; html-format : string -> list[value]
;; Converts a string with html markups into the correct formatting
(define (html-format str)
  (string->xexpr (string-append "<p>"
                                str
                                "</p>")))