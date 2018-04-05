;; Joseph Petitti & Peter Maida
(require test-engine/racket-tests)

;;==========
;;  PART 1
;;==========
;;MACROS
(define-syntax class
  (syntax-rules ()
    [(class (initvars var ...)
            (method name (args ...) fn) ...)
     (lambda (var ...)
       (lambda (message)
         (cond [(symbol=? message 'name) 
                (lambda (args ...) fn)]
               ...
               [else (error (format "Method: ~a is undefined" message))])))]))


(define-syntax send 
  (syntax-rules ()
    [(send d message args ...)
     ((d 'message) args ...)]))


;;INTERPRETER
(define dillo-class
  (class (initvars length dead?)
    (method longer-than? (len) (> length len))
    (method run-over () (dillo-class (+ length 1) true))))

(printf "Part 1:~n")
(define d3 (dillo-class 5 false))
(send d3 longer-than? 6)
(send d3 longer-than? 5)
(define d4 (send d3 run-over))
(send d4 longer-than? 5)
;;Error check test
;;(send d4 longer-than 5)



;;==========
;;  PART 2
;;==========
;; MACRO
(define-syntax policy-checker
  (syntax-rules ()
    [(policy-checker
      (employee (permission ...) (files ...)) ...)
     (let ([people-lst 
            (list (make-person 'employee (list 'permission ...) (list 'files ...)) ...)]
           [possible-perm (append (list 'permission ...) ...)]
           [possible-files (append (list 'files ...) ...)])
       (lambda (e p f)
         (if (not (member? p possible-perm))
             (error (format "~a is not a valid permission" p)))
         (if (not (member? f possible-files))
             (error (format "~a is not a valid type of file" f)))
         (cond [(member? e (map person-role people-lst))
                (check-matching-roles p f (filter (lambda (x) (symbol=? e(person-role x))) people-lst))]
               [else (error (format "~a is not a valid employee" e))])))]))

;; A person is (make-person symbol list[symbol] list[symbol])
(define-struct person (role list-of-perm list-of-file))

;; MACRO HELPERS
;; member? : value list[value] -> boolean
;; Determines whether a value is in a given list
(check-expect (member? 3 (list 1 2 3 4 5 6)) true)
(check-expect (member? 'blue (list 'red 'white 'blue)) true)
(check-expect (member? 2 (list 1 3 5 7)) false)
(define (member? aval lst)
  (cond [(empty? lst) false]
        [(cons? lst)
         (or (equal? aval (first lst))
             (member? aval (rest lst)))]))

;; check-matching-roles : symbol symbol list[person] -> boolean
;; Checks if any of the persons in given list has a permission and file that matches the given symbols
(check-expect (check-matching-roles 'read 'code
                                    (list (make-person 'tester (list 'read) (list 'code))
                                          (make-person 'tester (list 'write) (list 'documentation))))
              true)
(check-expect (check-matching-roles 'write 'documentation
                                    (list (make-person 'tester (list 'read) (list 'code))
                                          (make-person 'tester (list 'write) (list 'documentation))))
              true)
(check-expect (check-matching-roles 'read 'documentation
                                    (list (make-person 'tester (list 'read) (list 'code))
                                          (make-person 'tester (list 'write) (list 'documentation))))
              false)
(define (check-matching-roles p f lst)
  (cond [(empty? lst) false]
        [(cons? lst) (or (and (member? p (person-list-of-perm (first lst)))
                              (member? f (person-list-of-file (first lst))))
                         (check-matching-roles p f (rest lst)))]))

;;INTERPRETER
(define check-policy
     (policy-checker
      (programmer (read write) (code documentation))
      (tester (read) (code))
      (tester (write) (documentation))
      (manager (read write) (reports))
      (manager (read) (documentation))
      (ceo (read write) (code documentation reports))))


(printf "~nPart 2:~n")
(check-policy 'programmer 'write 'code)
(check-policy 'programmer 'write 'reports)
(check-policy 'manager 'read 'documentation)
;;Error check tests
;;(check-policy 'banana 'read 'reports)
;;(check-policy 'programmer 'wite 'code)
;;(check-policy 'programmer 'write 'cod)

(test)