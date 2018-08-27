;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname lab5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Joseph Petitti
(require readline)

;;;;;;; DATA DEFINITIONS FOR A SIMPLE TUTORING SYSTEM ;;;;;;;;;;;;;;;;

;; A question is a
;; - (make-open-question string list[value])
(define-struct open-question (test correct-answers))

;; A command is
;;  - (make-ask question)
;;  - (make-branch-on-results number list[commands] list[commands])
;;  - (make-display-status)

(define-struct ask (ques))
(define-struct branch-on-results (test passed failed))
(define-struct display-status ())

;; An exam is a list[command]

(define exam1
  (list 
   (make-ask (make-open-question "What is 3*4+2?" (list 14)))
   (make-ask (make-open-question "What is 2+3*4?" (list 14)))
   (make-ask (make-open-question "What is 5+2*6?" (list 17)))
   (make-branch-on-results 50
                           (list (make-ask (make-open-question "What is 3+5*2?" (list 13))))
                           empty)
   (make-ask (make-open-question "What is (3-2)*6?" (list 6)))   
   (make-display-status)))
   
(define total 0)
(define right 0)
;;===============
;;  Interpreter
;;===============
;; give-exam : exam -> void
;; gives an exam
(define (give-exam an-exam)
  (begin
    (set! total 0)
    (set! right 0)
    (run-cmd-list an-exam)))

;; run-cmd-list : list[cmd] -> void
;; takes in a list of commands, runs them all in order
(define (run-cmd-list cmdlist)
  (for-each run-cmd cmdlist))

;; run-cmd : cmd -> void
;; Takes in a single command and runs it
(define (run-cmd a-cmd) 
  (cond [(ask? a-cmd) (run-ask a-cmd)]
        [(branch-on-results? a-cmd) (run-branch-on-results a-cmd)]
        [(display-status? a-cmd) (run-display-status a-cmd)]))

;; run-ask : cmd -> void
;; Takes in an ask command, executes it, then returns true if the question was answered correctly
(define (run-ask an-ask)
  (begin
    (set! total (+ 1 total))
    (printf "~a~n" (open-question-test (ask-ques an-ask)))
    (check-correct (read)
                   (open-question-correct-answers (ask-ques an-ask)))))

;; check-correct : value list[value] -> boolean
;; checks if the value is in the list of correct values
(check-expect (check-correct 5 (list 4 5 6)) true)
(check-expect (check-correct 0 (list 100 200 300)) false)
(define (check-correct input answers)
  (cond [(empty? answers) false]
        [(cons? answers) 
         (cond [(or (equal? input (first answers))
                    (check-correct input (rest answers)))
                (begin
                  (set! right (+ 1 right))
                  true)]
               [else false])]))

;; percent-right : number number -> number
;; takes in the total number of questions so far and the number right, returns the percent correct
(define (percent-right total right)
  (* 100 (/ right total)))

;; run-branch-on-results : cmd -> void
;; Takes in a run-branch-on-results command, runs the passed commands if current score is above
;; or equal to test, runs failed commands if current score is below test
(define (run-branch-on-results a-cmd)
  (let ([score (percent-right total right)])
    (cond[(>= score (branch-on-results-test a-cmd))
          (run-cmd-list (branch-on-results-passed a-cmd))]
         [(< score (branch-on-results-test a-cmd))
          (run-cmd-list (branch-on-results-failed a-cmd))])))

;; run-display-status : cmd -> void
;; displays the current score
(define (run-display-status a-cmd)
  (printf "you answered ~a question(s) correctly, out of ~a total.~nYour score was ~a percent"
          right
          total
          (percent-right total right)))