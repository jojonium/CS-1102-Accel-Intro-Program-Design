;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname hwk4) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #t)))
;;Joseph Petitti & Peter Maida

;; A question is (make-question prompt answer string boolean)
(define-struct question (prompt ans subject mc?))


;; A prompt is either:
;; - A string
;; - list[string] (multiple choice)
#|
(define (prompt-fun a-prompt)
   (cond [(string? (prompt-ask a-prompt)...)
         [(empty? (prompt-ask a-prompt)) ...]
         [else (...(first (prompt-ask a-prompt))...
                   (prompt-fun (make-prompt (rest (prompt-ask a-prompt))))]))
|#
(define-struct prompt (ask))


;; An answer is (make-answer list[value] string)
#|
(define (answer-fun an-answer)
   (cond [(empty? (answer-values an-answer)) ...]
         [(cons? (answer-values an-answer)) (... (first (answer-values an-answer))...
                                             ... (answer-hint an-answer)...
                                             ... (answer-fun (make-answer
                                                             (rest (answer-values an-answer))
                                                             (answer-hint an-answer)))))]))
|#
(define-struct answer (values hint))


;; An exam is (make-exam list[cmd])
#|
(define (exam-fun an-exam)
   (cond [(empty? (exam-cmds an-exam))...]
         [(cons? (exam-cmds an-exam)) ... (cmd-fun (first (exam-cmds an-exam)))
                                      ... (exam-fun (make-exam (rest (exam-cmds an-exam))))]))
|#
(define-struct exam (cmds))


;; A cmd is either
;; - (make-nextcmd (question)), or
;; - (make-skipcond (number string list[cmd] list[cmd])), or
;; - (make-msgcmd (string))
;; - (make-scorescmd (string))
#|
(define (cmd-fun a-cmd)
   (cond [(nextcmd? a-cmd) (question-fun (next-cmd-question a-cmd)) ...]
         [(skipcmd? a-cmd) ...]
         [(msgcmd? a-cmd)...]
         [(scorescmd? a-cmd) (cond [(string=? "all" (scorescmd-subject a-cmd) ...]
                                   [else ...])))
|#
(define-struct nextcmd (question))
(define-struct skipcmd (score subjcet below-score above-score))
(define-struct msgcmd (msg))
(define-struct scorescmd (subject))



(define math-exam
  (let ([Question-1
         (make-question "What is 3*4+2?"
                        (make-answer (list 14) "")
                        "arithmetic"
                        false)]
        [Question-2
         (make-question "What is 2+3*4?"
                        (make-answer (list 14) "")
                        "arithmetic"
                        false)]
        [Question-3
         (make-question "What is 5+2*6?"
                        (make-answer (list 17) "")
                        "arithmetic"
                        false)]
        [Question-4
         (make-question "What is 3+5*2?"
                        (make-answer (list 13) "")
                        "arithmetic"
                        false)]
        [Question-5
         (make-question (list "What is the reduced form of 12/18?"
                              "6/9"
                              "1/1.5"
                              "2/3")
                        (make-answer (list 3) "")
                        "fractions"
                        true)]
        [Question-6
         (make-question "What is 8+3*2?"
                        (make-answer (list 14) "")
                        "arithmetic"
                        false)]
        [Question-7
         (make-question (list "What is 1/4 + 1/2?"
                              "3/4"
                              "1/6"
                              "2/6")
                        (make-answer (list 1) "")
                        "fractions"
                        true)])
    (list (make-nextcmd Question-1)
          (make-nextcmd Question-2)
          (make-nextcmd Question-3)
          (make-skipcmd 65 "arithmetic"
                        (list (make-msgcmd "You seem to be having trouble with these. Try again.")
                              (make-nextcmd Question-4))
                        empty)
          (make-nextcmd Question-5)
          (make-skipcmd 65 "arithmetic"
                        (list (make-nextcmd Question-6)) (list (make-nextcmd Question-7)))
          (make-scorescmd "arithmetic")
          (make-scorescmd "fractions"))))




(define wpi-history
  (let ([Question-1
         (make-question "When was WPI founded?"
                        (make-answer (list 1865) "")
                        ""
                        false)]
        [Question-2
         (make-question "What is Gompei"
                        (make-answer (list "goat") "Think bleating")
                        "personalities"
                        false)]
        [Question-3
         (make-question (list "Who was the first president of WPI?"
                              "Boynton"
                              "Washburn"
                              "Thompson")
                        (make-answer (list 3) "")
                        "personalities"
                        true)]
        [Question-4
         (make-question "Name one of the two towers behind a WPI education"
                        (make-answer (list "theory" "practice") "")
                        ""
                        false)])
    (list (make-nextcmd Question-1)
          (make-msgcmd "Let's see if you know your WPI personalities")
          (make-nextcmd Question-2)
          (make-nextcmd Question-3)
          (make-scorescmd "personalities")
          (make-nextcmd Question-4)
          (make-msgcmd "There's more WPI history on the web. And life.")
          (make-scorescmd "all"))))