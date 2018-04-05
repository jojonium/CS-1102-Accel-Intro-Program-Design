;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hwk1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
#|
Joseph Petitti and Peter Maida

6.  The benefit of returning false is the awareness of an issue so that any other functions that uses this function does not continue with an unmerged string.
    If the function just returned an unmerged string, it could cause problems with other functions using it that assume the string it returns is merged.

8.  (/ (- (* 9 3) (double 'a)) 2)
          ^^^^^^^
  = (/ (- 27 (double 'a)) 2)
             ^^^^^^^^^^^
  = (/ (- 27 (* 'a 2)) 2)
             ^^^^^^^^
    *: expects a number as 1st argument, given 'a

9.  (or (< 5 2) (and (= 15 (- 18 3)) (> 8 4)))
        ^^^^^^^
  = (or false (and (= 15 (-18 3)) (> 8 4)))
                         ^^^^^^^
  = (or false (and (= 15 15) (> 8 4)))
                   ^^^^^^^^^
  = (or false (and true (> 8 4)))
                        ^^^^^^^
  = (or false (and true true))
              ^^^^^^^^^^^^^^^
  = (or false true)
    ^^^^^^^^^^^^^^^
  = true

10. (and (+ 5 -1) false)
         ^^^^^^^^
  = (and 4 false)
    ^^^^^^^^^^^^^
    and: question result is not true or false: 4

11. (apply-patch 'remove "this is a test string")
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    (apply-op (patch-operation 'remove) "this is a test string" (patch-pos 'remove))
                                                                ^^^^^^^^^^^^^^^^^^^
    patch-pos: expects a patch, given 'remove

12. This means that the conditional clause has only one argument, when it is supposed to have two (a question and answer).
    ex: (define A 5)
        (cond
           [(> A  10)])

13. This means that Racket doesn't know what a variable is supposed to be.
    ex: (+ x 100)

14. This means that an open parenthesis is used in the wrong place, i.e. not before a function call
    ex: (+ (91) 2)
    
|#


(define DOC1 "Many modern software applications--such as word processors and version control systems--manage multiple versions of files and documents")
(define DOC2 "abcdefghijklmnopqrstuvwxyz")

;;An insert is (make-insert string string)
#|
(define (insert-fun an-insert)
        ... (insert-string an-insert) ...
        ... (insert? value) ... )
|#
(define-struct insert (string))
(define POTATOES (make-insert "potatoes "))
(define CARROTS (make-insert "carrots "))
(define TOMATOES (make-insert "tomatoes "))

;;A delete is (make-delete string number number)
#|
(define (delete-fun a-delete)
        ... (delete-number a-delete) ...
        ... (delete? value) ... )
|#
(define-struct delete (number))
(define SEVEN (make-delete 7))
(define FIVE (make-delete 5))
(define TWO (make-delete 2))
(define ONE (make-delete 1))

;;An operation is either
;;     -insert, or
;;     -delete
#|
(define (operation-fun ... an-operation)
  (cond [(insert? an-operation)
           (insert-string an-operation ...)]
        [(delete? an-operation)
           (delete-number an-operation ...)]))
|#
;;apply-op : string number operation -> string
;;Consumes a document, a position, and an operation
;;Produces a new document, with the given operation applied to it
(check-expect (apply-op POTATOES DOC1 5) "Many potatoes modern software applications--such as word processors and version control systems--manage multiple versions of files and documents")
(check-expect (apply-op SEVEN DOC1 4) "Many software applications--such as word processors and version control systems--manage multiple versions of files and documents")
(define (apply-op operation document pos)
  (cond [(insert? operation)
           (string-append (substring document 0 pos)
                          (insert-string operation)
                          (substring document pos (string-length document)))]
        [(delete? operation)
           (string-append (substring document 0 pos)
                          (substring document (+ (delete-number operation) pos) (string-length document)))]))

;;A patch is (make-patch operation number)
#|
(define (patch-fun a-patch ...)
  (patch-operation a-patch)
  (patch-pos a-patch))
  (patch? value))
|#
(define-struct patch (operation pos))
(define PATCH1 (make-patch POTATOES 5))
(define PATCH2 (make-patch SEVEN 4))
(define PATCH3 (make-patch TOMATOES 5))
(define PATCH4 (make-patch FIVE 3))
(define PATCH5 (make-patch TWO 5))
(define PATCH6 (make-patch CARROTS 9))
(define PATCH7 (make-patch ONE 5))
(define PATCH8 (make-patch TWO 3))
(define PATCH9 (make-patch ONE 100))
;;apply-patch : patch string -> string
;;Consumes a patch and document
;;Produces a new string of the document with the given patch applied to it
(check-expect (apply-patch PATCH1 DOC1) "Many potatoes modern software applications--such as word processors and version control systems--manage multiple versions of files and documents")
(check-expect (apply-patch PATCH2 DOC1) "Many software applications--such as word processors and version control systems--manage multiple versions of files and documents")
(define (apply-patch patch doc)
   (apply-op (patch-operation patch) doc (patch-pos patch) ))

;;is-in-range? : number patch -> boolean
;;Determines whether the given number is inside the range of a deletion, a-patch (from pos to pos+operation)
;;a-patch must be a deletion
(check-expect (is-in-range? 5 PATCH2) true)
(check-expect (is-in-range? 11 PATCH2) true)
(check-expect (is-in-range? 12 PATCH4) false)
(define (is-in-range? num a-patch)
  (and (>= num (patch-pos a-patch))
       (<= num (+ (patch-pos a-patch) (delete-number (patch-operation a-patch))))))


;;overlap? : patch patch -> boolean
;;Determines whether two patches overlap, as in:
;;     Two insertions start at same position
;;     Two deletions ranges overlap
;;     An insertion that starts inside the range of a deletion, unless the insertion and deletion start at the same position
(check-expect (overlap? PATCH1 PATCH3) true)     ;;Two instertions start at same position
(check-expect (overlap? PATCH2 PATCH4) true)     ;;Two deletions ranges overlap
(check-expect (overlap? PATCH7 PATCH2) true)
(check-expect (overlap? PATCH8 PATCH2) true)
(check-expect (overlap? PATCH1 PATCH2) true)     ;;An insertion that starts inside the range of a deletion
(check-expect (overlap? PATCH2 PATCH1) true)
(check-expect (overlap? PATCH1 PATCH5) false)    ;;The insertion and deletion start at the same position
(check-expect (overlap? PATCH5 PATCH1) false)
(check-expect (overlap? PATCH5 PATCH6) false)
(check-expect (overlap? PATCH1 PATCH9) false)
(define (overlap? patch-a patch-b)
  (cond [(and
            (and (insert? (patch-operation patch-a)) (insert? (patch-operation patch-b)))                      ;;If both patches are insertions
            (= (patch-pos patch-a) (patch-pos patch-b)))                                                       ;;If they start at the same location
         true]
        [(and
            (and (delete? (patch-operation patch-a)) (delete? (patch-operation patch-b)))                      ;;If both patches are deletions
            (or
               (is-in-range? (patch-pos patch-b) patch-a)                                                      ;;Start of b is in the range of a
               (is-in-range? (+ (patch-pos patch-b) (delete-number (patch-operation patch-b))) patch-a)        ;;End of b is in the range of a
               (is-in-range? (patch-pos patch-a) patch-b)))                                                    ;;Start of a is in the range of b
         true]
        [(and (insert? (patch-operation patch-a)) (delete? (patch-operation patch-b)))                         ;;If patch-a is an insert and patch-b is a delete
                    (cond [(= (patch-pos patch-a) (patch-pos patch-b)) false]                                  ;;An insertion that starts inside the range of a deletion
                          [(is-in-range? (patch-pos patch-a) patch-b) true]
                          [else false])]
        [(and (insert? (patch-operation patch-b)) (delete? (patch-operation patch-a)))                         ;;If patch-b is an insert and patch-a is a delete
                    (cond [(= (patch-pos patch-b) (patch-pos patch-a)) false]                                  ;;An insertion that starts inside the range of a deletion
                          [(is-in-range? (patch-pos patch-b) patch-a) true]
                          [else false])]))

;;merge : string patch patch -> string/boolean
;;Consumes a document and two patches for that document
;;Produces a new document if the patches don't overlap otherwise it will return false
(check-expect (merge DOC1 PATCH1 PATCH2) false)
(check-expect (merge DOC2 PATCH5 PATCH6) "abcdehicarrots jklmnopqrstuvwxyz")
(check-expect (merge DOC1 PATCH6 PATCH5) "Many decarrots rn software applications--such as word processors and version control systems--manage multiple versions of files and documents")
(check-expect (merge DOC1 PATCH5 PATCH3) "Many tomatoes dern software applications--such as word processors and version control systems--manage multiple versions of files and documents")
(define (merge document patch-a patch-b)
  (cond [(overlap? patch-a patch-b)
         false]
        [else (cond [(> (patch-pos patch-a) (patch-pos patch-b))
                     (apply-patch patch-b (apply-patch patch-a document))]
                    [(< (patch-pos patch-a) (patch-pos patch-b))
                     (apply-patch patch-a (apply-patch patch-b document))]
                    [else (cond [(delete? (patch-operation patch-a))
                                 (apply-patch patch-b (apply-patch patch-a document))]
                                [(delete? (patch-operation patch-b))
                                 (apply-patch patch-a (apply-patch patch-b document))])])]))


(define HAMLET1 "Hamlet: Do you see yonder cloud that's almost in shape of a camel? Polonius: By the mass, and 'tis like a camel, indeed. [...] Hamlet: Or like a whale? Polonius: Very like a whale.")
(define H-PATCH1 (make-patch  (make-delete 30) 19))
(define H-PATCH2 (make-patch (make-insert "the cloud over there that's almost the ") 19))
(define H-PATCH3 (make-patch (make-delete 19) 80))
(define H-PATCH4 (make-patch (make-insert "golly, it is ") 80))
(define H-PATCH5 (make-patch (make-delete 5) 162))
(define H-PATCH6 (make-patch (make-insert "It's totally ") 162))
(check-expect (merge (merge (merge HAMLET1 H-PATCH6 H-PATCH5) H-PATCH4 H-PATCH3) H-PATCH2 H-PATCH1)
              "Hamlet: Do you see the cloud over there that's almost the shape of a camel? Polonius: By golly, it is like a camel, indeed. [...] Hamlet: Or like a whale? Polonius: It's totally like a whale.")