;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hwk3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #t)))
#|
Joseph Petitti & Peter Maida
|#

;;A file is (make-file symbol number value)
(define-struct file (n s x))

;;A list-of-files is either
;;  -empty, or
;;  -(cons file lof)
#|
;;lof-fun : a-lof... -> a-lof ...
(define (lof-fun a-lof ...)
   (cond [(empty? a-lof) ...]
         [(cons? a-lof)
          ...(first a-lof)...
          ...(lof-fun (rest a-lof))]))
|#

;;A list-of-directories is either
;;  -empty, or
;;  -(cons directory list-of-directories)
;;A dir is (make-dir symbol list-of-directories list-of-files)
#|
;;dir-fun : a-dir ... -> ...
(define (dir-fun a-dir ...)
   (dir-n a-dir)
   (lof-fun a-dir)
   (lod-fun a-dir))

(define (lod-fun a-lod)
   (cond [(empty? a-lod) ...]
         [(cons? a-lod) (dir-fun (first a-lod))
                        (lod-fun (rest a-lod))]))
|#
(define-struct dir (n ds fs))


(define FILE1 (make-file 'file1 100 "hello world"))
(define FILE2 (make-file 'file2 50 "test"))
(define SUBFOLDER (make-dir 'folder1 empty (list FILE2)))
(define FOLDER (make-dir 'folder2 (cons SUBFOLDER empty) (list FILE1)))
(define FOLDER2 (make-dir 'test (list (make-dir 'test2 empty (list FILE1))) (list FILE2)))

(define FILEA (make-file 'filea 0 "a"))
(define FILEB (make-file 'fileb 10 "b"))
(define FILEC (make-file 'filec 100 5))
(define FILED (make-file 'filed 10000 5))
(define FILEE (make-file 'filee 0 "e"))
(define FILEG (make-file 'fileg 999 "g"))
(define FILEF (make-file 'filef 0 "f"))
(define FILEH (make-file 'fileh 85247 "h"))
(define SUBFOLDERD (make-dir 'folderd empty empty))
(define SUBFOLDERE (make-dir 'foldere empty (list FILEG FILEH)))
(define SUBFOLDERB (make-dir 'folderb (list SUBFOLDERD SUBFOLDERE) (list FILED FILEE)))
(define SUBFOLDERC (make-dir 'folderc empty (list FILEF)))
(define SUBFOLDERA (make-dir 'foldera (list SUBFOLDERC) (list FILEA FILEB FILEC)))
(define FOLDER3 (make-dir 'filesystem (list SUBFOLDERA SUBFOLDERB) empty))


;;any-huge-files? : dir number -> boolean
;;Consumes a filesystem and a number
;;Returns true if any file in the filesystem has size larger than the given size
(check-expect (any-huge-files? FOLDER 90) true)
(check-expect (any-huge-files? SUBFOLDER 90) false)
(check-expect (any-huge-files? (make-dir 'test empty empty) 999) false)
(check-expect (any-huge-files? FOLDER2 85) true)

(define (any-huge-files? a-dir n)
   (cond [(empty? (dir-ds a-dir))
          (huge-file? (dir-fs a-dir) n)]
         [(cons? (dir-ds a-dir))
          (or (huge-file? (dir-fs a-dir) n)
              (huge-files-in-list? (dir-ds a-dir) n))]))


;;huge-files-in-list? : list-of-directory number -> boolean
(check-expect (huge-files-in-list? empty 1000) false)
(check-expect (huge-files-in-list? (list (make-dir 'folder1 empty empty)
                                         (make-dir 'test2 empty (list FILE1))) 90) true)

(define (huge-files-in-list? a-lod n)
   (cond [(empty? a-lod) false]
         [(cons? a-lod)
          (or (any-huge-files? (first a-lod) n)
              (huge-files-in-list? (rest a-lod) n))]))


;;huge-file? : list-of-files number -> boolean
;;determines if any of the files have a size greater than given number
(check-expect (huge-file? (list FILE1 FILE2) 90) true)
(check-expect (huge-file? (list FILE2) 200) false)
(check-expect (huge-file? empty 999) false)

(define (huge-file? a-lof n)
   (cond [(empty? a-lof) false]
         [(cons? a-lof)
          (or (> (file-s (first a-lof)) n)
              (huge-file? (rest a-lof) n))]))


;;clean-directory : dir symbol -> dir
;;Consumes a filesystem and a name of an existing directory
;;Returns the same filesystem without the files of size 0 in directory with given name
(check-expect (clean-directory FOLDER3 'folderd) FOLDER3)
(check-expect (clean-directory FOLDER3 'folderc)
              (make-dir 'filesystem
                        (list (make-dir 'foldera
                                        (list (make-dir 'folderc empty empty))
                                        (list (make-file 'filea 0 "a")
                                              (make-file 'fileb 10 "b")
                                              (make-file 'filec 100 5)))
                              SUBFOLDERB)
                        empty))

(define (clean-directory fls n)
         (cond [(symbol=? (dir-n fls) n)
                (make-dir (dir-n fls)
                          (dir-ds fls)
                          (filter (lambda (x) (> (file-s x) 0)) (dir-fs fls)))]
               [else (make-dir (dir-n fls)
                               (map (lambda (x) (clean-directory x n)) (dir-ds fls))
                               (dir-fs fls))]))


;;find-file-path : dir symbol -> list[symbol] or boolean
;;Consumes a filesystem and name of a file, produces a list of directories leading to the file, or false if
;;the file doesn't exist
(check-expect (find-file-path FOLDER3 'bananas) false)
(check-expect (find-file-path FOLDER3 'filea) (list 'filesystem 'foldera))
(check-expect (find-file-path FOLDER3 'fileh) (list 'filesystem 'folderb 'foldere))
(define (find-file-path a-dir a-name)
  
  (cond [(filename-in-list? (dir-fs a-dir) a-name) (list (dir-n a-dir))]
        [(false? (contains-file (dir-ds a-dir) a-name)) false]
        [else (cons (dir-n a-dir)
                    (find-file-path (contains-file (dir-ds a-dir) a-name) a-name))]))


;;contains-file : list[dir] symbol -> boolean or dir
;;Consumes a list of directories, returns the name of the directory that heads the branch with a file of
;;given name in it, or false if a file of given name doesn't exist.
(check-expect (contains-file (dir-ds FOLDER3) 'filea) SUBFOLDERA)
(check-expect (contains-file (dir-ds FOLDER3) 'fileg) SUBFOLDERB)
(check-expect (contains-file empty 'potatoes) false)

(define (contains-file a-lod a-name)
   (cond [(empty? a-lod) false]
         [(cons? a-lod) (cond [(filename-in-dir? (first a-lod) a-name)
                               (first a-lod)]
                              [else (contains-file (rest a-lod) a-name)])]))


;;filename-in-dir? : dir symbol -> boolean
;;Consumes a directory, determines whether that directory or any of its subdirectories contain a file with the given name
(check-expect (filename-in-dir? SUBFOLDERA 'filef) true)
(check-expect (filename-in-dir? SUBFOLDERA 'fileg) false)
(check-expect (filename-in-dir? SUBFOLDERD 'bananas) false)
(check-expect (filename-in-dir? SUBFOLDERB 'fileh) true)
(check-expect (filename-in-dir? FOLDER3 'filea) true)

(define (filename-in-dir? a-dir name)
  (cond [(empty? (dir-ds a-dir))
         (filename-in-list? (dir-fs a-dir) name)]
        [(cons? (dir-ds a-dir))
         (or (filename-in-list? (dir-fs a-dir) name)
             (filename-in-dir? (first (dir-ds a-dir)) name)
             (ormap (lambda (x) (filename-in-dir? x name)) (rest (dir-ds a-dir))))]))


;;filename-in-list? : list[file] symbol -> boolean
;;Consumes a list of files, determines whether a file with the given name is in the list
(check-expect (filename-in-list? (list FILEA FILEB FILEC) 'fileb) true)
(check-expect (filename-in-list? (list FILEA FILEB FILEC) 'bananas) false)
(check-expect (filename-in-list? empty 'bananas) false)

(define (filename-in-list? a-lof n)
  (cond [(empty? (filter (lambda (x) (symbol=? n (file-n x))) a-lof)) false]
        [else true]))


;;file-names-satisfying-lod : a-lod (file -> boolean) -> list[file]
;;Consumes a list of directories and a function from file to boolean, returns a list of files in that
;;directory for which the given function evaluates to true.
(check-expect (file-names-satisfying-lod (list SUBFOLDERA SUBFOLDERB) (lambda (x) (= 0 (file-s x)))) (list FILEF FILEA FILEE))

(define (file-names-satisfying-lod a-lod file-fun)
   (cond [(empty? a-lod) empty]
         [(cons? a-lod) (append (file-names-satisfying-wrapper (first a-lod) file-fun)
                                (file-names-satisfying-lod (rest a-lod) file-fun))]))


;;file-names-satisfying-lof : a-lof (file -> boolean) -> list[file]
;;Consumes a list of files and function from file to boolean, returns a list of the files in the list for
;;which the function evaluates to true
(check-expect (file-names-satisfying-lof (dir-fs SUBFOLDERA) (lambda (x) (= 0 (file-s x)))) (list FILEA))

(define (file-names-satisfying-lof a-lof file-fun)
   (cond [(empty? a-lof) empty]
         [(cons? a-lof) (filter file-fun a-lof)]))


;;file-names-satisfying-wrapper : dir (file -> boolean) -> list[file]
;;Consumes a directory and a function from file to boolean, returns a list of files in that directory
;;and sub-directories that return true for given function
(check-expect (file-names-satisfying FOLDER3 (lambda (x) (> (file-s x) 10)))
              (list 'filec 'fileg 'fileh 'filed))

(define (file-names-satisfying-wrapper a-dir file-fun)
  (append (file-names-satisfying-lod (dir-ds a-dir) file-fun)
          (file-names-satisfying-lof (dir-fs a-dir) file-fun)))


;;file-names-satisfying : dir (file -> boolean) -> list[symbol]
;;consumes a filesystem and a function from file to boolean
;;returns a list of file names of files for which the given function returns true
(check-expect (file-names-satisfying FOLDER3 (lambda (x) (> (file-s x) 10)))
              (list 'filec 'fileg 'fileh 'filed))
(check-expect (file-names-satisfying FOLDER3 (lambda (x) (= (file-s x) 0)))
              (list 'filef 'filea 'filee))
(check-expect (file-names-satisfying FOLDER3 (lambda (x) (equal? (file-x x) 5)))
              (list 'filec 'filed))

(define (file-names-satisfying a-dir file-fun)
  (map file-n (file-names-satisfying-wrapper a-dir file-fun)))


;;files-containing : dir value -> list[symbol]
;;consumes a filesystem and a value, returns a list of names of files that contain the given value as
;;their contents
(check-expect (files-containing FOLDER3 5) (list 'filec 'filed))

(define (files-containing fls value)
   (file-names-satisfying fls (lambda (x) (equal? (file-x x) value))))