;;;; htmaker.lisp
(in-package #:htmaker)

(defparameter *ls-a-root-dir* "/home/antepod/Documents/www/ls-a/")
(defparameter *alexradcliffe-root-dir* "/home/antepod/Documents/www/alexradcliffe/")

(defun mak-page (skeleton &key (replacements nil)
                               (path "~"))
    (let ((form skeleton)
          (forward-2 (lambda (x) (cdr (cdr x))))) 
      (iterate (for sym in replacements by forward-2)
               (for rep in (cdr replacements) by forward-2)
               (setf form (subst rep
                                 sym 
                                 form
                                 :test #'equal)))
      (format t "~A~%" path)
      (mak-html form path)))

(defun mak-page-dynamic (filepath)
    (let* ((rawtext (read-from-file filepath))
          (info (parse-source-file rawtext))
          (skeleton (symbol-value (intern 
                      (string-upcase 
                        (cat "*" 
                             (let ((tempval (getf info 'texttype)))
                               (if tempval tempval "default"))
                             "-skeleton*")))))
          (replacements (nreverse
                          (do ((reps nil))
                              ((eq info nil) reps) 
                              (push (prepend-page (pop info)) reps)
                              (push (pop info) reps)))))
      ;(format t "~a~%" (find 'sourcefile4-2  replacements))
      ;(format t "~A~%" info)
      (mak-page skeleton 
                :replacements 
                (concatenate 'list replacements (list 'page-path filepath))
                :path
                filepath)
      ))

(defun update-directory (source-dir out-dir)
  (let* ((source-files (files-in-dir source-dir)))
    ;(format t "~A~%" out-dir)
    (iterate (for f in source-files)
      (ensure-directories-exist 
        (cat (replace-all 
               (directory-namestring f) 
               "/source/" "/") 
             (pathname-name f) 
             ".html"))
      (write-to-file (mak-page-dynamic (namestring f))
                     (cat (replace-all 
                            (directory-namestring f) 
                            "/source/" "/") 
                          (pathname-name f) 
                          ".html")
                     ))))


(defun complete-update (root-dir)
    (update-directory (cat root-dir "source/") root-dir)
    
    )
(defun complete-update-ls () (complete-update *ls-a-root-dir*)
  (format t "~A~%" (trivial-shell:shell-command "server syncls")))
(defun complete-update-rad () (complete-update *alexradcliffe-root-dir*)
  (format t "~A~%" (trivial-shell:shell-command "server syncrad")))

(defun main ()
    ;(complete-update-rad)
    (complete-update-ls))

(defun temp () 
  (merge 'list  (directory (cat *alexradcliffe-root-dir* "source/*/*.txt")) (directory (cat *alexradcliffe-root-dir* "source/*.txt"))
       (lambda (x y) (< (length (namestring x) ) (length (namestring y))))  ))
