(in-package #:htmaker)

(defun split-source-file-text (input-text)
    (print (split-sequence:split-sequence
    #\Newline 
    input-text)))

(defun get-source-file-info (lines &optional (marker "!inf:"))
    (append
      ;;;first, all of the non-text fields
      (iter (for line in lines)
            (when (starts-with-substr-p line marker)
              (let* ((longpair (split-sequence:split-sequence 
                            #\: 
                            (replace-all line marker "")))
                     (pair (list (car longpair)
                                 (cond ((eq (type-of (cadr longpair))
                                           'cons)
                                       (apply #'cat
                                        (cadr longpair)))
                                       (t
                                       (cadr longpair)))
                                 )))
                (collect (intern (string-upcase (car pair))))
                (collect (if (find #\, (cadr pair))
                             (split-sequence:split-sequence
                               #\, (cadr pair))
                             (cadr pair))))))
      ;;;finally, the text field
      (list 
        (intern "TEXT")
          (parse-pandoc (apply #'cat
            (let* ((only-text-lines
                     (remove-if (lambda (line) 
                                         (starts-with-substr-p line marker))
                                         lines)))
                 (mapcar #'cat
                        (n-list '(#\newline) (length only-text-lines))
                        ;(print only-text-lines)
                        only-text-lines))))))) 

(defun parse-source-file (rawtext)
    (let* ((lines (split-source-file-text rawtext))
           (info (get-source-file-info lines))
           (text (getf info 'text)))
      ;(print text)
      info)) 
