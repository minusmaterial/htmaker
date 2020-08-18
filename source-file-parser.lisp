(in-package #:htmaker)

(defun split-source-file-text (input-text)
    (split-sequence:split-sequence
    #\Newline 
    input-text))

(defun get-source-file-info (lines &optional (marker "!inf:"))
    (print lines)
    (append 
      ;;;first, all of the non-text fields
      (iter (for line in lines)
            (when (starts-with-substr-p line marker)
              (let ((pair (split-sequence:split-sequence 
                            #\: 
                            (replace-all line marker ""))))
                (collect (intern (string-upcase (car pair))))
                (collect (if (find #\, (cadr pair))
                             (split-sequence:split-sequence
                               #\, (cadr pair))
                             (cadr pair))))))
      ;;;finally, the text field
      (list 
        (intern "TEXT")
          (apply #'cat
            (mapcar #'cat
                    (remove-if (lambda (line) 
                                 (starts-with-substr-p line marker))
                               lines)
                    (n-list '(#\newline) (length (remove-if 
                                                  (lambda (line) 
                                                    (starts-with-substr-p line marker))
                                                  lines)))))))) 

(defun parse-source-file (rawtext)
    (let* ((lines (split-source-file-text (parse-md rawtext)))
           (info (get-source-file-info lines)))
      info)) 
