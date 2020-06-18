;;;; utils.lisp
(in-package #:htmaker)

(defun write-to-file (data filename)
    (progn 
    (with-open-file (f filename :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
      (write-sequence data f))
    (values)))

(defun read-from-file (filename)
    (uiop:read-file-string filename))

(defun cat (&rest sequences)
  (apply #'concatenate (cons 'string sequences)))

(defun non-empty-list-p (form)
  (and (listp form) form))

(defun replace-all (string part replacement &key (test #'char=))
    "Returns a new string in which all the occurences of the part
    is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
        for old-pos = 0 then (+ pos part-length)
        for pos = (search part string
                          :start2 old-pos
                          :test test)
        do (write-string string out
                         :start old-pos
                         :end (or pos (length string)))
        when pos do (write-string replacement out)
        while pos)))

(defun starts-with-substr-p (str substr)
    (when (>= (length str) (length substr))
      (equal (subseq str 0 (length substr))
           substr)))

(defun check-trailing-slash (path)
    (if (and
          (not (< (length path) 1))
          (equal (aref path
                       (- (length path) 1))
                 #\/))
        path
        (cat path "/")))

(defun strip-first-slash (path)
    (if (and (not (equal (length path) 0)) 
             (equal (aref path 0) #\/))
        (subseq path 1)
        path))

(defun split-blog-date (date-text)
    (let* ((dat (if (equal nil date-text) 
                    nil
                    (mapcar (lambda (x) 
                                    (read-from-string x))
                      (split-sequence:split-sequence #\- 
                                                     date-text)))))
      `(:y ,(car dat)
        :m ,(cadr dat)
        :d ,(caddr dat))))

(defun mak-blog-date (date)
    (cat (princ-to-string (getf date :y))
         "/"
         (princ-to-string (getf date :m))
         "/"
         (princ-to-string (getf date :d))))

(defun relative-root (depth)
    (let ((out "")) 
      (dotimes  (d depth) (setf out
                             (cat out "../")))
      (setf out (check-trailing-slash out))))

(defun parse-md (text)
    (markdown:parse text))

(defun n-list (thing n)
    (if (<= n 0)
        nil
        (cons thing (n-list thing (1- n)))))

(defun prepend-page (sym)
   (intern (cat "PAGE-" (symbol-name sym))))

