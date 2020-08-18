(in-package #:htmaker)

(defun split-source-file-text (input-text)
    (delete "" 
            (split-sequence:split-sequence
            #\Newline 
            input-text)
            :test #'equal))

(defun inject-blog-text-skeleton (lines) 
    (let ((out nil))
         (dolist (str 
                  lines
                  (nreverse out))
                  (push (cons 'in 
                              (cons 'p 
                                    (cons str nil))) 
                        out))))

(defun mak-blog-page (rawtext &optional (depth 0))
    (let ((blog (parse-source-file rawtext)))
      (mak-page *blog-post-skeleton*
                :replacements
                (list
                  'page-title (getf blog '|title|)
                  'page-text (getf blog '|text|)
                  'page-tags (getf blog '|tags|)
                  'page-date (getf blog '|date|))
                :depth 
                depth)))

(defun mak-blog-index (data links &optional (depth 0))
    (let* ((newlinks (mapcar (lambda (d l)
                            `(in p
                               ,(getf d 'date)
                              ,(link
                              (getf d 'title)
                              l)))
                          data links)))
      ;(print links)
      ;(print newlinks)
      (mak-page *blog-index-skeleton*
                :replacements
                (list
                  'index-links newlinks
                  'page-title "ls-a: Blog index")
                :depth depth)))

(defun write-blog-pages (source-dir out-dir)
    (let* ((in-dir (check-trailing-slash source-dir))
           (source-files (mapcar #'namestring
                                 (directory 
                                   (cat in-dir "/*.txt")))))
      (iterate (for f in source-files)
               (write-to-file (mak-page-dynamic f)
                              (merge-pathnames out-dir
                                                (cat (pathname-name f)
                                                     ".html"))))
      nil))

(defun update-blog-directory (root-dir)
    (let* ((root-dir (check-trailing-slash root-dir))
           (blog-dir (cat root-dir 
                          "blog/"))
           (source-dir (cat root-dir 
                            "source/blog/"))
           (source-files (directory (cat source-dir 
                                         "*.txt")))
           (relative-outfile-links 
            (mapcar (lambda (x) (cat "/blog/"
                                     x 
                                     ".html")) 
                    (mapcar #'pathname-name source-files))))
      
      (format t "~A~%" relative-outfile-links)
      (write-blog-pages source-dir blog-dir)
      ))

(defun update-directory (source-dir out-dir)
  (let* ((source-files (directory (cat source-dir "*.txt"))))
    (print source-files)
    (iterate (for f in source-files)
      (write-to-file (mak-page-dynamic (namestring f))
                     (merge-pathnames out-dir
                                      (cat (pathname-name f)
                                           ".html"))))))
