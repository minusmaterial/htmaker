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
      (print links)
      (print newlinks)
      (mak-page *blog-index-skeleton*
                :replacements
                (list
                  'blog-links newlinks
                  'page-title "ls-a: Blog index")
                :depth depth)))

(defun write-blog-page (fn-in fn-out)
    (write-to-file (mak-page-dynamic (uiop:read-file-string fn-in))
                 fn-out))

(defun write-blog-pages (source-dir out-dir)
  (format t "hahaha~%")
    (let* ((in-dir (check-trailing-slash source-dir))
           (source-files (directory 
                          (cat in-dir "/*.txt"))))
      (iterate (for f in source-files)
               (write-blog-page f 
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
           (outfile-names (mapcar #'pathname-name 
                                  source-files))
           (blog-data (mapcar #'parse-source-file
                              (mapcar #'uiop:read-file-string 
                                      source-files)))
           (relative-outfile-links 
            (mapcar (lambda (x) (cat "/blog/"
                                     x 
                                     ".html")) 
                    (mapcar #'pathname-name source-files))))
      
      (write-blog-pages source-dir blog-dir)
      (format t "~A~%" relative-outfile-links)
      (write-to-file (mak-blog-index blog-data relative-outfile-links)
                     (cat blog-dir "index_blog.html"))
      ))
