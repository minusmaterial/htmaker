;;;; htmaker.lisp
(in-package #:htmaker)

(defparameter *page-header*
      '((link-rel "stylesheet" "/standard.css")
        (link-rel "stylesheet" "/sidebar.css")
        (link-rel "icon" "/resources/crazybacterialgo.png")
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /> "))

(defparameter *sidebar*
    '(in-opt ul 
             ("id=\"container\""
              "class=\"sidenav\"")
             (img "/resources/a_crazy_bacteria_algo.png")
             (in li (link "Home" "/index.html"))
             (in li (link "Blog" "/blog/index_blog.html"))
             (in li (link "Contact" "/contact/contact.html"))
             (in-opt li ("class=\"foot\"") (in p "AR, 2020"))))

(defparameter *index-skeleton* 
    `("<!DOCTYPE html>"
      (in html
          (in head
              ,*page-header*
              (in title "ls-a"))
          (in body
              ,*sidebar*
              (in div
                  (in h1 "ls-a")
                  page-text)))))

(defparameter *contact-skeleton*
    `("<!DOCTYPE html>"
      (in html
          (in head
              ,*page-header*
              (in title "Contact"))
          (in body
              ,*sidebar*
              (in div
                  (in h1 "Contact Information")
                  page-text
                  )))))

(defparameter *blog-post-skeleton*
      `("<!DOCTYPE html>"
        (in html 
            (in head
                ,*page-header*
                (in title page-title))
            (in body 
                ,*sidebar*
                (in div 
                    (in h1 page-title)
                    (in h4 (in i page-date))
                    
                    "<HR />"
                    page-text
                    "<HR />"
                    (in-opt p ("class=\"tags\"") ("Tags: " page-tags))
                    (in p 
                      (in h4 "Comments")
                      
                      (in blockquote (in-opt form 
                        ("action=/comment.php method=\"post\" id=\"commentform\"")
                        (in p
                          "Name (optional):<br/>"
                          (in-opt input
                            ("type=\"text\" name=\"name\""))
                          "<br/>"
                          "Comment:<br/>"
                          "<textarea name=\"comment\" form=\"commentform\" rows=\"8\" cols=\"50\"></textarea>"
                          "<br/>"
                          (in-opt input 
                            ("type=\hidden\" name=\"sourcefile\""))
                          "<br/>"
                          (in-opt input
                            ("type=\"submit\" value=\"Submit\""))))
                          

                          ))


                    )
                ))))

(defparameter *blog-index-skeleton*
      `("<!DOCTYPE html>"
        (in html 
            (in head
                ,*page-header*
                (in title page-title))
            (in body 
                ,*sidebar*
                (in div 
                    (in h1 page-title)
                    "<HR />"
                    page-text
                    "<HR />"
                    index-links
                    "<HR />"
                    )
                ))))

(defparameter *blog-text* "nothing here")

(defparameter *root-dir* "/home/antepod/Documents/www/ls-a/")
(defparameter *blog-dir* (cat *root-dir* "blog/"))   
(defparameter *local-blog-index-path* 
              (pathname (cat *blog-dir* "index_blog.html")))

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
      (format nil (mak-html form path))))

(defun mak-page-dynamic (filepath)
    (let* ((rawtext (read-from-file filepath))
          (info (parse-source-file rawtext))
          (skeleton (symbol-value (intern 
                      (string-upcase 
                        (cat "*" 
                             (getf info 'texttype)
                             "-skeleton*")))))
          (replacements (nreverse
                          (do ((reps nil))
                              ((eq info nil) reps) 
                              (push (prepend-page (pop info)) reps)
                              (push (pop info) reps)))))
      (print filepath)
      
      (mak-page skeleton 
                :replacements 
                (concatenate 'list replacements (list 'page-path filepath))
                :path
                filepath)
      ))

(defun update-directory (source-dir out-dir)
  (let* ((source-files (directory (cat source-dir "*.txt"))))
    (iterate (for f in source-files)
      (write-to-file (mak-page-dynamic (namestring f))
                     (merge-pathnames out-dir
                                      (cat (pathname-name f)
                                           ".html"))))))


(defun complete-update-ls ()
    (update-directory (cat *root-dir* "source/") *root-dir*)
    (update-directory (cat *root-dir* "source/contact/") (cat *root-dir* "contact/"))
    (update-directory (cat *root-dir* "source/blog/") 
                      (cat *root-dir* "blog/"))
    (trivial-shell:shell-command "server syncls")
    )

(defun main ()
    (complete-update-ls))
