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
             (img "/resources/crazybacterialgo.png")
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
                          

                          )))))))

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
                    )))))

(defparameter *default-skeleton* *index-skeleton*)

(defparameter *blog-text* "nothing here")

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
      (format nil (mak-html form path))))

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
      
      (mak-page skeleton 
                :replacements 
                (concatenate 'list replacements (list 'page-path filepath))
                :path
                filepath)
      ))

(defun update-directory (source-dir out-dir)
  (let* ((source-files (files-in-dir source-dir)))
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
  (trivial-shell:shell-command "server syncls"))
(defun complete-update-rad () (complete-update *alexradcliffe-root-dir*)
  (trivial-shell:shell-command "server syncrad"))

(defun main ()
    (complete-update-ls))

(defun temp () 
  (merge 'list  (directory (cat *alexradcliffe-root-dir* "source/*/*.txt")) (directory (cat *alexradcliffe-root-dir* "source/*.txt"))
       (lambda (x y) (< (length (namestring x) ) (length (namestring y))))  ))
