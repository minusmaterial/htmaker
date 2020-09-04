;;;; htmaker.lisp
(in-package #:htmaker)

(defparameter *page-header*
      '((link-rel "stylesheet" "/standard.css")
        (link-rel "stylesheet" "/sidebar.css")
        (link-rel "icon" "/resources/a_crazy_bacteria_algo.png")))

(defparameter *sidebar*
    '(in-opt ul 
             ("id=\"container\""
              "class=\"sidenav\"")
             (img "/a_crazy_bacteria_algo.png")
             (in li (link "Home" "/index.html"))
             (in li (link "Blog" "/blog/index_blog.html"))
             (in li (link "Contact" "/contact.html"))
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
                  (in p name)
                  (in p email)
                  (in p address)
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
                    "<HR />")
                (in-opt div ("id=tags")
                        (in footer ("Tags: " page-tags))
                        "<hr />")))))

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
                    "<hr />"
                    blog-links
                    "<hr />")
                ))))

(defparameter *blog-text* "nothing here")

(defparameter *root-dir* "/home/antepod/Documents/www/ls-a/")
(defparameter *blog-dir* (cat *root-dir* "blog/"))   
(defparameter *local-blog-index-path* 
              (pathname (cat *blog-dir* "index_blog.html")))

(defun mak-page (skeleton &key (replacements nil)
                               (depth 0))
    (let ((form skeleton)
          (forward-2 (lambda (x) (cdr (cdr x))))) 
      (iterate (for sym in replacements by forward-2)
               (for rep in (cdr replacements) by forward-2)
               (setf form (subst rep
                                 sym 
                                 form
                                 :test #'equal)))
      (format nil (mak-html form depth))))

(defun mak-page-dynamic (rawtext)
    (let* ((info (parse-source-file rawtext))
          (skeleton (symbol-value (intern 
                      (string-upcase 
                        (cat "*" 
                             (getf info 'texttype)
                             "-skeleton*")))))
          (replacements (do ((reps nil))
                            ((eq info nil) reps) 
                            (push (list (pop info) 
                                        (pop info)) 
                            reps))))
      
      ;(format t "start:~%")
      ;(format t "~A~%" info)
      ;(format t "~A~%" skeleton)
      ;(format t "~A~%" replacements)
      ;(format t "end~%")
      (mak-page skeleton 
                :replacements 
                replacements)
      ))


(defun complete-update-ls ()
    (update-blog-directory *root-dir*)
    (trivial-shell:shell-command "server sync"))

(defun main ()
  (complete-update-ls))
