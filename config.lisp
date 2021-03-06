(in-package #:htmaker)

(defparameter *ar-page-header*
      '((link-rel "stylesheet" "/standard.css")
        (link-rel "stylesheet" "/sidebar.css")
        (link-rel "icon" "/resources/crazybacterialgo.png")
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /> "))

(defparameter *ar-sidebar-quotes*
  '((in p "But thus do I counsel you, my friends: distrust all in whom the impulse to punish is powerful! ")
    (in p "He is a barbarian, and thinks the customs of his tribe and island are the laws of nature.")
    (in p "Moloch! Moloch! Robot apartments! Invisible suburbs! Skeleton treasuries! Blind capitals! Demonic industries! Spectral nations!")
    (in p "The stories and information posted here are artistic works of fiction and falsehood. Only a fool would take anything posted here as fact.")
    (in p "If...if...We didn't love freedom enough. And even more, we had no awareness of the real situation.... We purely and simply deserved everything that happened afterward.")
    (in p "The true spirit of delight, the exaltation, the sense of being more than Man, which is the touchstone of the highest excellence, is to be found in mathematics as surely as in poetry.")
    ))

(defparameter *ar-sidebar*
    `(in-opt ul 
             ("id=\"container\""
              "class=\"sidenav\"")
             (img "/resources/crazybacterialgo.png")
             (in li (link "Home" "/index.html"))
             (in li (link "Contact" "/contact.html"))
             (in li (link "Projects" "projects/index_projects.html"))
             (in li (link "Blog" "blog/index_blog.html"))
             (in-opt li ("class=\"foot\"") 
               (evaluate (nth (random (length *ar-sidebar-quotes*))
                              *ar-sidebar-quotes*))))) 


(defparameter *ar-standard-skeleton*
  `("<!DOCTYPE html>"
      (in html
          (in head
              ,*ar-page-header*
              (in title page-title))
          (in body
              ,*ar-sidebar*
              (in div
                  (in h1 page-title)
                  page-text
                  "<HR />"
                  (in-opt p ("class=\"tags\"") ("Tags: " page-tags))
                  )))))

(defparameter *ar-quotes-skeleton*
  `("<!DOCTYPE html>"
      (in html
          (in head
              ,*ar-page-header*
              (in title page-title))
          (in body
              ,*ar-sidebar*
              (in div
                  (in h1 page-title)
                  page-text
                  "<HR />"
                  ,(mapcar 
                     (lambda (x)
                       (list 
                         'in 'blockquote
                         x))
                     *ar-sidebar-quotes*)
                  "<HR />"
                  (in-opt p ("class=\"tags\"") ("Tags: " page-tags))
                  )))))

(defparameter *ar-blog-post-skeleton*
      `("<!DOCTYPE html>"
        (in html 
            (in head
                ,*ar-page-header*
                (in title page-title))
            (in body 
                ,*ar-sidebar*
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

(defparameter *ar-index-skeleton* 
    `("<!DOCTYPE html>"
      (in html
          (in head
              ,*ar-page-header*
              (in title page-title))
          (in body
              ,*ar-sidebar*
              (in div
                  page-text)))))

(defparameter *ar-folder-index-skeleton*
      `("<!DOCTYPE html>"
        (in html 
            (in head
                ,*ar-page-header*
                (in title page-title))
            (in body 
                ,*ar-sidebar*
                (in div 
                    (in h1 page-title)
                    "<HR />"
                    page-text
                    "<HR />"
                    index-links
                    "<HR />"
                    )))))

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
             (in li (link "Advent of Code" "/advent/2020/advent_2020.html"))
             (in li (link "Files" "files/files.html"))
             (in-opt li ("class=\"foot\"") 
                     (evaluate 
                       (nth 
                         (random 
                           (length *ar-sidebar-quotes*))
                         *ar-sidebar-quotes*))))) 

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

(defparameter *standard-skeleton*
  `("<!DOCTYPE html>"
      (in html
          (in head
              ,*page-header*
              (in title page-title))
          (in body
              ,*sidebar*
              (in div
                  (in h1 page-title)
                  page-text
                  "<HR />"
                  (in-opt p ("class=\"tags\"") ("Tags: " page-tags))
                  )))))

(defparameter *default-skeleton* *standard-skeleton*)

(defparameter *folder-index-skeleton*
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

(defparameter *ar-blog-index-skeleton*
      `("<!DOCTYPE html>"
        (in html 
            (in head
                ,*ar-page-header*
                (in title page-title))
            (in body 
                ,*ar-sidebar*
                (in div 
                    (in h1 page-title)
                    "<HR />"
                    page-text
                    "<HR />"
                    index-links
                    "<HR />"
                    )))))

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

(defparameter *lh-page-header*
      '((link-rel "stylesheet" "/style.css")))

(defparameter *lh-skeleton* 
  `("<!DOCTYPE html>"
      (in html
          (in head
              ,*lh-page-header*
              (in title page-title))
          (in body
              (in div
                  (in h1 page-title)
                  page-text
                  )))))

(defparameter *lh-index-skeleton* 
  `("<!DOCTYPE html>"
      (in html
          (in head
              ,*lh-page-header*
              (in title page-title))
          (in body
              (in div
                  (in h1 page-title)
                  page-text
                  )))))

(defparameter *lh-essay-skeleton* 
  `("<!DOCTYPE html>"
      (in html
          (in head
              ,*lh-page-header*
              (in title page-title))
          (in body
              (in div
                  (in h1 page-title)
                  page-text
                  )))))
