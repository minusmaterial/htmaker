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
    (in p "The stories and information posted here are artistic works of fiction and falsehood. Only a fool would take anything posted here as fact.")))

(defparameter *ar-sidebar*
    `(in-opt ul 
             ("id=\"container\""
              "class=\"sidenav\"")
             (img "/resources/crazybacterialgo.png")
             (in li (link "Home" "/index.html"))
             (in li (link "Contact" "/contact.html"))
             (in li (link "Projects" "projects/index_projects.html"))
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

(defparameter *default-skeleton* *index-skeleton*)

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


