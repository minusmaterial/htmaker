(in-package #:htmaker)

(defparameter *ar-page-header*
      '((link-rel "stylesheet" "/standard.css")
        (link-rel "stylesheet" "/sidebar.css")
        (link-rel "icon" "/resources/crazybacterialgo.png")
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /> "))

(defparameter *ar-sidebar*
    '(in-opt ul 
             ("id=\"container\""
              "class=\"sidenav\"")
             (img "/resources/crazybacterialgo.png")
             (in li (link "Home" "/index.html"))
             (in li (link "Contact" "/contact.html"))
             (in li (link "Projects" "projects/index_projects.html"))
             (in-opt li ("class=\"foot\"") (in p "AR, 2020")))) 

(defparameter *ar-standard-skeleton*
  `("<!DOCTYPE html>"
      (in html
          (in head
              ,*ar-page-header*
              (in title page-title))
          (in body
              ,*ar-sidebar*
              (in div
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
