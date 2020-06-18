(in-package #:htmaker)

(defun link (text addr &key  (cla nil cla-p) 
                             (depth 0))
    (if cla-p
        (cat "<a href=\""
             (relative-root depth)
             (strip-first-slash addr) 
             "\" class=\"" cla 
             "\">" text "</a>")
        (cat "<a href=\"" 
             (relative-root depth) 
             (strip-first-slash addr) 
             "\">" text "</a>")
        ))

(defun link-rel (rel addr)
    (cat "<link rel=" rel " href=" addr ">"))

(defun img (addr &key (alt-text "alt-text"))
    (cat "<img src=\"" addr 
         "\" alt=\"" alt-text 
         "\"" ">"))

(defun i-to-tabspace (i)
  (let ((output nil)) 
       (iter (repeat i) 
             (setf output 
                   (cat "    " output)))
       (cat "" output)))

(defun parse-special-forms (form i &optional (depth 0))
    (cond ((eq (car form) 'in)
           (cat (i-to-tabspace i) 
                "<" (symbol-name (car (cdr form))) ">"
                "~%"
                (mak-html-iterator (cdr (cdr form)) (+ i 1))
                (i-to-tabspace i)
                "</" (symbol-name (car (cdr form))) ">"
                "~%"
                ))
          ((eq (car form) 'in-opt)
                   (cat (i-to-tabspace i)
                        "<" (symbol-name (car (cdr form))) 
                        " " (apply #'cat (car (cdr (cdr form))))
                        ">"
                        "~%"
                        (mak-html-iterator (cdr (cdr (cdr form))) 
                                           (+ i 1))
                        (i-to-tabspace i)
                        "</" (symbol-name (car (cdr form))) ">"
                        "~%"
                        ))
          ((eq (car form) 'link)
           (cat (i-to-tabspace i)
                (link (car (cdr form)) (car (cdr (cdr form)))
                      :depth depth)
                "~%"))
          ((eq (car form) 'link-rel)
           (cat (i-to-tabspace i)
                (link-rel (car (cdr form)) (car (cdr (cdr form))))
                "~%"))
          ((eq (car form) 'img)
           (cat (i-to-tabspace i)
                (img (car (cdr form)))
                "~%"))
          (T (cat (i-to-tabspace i)
                  (symbol-name (car form))
                  "~%"
                  (mak-html-iterator (cdr form)  i )))))
  
(defun mak-html-iterator (form i &optional (depth 0))
    (if (not (non-empty-list-p form))
        ;It's not a list- just a thing itself.  Just put the thing
        (cond ((eq nil form) "")
              ((typep form 'string) (cat (i-to-tabspace i) form "~%"))
              (T (cat (symbol-name form) "~%")))
        (if (not (symbolp (car form)))
            (cat (mak-html-iterator (car form) i)
                 (mak-html-iterator (cdr form) i))
            (parse-special-forms form i depth))))

(defun mak-html (form &optional (depth 0))
    (mak-html-iterator form 0 depth))
