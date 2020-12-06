(in-package #:htmaker)

(defparameter nline '(#\newline))

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

(defun tabs (i)
  (let ((output nil)) 
       (iter (repeat i) 
             (setf output 
                   (cat "    " output)))
       (cat "" output)))

(defun parse-special-forms (form i &key (filepath "~"))
    (cond ((eq (car form) 'in)
           (cat (tabs i) 
                "<" (symbol-name (car (cdr form))) ">"
                nline
                (mak-html-iterator (cdr (cdr form)) (+ i 1) filepath)
                (tabs i)
                "</" (symbol-name (car (cdr form))) ">"
                nline
                ))
          ((eq (car form) 'in-opt)
                   (cat (tabs i)
                        "<" (symbol-name (car (cdr form))) 
                        " " (apply #'cat (car (cdr (cdr form))))
                        ">"
                        nline
                        (mak-html-iterator (cdr (cdr (cdr form))) 
                                           (+ i 1) filepath)
                        (tabs i)
                        "</" (symbol-name (car (cdr form))) ">"
                        nline
                        ))
          ((eq (car form) 'link)
           (cat (tabs i)
                (link (car (cdr form)) (car (cdr (cdr form))))
                nline))
          ((eq (car form) 'link-rel)
           (cat (tabs i)
                (link-rel (car (cdr form)) (car (cdr (cdr form))))
                nline))
          ((eq (car form) 'img)
           (cat (tabs i)
                (img (car (cdr form)))
                nline))
          ((eq (car form) 'index-links)
           (cat (tabs i)
                (mak-html-iterator 
                  (file-brothers-links
                    (file-brothers filepath))
                  i
                  filepath)
                (mak-html-iterator
                  (non-source-brothers filepath)
                  i
                  filepath)
                "<HR />~%"))
          ((eq (car form) 'evaluate)
           (mak-html-iterator (eval (first (cdr form)))
                              i
                              filepath))
          (T (cat (tabs i)
                  (symbol-name (car form))
                  nline
                  (mak-html-iterator (cdr form) i filepath)))))
  
(defun mak-html-iterator (form i &optional (path "~"))
    (if (not (non-empty-list-p form))
        ;It's not a list of atoms- just an atom.  So echo the atom.
        (cond ((eq nil form) "")
              ((typep form 'string) (cat (tabs i) form nline))
              (T (cat (symbol-name form) nline)))
        ;Else, it's a non-empty list.  So unless it's a special form, recurse.
        (if (not (symbolp (car form)))
            (cat (mak-html-iterator (car form) i path)
                 (mak-html-iterator (cdr form) i path))
            (parse-special-forms form i :filepath path))))

(defun mak-html (form &optional (path "~"))
    (mak-html-iterator form 0 path))
