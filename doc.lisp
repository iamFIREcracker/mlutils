(in-package #:mlutils)

(pax:defsection @mlutils-manual (:title "cl-mlutils")
  "Personal list of utils to make Common Lisp feel a bit more like home"
  (@mlutils-reference pax:section))



;;;; Register in PAX World

(defun pax-sections ()
  (list @mlutils-manual))
(defun pax-pages ()
  `((:objects (, @pax-sections)
     :source-uri-fn ,(pax:make-github-source-uri-fn
                      :mlutils
                      "https://github.com/iamFIREcracker/cl-mlutils"))))
(pax:register-doc-in-pax-world :mlutils 'pax-sections 'pax-pages)

(defun build-doc ()
  (progn
    (pax:update-asdf-system-readmes @mlutils-manual :net.matteolandi.utils
                                    :formats '(:markdown))
    #+#:excluded (pax:update-asdf-system-html-docs @mlutils-manual :net.matteolandi.utils
                                                   :pages (pax-pages))))
#+#:excluded (build-doc)
