(pushnew (merge-pathnames (parse-namestring "vendor/mgl-pax/")
                          *default-pathname-defaults*)
         asdf:*central-registry*)

(push (merge-pathnames (parse-namestring "vendor/")
                       *default-pathname-defaults*)
      ql:*local-project-directories*)

;;; See MLUTILS::@MLUTILS-MANUAL for the user guide.
(asdf:defsystem #:net.matteolandi.utils
  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"
  :serial t
  :depends-on (
                 #:assoc-utils
                 #:named-readtables
                 #:pythonic-string-reader

                 #:mgl-pax/document
               )
  :components
  (
   (:file "mlutils-package")
   (:file "mlsyntax")
   (:file "mlutils")

   (:file "doc")
   (:file "mlutils-reference")
   )
  )


(asdf:defsystem #:net.matteolandi.utils/doc
  :depends-on (

               #:net.matteolandi.utils

               #:mgl-pax/navigate

               )
  :perform (build-op (o c) (uiop:symbol-call :mlutils '#:build-doc)))
