(pushnew '(merge-pathnames (parse-namestring "vendor/quickutil/quickutil-utilities/")
           *default-pathname-defaults*)
         asdf:*central-registry*)

(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "mlutils.lisp"
  :utilities '(

               :bnd*
               :bnd1
               :fn
               :if-not
               :iota
               :mklist
               :once-only
               :range
               :recursively
               :split-sequence
               :symb
               :until

               )

  :categories '(:anaphoric :printing)
  :package "MLUTILS")


(with-open-file (outfile "mlutils-package.lisp"
                         :direction :output
                         :if-exists :supersede)
  (prin1 '(eval-when (:compile-toplevel :load-toplevel :execute)
           (unless (find-package "MLUTILS")
             (defpackage "MLUTILS"
               (:documentation "Package that contains Quickutil utility functions.")
               (:use :cl))))
         outfile)
  (terpri outfile)
  (prin1 '(in-package "MLUTILS") outfile)
  (terpri outfile)
  (terpri outfile)
  ;; need to define this here so sbcl will shut the hell up about it being
  ;; undefined when compiling mlutils.lisp.  computers are trash.
  (prin1 '(defparameter *utilities* nil) outfile)
  )
