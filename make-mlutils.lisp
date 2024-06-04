(pushnew '(merge-pathnames (parse-namestring "vendor/quickutil/quickutil-utilities/")
           *default-pathname-defaults*)
         asdf:*central-registry*)

(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "mlutils.lisp"
  :utilities '(

               :@
               :alist-keys
               :alist-values
               :appendf
               :assoc-value
               :bnd*
               :bnd1
               :d-b
               :dolists
               :dorange
               :dorangei
               :doseq
               :doseq
               :flet*
               :fn
               :if-let
               :if-not
               :iota
               :keep-if
               :keep-if-not
               :last-elt
               :let1
               :looping
               :m-v-b
               :mklist
               :once-only
               :plist-keys
               :plist-values
               :pmx1
               :range
               :recursively
               :split-sequence
               :string-ends-with-p
               :string-starts-with-p
               :subdivide
               :symb
               :undefun
               :undefmacro
               :undefvar
               :undefparameter
               :undefconstant
               :undefpackage
               :undefclass
               :undefmethod
               :until
               :w/gensyms
               :w/slots
               :when-let
               :while
               :with-gensyms
               :~>

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
               (:use :cl :named-readtables))))
         outfile)
  (terpri outfile)
  (prin1 '(in-package "MLUTILS") outfile)
  (terpri outfile)
  (terpri outfile)
  ;; need to define this here so sbcl will shut the hell up about it being
  ;; undefined when compiling mlutils.lisp.  computers are trash.
  (prin1 '(defparameter *utilities* nil) outfile)
  )
