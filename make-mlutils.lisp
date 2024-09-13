(pushnew '(merge-pathnames (parse-namestring "vendor/quickutil/quickutil-utilities/")
           *default-pathname-defaults*)
         asdf:*central-registry*)

(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "mlutils.lisp"
  :utilities '(

               :@
               :aand
               :aif
               :alist-keys
               :alist-values
               :appendf
               :aprog1
               :assoc-value
               :awhen
               :bnd*
               :bnd1
               :continuable
               :d-b
               :dbg
               :dbgl
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
               :make-keyword
               :mklist
               :once-only
               :plist-keys
               :plist-values
               :pmx
               :pr
               :prn
               :prs
               :range
               :recursively
               :retriable
               :split
               :split-sequence
               :spr
               :sprn
               :sprs
               :string-ends-with-p
               :string-starts-with-p
               :subdivide
               :symb
               :undefclass
               :undefconstant
               :undefmacro
               :undefmethod
               :undefpackage
               :undefparameter
               :undefun
               :undefvar
               :until
               :w/gensyms
               :w/slots
               :when-let
               :while
               :with-gensyms
               :~>
               :~>>

               )

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
