(pushnew '(merge-pathnames (parse-namestring "vendor/quickutil/quickutil-utilities/")
           *default-pathname-defaults*)
         asdf:*central-registry*)
(pushnew (merge-pathnames (parse-namestring "vendor/mgl-pax/")
                          *default-pathname-defaults*)
         asdf:*central-registry*)

#+#:excluded (push (merge-pathnames (parse-namestring "vendor/")
                                    *default-pathname-defaults*)
                   ql:*local-project-directories*)


(ql:quickload '(quickutil mgl-pax/document))

(defparameter *utilities* '(

                            (@ pax:macro)
                            (aand pax:macro)
                            (aif pax:macro)
                            (alist function)
                            (alist-keys function)
                            (alist-values function)
                            (appendf pax:macro)
                            (aprog1 pax:macro)
                            (assoc-value function)
                            (awhen pax:macro)
                            (bnd* pax:macro)
                            (bnd1 pax:macro)
                            (continuable pax:macro)
                            (d-b pax:macro)
                            (dbg function)
                            (dbgl pax:macro)
                            (doalist pax:macro)
                            (doeseq pax:macro)
                            (dohash pax:macro)
                            (dohashk pax:macro)
                            (dohashv pax:macro)
                            (dolists pax:macro)
                            (dorange pax:macro)
                            (dorangei pax:macro)
                            (doseq pax:macro)
                            (doseqs pax:macro)
                            (dosublists pax:macro)
                            (enumerate generic-function)
                            (flet* pax:macro)
                            (fn pax:macro)
                            (hash-table-keys function)
                            (hash-table-values function)
                            (if-let pax:macro)
                            (if-not pax:macro)
                            (iota function)
                            (keep-if function)
                            (keep-if-not function)
                            (last-elt function)
                            (let1 pax:macro)
                            (looping pax:macro)
                            (m-v-b pax:macro)
                            (make-keyword function)
                            (mklist function)
                            (once-only pax:macro)
                            (plist-keys function)
                            (plist-values function)
                            (pmx pax:macro)
                            (pr function)
                            (prn function)
                            (prog1-let pax:macro)
                            (prs function)
                            (psx pax:macro)
                            (range function)
                            (recursively pax:macro)
                            (repeat pax:macro)
                            (retriable pax:macro)
                            (split function)
                            (split-sequence function)
                            (spr function)
                            (sprn function)
                            (sprs function)
                            (string-ends-with-p function)
                            (string-starts-with-p function)
                            (subdivide function)
                            (symb function)
                            (take function)
                            (undefclass pax:macro)
                            (undefconstant pax:macro)
                            (undefmacro pax:macro)
                            (undefmethod pax:macro)
                            (undefpackage pax:macro)
                            (undefparameter pax:macro)
                            (undefun pax:macro)
                            (undefvar pax:macro)
                            (until pax:macro)
                            (value-at generic-function)
                            (w/gensyms pax:macro)
                            (w/slots pax:macro)
                            (when-let pax:macro)
                            (when-not pax:macro)
                            (while pax:macro)
                            (while-not pax:macro)
                            (with-gensyms pax:macro)
                            (zapf pax:macro)
                            (~> pax:macro)
                            (~>> pax:macro)

                            ))

(qtlc:save-utils-as
  "mlutils.lisp"
  :utilities (mapcar (lambda (xref)
                       (intern (string-upcase (string (first xref))) :keyword))
                     *utilities*)

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


(with-open-file (outfile "mlutils-reference.lisp"
                         :direction :output
                         :if-exists :supersede)
  (prin1 '(in-package "MLUTILS") outfile)
  (terpri outfile)
  (terpri outfile)
  (prin1 `(pax:defsection @mlutils-reference (:title "Reference")
            ,@*utilities*)
         outfile))
