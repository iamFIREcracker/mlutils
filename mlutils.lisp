;;;; This file was automatically generated by Quickutil.
;;;; See http://quickutil.org for details.

;;;; To regenerate:
;;;; (qtlc:save-utils-as "mlutils.lisp" :utilities '(:BND* :BND1 :D-B :DOLIST+ :DOLISTS :DORANGE :DORANGEI :DOSEQ :FLET* :FN :IF-NOT :IOTA :KEEP-IF :KEEP-IF-NOT :LOOPING :M-V-B :MKLIST :ONCE-ONLY :RANGE :RECURSIVELY :SPLIT-SEQUENCE :SYMB :UNTIL :WHILE :WITH-GENSYMS) :categories '(:ANAPHORIC :PRINTING) :ensure-package T :package "MLUTILS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "MLUTILS")
    (defpackage "MLUTILS"
      (:documentation "Package that contains Quickutil utility functions.")
      (:use #:cl))))

(in-package "MLUTILS")

(when (boundp '*utilities*)
  (setf *utilities* (union *utilities* '(:BND* :BND1 :ABBR :D-B :DOLIST+
                                         :DOLISTS :DORANGE :DORANGEI :DOSEQ
                                         :FLET* :FN :IF-NOT :IOTA :KEEP-IF
                                         :KEEP-IF-NOT :MKSTR :SYMB
                                         :STRING-DESIGNATOR :WITH-GENSYMS
                                         :LOOPING :M-V-B :MKLIST
                                         :MAKE-GENSYM-LIST :ONCE-ONLY :RANGE
                                         :LET1 :RECURSIVELY :SPLIT-SEQUENCE
                                         :UNTIL :WHILE :AIF :AAND :AWHEN :SPRS
                                         :SPRN :SPR :PRS :PRN :PR))))

  (defmacro bnd* (bindings &body body)
    "Like LET*, but more powerful.

Use a symbol as the name of the binding to expand to a standard LET:

(bnd* (x
       (y (list 1 2 3)))
  (list x y)) ≡
(let (x)
  (let ((y (list 1 2 3)))
    (list x y)))

Use a list as the name of the binding to enable special type of expansions.

If the CAR of the list is the symbol VALUES, expand to MULTIPLE-VALUE-BIND
call:

(bnd* (((values f r) (floor 130 11)))
  (list f r)) ≡
(multiple-value-bind (f r)
     (floor 130 11)
   (list f r))

If the CAR of the list is the symbol WITH-SLOTS, expand to a WITH-SLOTS call:

(bnd* (((with-slots x y) thing))
  (incf x) (incf y))
≡
(with-slots (x y) thing
  (incf x) (incf y))

Otherwise, if the name of the binding is a list but none of the above applies,
BND* will expand to a DESTRUCTURING-BIND call:

(bnd* (((a b) '(1 2)))
  (list a b))
≡
(destructuring-bind (a b)
    '(1 2)
  (list a b))"
    (labels ((mklist (x) (if (atom x) (list x) x))
             (expand (bb)
               (cond ((null bb) (signal 'unexpected))
                     (t (let* ((b (mklist (car bb)))
                               (var (car b))
                               (val (cadr b)))
                          (cond ((symbolp var)
                                 `(let (,b)
                                    ,@(if (rest bb)
                                        (list (expand (rest bb)))
                                        body)))
                                ((eq (car var) 'values)
                                 `(multiple-value-bind ,(rest var) ,val
                                    ,@(if (rest bb)
                                        (list (expand (rest bb)))
                                        body)))
                                ((eq (car var) 'with-slots)
                                 `(with-slots ,(rest var) ,val
                                    ,@(if (rest bb)
                                        (list (expand (rest bb)))
                                        body)))
                                (t `(destructuring-bind ,@b
                                      ,@(if (rest bb)
                                          (list (expand (rest bb)))
                                          body)))))))))
      (expand bindings)))
  

  (defmacro bnd1 (binding &body body)
    "Equivalent to BND* with one binding."
    `(bnd* (,binding)
       ,@body))
  

  (defmacro abbr (short long)
    "Defines a new function/macro named `short` and sharing
FDEFINITION/MACRO-FUNCTION with `long`."
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (cond
         ((macro-function ',long)
          (setf (macro-function ',short) (macro-function ',long))
          #+ccl (setf (ccl:arglist ',short) (ccl:arglist ',long)))
         ((fboundp ',long)
          (setf (fdefinition ',short) (fdefinition ',long))
          #+ccl (setf (ccl:arglist ',short) (ccl:arglist ',long)))
         (t
           (error "Can't abbreviate ~a" ',long)))))
  
  (abbr d-b destructuring-bind)

  (defmacro dolist+ ((var list &optional (result nil result?)) &body body)
    "Like DOLIST, except it supports destructuring of `var`.

  > (let ((list '((1 a) (2 b))))
      (dolist+ ((a b) list :ret)
        (print (list a b))))
  ;;(1 A)
  ;;(2 B)
  :RET
  "
    `(loop :for ,var :in ,list do ,@body ,@(when result? `(:finally (return ,result)))))
  

  (defmacro dolists (((var1 list1) (var2 list2) &rest var-list-specs) &body body)
    "Like DOLIST, except it allows you to iterate over multiple lists in parallel.

  > (let ((list '(1 2 3 4)))
      (dolists ((x1 list)
                (x2 (cdr list)))
        (print (list x1 x2))))
  ;; (1 2)
  ;; (2 3)
  ;; (3 4)
  NIL
  "
    `(loop
       :for ,var1 :in ,list1 :for ,var2 :in ,list2
       ,@(loop for (var list) in var-list-specs
               collect 'FOR collect var collect 'IN collect list)
       do ,@body))
  

  (defmacro dorange ((var from to &optional (step 1) (result nil result?)) &body body)
    "Binds `var` to all the distinct values in the range [`from`, `to`[, with
`step` step (note: `to` is excluded), and runs `body` inside that
lexical environmnet."
    (let ((step-g (gensym "step"))
          (to-g (gensym "to")))
      `(do* ((,step-g ,step)
             (,to-g ,to)
             (,var ,from (+ ,var ,step-g)))
         ((if (>= ,step-g 0) (>= ,var ,to-g) (<= ,var ,to-g))
          ,@(when result? `(,result)))
         ,@body)))
  

  (defmacro dorangei ((var from to &optional (step 1) (result nil result?)) &body body)
    "Like DORANGE, `to` is inclusive (the range is: [`from`, `to`])."
    (let ((step-g (gensym "step"))
          (to-g (gensym "to")))
      `(do* ((,step-g ,step)
             (,to-g ,to)
             (,var ,from (+ ,var ,step-g)))
         ((if (>= ,step-g 0) (> ,var ,to-g) (< ,var ,to-g))
          ,@(when result? `(,result)))
         ,@body)))
  

  (defmacro doseq ((var seq &optional return) &body body)
    "Iterate across the sequence `seq`, binding the variable `var` to
each element of the sequence and executing `body`. Return the value
`return` from the iteration form."
    `(block nil
       (map nil #'(lambda (,var)
                    (tagbody
                       ,@body))
            ,seq)
       ,return))
  

  (defmacro flet* (&rest body)
    "Like LABELS, but 1 character shorter.
Also, FLET* is to FLET what LET* is to LET.

Note: cannot use ABBR for this, because LABELS is a special operator."
    `(labels ,@body))
  

  (defmacro fn (name lambda-list &body body)
    "Like LAMBDA, but 4 characters shorter."
    `(lambda ,name ,lambda-list ,@body))
  

  (defmacro if-not (test then &optional else)
    "Like IF, except TEST gets wrapped inside NOT."
    `(if (not ,test) ,then ,else))
  

  (declaim (inline iota))
  (defun iota (n &key (start 0) (step 1))
    "Return a list of `n` numbers, starting from `start` (with numeric contagion
from `step` applied), each consequtive number being the sum of the previous one
and `step`. `start` defaults to `0` and `step` to `1`.

Examples:

    (iota 4)                      => (0 1 2 3)
    (iota 3 :start 1 :step 1.0)   => (1.0 2.0 3.0)
    (iota 3 :start -1 :step -1/2) => (-1 -3/2 -2)"
    (declare (type (integer 0) n) (number start step))
    (loop repeat n
          ;; KLUDGE: get numeric contagion right for the first element too
          for i = (+ (- (+ start step) step)) then (+ i step)
          collect i))
  
  (abbr keep-if remove-if-not)
  (abbr keep-if-not remove-if)

  (defun mkstr (&rest args)
    "Receives any number of objects (string, symbol, keyword, char, number), extracts all printed representations, and concatenates them all into one string.

Extracted from _On Lisp_, chapter 4."
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))
  

  (defun symb (&rest args)
    "Receives any number of objects, concatenates all into one string with `#'mkstr` and converts them to symbol.

Extracted from _On Lisp_, chapter 4.

See also: `symbolicate`"
    (values (intern (apply #'mkstr args))))
  

  (deftype string-designator ()
    "A string designator type. A string designator is either a string, a symbol,
or a character."
    `(or symbol string character))
  

  (defmacro with-gensyms (names &body forms)
    "Binds each variable named by a symbol in `names` to a unique symbol around
`forms`. Each of `names` must either be either a symbol, or of the form:

    (symbol string-designator)

Bare symbols appearing in `names` are equivalent to:

    (symbol symbol)

The string-designator is used as the argument to `gensym` when constructing the
unique symbol the named variable will be bound to."
    `(let ,(mapcar (lambda (name)
                     (multiple-value-bind (symbol string)
                         (etypecase name
                           (symbol
                            (values name (symbol-name name)))
                           ((cons symbol (cons string-designator null))
                            (values (first name) (string (second name)))))
                       `(,symbol (gensym ,string))))
            names)
       ,@forms))

  (defmacro with-unique-names (names &body forms)
    "Binds each variable named by a symbol in `names` to a unique symbol around
`forms`. Each of `names` must either be either a symbol, or of the form:

    (symbol string-designator)

Bare symbols appearing in `names` are equivalent to:

    (symbol symbol)

The string-designator is used as the argument to `gensym` when constructing the
unique symbol the named variable will be bound to."
    `(with-gensyms ,names ,@forms))
  

  (defmacro looping (&body body)
    "Run `body` in an environment where the symbols COLLECT!, SUM!, COUNT!, MIN!
and MAX! are bound to functions that can be used to collect, sum, count,
minimize or maximize things respectively.

Mixed usage of COLLECT!, SUM!, COUNT!, MIN! and MAX! is not supported.

Examples:

  (looping
    (dotimes (i 5)
      (if (oddp i)
        (collect! i))))
  =>
  (1 3)

  (looping
    (dotimes (i 5)
      (if (oddp i)
        (sum! i))))
  =>
  4

  (looping
    (dotimes (i 5)
      (count! (oddp i))))
  =>
  2

  (looping
    (dotimes (i 5)
      (sum! i)
      (count! (oddp i))))
  ;; Signals an ERROR: Cannot use COUNT! together with SUM!
  "
    (with-gensyms (loop-type result)
      `(let (,loop-type ,result)
         (flet ((,(symb "COLLECT!") (item)
                 (if (and ,loop-type (not (eql ,loop-type 'collect!)))
                   (error "Cannot use COLLECT! together with ~A" ,loop-type)
                   (progn
                     (if (not ,loop-type)
                       (setf ,loop-type 'collect! ,result nil))
                     (push item ,result)
                     item)))
                (,(symb "SUM!") (item)
                 (if (and ,loop-type (not (eql ,loop-type 'sum!)))
                   (error "Cannot use SUM! together with ~A" ,loop-type)
                   (progn
                     (if (not ,loop-type)
                       (setf ,loop-type 'sum! ,result 0))
                     (incf ,result item)
                     item)))
                (,(symb "COUNT!") (item)
                 (if (and ,loop-type (not (eql ,loop-type 'count!)))
                   (error "Cannot use COUNT! together with ~A" ,loop-type)
                   (progn
                     (if (not ,loop-type)
                       (setf ,loop-type 'count! ,result 0))
                     (when item
                       (incf ,result)
                       item))))
                (,(symb "MIN!") (item)
                 (if (and ,loop-type (not (eql ,loop-type 'min!)))
                   (error "Cannot use MIN! together with ~A" ,loop-type)
                   (progn
                     (if (not ,loop-type)
                       (setf ,loop-type 'min! ,result item))
                     (setf ,result (min ,result item)))))
                (,(symb "MAX!") (item)
                 (if (and ,loop-type (not (eql ,loop-type 'max!)))
                   (error "Cannot use MAX! together with ~A" ,loop-type)
                   (progn
                     (if (not ,loop-type)
                       (setf ,loop-type 'max! ,result item))
                     (setf ,result (max ,result item))))))
           ,@body)
         (if (eq ,loop-type 'collect!)
           (nreverse ,result)
           ,result))))
  
  (abbr m-v-b multiple-value-bind)

  (defun mklist (obj)
    "If not already a list, mklist will return a
   new list with its param as element"
    (if (listp obj)
      obj
      (list obj)))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-gensym-list (length &optional (x "G"))
    "Returns a list of `length` gensyms, each generated as if with a call to `make-gensym`,
using the second (optional, defaulting to `\"G\"`) argument."
    (let ((g (if (typep x '(integer 0)) x (string x))))
      (loop repeat length
            collect (gensym g))))
  )                                        ; eval-when

  (defmacro once-only (specs &body forms)
    "Evaluates `forms` with symbols specified in `specs` rebound to temporary
variables, ensuring that each initform is evaluated only once.

Each of `specs` must either be a symbol naming the variable to be rebound, or of
the form:

    (symbol initform)

Bare symbols in `specs` are equivalent to

    (symbol symbol)

Example:

    (defmacro cons1 (x) (once-only (x) `(cons ,x ,x)))
      (let ((y 0)) (cons1 (incf y))) => (1 . 1)"
    (let ((gensyms (make-gensym-list (length specs) "ONCE-ONLY"))
          (names-and-forms (mapcar (lambda (spec)
                                     (etypecase spec
                                       (list
                                        (destructuring-bind (name form) spec
                                          (cons name form)))
                                       (symbol
                                        (cons spec spec))))
                                   specs)))
      ;; bind in user-macro
      `(let ,(mapcar (lambda (g n) (list g `(gensym ,(string (car n)))))
              gensyms names-and-forms)
         ;; bind in final expansion
         `(let (,,@(mapcar (lambda (g n)
                             ``(,,g ,,(cdr n)))
                           gensyms names-and-forms))
            ;; bind in user-macro
            ,(let ,(mapcar (lambda (n g) (list (car n) g))
                    names-and-forms gensyms)
               ,@forms)))))
  

  (defun range (start end &key (step 1) (key 'identity))
    "Return the list of numbers `n` such that `start <= n < end` and
`n = start + k*step` for suitable integers `k`. If a function `key` is
provided, then apply it to each number."
    (assert (<= start end))
    (loop :for i :from start :below end :by step :collecting (funcall key i)))
  

  (defmacro let1 (var val &body body)
    "Bind VAR to VAL within BODY. Equivalent to LET with one binding."
    `(let ((,var ,val))
       ,@body))
  

  (defmacro recursively (bindings &body body)
    (let ((names (mapcar #'(lambda (b) (if (atom b) b (first b))) bindings))
          (values (mapcar #'(lambda (b) (if (atom b) nil (second b))) bindings)))
      (let1 recur (intern "RECUR")
        `(labels ((,recur (,@names)
                    ,@body))
           (,recur ,@values)))))
  

  (defun split-from-end (position-fn sequence start end count remove-empty-subseqs)
    (loop
      :for right := end :then left
      :for left := (max (or (funcall position-fn sequence right) -1)
                        (1- start))
      :unless (and (= right (1+ left))
                   remove-empty-subseqs) ; empty subseq we don't want
        :if (and count (>= nr-elts count))
          ;; We can't take any more. Return now.
          :return (values (nreverse subseqs) right)
      :else
        :collect (subseq sequence (1+ left) right) into subseqs
        :and :sum 1 :into nr-elts
      :until (< left start)
      :finally (return (values (nreverse subseqs) (1+ left)))))

  (defun split-from-start (position-fn sequence start end count remove-empty-subseqs)
    (let ((length (length sequence)))
      (loop
        :for left := start :then (+ right 1)
        :for right := (min (or (funcall position-fn sequence left) length)
                           end)
        :unless (and (= right left)
                     remove-empty-subseqs) ; empty subseq we don't want
          :if (and count (>= nr-elts count))
            ;; We can't take any more. Return now.
            :return (values subseqs left)
        :else
          :collect (subseq sequence left right) :into subseqs
          :and :sum 1 :into nr-elts
        :until (>= right end)
        :finally (return (values subseqs right)))))
  
  (macrolet ((check-bounds (sequence start end)
               (let ((length (gensym (string '#:length))))
                 `(let ((,length (length ,sequence)))
                    (check-type ,start unsigned-byte "a non-negative integer")
                    (when ,end (check-type ,end unsigned-byte "a non-negative integer or NIL"))
                    (unless ,end
                      (setf ,end ,length))
                    (unless (<= ,start ,end ,length)
                      (error "Wrong sequence bounds. start: ~S end: ~S" ,start ,end))))))

    (defun split-sequence (delimiter sequence &key (start 0) (end nil) (from-end nil)
                                                   (count nil) (remove-empty-subseqs nil)
                                                   (test #'eql) (test-not nil) (key #'identity))
      "Return a list of subsequences in seq delimited by delimiter.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
      (check-bounds sequence start end)
      (cond
        ((and (not from-end) (null test-not))
         (split-from-start (lambda (sequence start)
                             (position delimiter sequence :start start :key key :test test))
                           sequence start end count remove-empty-subseqs))
        ((and (not from-end) test-not)
         (split-from-start (lambda (sequence start)
                             (position delimiter sequence :start start :key key :test-not test-not))
                           sequence start end count remove-empty-subseqs))
        ((and from-end (null test-not))
         (split-from-end (lambda (sequence end)
                           (position delimiter sequence :end end :from-end t :key key :test test))
                         sequence start end count remove-empty-subseqs))
        ((and from-end test-not)
         (split-from-end (lambda (sequence end)
                           (position delimiter sequence :end end :from-end t :key key :test-not test-not))
                         sequence start end count remove-empty-subseqs))))

    (defun split-sequence-if (predicate sequence &key (start 0) (end nil) (from-end nil)
                                                      (count nil) (remove-empty-subseqs nil) (key #'identity))
      "Return a list of subsequences in seq delimited by items satisfying
predicate.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
      (check-bounds sequence start end)
      (if from-end
          (split-from-end (lambda (sequence end)
                            (position-if predicate sequence :end end :from-end t :key key))
                          sequence start end count remove-empty-subseqs)
          (split-from-start (lambda (sequence start)
                              (position-if predicate sequence :start start :key key))
                            sequence start end count remove-empty-subseqs)))

    (defun split-sequence-if-not (predicate sequence &key (count nil) (remove-empty-subseqs nil)
                                                          (from-end nil) (start 0) (end nil) (key #'identity))
      "Return a list of subsequences in seq delimited by items satisfying
\(CL:COMPLEMENT predicate).

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF-NOT.  In particular,
the behaviour of :from-end is possibly different from other versions
of this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
      (check-bounds sequence start end)
      (if from-end
          (split-from-end (lambda (sequence end)
                            (position-if-not predicate sequence :end end :from-end t :key key))
                          sequence start end count remove-empty-subseqs)
          (split-from-start (lambda (sequence start)
                              (position-if-not predicate sequence :start start :key key))
                            sequence start end count remove-empty-subseqs))))
  

  (defmacro until (expression &body body)
    "Executes `body` until `expression` is true."
    `(do ()
         (,expression)
       ,@body))
  

  (defmacro while (expression &body body)
    "Executes `body` while `expression` is true."
    `(loop while ,expression do
       ,@body))
  

  (defmacro aif (test then &optional else)
    "Like IF, except binds the result of `test` to IT (via LET) for the scope of `then` and `else` expressions."
    (aif-expand test then else))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun aif-expand (test then &optional else)
      (let1 it (intern "IT")
        `(let1 ,it ,test
           (if ,it ,then ,else)))))
  

  (defmacro aand (&rest forms)
    "Like AND, except binds the result of each form to IT (via LET)."
    (aand-expand forms))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun aand-expand (forms)
      (cond ((not (car forms)) nil)
            ((not (cdr forms)) (car forms))
            (t (let1 car (car forms)
                 `(aif ,car
                    (aand ,@(cdr forms))))))))
  

  (defmacro awhen (test &body body)
    "Like WHEN, except binds the result of `test` to IT (via LET) for the scope of `body`."
    (awhen-expand test body))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun awhen-expand (test body)
      (let1 it (intern "IT")
        `(let1 ,it ,test
           (when ,it
             ,@body)))))
  

  (defun sprs (&rest args)
    "Print all its `args` into a string, separated by a space, and return it."
    (format nil "~{~A~^ ~}" args))
  

  (defun sprn (&rest args)
    "Prints all its `args` into a string, separated by a newline, and return it."
    (format nil "~{~A~^~%~}" args))
  

  (defun spr (&rest args)
    "Prints all its `args` into a string, and return it."
    (format nil "~{~A~}" args))
  

  (defun prs (&rest args)
    "Print all its `args` to screen, separated by a space. Returns the first arg."
    (format t "~{~A~^ ~}" args)
    (finish-output)
    (first args))
  

  (defun prn (&rest args)
    "Prints all its `args` to screen, separated by a newline. Returns the first arg."
    (format t "~{~A~^~%~}" args)
    (finish-output)
    (first args))
  

  (defun pr (&rest args)
    "Prints all its `args` to screen. Returns the first arg."
    (format t "~{~A~}" args)
    (finish-output)
    (first args))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(bnd* bnd1 d-b dolist+ dolists dorange dorangei doseq flet* fn
            if-not iota keep-if keep-if-not looping m-v-b mklist once-only
            range recursively split-sequence split-sequence-if
            split-sequence-if-not symb until while with-gensyms
            with-unique-names aand awhen aif sprs sprn spr prs prn pr)))

;;;; END OF mlutils.lisp ;;;;
