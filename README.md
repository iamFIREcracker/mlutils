<a id="x-28MLUTILS-3A-40MLUTILS-MANUAL-20MGL-PAX-3ASECTION-29"></a>

# cl-mlutils

## Table of Contents

- [1 Reference][44b5]

###### \[in package MLUTILS\]
Personal list of utils to make Common Lisp feel a bit more like home

<a id="x-28MLUTILS-3A-40MLUTILS-REFERENCE-20MGL-PAX-3ASECTION-29"></a>

## 1 Reference

<a id="x-28MLUTILS-3A-40-20MGL-PAX-3AMACRO-29"></a>

- [macro] **@** *X &REST PLACES*

<a id="x-28MLUTILS-3AAAND-20MGL-PAX-3AMACRO-29"></a>

- [macro] **AAND** *&REST FORMS*

    Like `AND`([`0`][425d] [`1`][dd55]), except binds the result of each form to IT (via [`LET`][4853]).

<a id="x-28MLUTILS-3AAIF-20MGL-PAX-3AMACRO-29"></a>

- [macro] **AIF** *TEST THEN &OPTIONAL ELSE*

    Like [`IF`][02ad], except binds the result of `test` to IT (via [`LET`][4853]) for the scope of `then` and `else` expressions.

<a id="x-28MLUTILS-3AALIST-KEYS-20FUNCTION-29"></a>

- [function] **ALIST-KEYS** *ALIST*

    Return all the keys of `alist`.

<a id="x-28MLUTILS-3AALIST-VALUES-20FUNCTION-29"></a>

- [function] **ALIST-VALUES** *ALIST*

    Return all the values of `alist`.

<a id="x-28MLUTILS-3AAPPENDF-20MGL-PAX-3AMACRO-29"></a>

- [macro] **APPENDF** *PLACE &REST LISTS*

    Modify-macro for [`append`][d675]. Appends `lists` to the place designated by the first
    argument.

<a id="x-28MLUTILS-3AAPROG1-20MGL-PAX-3AMACRO-29"></a>

- [macro] **APROG1** *RESULT-FORM &BODY BODY*

    Like [`PROG1`][2053], except binds the result of the `result-form` (i.e., the returned
    form) to IT (via [`LET`][4853]) for the scope of `body`.
    
    Inspired by ActiveSupport: Object#returning
    https://weblog.jamisbuck.org/2006/10/27/mining-activesupport-object-returning.html

<a id="x-28MLUTILS-3AASSOC-VALUE-20FUNCTION-29"></a>

- [function] **ASSOC-VALUE** *ALIST KEY &KEY (TEST 'EQUAL)*

    `ASSOC-VALUE` is an alist accessor very much like [`ASSOC`][e5fc], but it can
    be used with [`SETF`][a138].

<a id="x-28MLUTILS-3AAWHEN-20MGL-PAX-3AMACRO-29"></a>

- [macro] **AWHEN** *TEST &BODY BODY*

    Like [`WHEN`][baaf], except binds the result of `test` to IT (via [`LET`][4853]) for the scope of `body`.

<a id="x-28MLUTILS-3ABND-2A-20MGL-PAX-3AMACRO-29"></a>

- [macro] **BND\*** *BINDINGS &BODY BODY*

    Like [`LET*`][49f5], but more powerful.
    
    Use a symbol as the name of the binding to expand to a standard [`LET`][4853]:
    
    (bnd\* (x
           (y (list 1 2 3)))
      (list x y)) ≡
    (let (x)
      (let ((y (list 1 2 3)))
        (list x y)))
    
    Use a list as the name of the binding to enable special type of expansions.
    
    If the [`CAR`][d5a2] of the list is the symbol `VALUES`([`0`][fc69] [`1`][eef3]), expand to [`MULTIPLE-VALUE-BIND`][625d]
    call:
    
    (bnd\* (((values f r) (floor 130 11)))
      (list f r)) ≡
    (multiple-value-bind (f r)
         (floor 130 11)
       (list f r))
    
    If the `CAR` of the list is the symbol [`WITH-SLOTS`][7de6], expand to a `WITH-SLOTS` call:
    
    (bnd\* (((with-slots x y) thing))
      (incf x) (incf y))
    ≡
    (with-slots (x y) thing
      (incf x) (incf y))
    
    Otherwise, if the name of the binding is a list but none of the above applies,
    `BND*` will expand to a [`DESTRUCTURING-BIND`][b105] call:
    
    (bnd\* (((a b) '(1 2)))
      (list a b))
    ≡
    (destructuring-bind (a b)
        '(1 2)
      (list a b))

<a id="x-28MLUTILS-3ABND1-20MGL-PAX-3AMACRO-29"></a>

- [macro] **BND1** *VAR VAL &BODY BODY*

    `BND1` is to [`BND*`][f029] like [`LET1`][8b63] is to [`LET*`][49f5].

<a id="x-28MLUTILS-3ACONTINUABLE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **CONTINUABLE** *&BODY BODY*

    Wraps `body` in a [`RESTART-CASE`][a458] with a [`CONTINUE`][1867] restart. When invoked, the
    restart will simply return `NIL`, allowing the program to continue execution.
    
    Returns the value of the last form in body, or `NIL` if the [`CONTINUE`][1867] restart is
    invoked.
    
    By default, the message reported by the restart case will be "Continue.".
    This can be overridden by providing a :report form as the first element of the
    body.
    
    Examples:
    
    ;; Basic usage
      (continuable
        (format t "This might fail~%")
        (/ 1 0))
    
      ;; With custom report message
      (continuable
        (:report "Ignore division by zero and continue")
        (format t "This might fail~%")
        (/ 1 0))

<a id="x-28MLUTILS-3AD-B-20MGL-PAX-3AMACRO-29"></a>

- [macro] **D-B** *LAMBDA-LIST EXPRESSION &BODY BODY*

    Bind the variables in `LAMBDA-LIST` to the corresponding values in the
    tree structure resulting from the evaluation of `EXPRESSION`.

<a id="x-28MLUTILS-3ADBG-20FUNCTION-29"></a>

- [function] **DBG** *&REST ARGS*

    Print `args` to screen, separated by a space, and followed by a newline.
    Returns the first arg.

<a id="x-28MLUTILS-3ADBGL-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DBGL** *&REST ARGS*

    Print `args`, labeled, separated by a newline, and followed by a final
    newline.  Returns the last arg. labeled and readably.

<a id="x-28MLUTILS-3ADOLISTS-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DOLISTS** *((VAR1 LIST1) (VAR2 LIST2) &REST VAR-LIST-SPECS) &BODY BODY*

    Like [`DOLIST`][1bfd], except it allows you to iterate over multiple lists in parallel.
    
    > (let ((list '(1 2 3 4)))
    >     (dolists ((x1 list)
    >               (x2 (cdr list)))
    >       (print (list x1 x2))))
    > ;; (1 2)
    > ;; (2 3)
    > ;; (3 4)
    > `NIL`


<a id="x-28MLUTILS-3ADORANGE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DORANGE** *(VAR FROM TO &OPTIONAL (STEP 1) (RESULT NIL)) &BODY BODY*

    Binds `var` to all the distinct values in the range \[`from`, `to`\[, with
    `step` step (note: `to` is excluded), and runs `body` inside that
    lexical environmnet.

<a id="x-28MLUTILS-3ADORANGEI-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DORANGEI** *(VAR FROM TO &OPTIONAL (STEP 1) (RESULT NIL)) &BODY BODY*

    Like [`DORANGE`][6428], `to` is inclusive (the range is: \[`from`, `to`\]).

<a id="x-28MLUTILS-3ADOSEQ-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DOSEQ** *(VAR SEQ &OPTIONAL (RESULT NIL)) &BODY BODY*

    Iterate across the sequence `seq`, binding the variable `var` to
    each element of the sequence and executing `body`. Return the value
    [`return`][5b0b] from the iteration form.
    
    Note: `DOSEQ` expands to a [`LOOP`][30e1] form, so `var` can either be a symbol, or a
    lambda-list

<a id="x-28MLUTILS-3AFLET-2A-20MGL-PAX-3AMACRO-29"></a>

- [macro] **FLET\*** *&REST BODY*

    Like [`LABELS`][c2ef], but 1 character shorter.
    Also, `FLET*` is to [`FLET`][091c] what [`LET*`][49f5] is to [`LET`][4853].
    
    Note: cannot use `ABBR` for this, because `LABELS` is a special operator.

<a id="x-28MLUTILS-3AFN-20MGL-PAX-3AMACRO-29"></a>

- [macro] **FN** *NAME LAMBDA-LIST &BODY BODY*

    Like `LAMBDA`([`0`][e400] [`1`][5c01]), but 4 characters shorter.

<a id="x-28MLUTILS-3AIF-LET-20MGL-PAX-3AMACRO-29"></a>

- [macro] **IF-LET** *BINDINGS &BODY (THEN-FORM &OPTIONAL ELSE-FORM)*

    Creates new variable bindings, and conditionally executes either
    `then-form` or `else-form`. `else-form` defaults to `nil`.
    
    `bindings` must be either single binding of the form:
    
        (variable initial-form)
    
    or a list of bindings of the form:
    
        ((variable-1 initial-form-1)
         (variable-2 initial-form-2)
         ...
         (variable-n initial-form-n))
    
    All initial-forms are executed sequentially in the specified order. Then all
    the variables are bound to the corresponding values.
    
    If all variables were bound to true values, the `then-form` is executed with the
    bindings in effect, otherwise the `else-form` is executed with the bindings in
    effect.

<a id="x-28MLUTILS-3AIF-NOT-20MGL-PAX-3AMACRO-29"></a>

- [macro] **IF-NOT** *TEST THEN &OPTIONAL ELSE*

    Like [`IF`][02ad], except `TEST` gets wrapped inside `NOT`([`0`][1013] [`1`][954a]).

<a id="x-28MLUTILS-3AIOTA-20FUNCTION-29"></a>

- [function] **IOTA** *N &KEY (START 0) (STEP 1)*

    Return a list of `n` numbers, starting from `start` (with numeric contagion
    from `step` applied), each consequtive number being the sum of the previous one
    and `step`. `start` defaults to `0` and `step` to `1`.
    
    Examples:
    
        (iota 4)                      => (0 1 2 3)
        (iota 3 :start 1 :step 1.0)   => (1.0 2.0 3.0)
        (iota 3 :start -1 :step -1/2) => (-1 -3/2 -2)


<a id="x-28MLUTILS-3AKEEP-IF-20FUNCTION-29"></a>

- [function] **KEEP-IF** *PREDICATE SEQUENCE &REST ARGS &KEY FROM-END (START 0) (END NIL) (COUNT NIL) (KEY NIL)*

    Return a copy of sequence with elements not satisfying `PREDICATE` removed.

<a id="x-28MLUTILS-3AKEEP-IF-NOT-20FUNCTION-29"></a>

- [function] **KEEP-IF-NOT** *PREDICATE SEQUENCE &REST ARGS &KEY FROM-END (START 0) (END NIL) (COUNT NIL) (KEY NIL)*

    Return a copy of sequence with elements satisfying `PREDICATE` removed.

<a id="x-28MLUTILS-3ALAST-ELT-20FUNCTION-29"></a>

- [function] **LAST-ELT** *SEQUENCE*

    Returns the last element of `SEQUENCE`. Signals a type-error if `SEQUENCE` is
    not a proper sequence, or is an empty sequence.

<a id="x-28MLUTILS-3ALET1-20MGL-PAX-3AMACRO-29"></a>

- [macro] **LET1** *VAR VAL &BODY BODY*

    Bind `VAR` to `VAL` within `BODY`. Equivalent to [`LET`][4853] with one binding.

<a id="x-28MLUTILS-3ALOOPING-20MGL-PAX-3AMACRO-29"></a>

- [macro] **LOOPING** *&BODY BODY*

    Run `body` in an environment where the symbols `COLLECT!`, `APPEND!`, `ADJOIN!`,
    `SUM!`, `MULTIPLY!`, `COUNT!`, `MINIMIZE!`, and `MAXIMIZE!` are bound to functions that
    can be used to collect / append, sum, multiply, count, minimize or maximize
    things respectively.
    
    Mixed usage of COLLECT!/APPEND!/ADJOIN!, `SUM!`, `MULTIPLY!`, `COUNT!`, `MINIMIZE!` and
    `MAXIMIZE!` is not supported.
    
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
      ;; Signals an `ERROR`([`0`][d162] [`1`][35ba]): Cannot use `COUNT!` together with `SUM!`
      

<a id="x-28MLUTILS-3AM-V-B-20MGL-PAX-3AMACRO-29"></a>

- [macro] **M-V-B** *VARS VALUE-FORM &BODY BODY*

<a id="x-28MLUTILS-3AMAKE-KEYWORD-20FUNCTION-29"></a>

- [function] **MAKE-KEYWORD** *NAME*

    Interns the string designated by `name` in the [`keyword`][077a] package.

<a id="x-28MLUTILS-3AMKLIST-20FUNCTION-29"></a>

- [function] **MKLIST** *OBJ*

    If not already a list, mklist will return a
    new list with its param as element

<a id="x-28MLUTILS-3AONCE-ONLY-20MGL-PAX-3AMACRO-29"></a>

- [macro] **ONCE-ONLY** *SPECS &BODY FORMS*

    Evaluates `forms` with symbols specified in `specs` rebound to temporary
    variables, ensuring that each initform is evaluated only once.
    
    Each of `specs` must either be a symbol naming the variable to be rebound, or of
    the form:
    
        (symbol initform)
    
    Bare symbols in `specs` are equivalent to
    
        (symbol symbol)
    
    Example:
    
        (defmacro cons1 (x) (once-only (x) `(cons ,x ,x)))
          (let ((y 0)) (cons1 (incf y))) => (1 . 1)


<a id="x-28MLUTILS-3APLIST-KEYS-20FUNCTION-29"></a>

- [function] **PLIST-KEYS** *PLIST*

    Return all the keys of `plist`.

<a id="x-28MLUTILS-3APLIST-VALUES-20FUNCTION-29"></a>

- [function] **PLIST-VALUES** *PLIST*

    Return all the values of `plist`.

<a id="x-28MLUTILS-3APMX-20MGL-PAX-3AMACRO-29"></a>

- [macro] **PMX** *FORM*

    [`MACROEXPAND-1`][ac48] and then PRETTY-PRINT `form`.

<a id="x-28MLUTILS-3APR-20FUNCTION-29"></a>

- [function] **PR** *&REST ARGS*

    Print `args` to screen. Returns the first arg.

<a id="x-28MLUTILS-3APRN-20FUNCTION-29"></a>

- [function] **PRN** *&REST ARGS*

    Print `args` to screen, separated by a newline. Returns the first arg.

<a id="x-28MLUTILS-3APRS-20FUNCTION-29"></a>

- [function] **PRS** *&REST ARGS*

    Print `args` to screen, separated by a space. Returns the first arg.

<a id="x-28MLUTILS-3ARANGE-20FUNCTION-29"></a>

- [function] **RANGE** *START END &KEY (STEP 1) (KEY 'IDENTITY)*

    Return the list of numbers `n` such that `start <= n < end` and
    `n = start + k*step` for suitable integers `k`. If a function `key` is
    provided, then apply it to each number.

<a id="x-28MLUTILS-3ARECURSIVELY-20MGL-PAX-3AMACRO-29"></a>

- [macro] **RECURSIVELY** *BINDINGS &BODY BODY*

    Execute `body` recursively, like Clojure's [`loop`][30e1]/`recur`.
    
    `bindings` should contain a list of symbols and (optional) starting values.
    
    In `body` the symbol `recur` will be bound to the function for recurring.

<a id="x-28MLUTILS-3ARETRIABLE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **RETRIABLE** *&BODY BODY*

    Wraps `body` in a [`RESTART-CASE`][a458] with a `RETRY` restart. When invoked, the
    restart will re-execute the body forms until they return a non-`NIL` value.
    
    Returns the first non-`NIL` value returned by the body forms.
    
    By default, the message reported by the restart case will be "Retry.".  This
    can be overridden by providing a :report form as the first element of the
    body.
    
    Examples:
    
    ;; Basic usage
      (retriable
        (let ((x (random 10)))
          (when (> x 5)
            x)))
    
      ;; With custom report message
      (retriable
        (:report "Try again to get a number greater than 5")
        (let ((x (random 10)))
          (when (> x 5)
            x)))

<a id="x-28MLUTILS-3ASPLIT-20FUNCTION-29"></a>

- [function] **SPLIT** *DELIMITER SEQUENCE &KEY (START 0) (END NIL) (FROM-END NIL) (COUNT NIL) (REMOVE-EMPTY-SUBSEQS NIL) (TEST \#'EQL) (TEST-NOT NIL) (KEY \#'IDENTITY)*

    Return a list of subsequences in seq delimited by delimiter.
    
    If :remove-empty-subseqs is `NIL`, empty subsequences will be included
    in the result; otherwise they will be discarded.  All other keywords
    work analogously to those for [`CL:SUBSTITUTE`][f365].  In particular, the
    behaviour of :from-end is possibly different from other versions of
    this function; :from-end values of `NIL` and `T` are equivalent unless
    :count is supplied. The second return value is an index suitable as an
    argument to [`CL:SUBSEQ`][4a86] into the sequence indicating where processing
    stopped.

<a id="x-28MLUTILS-3ASPLIT-SEQUENCE-20FUNCTION-29"></a>

- [function] **SPLIT-SEQUENCE** *DELIMITER SEQUENCE &KEY (START 0) (END NIL) (FROM-END NIL) (COUNT NIL) (REMOVE-EMPTY-SUBSEQS NIL) (TEST \#'EQL) (TEST-NOT NIL) (KEY \#'IDENTITY)*

    Return a list of subsequences in seq delimited by delimiter.
    
    If :remove-empty-subseqs is `NIL`, empty subsequences will be included
    in the result; otherwise they will be discarded.  All other keywords
    work analogously to those for [`CL:SUBSTITUTE`][f365].  In particular, the
    behaviour of :from-end is possibly different from other versions of
    this function; :from-end values of `NIL` and `T` are equivalent unless
    :count is supplied. The second return value is an index suitable as an
    argument to [`CL:SUBSEQ`][4a86] into the sequence indicating where processing
    stopped.

<a id="x-28MLUTILS-3ASPR-20FUNCTION-29"></a>

- [function] **SPR** *&REST ARGS*

    Print `args` into a string, and return it.

<a id="x-28MLUTILS-3ASPRN-20FUNCTION-29"></a>

- [function] **SPRN** *&REST ARGS*

    Print `args` into a string, separated by a newline, and return it.

<a id="x-28MLUTILS-3ASPRS-20FUNCTION-29"></a>

- [function] **SPRS** *&REST ARGS*

    Print `args` into a string, separated by a space, and return it.

<a id="x-28MLUTILS-3ASTRING-ENDS-WITH-P-20FUNCTION-29"></a>

- [function] **STRING-ENDS-WITH-P** *SUFFIX S*

    Returns `T` if the last few characters of `s` are equal to `suffix`.

<a id="x-28MLUTILS-3ASTRING-STARTS-WITH-P-20FUNCTION-29"></a>

- [function] **STRING-STARTS-WITH-P** *PREFIX S*

    Returns `T` if the first few characters of `s` are equal to `prefix`.

<a id="x-28MLUTILS-3ASUBDIVIDE-20FUNCTION-29"></a>

- [function] **SUBDIVIDE** *SEQUENCE CHUNK-SIZE*

    Split `sequence` into subsequences of size `chunk-size`.

<a id="x-28MLUTILS-3ASYMB-20FUNCTION-29"></a>

- [function] **SYMB** *&REST ARGS*

    Receives any number of objects, concatenates all into one string with `#'mkstr` and converts them to symbol.
    
    Extracted from *On Lisp*, chapter 4.
    
    See also: `symbolicate`

<a id="x-28MLUTILS-3AUNDEFCLASS-20MGL-PAX-3AMACRO-29"></a>

- [macro] **UNDEFCLASS** *CLASS DIRECT-SUPERCLASSES DIRECT-SLOTS &REST OPTIONS*

    Removes the association between `class` and its class object.
    
    A mere wrapper around (setf (find-class class) nil), except it has the same
    signature of [`DEFCLASS`][ead6]; this makes it particularly easy to undefine a class by
    simply changing `DEFCLASS` into `UNDEFCLASS`

<a id="x-28MLUTILS-3AUNDEFCONSTANT-20MGL-PAX-3AMACRO-29"></a>

- [macro] **UNDEFCONSTANT** *NAME VALUE &OPTIONAL (DOC NIL)*

    Makes the symbol be unbound, regardless of whether it was previously bound.
    
    Similar to [`MAKUNBOUND`][35b1], except it has the same signature of [`DEFCONSTANT`][8934]; this
    makes it particularly easy to make a symbol unbound by simply changing
    `DEFCONSTANT` into [`UNDEFVAR`][87a5]

<a id="x-28MLUTILS-3AUNDEFMACRO-20MGL-PAX-3AMACRO-29"></a>

- [macro] **UNDEFMACRO** *NAME LAMBDA-LIST &BODY BODY*

    Removes the function or macro definition, if any, of `name` in the global
    environment.
    
    Similar to [`FMAKUNBOUND`][609c], except it has the same signature of [`DEFUN`][f472]; this makes
    it particularly easy to undefine a function or a macro by simply changing `DEFUN`
    into [`UNDEFUN`][0a82] and [`DEFMACRO`][14cb] into `UNDEFMACRO`

<a id="x-28MLUTILS-3AUNDEFMETHOD-20MGL-PAX-3AMACRO-29"></a>

- [macro] **UNDEFMETHOD** *NAME &REST ARGS*

    Removes a method from a generic-function `name`.
    
    This macro's signature matches [`DEFMETHOD`][6832]'s one, and `args` will be used to
    extract the method qualifiers and specializers necessary to find the right
    method to remove; this makes it particularly easy to undefine a method by
    simply changing `DEFMETHOD` into `UNDEFMETHOD`

<a id="x-28MLUTILS-3AUNDEFPACKAGE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **UNDEFPACKAGE** *NAME &REST OPTIONS*

    Deletes [`package`][1d5a] from all system data structures.
    
    Similar to [`DELETE-PACKAGE`][329a], except it has the same signature of [`DEFPACKAGE`][9b43]; this
    makes it particularly easy to delete a package by simply changing `DEFPACKAGE`
    into `UNDEFPACKAGE`

<a id="x-28MLUTILS-3AUNDEFPARAMETER-20MGL-PAX-3AMACRO-29"></a>

- [macro] **UNDEFPARAMETER** *VAR VAL &OPTIONAL (DOC NIL)*

    Makes the symbol be unbound, regardless of whether it was previously bound.
    
    Similar to [`MAKUNBOUND`][35b1], except it has the same signature of [`DEFPARAMETER`][570e]; this
    makes it particularly easy to make a symbol unbound by simply changing
    `DEFPARAMETER` into [`UNDEFVAR`][87a5]

<a id="x-28MLUTILS-3AUNDEFUN-20MGL-PAX-3AMACRO-29"></a>

- [macro] **UNDEFUN** *NAME LAMBDA-LIST &BODY BODY*

    Removes the function or macro definition, if any, of `name` in the global
    environment.
    
    Similar to [`FMAKUNBOUND`][609c], except it has the same signature of [`DEFUN`][f472]; this makes
    it particularly easy to undefine a function or a macro by simply changing `DEFUN`
    into `UNDEFUN` and [`DEFMACRO`][14cb] into [`UNDEFMACRO`][f73d]

<a id="x-28MLUTILS-3AUNDEFVAR-20MGL-PAX-3AMACRO-29"></a>

- [macro] **UNDEFVAR** *VAR &OPTIONAL (VAL NIL) (DOC NIL)*

    Makes the symbol be unbound, regardless of whether it was previously bound.
    
    Similar to [`MAKUNBOUND`][35b1], except it has the same signature of [`DEFVAR`][7334]; this makes
    it particularly easy to make a symbol unbound by simply changing `DEFVAR` into
    `UNDEFVAR`

<a id="x-28MLUTILS-3AUNTIL-20MGL-PAX-3AMACRO-29"></a>

- [macro] **UNTIL** *EXPRESSION &BODY BODY*

    Executes `body` until `expression` is true.

<a id="x-28MLUTILS-3AW-2FGENSYMS-20MGL-PAX-3AMACRO-29"></a>

- [macro] **W/GENSYMS** *NAMES &BODY FORMS*

    Binds each variable named by a symbol in `names` to a unique symbol around
    `forms`. Each of `names` must either be either a symbol, or of the form:
    
        (symbol string-designator)
    
    Bare symbols appearing in `names` are equivalent to:
    
        (symbol symbol)
    
    The string-designator is used as the argument to [`gensym`][0e59] when constructing the
    unique symbol the named variable will be bound to.

<a id="x-28MLUTILS-3AW-2FSLOTS-20MGL-PAX-3AMACRO-29"></a>

- [macro] **W/SLOTS** *SLOTS INSTANCE &BODY BODY*

<a id="x-28MLUTILS-3AWHEN-LET-20MGL-PAX-3AMACRO-29"></a>

- [macro] **WHEN-LET** *BINDINGS &BODY FORMS*

    Creates new variable bindings, and conditionally executes `FORMS`.
    
    `BINDINGS` must be either single binding of the form:
    
    (variable initial-form)
    
    or a list of bindings of the form:
    
    ((variable-1 initial-form-1)
      (variable-2 initial-form-2)
      ...
      (variable-n initial-form-n))
    
    All initial-forms are executed sequentially in the specified order. Then all
    the variables are bound to the corresponding values.
    
    If all variables were bound to true values, then `FORMS` are executed as an
    implicit [`PROGN`][0cc3].

<a id="x-28MLUTILS-3AWHILE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **WHILE** *EXPRESSION &BODY BODY*

    Executes `body` while `expression` is true.

<a id="x-28MLUTILS-3AWITH-GENSYMS-20MGL-PAX-3AMACRO-29"></a>

- [macro] **WITH-GENSYMS** *NAMES &BODY FORMS*

    Binds each variable named by a symbol in `names` to a unique symbol around
    `forms`. Each of `names` must either be either a symbol, or of the form:
    
        (symbol string-designator)
    
    Bare symbols appearing in `names` are equivalent to:
    
        (symbol symbol)
    
    The string-designator is used as the argument to [`gensym`][0e59] when constructing the
    unique symbol the named variable will be bound to.

<a id="x-28MLUTILS-3A-7E-3E-20MGL-PAX-3AMACRO-29"></a>

- [macro] **~>** *X &REST FORMS*

    Threads the expr through the forms, like Clojure's `->`.
    
    While threading, for each element of `forms`:
    
    - if a [`SYMBOL`][e5af], it's converted into a function call with the accumulated value
    as it's first argument
    
    - if a function call already, the accumulated value is **prepended** to the
    list of args unless it contains the placeholder '~ (in which case '~ is
    replaced with the accumulated value)
    
    Examples:
    
    (~> 'Hello
      (list 'World))
    =>
    (HELLO WORLD)
    
    (~> 'Hello
      (list 'World ~))
    =>
    (WORLD HELLO)
    
    (~> 'Hello
      (list 'World ~)
      reverse)
    =>
    (HELLO WORLD)
      

<a id="x-28MLUTILS-3A-7E-3E-3E-20MGL-PAX-3AMACRO-29"></a>

- [macro] **~>>** *X &REST FORMS*

    Threads the expr through the forms, like Clojure's `->>`.
    
    While threading, for each element of `forms`:
    
    - if a [`SYMBOL`][e5af], it's converted into a function call with the accumulated value
    as it's first argument
    
    - if a function call already, the accumulated value is **appended** to the
    list of args unless it contains the placeholder '~ (in which case '~ is
    replaced with the accumulated value)
    
    Examples:
    (~>> 'World
      (list 'Hello))
    =>
    (HELLO WORLD)
    
    (~>> 'World
      (list ~ 'Hello))
    =>
    (HELLO WORLD)
    
    (~>> 'World
      (list ~ 'Hello)
      reverse)
    =>
    (HELLO WORLD)
      

  [02ad]: http://www.lispworks.com/documentation/HyperSpec/Body/s_if.htm "IF (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [077a]: http://www.lispworks.com/documentation/HyperSpec/Body/t_kwd.htm "KEYWORD (MGL-PAX:CLHS TYPE)"
  [091c]: http://www.lispworks.com/documentation/HyperSpec/Body/s_flet_.htm "FLET (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [0a82]: #x-28MLUTILS-3AUNDEFUN-20MGL-PAX-3AMACRO-29 "MLUTILS:UNDEFUN MGL-PAX:MACRO"
  [0cc3]: http://www.lispworks.com/documentation/HyperSpec/Body/s_progn.htm "PROGN (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [0e59]: http://www.lispworks.com/documentation/HyperSpec/Body/f_gensym.htm "GENSYM (MGL-PAX:CLHS FUNCTION)"
  [1013]: http://www.lispworks.com/documentation/HyperSpec/Body/f_not.htm "NOT (MGL-PAX:CLHS FUNCTION)"
  [14cb]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defmac.htm "DEFMACRO (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [1867]: http://www.lispworks.com/documentation/HyperSpec/Body/r_contin.htm "CONTINUE (MGL-PAX:CLHS RESTART)"
  [1bfd]: http://www.lispworks.com/documentation/HyperSpec/Body/m_dolist.htm "DOLIST (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [1d5a]: http://www.lispworks.com/documentation/HyperSpec/Body/t_pkg.htm "PACKAGE (MGL-PAX:CLHS CLASS)"
  [2053]: http://www.lispworks.com/documentation/HyperSpec/Body/m_prog1c.htm "PROG1 (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [30e1]: http://www.lispworks.com/documentation/HyperSpec/Body/m_loop.htm "LOOP (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [329a]: http://www.lispworks.com/documentation/HyperSpec/Body/f_del_pk.htm "DELETE-PACKAGE (MGL-PAX:CLHS FUNCTION)"
  [35b1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_makunb.htm "MAKUNBOUND (MGL-PAX:CLHS FUNCTION)"
  [35ba]: http://www.lispworks.com/documentation/HyperSpec/Body/f_error.htm "ERROR (MGL-PAX:CLHS FUNCTION)"
  [425d]: http://www.lispworks.com/documentation/HyperSpec/Body/m_and.htm "AND (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [44b5]: #x-28MLUTILS-3A-40MLUTILS-REFERENCE-20MGL-PAX-3ASECTION-29 "Reference"
  [4853]: http://www.lispworks.com/documentation/HyperSpec/Body/s_let_l.htm "LET (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [49f5]: http://www.lispworks.com/documentation/HyperSpec/Body/s_let_l.htm "LET* (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [4a86]: http://www.lispworks.com/documentation/HyperSpec/Body/f_subseq.htm "SUBSEQ (MGL-PAX:CLHS FUNCTION)"
  [570e]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defpar.htm "DEFPARAMETER (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [5b0b]: http://www.lispworks.com/documentation/HyperSpec/Body/m_return.htm "RETURN (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [5c01]: http://www.lispworks.com/documentation/HyperSpec/Body/m_lambda.htm "LAMBDA (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [609c]: http://www.lispworks.com/documentation/HyperSpec/Body/f_fmakun.htm "FMAKUNBOUND (MGL-PAX:CLHS FUNCTION)"
  [625d]: http://www.lispworks.com/documentation/HyperSpec/Body/m_multip.htm "MULTIPLE-VALUE-BIND (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [6428]: #x-28MLUTILS-3ADORANGE-20MGL-PAX-3AMACRO-29 "MLUTILS:DORANGE MGL-PAX:MACRO"
  [6832]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defmet.htm "DEFMETHOD (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [7334]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defpar.htm "DEFVAR (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [7de6]: http://www.lispworks.com/documentation/HyperSpec/Body/m_w_slts.htm "WITH-SLOTS (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [87a5]: #x-28MLUTILS-3AUNDEFVAR-20MGL-PAX-3AMACRO-29 "MLUTILS:UNDEFVAR MGL-PAX:MACRO"
  [8934]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defcon.htm "DEFCONSTANT (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [8b63]: #x-28MLUTILS-3ALET1-20MGL-PAX-3AMACRO-29 "MLUTILS:LET1 MGL-PAX:MACRO"
  [954a]: http://www.lispworks.com/documentation/HyperSpec/Body/t_not.htm "NOT (MGL-PAX:CLHS TYPE)"
  [9b43]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defpkg.htm "DEFPACKAGE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [a138]: http://www.lispworks.com/documentation/HyperSpec/Body/m_setf_.htm "SETF (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [a458]: http://www.lispworks.com/documentation/HyperSpec/Body/m_rst_ca.htm "RESTART-CASE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [ac48]: http://www.lispworks.com/documentation/HyperSpec/Body/f_mexp_.htm "MACROEXPAND-1 (MGL-PAX:CLHS FUNCTION)"
  [b105]: http://www.lispworks.com/documentation/HyperSpec/Body/m_destru.htm "DESTRUCTURING-BIND (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [baaf]: http://www.lispworks.com/documentation/HyperSpec/Body/m_when_.htm "WHEN (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [c2ef]: http://www.lispworks.com/documentation/HyperSpec/Body/s_flet_.htm "LABELS (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [d162]: http://www.lispworks.com/documentation/HyperSpec/Body/e_error.htm "ERROR (MGL-PAX:CLHS CONDITION)"
  [d5a2]: http://www.lispworks.com/documentation/HyperSpec/Body/f_car_c.htm "CAR (MGL-PAX:CLHS FUNCTION)"
  [d675]: http://www.lispworks.com/documentation/HyperSpec/Body/f_append.htm "APPEND (MGL-PAX:CLHS FUNCTION)"
  [dd55]: http://www.lispworks.com/documentation/HyperSpec/Body/t_and.htm "AND (MGL-PAX:CLHS TYPE)"
  [e400]: http://www.lispworks.com/documentation/HyperSpec/Body/s_lambda.htm '"s_lambda" (MGL-PAX:CLHS MGL-PAX:SECTION)'
  [e5af]: http://www.lispworks.com/documentation/HyperSpec/Body/t_symbol.htm "SYMBOL (MGL-PAX:CLHS CLASS)"
  [e5fc]: http://www.lispworks.com/documentation/HyperSpec/Body/f_assocc.htm "ASSOC (MGL-PAX:CLHS FUNCTION)"
  [ead6]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defcla.htm "DEFCLASS (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [eef3]: http://www.lispworks.com/documentation/HyperSpec/Body/t_values.htm "VALUES (MGL-PAX:CLHS TYPE)"
  [f029]: #x-28MLUTILS-3ABND-2A-20MGL-PAX-3AMACRO-29 "MLUTILS:BND* MGL-PAX:MACRO"
  [f365]: http://www.lispworks.com/documentation/HyperSpec/Body/f_sbs_s.htm "SUBSTITUTE (MGL-PAX:CLHS FUNCTION)"
  [f472]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defun.htm "DEFUN (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [f73d]: #x-28MLUTILS-3AUNDEFMACRO-20MGL-PAX-3AMACRO-29 "MLUTILS:UNDEFMACRO MGL-PAX:MACRO"
  [fc69]: http://www.lispworks.com/documentation/HyperSpec/Body/f_values.htm "VALUES (MGL-PAX:CLHS FUNCTION)"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
