;; For license see LICENSE


;;; predicates and other logical operations

(defmacro xor (&rest args)
  "Evaluates the ARGS one at a time. If more than one is T,
   evaluation stops and NIL is returned. If exactly one arg is T,
   that value is returned."
  (let ((state (gensym "XOR-state-"))
        (block-name (gensym "XOR-block-"))
        (arg-temp (gensym "XOR-arg-temp-")))
    `(let (,state
           ,arg-temp)
       (block ,block-name
         ,@(loop
              :for arg :in args
              :collect `(setf ,arg-temp ,arg)
              :collect `(when ,arg-temp
                          ;; arg is T, this can change the state
                          (if ,state
                              ;; a second T value, return NIL
                              (return-from ,block-name nil)
                              ;; a first T, swap the state
                              (setf ,state ,arg-temp))))
         (return-from ,block-name ,state)))))


;;; Predicate case


;;; Desctructuring case

(defun expand-destructuring-case (key clauses case)
  (once-only (key)
    `(if (typep ,key 'cons)
         (,case (car ,key)
           ,@(mapcar (lambda (clause)
                       (destructuring-bind ((keys . lambda-list) &body body)
                           clause
                         `(,keys
                           (destructuring-bind ,lambda-list (cdr ,key)
                             ,@body))))
                     clauses))
         (error "Invalid key to D~S: ~S" ',case ,key))))

(defmacro dcase (keyform &body clauses)
  "DCASE is a combination of CASE and DESTRUCTURING-BIND.
   KEYFORM must evaluate to a CONS.

   Clauses are of the form:

       ((CASE-KEYS . DESTRUCTURING-LAMBDA-LIST) FORM*)

   The clause whose CASE-KEYS matches CAR of KEY, as if by CASE,
   is selected, and FORMs are then executed with CDR of KEY is destructured and
   bound by the DESTRUCTURING-LAMBDA-LIST."
  (expand-destructuring-case keyform clauses 'case))

(defmacro dccase (keyform &body clauses)
  "DCCASE is a combination of CCASE and DESTRUCTURING-BIND.
   KEYFORM must evaluate to a CONS.

   Clauses are of the form:

       ((CASE-KEYS . DESTRUCTURING-LAMBDA-LIST) FORM*)

   The clause whose CASE-KEYS matches CAR of KEY, as if by CCASE,
   is selected, and FORMs are then executed with CDR of KEY is destructured and
   bound by the DESTRUCTURING-LAMBDA-LIST."
  (expand-destructuring-case keyform clauses 'ccase))

(defmacro decase (keyform &body clauses)
  "DECASE is a combination of ECASE and DESTRUCTURING-BIND.
   KEYFORM must evaluate to a CONS.

   Clauses are of the form:

       ((CASE-KEYS . DESTRUCTURING-LAMBDA-LIST) FORM*)

   The clause whose CASE-KEYS matches CAR of KEY, as if by ECASE,
   is selected, and FORMs are then executed with CDR of KEY is destructured and
   bound by the DESTRUCTURING-LAMBDA-LIST."
  (expand-destructuring-case keyform clauses 'ecase))


;;; Switch

(defun extract-function-name (spec)
  "Useful for macros that want to mimic the functional interface for functions
   like #'eq and 'eq."
  (if (and (consp spec)
           (member (first spec) '(quote function)))
      (second spec)
      spec))

(defun generate-switch-body (whole object clauses test key &optional default)
  (with-gensyms (value)
    `(let ((,value (funcall ,key ,object)))
      (cond ,@(mapcar (lambda (clause)
                        (if (member (first clause) '(t otherwise))
                            (progn
                              (when default
                                (error "Multiple default clauses or illegal use ~
                                        of a default clause in ~S."
                                       whole))
                              (setf default `(progn ,@(rest clause)))
                              '(()))
                            (destructuring-bind (key-form &body forms) clause
                              `((funcall ,test ,value ,key-form)
                                ,@forms))))
                      clauses)
            (t ,default)))))

(defmacro switch (&whole whole (object &key (test ''eql) (key ''identity))
                         &body clauses)
  "Evaluate first matching clause, returning its values, or evaluates and
   returns the values of DEFAULT if no keys match."
  (generate-switch-body whole object clauses test key))

(defmacro cswitch (&whole whole (object &key (test ''eql) (key ''identity))
                          &body clauses)
  "Like SWITCH, but signals a continuable error if no key matches."
  (generate-switch-body whole object clauses test key
                        `(cerror "Return NIL from SWITCH" 'case-failure
                                 :datum ,object
                                 :possibilities ',(mapcar #'first clauses)
                                 :name 'cswitch)))

(defmacro eswitch (&whole whole (object &key (test ''eql) (key ''identity))
                          &body clauses)
  "Like SWITCH, but signals an error if no key matches."
  (generate-switch-body whole object clauses test key
                        `(error 'case-failure
                                :datum ,object
                                :possibilities ',(mapcar #'first clauses)
                                :name ,(format nil "ESWITCH with test ~A" test))))


;;; Clojure-like threading macros

(defmacro -> (x &rest forms)
  "Threads the expr through FORMS. Inserts X as the
   second item in the first form, or in place of % if it is present in the form,
   making a list of it if it is not a list already.
   If there are more forms, inserts the first form
   as the second item in second form, etc."
  (if (first forms)
      (let* ((form (first forms))
             (threaded (if (listp form)
                           (if (member '% form)
                               `(funcall (lambda (%) ,form) ,x)
                               `(,(first form) ,x ,@(rest form)))
                           `(,form ,x))))
        `(-> ,threaded ,@(rest forms)))
      x))

(defmacro ->> (x &rest forms)
  "Threads the expr through FORMS. Inserts X as the
   last item in the first form, or in place of % if it is present in the form,
   making a list of it if it is not a list already.
   If there are more forms, inserts the first form
   as the last item in second form, etc."
  (if (first forms)
      (let* ((form (first forms))
             (threaded (if (listp form)
                           (if (member '% form)
                               `(funcall (lambda (%) ,form) ,x)
                               `(,(first form) ,@(rest form) ,x))
                           `(,form ,x))))
        `(->> ,threaded ,@(rest forms)))
      x))

(defmacro => (fn1 fn2 &rest fns)
  "Function composition of FN1, FN2 and all the other FNS."
  (let ((arg (gensym "ARG")))
    `(lambda (,arg)
       ,(if fns
            `(,fn1 (call (=> ,fn2 ,@fns) ,arg))
            `(,fn1 (,fn2 ,arg))))))

;;; Emoji-setters

(handler-bind ((error (lambda (e)
                        (let ((r (find-restart 'continue e)))
                          (when r
                            (invoke-restart r))))))
  (defmacro := (&rest places-vals &environment env)
    "Like PSETF but returns the set value of the last expression."
    (declare (ignore env))
    (with-gensyms (rez)
      `(let (,rez)
         (psetf ,@(butlast places-vals 2)
                ,(first (last places-vals 2))
                (setf ,rez ,(first (last places-vals))))
         ,rez)))

  (abbr :+ incf)
  (abbr :- decf)

  (defmacro :* (place n)
    "Multiply in-place PLACE by N."
    `(setf ,place (* ,place ,n)))

  (defmacro :/ (place n)
    "Divide in-place PLACE by N."
    `(setf ,place (/ ,place ,n)))

  (defmacro <- (place fn)
    "Apply FN in-place to PLACE."
    `(setf ,place (call ,fn ,place))))

