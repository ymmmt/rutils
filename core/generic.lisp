(defmacro adding-smart-slot-methods (obj slot expr)
  (with-gensyms (err class name alt args val)
    `(handler-case ,expr
       (simple-error (,err)
         (let ((,class (class-of ,obj))
               (,name (symbol-name ,slot)))
           (dolist (,alt (c2mop:class-slots ,class)
                         (error ,err))
             (let ((,alt (c2mop:slot-definition-name ,alt)))
               (when (string= ,name (symbol-name ,alt))
                 (add-method (ensure-generic-function 'smart-slot-value)
                             (make-instance
                              'standard-method
                              :specializers
                              (list ,class
                                    (c2mop:intern-eql-specializer ,slot))
                              :lambda-list '(,obj ,slot)
                              :function
                              (lambda (,args _)
                                (declare (ignorable _))
                                (slot-value (first ,args) ,alt))))
                 (add-method (ensure-generic-function 'smart-set-slot-value)
                             (make-instance
                              'standard-method
                              :specializers
                              (list ,class
                                    (c2mop:intern-eql-specializer ,slot)
                                    (find-class 't))
                              :lambda-list '(,obj ,slot ,val)
                              :function
                              (lambda (,args _)
                                (declare (ignorable _))
                                (:= (slot-value (first ,args) ,alt)
                                    (third ,args)))))
                 (let ((,slot ,alt))
                   (return ,expr))))))))))

(defgeneric smart-slot-value (obj slot)
  (:documentation
   "Similar to SLOT-VALUE but tries to find slot definitions regardless
    of the package.")
  (:method (obj slot)
    (adding-smart-slot-methods obj slot (slot-value obj slot))))

(defgeneric smart-set-slot-value (obj slot val)
  (:documentation
   "Similar to (SETF SLOT-VALUE) but tries to find slot definitions regardless
    of the package.")
  (:method (obj slot val)
    (adding-smart-slot-methods obj slot (:= (slot-value obj slot) val))))

(defsetf smart-slot-value smart-set-slot-value)

;;; generic element access protocol

(defmethod generic-elt ((obj structure-object) key &rest keys)
  (declare (ignore keys))
  (smart-slot-value obj key))

(defmethod generic-elt ((obj standard-object) key &rest keys)
  (declare (ignore keys))
  (smart-slot-value obj key))

(defmethod generic-elt ((obj (eql nil)) key &rest keys)
  (declare (ignore key keys))
  (error "Can't access NIL with generic-elt!"))

(defgeneric generic-setf (obj key &rest keys-and-val)
  (:method :around (obj key &rest keys-and-val)
    (if (single keys-and-val)
        (call-next-method)
        (multiple-value-bind (prev-keys kv) (butlast2 keys-and-val 2)
          (apply #'generic-setf
                 (apply #'generic-elt obj key prev-keys)
                 kv)))))
