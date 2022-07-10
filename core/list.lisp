
(defun delete-from-plist (plist &rest keys)
  "Just like REMOVE-FROM-PLIST, but this version may destructively
   modify the provided PLIST."
  (declare (optimize (speed 3)))
  (loop :for pos := 0 :then (incf pos 2)
        :for (key . rest) :on plist :by #'cddr
        :while rest :do
          (when (member key keys :test #'eql)
            (if (zerop pos)
                (setf plist (nthcdr 2 plist))
                (progn
                  (rplacd (nthcdr (1- pos) plist)
                          (nthcdr (+ pos 2) plist))
                  (decf pos 2))))
        :finally (return plist)))

(defun permutations (list)
  "Generate all permutations of LIST.
   Complexity: O(n!)"
  (labels ((perms (list acc)
             (if list
                 (mapcanindex (lambda (i el)
                                (perms (remove-idx i list)
                                       (mapcar #`(cons el %) acc)))
                              list)
                 acc)))
    (when list
      (perms list (list nil)))))

(defmacro listcase (list &body cases)
  "A typecase-like macro to destinguish between 3 possible kinds of LIST:
   simple lists, alists, and dlists.
   Evaluates the appropriate key-form depending on the kind:
   ALIST, DLIST or simple list (T)."
  (once-only (list)
    `(if (consp (first ,list))
         (cond ((consp (second ,list))
                (progn ,@(assoc1 'alist cases)))
               ((second ,list)
                (progn ,@(assoc1 'dlist cases)))
               (t
                (progn ,@(assoc1 't cases))))
         (progn ,@(assoc1 't cases)))))

;; D-Lists (see https://groups.google.com/forum/#!msg/comp.lang.lisp/pE-4JL9lnAA/7hiQSBexGLgJ)
