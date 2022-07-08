;; For license see LICENSE

(in-package #:rutils.readtable)
(eval-when (:compile-toplevel)
  (declaim #.+default-opts+))


(defun |@-reader| (stream char)
  "Short slot and sequence elements access syntax.

   Examples:

       CL-USER> (defclass foo () ((bar :initform 42)))
       CL-USER> (defvar *foo* (make 'foo))
       CL-USER> @*foo*.bar
       42
       CL-USER> (defvar *baz* #(1 *foo*))
       CL-USER> @*baz*#1.bar
       42
  "
  (declare (ignore char))
  (if (member (peek-char nil stream)
              '(#\Space #\Newline #\Tab #\Return #\Linefeed #\)))
      '@
      (let ((whole (symbol-name (read stream)))
            sep
            acc)
        (flet ((next-sep (str start)
                 (position-if (lambda (x) (member x '(#\. #\#)))
                              str :start (1+ start))))
          (do* ((prev 0 (1+ next))
                (next (next-sep whole prev) (next-sep whole prev)))
               ((null next) (push (cons sep (subseq whole prev)) acc))
            (push (cons sep (subseq whole prev next))
                  acc)
            (setf sep (char whole next))))
        (setf acc (reverse acc))
        (let ((rez (intern (cdar acc))))
          (dolist (pair (rest acc))
            (ecase (car pair)
              (#\. (setf rez `(rutils.generic:smart-slot-value ,rez ',(intern (cdr pair)))))
              (#\# (setf rez `(elt ,rez ,(parse-integer (cdr pair)))))))
          rez))))

(defreadtable rutils-readtable
    (:merge :standard)
  (:macro-char #\} (get-macro-character #\)))
  (:macro-char #\^ #'|^-reader|)
  (:macro-char #\@ #'|@-reader| t)
  (:dispatch-macro-char #\# #\v #'|#v-reader|)
  (:dispatch-macro-char #\# #\h #'|#h-reader|)
  (:dispatch-macro-char #\# #\{ #'|#{-reader|)
  (:dispatch-macro-char #\# #\` #'|#`-reader|)
  (:dispatch-macro-char #\# #\/ #'|#/-reader|))

(defreadtable standard-readtable
  (:merge :standard))

)
