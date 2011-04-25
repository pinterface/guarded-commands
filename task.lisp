(in-package #:guarded-commands)

(defvar *task-rollback* nil "The list of rollback functions to be called in the event a step fails.")

;; FIXME: make docstrings and declare forms work
(defmacro define-task (name args &body body)
  "Defines a task, within which steps and rollbacks can be used."
  (multiple-value-bind (body decl doc)
      (parse-body body :documentation t)
    `(defun ,name ,args
       ,@(and doc (list doc))
       ,@decl
       (with-task ,@body))))

(defmacro with-task (&body body)
  `(let ((*task-rollback* nil))
     (handler-bind ((error #'%do-rollbacks))
       ,@body)))

(defun %do-rollbacks (c)
  (declare (ignore c))
  (mapcar #'funcall *task-rollback*)
  nil)

#+(or)
(define-task my-task ()
  (with-step "step one"
    (ensure nil)
    (using nil)))
#+(or)
(my-task)
