(defpackage #:guarded-commands.test
  (:use #:cl #:guarded-commands))
(in-package #:guarded-commands.test)

(define-task task-1 ()
  "A task is a sequence of steps."
  (let ((var nil))
    (with-step "set variable"
      (ensure var)
      (using (setf var t))))
  (let ((list (list 'a 'b 'c)))
    (with-step "put 'b in list"
      (ensure (member 'b list))
      (using (push 'b list))
      (sanity (listp list)))
    (with-step "put 'd in list"
      (ensure (member 'd list))
      (using (push 'd list)))
    (with-step "remove 'd from list"
      (ensure (not (member 'd list)))
      (using (setf list (remove 'd list))))
    (with-step "assert all symbols"
      (ensure (every #'symbolp list)))
    (with-step "outer step"
      (ensure (member 'e list))
      (using
       (with-step "inner step"
         (ensure (member 'f list))
         (using (nconc list (list 'e 'f))))))))

#+(or) (task-1)

(defun task-2 ()
  (let ((list (list)))
    (ignore-errors
      (with-task
        (with-step "init list"
          (ensure (and (not (null list))
                       (listp list)))
          (using (push 1 list)
                 (push 2 list)
                 (push 3 list))
          (rollback (setf list :rolled-back)))
        (with-step "fail at something"
          (ensure (member 4 list))
          (using :do-nothing))))
    list))

#+(or) (task-2)

(define-task task-3 ()
  (let ((a nil))
    (with-step "error in using"
      (ensure a)
      (using (error "oh noez!")
             (setf a t)))))

#+(or) (task-3)

(define-task task-4 ()
  (let ((a nil))
    (handler-bind ((error #'continue))
      (with-step "testing errors"
        (ensure a)
        (using (cerror "continue to complete step" nil)
               (setf a t))))))

#+(or) (task-4)
