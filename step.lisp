(in-package #:guarded-commands)

(defvar *debug* nil "Set to t allow errors within USING forms to trigger the debugger.")

;; I already require Alexandria, so what's my motivation for avoiding assoc-value?
(defun assocdr (item alist)
  (cdr (assoc item alist)))

;; ignore-errors-like functionality, but allows earlier handlers to run first
(defmacro avoid-errors (&body body)
  (if *debug*
      `(progn ,@body)
      (with-unique-names (block)
        `(block ,block
           (handler-bind ((error (lambda (c)
                                   (signal c)
                                   (return-from ,block (values nil c)))))
             ,@body)))))

;;; TODO: We've got plenty of room for optimizations here.  Ideas:
;;; * [ ] Don't track rollbacks if unspecified
;;; * [ ] Don't run using if unspecified
;;; * [ ] Avoid saving return values if with-step is in a null context (e.g., not the last element within a progn) -- can we even detect this? :/

;; I'd like to call this "STEP", but that would conflict with a very different CL:STEP.
(defmacro with-step (name &body body)
  "Defines one step of a task.  Within the body of step, several macros are provided:
  (ensure <condition>) defines a condition which must be true for the step to be considered successful.  If ensure is false, the using block will be run.  If ensure remains false, a condition of type ? will be signalled.
  (using <body>) defines code to be run to make the ensure block return true.
  (sanity <condition>) defines an invariant condition.  If defined, it is run before ensure and after using are run, and if it returns false will signal a condition of type ?.
  (rollback <body>) defines rollback code for this step.  If this step, or any step after it in the task, fails the rollback will be run."
  (let ((ensure (or (assocdr 'ensure body)
                    (%syntax-error "Step ~S is missing the required ENSURE condition." name)))
        (using (assocdr 'using body))
        ;; FIXME: I hate the name sanity, but what's better?  requiring?  assuring?  positing?
        ;; Hrm... "positing X, ensure Y using Z"
        ;; "ensure X using Y, requiring Z"
        (invariant (assocdr 'sanity body))
        (rollback (assocdr 'rollback body)))
    (with-unique-names (ensure-fn using-fn invariant-fn rollback-fn)
      (once-only ((return-values nil))
        `(flet ((,ensure-fn    () ,@ensure)
                (,using-fn     () (avoid-errors ,@using))
                ,@(when invariant `((,invariant-fn () ,@invariant)))
                (,rollback-fn  () ,@rollback))
           ,@(when invariant `((%check-invariant #',invariant-fn ,name)))
           (push #',rollback-fn *task-rollback*)
           (unless (,ensure-fn)
             (setf ,return-values (multiple-value-list (,using-fn)))
             ,@(when invariant `((%check-invariant #',invariant-fn ,name)))
             (unless (,ensure-fn)
               (%step-failed ,name)))
           (values-list ,return-values))))))

(defun %check-invariant (invariant-fn step-name)
  (unless (funcall invariant-fn)
    (%invariant-failed step-name)))

#+(or)
(with-step "step one"
  (ensure t)
  (using nil))
