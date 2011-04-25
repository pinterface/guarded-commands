(in-package #:guarded-commands)

(define-condition syntax-error (simple-error) ())
(define-condition step-error (error)
  ((step-name :accessor step-error-name :initarg :name)))
(define-condition step-failed (step-error) ()
  (:report (lambda (c s) (format s "Step ~S failed." (step-error-name c)))))
(define-condition invariant-failed (step-error) ()
  (:report (lambda (c s) (format s "Invariant for step ~S failed." (step-error-name c)))))

(defun %syntax-error (string step-name)
  (error 'syntax-error :format-control string :format-arguments (list step-name)))

(defun %step-failed (name) (error 'step-failed :name name))
(defun %invariant-failed (name) (error 'invariant-failed :name name))
