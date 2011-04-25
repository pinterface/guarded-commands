(defpackage #:guarded-commands
  (:use #:cl)
  (:import-from #:alexandria
                #:with-unique-names
                #:once-only
                #:parse-body)
  (:export #:define-task
           #:with-task

           #:with-step
           #:ensure
           #:using
           #:rollback
           #:sanity

           #:step-failed
           #:invariant-failed))
