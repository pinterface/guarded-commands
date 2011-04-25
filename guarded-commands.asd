(asdf:defsystem guarded-commands
  :version "0"
  :description "A lispy analog to Perl's Commands::Guarded"
  :author "pinterface <pix@kepibu.org>"
  :license "BSD-style"
  :depends-on (:alexandria)
  :components ((:file "package")
               (:file "errors" :depends-on ("package"))
               (:file "task" :depends-on ("package"))
               (:file "step" :depends-on ("task" "errors" "package"))))
