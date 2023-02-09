;;;; auto-suspend.asd

(asdf:defsystem #:auto-suspend
  :description "Describe auto-suspend here"
  :author "Kamy"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "auto-suspend")))
