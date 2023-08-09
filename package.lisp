;;;; package.lisp

(defpackage #:auto-suspend
  (:use #:cl)
  (:import-from #:stumpwm #:run-shell-command #:cancel-timer #:run-with-timer #:*timer-list*)
  (:export  
   #:start-loop #:update-values
   #:*battery-status-path* #:*battery-capacity-path* #:*notify-time*  #:*percent-to-suspend* #:*percent-to-notify* #:*notification-title* #:*notification-body*
   ))
