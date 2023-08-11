;;;; auto-suspend.lisp

(in-package #:auto-suspend)

(defvar *battery-check-timer* nil "Holds the main loop timer")
(defvar *battery-capacity-path* "/sys/class/power_supply/BAT0/capacity")
(defvar *battery-status-path* "/sys/class/power_supply/BAT0/status")
(defvar *notify-time* 1 "Amount of time (in milliseconds) to show notifications. The official
documentation of =notify-send= claims it is time in milliseconds, but
it seems more likely that it notifies for 10 seconds regardless of the
value.")
(defvar *loop-time* 30)
(defvar *suspend-command* "sudo pm-suspend")
(defvar *percent-to-suspend* 10)
(defvar *percent-to-notify* 20)
(defvar *notification-title* "Battery Low")
(defvar *notification-body* "Battery is at ~A%"
  "The BODY can be in Markup. For example:
bold: <b>...</b>
italic: <i>...</b>
underline: <u>...</u>
hyperlink: <a href=\"link\">name</a>
image: <img src=\"source_link\" alt=\"alt_link\"/>")

(defvar *debug* nil "Set this to t to stop SUSPEND function from suspending the pc, this
way you can test to find out where the problem is with the program if
it isnt suspending.")

(defun get-battery-capacity ()
  "Get battery capacity from *BATTERY-CAPACITY-PATH*"
  (parse-integer 
   (with-open-file (capacity-file *battery-capacity-path*)
     (read-line capacity-file))))

(defun battery-charging? ()
  "Get battery capacity from *BATTERY-CAPACITY-PATH*"
  (let ((status (with-open-file (status-file *battery-status-path*)
		  (read-line status-file))))
    (string= status "Charging")))

(defun notify-send (title body)
  "Send message to user using notify-send command"
  (let* (;(notification-time-string (format nil "-t ~A "
					; *notify-time*)) this isnt
					; even working right now due
					; to upstream notify-send
					; problems
	 (command (concatenate 'string "notify-send "
			       ;notification-time-string
			       "'" title "' "
			       "'" body "'")))
    (run-shell-command command)
    (if *debug* command)))

(defun suspend () "Suspend the computer"
  (if *debug*
      (notify-send "DEBUG" "The *debug* var is t, otherwise pc would have suspended")
      (run-shell-command *suspend-command*)))

(defun main ()
  (let ((capacity (get-battery-capacity)))
    (cond ((and (not (battery-charging?)) (< capacity *percent-to-suspend*))
	   (suspend))
	  ((and (not (battery-charging?)) (< capacity *percent-to-notify*))
	   (notify-send *notification-title* 
			(format nil *notification-body* capacity)))
	  (*debug* (notify-send "Battery Level" "Battery is fine.")))))

(defun in-timer-list? (timer)
  (if timer
      (some (lambda (x) (eq x timer)) *timer-list*)))

(defun start-loop ()
  (if (in-timer-list? *battery-check-timer*)
      (cancel-timer *battery-check-timer*))
  (setq *battery-check-timer* (run-with-timer 0 *loop-time* #'main)))

(defmacro string-concatenate (string &rest strings)
  `(concatenate 'string ,string ,@strings))
