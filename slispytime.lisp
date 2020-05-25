(defpackage :wake
  (:use :common-lisp :lisp-unit)
  (:export :scheduled :org-timestamp
           :today :tomorrow
           :bedtime-when-wake-at
           :waketime-when-bed-at
           :sleep-at :wake-at))

(in-package :wake)

(load "test.lisp")

(defconstant +min+ 60
  "A minute in seconds.")

(defconstant +hour+ (* 60 +min+)
  "An hour in seconds.")

(defvar *day-names* '("Monday" "Tuesday" "Wednesday"
                      "Thursday" "Friday" "Saturday" "Sunday")
  "The days of the week.")

(defvar *time-in-bed* (+ (* 9 +hour+) (* 30 +min+))
  "The proposed amount of time to be asleep for.")

(defparameter *now-timestamp* (get-universal-time))

(defun date-string (timestamp)
  "Returns a human-readable date representation of TIMESTAMP."
  (multiple-value-bind
        (second minute hour date month year dow)
      (decode-universal-time timestamp)
    (format nil "~2,'0d:~2,'0d of ~a, ~2,'0d/~2,'0d/~d"
            hour minute
            (nth dow *day-names*)
            date month year)))

(defun time-string (timestamp)
  "Returns a human-readable time representation of TIMESTAMP."
  (multiple-value-bind
        (second minute hour)
      (decode-universal-time timestamp)
    (format nil "~2,'0d:~2,'0d"
            hour minute)))

(defun date-with-description (timestamp description tab-column print-p)
  "Prints or returns TIMESTAMP formatted and labelled with
DESCRIPTION, with a separation of TAB-COLUMN."
  (format print-p (concatenate 'string
                               "~a:~" (write-to-string tab-column)
                               "t~a~%")
          description (date-string timestamp)))

(defun adjust-timestamp (hour minute timestamp)
  "Returns TIMESTAMP replaced with HOUR and MINUTE."
  (multiple-value-bind
        (parsed-second
         parsed-minute
         parsed-hour
         parsed-date
         parsed-month
         parsed-year)
      (decode-universal-time timestamp)
    (encode-universal-time 0 minute hour
                           parsed-date parsed-month parsed-year)))

(defun bedtime-when-wake-at (timestamp)
  "Returns the timestamp of HOUR and MINUTE of the current day, minus
*TIME-IN-BED* to give a bedtime."
  (- timestamp
     *time-in-bed*))

(defun waketime-when-sleep-at (timestamp)
  "Returns the timestamp of HOUR and MINUTE of the current day, plus
*TIME-IN-BED* to give a waking-up time."
  (+ timestamp
     *time-in-bed*))

(defun today (timestamp)
  "Day-shifter for today."
  timestamp)

(defun tomorrow (timestamp)
  "Day-shifter for tomorrow."
  (+ timestamp (* 24 +hour+)))

(defun wake-at (hour minute &optional (day-shift 'tomorrow))
  (let* ((bedtime (funcall day-shift (bedtime-when-wake-at hour minute)))
         (waketime (funcall day-shift (adjust-timestamp hour minute
                                                        *now-timestamp*)))
         (dress-down (- bedtime (* 15 +min+))))
    (date-with-description dress-down 'dress-down 12)
    (date-with-description bedtime 'bedtime 12)))

(defun sleep-at (hour minute &optional (day-shift 'tomorrow))
  (let* ((bedtime (funcall day-shift (adjust-timestamp hour minute
                                                       *now-timestamp*)))
         (waketime (funcall day-shift (waketime-when-sleep-at hour minute)))
         (dress-down (- bedtime (* 15 +min+))))
    (date-with-description dress-down 'dress-down 12)
    (date-with-description waketime 'waketime 12)))

(defun org-timestamp (timestamp)
  "Return an org-timestamp representing TIMESTAMP."
  (multiple-value-bind
        (second minute hour date month year dow)
      (decode-universal-time timestamp)
    (format nil "<~a-~2,'0d-~2,'0d ~a ~2,'0d:~2,'0d>"
            year month date
            (subseq (nth dow *day-names*) 0 3) hour minute)))

(defun scheduled (org-timestamp)
  "Returns ORG-TIMESTAMP with the scheduled tag."
  (format nil "SCHEDULED: ~a" org-timestamp))
