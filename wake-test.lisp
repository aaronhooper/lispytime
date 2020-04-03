(defvar *utime1* 3794912238)
(defvar *utime2* 2793912499)

(define-test date-string-returns-correctly-formatted-string
  (assert-equal (date-string *utime1*)
                "15:17 of Friday, 03/04/2020")
  (assert-equal (date-string *utime2*)
                "23:48 of Thursday, 14/07/1988"))

(define-test time-string-returns-correctly-formatted-string
  (assert-equal (time-string *utime1*)
                "15:17")
  (assert-equal (time-string *utime2*)
                "23:48"))
                
                     
  




