(load "./rv8803.lisp")

; Set current date.
(i2c-rv8803:write-date)
; Read date from RV8803-C and show print it in ISO format.
(format t "~a~%" (i2c-rv8803:isoformat (i2c-rv8803:read-date)))

; Set periodic countdown timer to
; output interrupt on each 30sec.

; Set 1Hz (TD = 0b10).
(i2c-rv8803:set-td 2)
; Set 30sec.
(i2c-rv8803:set-timer-counter 30)
; Enable countdown timer by setting TE = 1
; and enable interrupt signal output by setting TIE = 1
(i2c-rv8803:set-te t)
(i2c-rv8803:set-tie t)
(format t "Start countdown timer interrupt.~%")

