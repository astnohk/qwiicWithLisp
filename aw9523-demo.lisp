(load "aw9523.lisp")

;
; Generic Functions
;
(defun get-elapsed-time () (/ (get-internal-real-time) internal-time-units-per-second))

;
; MAIN
;
(i2c-aw9523:set-led-mode)
(i2c-aw9523:set-current-limit)
(loop
    (i2c-aw9523:set-led-current i2c-aw9523:LED-P0_1-DIM #x50)
    (sleep 0.5)
    (i2c-aw9523:set-led-current i2c-aw9523:LED-P0_1-DIM #x00)
    (sleep 0.5))

