(load "./haptic.lisp")

(defun get-elapsed-time () (/ (get-internal-real-time) internal-time-units-per-second))

(format t "start.~%")
(i2c-dev:set-i2c-dev "/dev/i2c-1")
(i2c-haptic:set-override-val #x00)
(defun haptic-loop ()
    (let ((last-beep-time (get-elapsed-time)))
        (loop
            (let ((duration 0.5)
                  (now (get-elapsed-time)))
                (cond ((> (- now last-beep-time) duration)
                           (format t "beep.~%")
                           (setf last-beep-time now)
                           (i2c-haptic:set-override-val #x7f)
                           (sleep 0.02)))
                (i2c-haptic:set-override-val #x00)
                (sleep 0.1)))))

(haptic-loop)
