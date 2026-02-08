(load "./amg8833.lisp")

(defun show-data (data)
    (loop for y from 0 below 8 do
        (loop for x from 0 below 8 do
            (format t " ~d" (aref data
                                  (+ (* y 8) x))))
        (format t "~%"))
    (format t "~%"))

(i2c-amg8833:reset)
(sleep 0.25)
(i2c-amg8833:set-framerate t)
(i2c-amg8833:set-average t)
(sleep 0.5)
(format t "~a~%" (i2c-amg8833:read-status))
(loop
    (i2c-amg8833:read-temperature)
    (show-data i2c-amg8833:*temperature*)
    (sleep 0.5))
