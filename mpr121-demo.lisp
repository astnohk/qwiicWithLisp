(load "./mpr121.lisp")

(i2c-mpr121:start)
(loop
    (format t "~a~%" (i2c-mpr121:read-touch-status))
    (format t "~a~%" (i2c-mpr121:read-electrode-filtered))
    (sleep 0.125))
(i2c-mpr121:free-resources)
