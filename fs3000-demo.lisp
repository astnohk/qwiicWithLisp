(load "fs3000.lisp")

(loop
    (format t "~d~%" (i2c-fs3000:read-data))
    (sleep 0.125))

