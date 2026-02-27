(load "lsm6ds3tr.lisp")

(i2c-lsm6ds3tr:wake-gyro-accel)
(i2c-lsm6ds3tr:wake-magnet)
(loop
    (multiple-value-bind
        (temp gx gy gz ax ay az)
        (i2c-lsm6ds3tr:read-temp-gyro-accel)
        (format t "~d (~d, ~d, ~d) (~d, ~d, ~d)~%"
            temp gx gy gz ax ay az))
    (multiple-value-bind
        (mx my mz)
        (i2c-lsm6ds3tr:read-magnet)
        (format t "(~d, ~d, ~d)~%"
            mx my mz))
    (sleep 0.125))

