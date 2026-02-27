(load "ads7830.lisp")

(loop
    (format t "read all channels.~%")
    (format t "CH0: ~d~%"
        (i2c-ads7830:read-single-ended 0))
    (format t "CH1: ~d~%"
        (i2c-ads7830:read-single-ended 1))
    (format t "CH2: ~d~%"
        (i2c-ads7830:read-single-ended 2))
    (format t "CH3: ~d~%"
        (i2c-ads7830:read-single-ended 3))
    (format t "CH4: ~d~%"
        (i2c-ads7830:read-single-ended 4))
    (format t "CH5: ~d~%"
        (i2c-ads7830:read-single-ended 5))
    (format t "CH6: ~d~%"
        (i2c-ads7830:read-single-ended 6))
    (format t "CH7: ~d~%"
        (i2c-ads7830:read-single-ended 7))
    (format t "~%")
    (sleep 0.25))

(i2c-dev:free-i2c-buffer *rbuffer*)
(i2c-dev:free-i2c-buffer *wbuffer*)

