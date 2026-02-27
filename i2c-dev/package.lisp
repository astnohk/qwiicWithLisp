(defpackage :I2C-DEV
    (:use :cl :cffi)
    (:export
        :set-i2c-dev
        :create-i2c-buffer
        :free-i2c-buffer
        :i2c-buffer-aref
        :i2c-read
        :i2c-read-register
        :i2c-write))
