(in-package :I2C-DEV)

(require :cffi)

(defparameter *dev*
              (cffi:foreign-string-alloc "/dev/i2c-1"))

;;;; Low level functions
(defun set-i2c-dev (device-path)
    (if *dev*
        (cffi:foreign-free *dev*))
    (setf *dev*
          (cffi:foreign-string-alloc device-path)))

(defun i2c-read ()
    0)

(defun i2c-read-register ()
    0)

(defun i2c-write ()
    0)

