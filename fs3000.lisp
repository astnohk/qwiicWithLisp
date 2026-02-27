(defpackage :i2c-fs3000
    (:use :cl)
    (:export
        :read-data
        :free-resources))
(in-package :i2c-fs3000)

(require :i2c-dev)

(defconstant DEVADDR #x28)
(defparameter *buffer* (i2c-dev:create-i2c-buffer 5))

(defun read-data ()
    (i2c-dev:i2c-read DEVADDR *buffer* 5)
    (let ((count
        (+ (ash (logand #x0f
                        (i2c-dev:i2c-buffer-aref *buffer* 1))
                8)
           (i2c-dev:i2c-buffer-aref *buffer* 2))))
        (* 7.23
           (/ (max 0 (- count 409))
              (- 3686 409)))))

(defun free-resources ()
    (cffi:foreign-free *buffer*))

