(defpackage :I2C-HTU21D
    (:use :cl)
    (:export
        :read-temperature
        :read-humidity))
(in-package :I2C-HTU21D)

(require :i2c-dev)

(defconstant DEVADDR #x40)
(defconstant MAX_WAIT_COUNT 5)

(i2c-dev:set-i2c-dev "/dev/i2c-1")
(defparameter *buffer* (i2c-dev:create-i2c-buffer 3))

(defun read-wait (count N-trial)
    (sleep 0.1)
    (if (= 0
           (i2c-dev:i2c-read DEVADDR *buffer* 3))
        (logior (ash (i2c-dev:i2c-buffer-aref *buffer* 0) 8)
                (i2c-dev:i2c-buffer-aref *buffer* 1))
        (if (< count N-trial)
            (read-wait (+ 1 count) N-trial)
            #xFFFF)))

(defun read-temperature ()
    (setf (i2c-dev:i2c-buffer-aref *buffer* 0) #xF3)
    (i2c-dev:i2c-write DEVADDR
                       *buffer*
                       1)
    (let* ((ret (read-wait 0 MAX_WAIT_COUNT))
           (val (if (/= ret #xFFFF)
                (logand #xFFFC ret)
                0)))
        (- (* 175.72
              (/ val
                 65536))
           46.85)))

(defun read-humidity ()
    (setf (i2c-dev:i2c-buffer-aref *buffer* 0) #xF5)
    (i2c-dev:i2c-write DEVADDR
                       *buffer*
                       1)
    (let* ((ret (read-wait 0 10))
           (val (if (/= ret #xFF)
                    (logand #xFFFC ret)
                    0)))
        (- (* 125
              (/ val
                 65536))
           6.0)))

(defun free ()
    (i2c-dev:free-i2c-buffer *buffer*))

