(defpackage :I2C-HTU21D
    (:use :cl)
    (:export
        :read-temperature
        :read-humidity))
(in-package :I2C-HTU21D)

(require :cffi)

(defconstant DEVADDR #x40)
(defconstant MAX_WAIT_COUNT 5)

(cffi:load-foreign-library "./i2c_lib.so")

(cffi:defcfun ("i2c_write" i2c-write) :uint8
    (dev (:pointer :char))
    (addr :uint8)
    (buf :pointer)
    (buf_len :size))

(cffi:defcfun ("i2c_read" i2c-read) :uint8
    (dev (:pointer :char))
    (addr :uint8)
    (read_buf :pointer)
    (read_buf_len :size))

(cffi:defcfun ("i2c_read_register" i2c-read-register) :uint8
    (dev (:pointer :char))
    (addr :uint8)
    (write_buf :pointer)
    (write_buf_len :size)
    (read_buf :pointer)
    (read_buf_len :size))

(defparameter *dev* (cffi:foreign-string-alloc "/dev/i2c-1"))
(defparameter *buffer*
    (cffi:foreign-alloc :uint8
                        :initial-contents
                        (loop for x from 0 below 3 collect 0)))

(defun set-dev (device-path)
    (if *dev*
        (cffi:foreign-free *dev*))
    (setf *dev*
          (cffi:foreign-string-alloc device-path)))

(defun read-wait (count N-trial)
    (sleep 0.1)
    (if (= 0
           (i2c-read *dev* DEVADDR *buffer* 3))
        (logior (ash (cffi:mem-aref *buffer* :uint8 0) 8)
                (cffi:mem-aref *buffer* :uint8 1))
        (if (< count N-trial)
            (read-wait (+ 1 count) N-trial)
            #xFFFF)))

(defun read-temperature ()
    (setf (cffi:mem-aref *buffer* :uint8 0) #xF3)
    (i2c-write *dev*
               DEVADDR
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
    (setf (cffi:mem-aref *buffer* :uint8 0) #xF5)
    (i2c-write *dev*
               DEVADDR
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
    (cffi:foreign-free *dev*)
    (cffi:foreign-free *buffer*))

