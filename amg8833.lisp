(defpackage :i2c-amg8833
    (:use :cl)
    (:export
        :reset
        :set-framerate
        :set-average
        :read-status
        :read-temperature
        *temperature*))
(in-package :i2c-amg8833)

(require :cffi)

;
; Generic Functions
;
(defun get-elapsed-time () (/ (get-internal-real-time) internal-time-units-per-second))
(defun zeros (N) (loop for i from 0 below N collect 0))

;
; Generic I2C Driver
;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AMG8833 Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defconstant DEVADDR #x68)
(defconstant DEVADDR #x69)

(defconstant STATUS-INTERRUPT-OUTBREAK-BIT 1)
(defconstant STATUS-TEMP-OVERFLOW-BIT 2)
(defconstant TEMP-COEFF 0.25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AMG8833 Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *dev*
    (cffi:foreign-string-alloc "/dev/i2c-1"))
(defparameter *rbuf*
    (cffi:foreign-alloc :uint8
                        :initial-contents
                        (zeros (* 64 2))))
(defparameter *wbuf*
    (cffi:foreign-alloc :uint8
                        :initial-contents
                        (zeros 10)))
(defparameter *temperature*
    (make-array '(64) :initial-element 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AMG8833 Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reset ()
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x01)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #x30)
    (i2c-write *dev* DEVADDR *wbuf* 2))

(defun set-framerate (enable-high-framerate)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x02)
    (setf (cffi:mem-aref *wbuf* :uint8 1) (if enable-high-framerate #x00 #x01))
    (i2c-write *dev* DEVADDR *wbuf* 2))

(defun set-average (enable)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x1F)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #x50)
    (setf (cffi:mem-aref *wbuf* :uint8 2) #x1F)
    (setf (cffi:mem-aref *wbuf* :uint8 3) #x45)
    (setf (cffi:mem-aref *wbuf* :uint8 4) #x1F)
    (setf (cffi:mem-aref *wbuf* :uint8 5) #x57)
    (setf (cffi:mem-aref *wbuf* :uint8 6) #x07)
    (setf (cffi:mem-aref *wbuf* :uint8 7) (if enable #x20 #x00))
    (setf (cffi:mem-aref *wbuf* :uint8 8) #x1F)
    (setf (cffi:mem-aref *wbuf* :uint8 9) #x00)
    (i2c-write *dev* DEVADDR *wbuf* 10))

(defun read-status ()
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x04)
    (i2c-read-register *dev* DEVADDR *wbuf* 1 *rbuf* 1)
    (cffi:mem-aref *rbuf* :uint8 0))

(defun read-temperature ()
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x80)
    (i2c-write *dev* DEVADDR *wbuf* 1)
    (i2c-read-register *dev* DEVADDR *wbuf* 1 *rbuf* (* 64 2))
    (loop for i from 0 below 64 do
        (let* ((vl (cffi:mem-aref *rbuf*
                                  :uint8
                                  (* 2 i)))
               (vh_raw (cffi:mem-aref *rbuf*
                                      :int8
                                      (+ (* 2 i) 1)))
               (vh (if (> (logand #x08 vh_raw) 0) (* -1 vh_raw) vh_raw)))
            (setf (aref *temperature* i)
                  (* TEMP-COEFF
                     (logior (ash vh 8)
                             vl))))))

