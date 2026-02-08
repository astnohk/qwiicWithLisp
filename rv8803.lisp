(defpackage :I2C-RV8803
    (:use :cl)
    (:export :read-date
             :isoformat
             :write-date))
(in-package :I2C-RV8803)

(require :cffi)
(require :local-time)

(defconstant DEVADDR #x32)
(defconstant REGISTERS_LENGTH 7)
(defconstant CENTURY 21)

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
(defparameter *rbuffer*
    (cffi:foreign-alloc :uint8
                        :initial-contents
                        (loop for x from 0 below REGISTERS_LENGTH collect 0)))
(defparameter *wbuffer*
    (cffi:foreign-alloc :uint8
                        :initial-contents
                        (loop for x from 0 below 8 collect 0)))

;
; low-level functions for RV-8803
;
(defun read-date ()
    (setf (cffi:mem-aref *wbuffer* :uint8 0)
          #x00)
    (i2c-read-register
        *dev*
        DEVADDR
        *wbuffer*
        1
        *rbuffer*
        REGISTERS_LENGTH)
    (loop for x from 0 below REGISTERS_LENGTH collect
        (cffi:mem-aref *rbuffer* :uint8 x)))

(defun isoformat (date)
    (format nil "~d~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
        (- CENTURY 1)
        (nth 6 date)
        (nth 5 date)
        (nth 4 date)
        (nth 2 date)
        (nth 1 date)
        (nth 0 date)))

(defun write-date ()
    (let* ((now (local-time:now))
           (second (local-time:timestamp-second now))
           (minute (local-time:timestamp-minute now))
           (hour (local-time:timestamp-hour now))
           (day (+ 1 (local-time:timestamp-day-of-week now)))
           (date (local-time:timestamp-day now))
           (month (local-time:timestamp-month now))
           (year (mod (local-time:timestamp-year now) 100)))
        (setf (cffi:mem-aref *wbuffer* :uint8 0)
              #x00)
        (setf (cffi:mem-aref *wbuffer* :uint8 1)
              second)
        (setf (cffi:mem-aref *wbuffer* :uint8 2)
              minute)
        (setf (cffi:mem-aref *wbuffer* :uint8 3)
              hour)
        (setf (cffi:mem-aref *wbuffer* :uint8 4)
              day)
        (setf (cffi:mem-aref *wbuffer* :uint8 5)
              date)
        (setf (cffi:mem-aref *wbuffer* :uint8 6)
              month)
        (setf (cffi:mem-aref *wbuffer* :uint8 7)
              year)
        (i2c-write
            *dev*
            DEVADDR
            *wbuffer*
            (+ 1 REGISTERS_LENGTH))))
