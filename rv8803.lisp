(defpackage :I2C-RV8803
    (:use :cl)
    (:export
        :read-date
        :isoformat
        :write-date
        :set-timer-counter
        :set-te
        :set-td
        :set-fd
        :set-tie
        :clear-flags
        :read-flags))
(in-package :I2C-RV8803)

(require :cffi)
(require :local-time)

(defconstant DEVADDR #x32)
(defconstant REGISTERS_LENGTH 7)
(defconstant CENTURY 21)

(defconstant REGADDR-SECONDS #x00)
(defconstant REGADDR-WADA #x0A)
(defconstant REGADDR-TIMER-COUNTER-0 #x0B)
(defconstant REGADDR-TIMER-COUNTER-1 #x0C)
(defconstant REGADDR-EXTENSION #x0D)
(defconstant REGADDR-FLAG #x0E)
(defconstant REGADDR-CONTROL #x0F)
(defconstant REGADDR-OFFSET #x2C)

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
(defun set-dev (device-path)
    (if *dev*
        (cffi:foreign-free *dev*))
    (setf *dev*
          (cffi:foreign-string-alloc device-path)))

(defun read-date ()
    (setf (cffi:mem-aref *wbuffer* :uint8 0)
          REGADDR-SECONDS)
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
              REGADDR-SECONDS)
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

(defun set-timer-counter (val)
    (setf (cffi:mem-aref *wbuffer* :uint8 0)
          REGADDR-TIMER-COUNTER-0)
    (setf (cffi:mem-aref *wbuffer* :uint8 1)
          (logand #xFF val))
    (setf (cffi:mem-aref *wbuffer* :uint8 2)
          (logand #x0F
                  (ash val -8)))
    (i2c-write
        *dev*
        DEVADDR
        *wbuffer*
        3))

(defun set-te (enable)
    (let ((oldreg #x00))
        ; Read old registers
        (setf (cffi:mem-aref *wbuffer* :uint8 0)
              REGADDR-EXTENSION)
        (i2c-read-register
            *dev*
            DEVADDR
            *wbuffer*
            1
            *rbuffer*
            1)
        (setf oldreg
              (cffi:mem-aref *rbuffer* :uint8 0))
        ; Write
        (setf (cffi:mem-aref *wbuffer* :uint8 0)
              REGADDR-EXTENSION)
        (setf (cffi:mem-aref *wbuffer* :uint8 1)
              (logior (logand #xEF oldreg)
                      (if enable #x10 #x00)))
        (i2c-write
            *dev*
            DEVADDR
            *wbuffer*
            2)))

(defun set-td (val)
    (let ((oldreg #x00))
        ; Read old registers
        (setf (cffi:mem-aref *wbuffer* :uint8 0)
              REGADDR-EXTENSION)
        (i2c-read-register
            *dev*
            DEVADDR
            *wbuffer*
            1
            *rbuffer*
            1)
        (setf oldreg
              (cffi:mem-aref *rbuffer* :uint8 0))
        ; Write
        (setf (cffi:mem-aref *wbuffer* :uint8 0)
              REGADDR-EXTENSION)
        (setf (cffi:mem-aref *wbuffer* :uint8 1)
              (logior (logand #xFC oldreg)
                      (logand #x03 val)))
        (i2c-write
            *dev*
            DEVADDR
            *wbuffer*
            2)))

(defun set-fd (val)
    (let ((oldreg #x00))
        ; Read old registers
        (setf (cffi:mem-aref *wbuffer* :uint8 0)
              REGADDR-EXTENSION)
        (i2c-read-register
            *dev*
            DEVADDR
            *wbuffer*
            1
            *rbuffer*
            1)
        (setf oldreg
              (cffi:mem-aref *rbuffer* :uint8 0))
        (setf (cffi:mem-aref *wbuffer* :uint8 0)
              REGADDR-EXTENSION)
        (setf (cffi:mem-aref *wbuffer* :uint8 1)
              (logior (logand #xF3 oldreg)
                      (logand #x0C
                              (ash val 2))))
        (i2c-write
            *dev*
            DEVADDR
            *wbuffer*
            2)))

(defun clear-flags ()
    (setf (cffi:mem-aref *wbuffer* :uint8 0)
          REGADDR-FLAG)
    (setf (cffi:mem-aref *wbuffer* :uint8 1)
          #x00)
    (i2c-write
        *dev*
        DEVADDR
        *wbuffer*
        2))

(defun read-flags ()
    (let ((reg #x00))
        ; Read FLAG registers
        (setf (cffi:mem-aref *wbuffer* :uint8 0)
              REGADDR-FLAG)
        (i2c-read-register
            *dev*
            DEVADDR
            *wbuffer*
            1
            *rbuffer*
            1)
        (setf reg
              (cffi:mem-aref *rbuffer* :uint8 0))
        (format t "Voltage Low Flag:              ~d~%" (if (> (logand #x01 reg) 0) 1 0))
        (format t "Voltage Low Flag:              ~d~%" (if (> (logand #x02 reg) 0) 1 0))
        (format t "External Event Flag:           ~d~%" (if (> (logand #x04 reg) 0) 1 0))
        (format t "Alarm Flag:                    ~d~%" (if (> (logand #x08 reg) 0) 1 0))
        (format t "Periodic Countdown Timer Flag: ~d~%" (if (> (logand #x10 reg) 0) 1 0))
        (format t "Periodic Time Update Flag:     ~d~%" (if (> (logand #x20 reg) 0) 1 0))))

(defun set-tie (enable)
    (let ((oldreg #x00))
        ; Read old registers
        (setf (cffi:mem-aref *wbuffer* :uint8 0)
              REGADDR-CONTROL)
        (i2c-read-register
            *dev*
            DEVADDR
            *wbuffer*
            1
            *rbuffer*
            1)
        (setf oldreg
              (cffi:mem-aref *rbuffer* :uint8 0))
        ; Write
        (setf (cffi:mem-aref *wbuffer* :uint8 0)
              REGADDR-CONTROL)
        (setf (cffi:mem-aref *wbuffer* :uint8 1)
              (logior (logand #xEF oldreg)
                      (if enable #x10 #x00)))
        (i2c-write
            *dev*
            DEVADDR
            *wbuffer*
            2)))
