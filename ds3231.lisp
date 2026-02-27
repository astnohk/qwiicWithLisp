(defpackage :i2c-ds3231
    (:use :cl)
    (:export
        :read-all-registers
        :read-date
        :parse-date
        :write-date))
(in-package :i2c-ds3231)

(require :local-time)
(require :i2c-dev)

(defconstant DEVADDR #x68)
(defconstant REGISTERS_LENGTH 18)

(defparameter *rbuffer* (i2c-dev:create-i2c-buffer REGISTERS_LENGTH))
(defparameter *wbuffer* (i2c-dev:create-i2c-buffer 8))

;
; low-level functions for DS3231
;
(defun read-all-registers ()
    (i2c-dev:i2c-read DEVADDR
                      *rbuffer*
                      REGISTERS_LENGTH)
    (loop for i from 0 below REGISTERS_LENGTH collect
        (i2c-dev:i2c-buffer-aref *rbuffer* i)))

(defun read-date()
    (let ((length 7)
          (value ()))
        (setf (i2c-dev:i2c-buffer-aref *wbuffer* 0)
              0)
        (i2c-dev:i2c-read-register DEVADDR
                                   *wbuffer*
                                   1
                                   *rbuffer*
                                   length)
        (setf value
              (loop for x from 0 below length collect
                  (i2c-dev:i2c-buffer-aref *rbuffer* x)))
        (loop for x from 0 below length do
            (format t "~4,'0b ~4,'0b~%"
                (ash (nth x value) -4)
                (logand #x0f (nth x value))))
        value))

(defun parse-value (value)
    (+ (* 10 (ash value -4))
       (logand value #x0f)))

(defun parse-date (value)
    (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            (+ 2000 (parse-value (nth 6 value)))
            (parse-value (nth 5 value))
            (parse-value (nth 4 value))
            (parse-value (nth 2 value))
            (parse-value (nth 1 value))
            (parse-value (nth 0 value))))

(defun write-date ()
    (let* ((length 7)
           (now (local-time:now))
           (second (local-time:timestamp-second now))
           (minute (local-time:timestamp-minute now))
           (hour (local-time:timestamp-hour now))
           (day (+ 1 (local-time:timestamp-day-of-week now)))
           (date (local-time:timestamp-day now))
           (month (local-time:timestamp-month now))
           (year (mod (local-time:timestamp-year now) 100)))
        (setf (i2c-dev:i2c-buffer-aref *wbuffer* 0)
              0)
        (setf (i2c-dev:i2c-buffer-aref *wbuffer* 1)
              (logior (ash (floor (/ second 10)) 4)
                      (mod second 10)))
        (setf (i2c-dev:i2c-buffer-aref *wbuffer* 2)
              (logior (ash (floor (/ minute 10)) 4)
                      (mod minute 10)))
        (setf (i2c-dev:i2c-buffer-aref *wbuffer* 3)
              (logior (ash (floor (/ hour 10)) 4)
                      (mod hour 10)))
        (setf (i2c-dev:i2c-buffer-aref *wbuffer* 4)
              day)
        (setf (i2c-dev:i2c-buffer-aref *wbuffer* 5)
              (logior (ash (floor (/ date 10)) 4)
                      (mod date 10)))
        (setf (i2c-dev:i2c-buffer-aref *wbuffer* 6)
              (logior (ash (floor (/ month 10)) 4)
                      (mod month 10)))
        (setf (i2c-dev:i2c-buffer-aref *wbuffer* 7)
              (logior (ash (floor (/ year 10)) 4)
                      (mod year 10)))
        (loop for x from 0 below length do
            (format t "~4,'0b ~4,'0b~%"
                (ash (i2c-dev:i2c-buffer-aref *wbuffer* (+ 1 x)) -4)
                (logand #x0f (i2c-dev:i2c-buffer-aref *wbuffer* (+ 1 x)))))
        (i2c-dev:i2c-write DEVADDR
                           *wbuffer*
                           (+ 1 length))))

