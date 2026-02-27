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

(require :i2c-dev)

;
; Generic Functions
;
(defun get-elapsed-time () (/ (get-internal-real-time) internal-time-units-per-second))
(defun zeros (N) (loop for i from 0 below N collect 0))


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
(defparameter *rbuf* (i2c-dev:create-i2c-buffer (* 64 2)))
(defparameter *wbuf* (i2c-dev:create-i2c-buffer 10))
(defparameter *temperature* (make-array '(64) :initial-element 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AMG8833 Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reset ()
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x01)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #x30)
    (i2c-dev:i2c-write DEVADDR *wbuf* 2))

(defun set-framerate (enable-high-framerate)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x02)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) (if enable-high-framerate #x00 #x01))
    (i2c-dev:i2c-write DEVADDR *wbuf* 2))

(defun set-average (enable)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x1F)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #x50)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 2) #x1F)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 3) #x45)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 4) #x1F)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 5) #x57)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 6) #x07)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 7) (if enable #x20 #x00))
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 8) #x1F)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 9) #x00)
    (i2c-dev:i2c-write DEVADDR *wbuf* 10))

(defun read-status ()
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x04)
    (i2c-dev:i2c-read-register DEVADDR *wbuf* 1 *rbuf* 1)
    (i2c-dev:i2c-buffer-aref *rbuf* 0))

(defun read-temperature ()
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x80)
    (i2c-dev:i2c-write DEVADDR *wbuf* 1)
    (i2c-dev:i2c-read-register DEVADDR *wbuf* 1 *rbuf* (* 64 2))
    (loop for i from 0 below 64 do
        (let* ((vl (i2c-dev:i2c-buffer-aref *rbuf* (* 2 i)))
               (vh_raw (i2c-dev:i2c-buffer-aref *rbuf* (+ (* 2 i) 1)))
               (vh (if (> (logand #x08 vh_raw) 0) (* -1 vh_raw) vh_raw)))
            (setf (aref *temperature* i)
                  (* TEMP-COEFF
                     (logior (ash vh 8)
                             vl))))))

