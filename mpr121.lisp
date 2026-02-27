(defpackage :i2c-mpr121
    (:use :cl)
    (:export
        :reset
        :start
        :read-touch-status
        :read-electrode-filtered
        :free-resources))
(in-package :i2c-mpr121)

(require :i2c-dev)

(defconstant DEVADDR #x5a)

(defparameter *rbuffer* (i2c-dev:create-i2c-buffer 26))
(defparameter *wbuffer* (i2c-dev:create-i2c-buffer 2))

(defun reset ()
    (setf (i2c-dev:i2c-buffer-aref *wbuffer* 0) #x80)
    (setf (i2c-dev:i2c-buffer-aref *wbuffer* 1) #x63)
    (i2c-dev:i2c-write DEVADDR
                       *wbuffer*
                       2))

(defun start ()
    (setf (i2c-dev:i2c-buffer-aref *wbuffer* 0) #x5e)
    (setf (i2c-dev:i2c-buffer-aref *wbuffer* 1)
          (logior (ash #x02 6)  ; CL (Calibration Lock)
                  (ash #x00 4)  ; ELEPROX_EN
                  #x0c))        ; ELE_EN
    (i2c-dev:i2c-write DEVADDR
                       *wbuffer*
                       2))

(defun read-touch-status ()
    (setf (i2c-dev:i2c-buffer-aref *wbuffer* 0) #x00)
    (i2c-dev:i2c-read-register DEVADDR
                               *wbuffer*
                               1
                               *rbuffer*
                               2)
    (let ((data (logior (ash 8
                             (logand #x1f
                                     (i2c-dev:i2c-buffer-aref *rbuffer* 1)))
                        (i2c-dev:i2c-buffer-aref *rbuffer* 0))))
        (loop for i from 0 below 13 collect
            (logand #x01
                    (ash data (* -1 i))))))

(defun read-electrode-filtered ()
    (setf (i2c-dev:i2c-buffer-aref *wbuffer* 0) #x04)
    (i2c-dev:i2c-read-register DEVADDR
                               *wbuffer*
                               1
                               *rbuffer*
                               26)
    (loop for i from 0 below 13 collect
        (logior (ash 8
                     (logand #x03
                             (i2c-dev:i2c-buffer-aref *rbuffer* (+ 1 (* 2 i)))))
                (i2c-dev:i2c-buffer-aref *rbuffer* (* 2 i)))))

(defun free-resources ()
    (if *rbuffer* (cffi:foreign-free *rbuffer*))
    (setf *rbuffer* nil)
    (if *wbuffer* (cffi:foreign-free *wbuffer*))
    (setf *wbuffer* nil))

