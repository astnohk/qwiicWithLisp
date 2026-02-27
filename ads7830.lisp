(defpackage :i2c-ads7830
    (:use :cl)
    (:export
        :set-framerate
        :set-average
        :read-status
        :read-channel
        :read-single-ended))
(in-package :i2c-ads7830)

(require :i2c-dev)

(defconstant DEVADDR #x48)

(defparameter *rbuffer* (i2c-dev:create-i2c-buffer 1))
(defparameter *wbuffer* (i2c-dev:create-i2c-buffer 1))

(defconstant DIFFERENTIAL-INPUTS 0)
(defconstant SINGLE-ENDED-INPUTS 1)
(defconstant PD-POWER-DOWN 0)
(defconstant PD-INTERNAL-OFF-AD-ON 1)
(defconstant PD-INTERNAL-ON-AD-OFF 2)
(defconstant PD-INTERNAL-ON-AD-ON 3)

(defun read-channel (ch SD PD)
    (setf (i2c-dev:i2c-buffer-aref *wbuffer* 0)
          (logior (ash SD 7)
                  (ash ch 4)
                  (ash PD 2)))
    (i2c-dev:i2c-read-register DEVADDR
                               *wbuffer*
                               1
                               *rbuffer*
                               1)
    (i2c-dev:i2c-buffer-aref *rbuffer* 0))

(defun read-single-ended (ch)
    (read-channel ch
                  SINGLE-ENDED-INPUTS
                  PD-INTERNAL-ON-AD-ON))
