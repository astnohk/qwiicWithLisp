(defpackage :i2c-haptic
    (:use :cl)
    (:export
        :enable-freq-track
        :set-operation-mode-idle
        :set-operation-mode-dro
        :set-override-val))

(in-package :i2c-haptic)

(defconstant DEVADDR #x4a)

(require :i2c-dev)

(defparameter *rbuffer* (i2c-dev:create-i2c-buffer 2))
(defparameter *wbuffer* (i2c-dev:create-i2c-buffer 2))

(defun enable-freq-track (enable)
    (setf (i2c-dev:i2c-buffer-aref *wbuffer* 0)
          #x13)
    (setf (i2c-dev:i2c-buffer-aref *wbuffer* 1)
          (logior #x16
                  (if (= 0 enable) #x00 #x08)))
    (i2c-dev:i2c-write DEVADDR
                       *wbuffer*
                       2))

(defun set-operation-mode-idle ()
    (setf (i2c-dev:i2c-buffer-aref *wbuffer* 0)
          #x22)
    (setf (i2c-dev:i2c-buffer-aref *wbuffer* 1)
          #x00)
    (i2c-dev:i2c-write DEVADDR
                       *wbuffer*
                       2))

(defun set-operation-mode-dro ()
    (setf (i2c-dev:i2c-buffer-aref *wbuffer* 0)
          #x22)
    (setf (i2c-dev:i2c-buffer-aref *wbuffer* 1)
          #x01)
    (i2c-dev:i2c-write DEVADDR
                       *wbuffer*
                       2))

(defun set-override-val (val)
    (setf (i2c-dev:i2c-buffer-aref *wbuffer* 0)
          #x23)
    (setf (i2c-dev:i2c-buffer-aref *wbuffer* 1)
          (logand #xFF
                  val))
    (i2c-dev:i2c-write DEVADDR
                       *wbuffer*
                       2))

