(defpackage :i2c-aw9523
    (:use :cl)
    (:export
        :LED-P1_0-DIM
        :LED-P1_1-DIM
        :LED-P1_2-DIM
        :LED-P1_3-DIM
        :LED-P0_0-DIM
        :LED-P0_1-DIM
        :LED-P0_2-DIM
        :LED-P0_3-DIM
        :LED-P0_4-DIM
        :LED-P0_5-DIM
        :LED-P0_6-DIM
        :LED-P0_7-DIM
        :LED-P1_4-DIM
        :LED-P1_5-DIM
        :LED-P1_6-DIM
        :LED-P1_7-DIM
        :set-led-mode
        :set-current-limit
        :set-led-current))
(in-package :i2c-aw9523)

(require :i2c-dev)

(defconstant DEVADDR #x58)

;
; Constants
;
(defconstant LED-P1_0-DIM #x20)
(defconstant LED-P1_1-DIM #x21)
(defconstant LED-P1_2-DIM #x22)
(defconstant LED-P1_3-DIM #x23)
(defconstant LED-P0_0-DIM #x24)
(defconstant LED-P0_1-DIM #x25)
(defconstant LED-P0_2-DIM #x26)
(defconstant LED-P0_3-DIM #x27)
(defconstant LED-P0_4-DIM #x28)
(defconstant LED-P0_5-DIM #x29)
(defconstant LED-P0_6-DIM #x2a)
(defconstant LED-P0_7-DIM #x2b)
(defconstant LED-P1_4-DIM #x2c)
(defconstant LED-P1_5-DIM #x2d)
(defconstant LED-P1_6-DIM #x2e)
(defconstant LED-P1_7-DIM #x2f)

;
; Parameters
;
(defparameter *wbuf* (i2c-dev:create-i2c-buffer 2))

;
; LED Controller
;
(defun set-led-mode ()
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x12)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #x00)
    (i2c-dev:i2c-write DEVADDR *wbuf* 2)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x13)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #x00)
    (i2c-dev:i2c-write DEVADDR *wbuf* 2))

(defun set-current-limit ()
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x11)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #x03)
    (i2c-dev:i2c-write DEVADDR *wbuf* 2))

(defun set-led-current (port current)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) port)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) current)
    (i2c-dev:i2c-write DEVADDR *wbuf* 2))

