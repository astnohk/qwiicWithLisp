(defpackage :i2c-lsm6ds3tr
    (:use :cl)
    (:export
        :wake-gyro-accel
        :read-temp-gyro-accel
        :wake-magnet
        :read-magnet))
(in-package :i2c-lsm6ds3tr)

(require :i2c-dev)

(defconstant LSM6DS3TR_DEVADDR #x6a)
(defconstant LIS3MDL_DEVADDR #x1c)

;
; Parameters
;
(defparameter *wbuf* (i2c-dev:create-i2c-buffer 2))
(defparameter *temp-gyro-accel* (i2c-dev:create-i2c-buffer 14))
(defparameter *magnet* (i2c-dev:create-i2c-buffer 6))

;
; Gyro Accelorometer
;
(defun wake-gyro-accel ()
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x10)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #x3c)
    (i2c-dev:i2c-write LSM6DS3TR_DEVADDR
                       *wbuf*
                       2)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x11)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #x30)
    (i2c-dev:i2c-write LSM6DS3TR_DEVADDR
                       *wbuf*
                       2))

(defun parse-temp-gyro-accel (buf)
    (values
        ; temp
        (+ (ash (i2c-dev:i2c-buffer-aref buf 1) 8)
           (logand #xFF (i2c-dev:i2c-buffer-aref buf 0)))
        ; gyro
        (+ (ash (i2c-dev:i2c-buffer-aref buf 3) 8)
           (logand #xFF (i2c-dev:i2c-buffer-aref buf 2)))
        (+ (ash (i2c-dev:i2c-buffer-aref buf 5) 8)
           (logand #xFF (i2c-dev:i2c-buffer-aref buf 4)))
        (+ (ash (i2c-dev:i2c-buffer-aref buf 7) 8)
           (logand #xFF (i2c-dev:i2c-buffer-aref buf 6)))
        ; accel
        (+ (ash (i2c-dev:i2c-buffer-aref buf 9) 8)
           (logand #xFF (i2c-dev:i2c-buffer-aref buf 8)))
        (+ (ash (i2c-dev:i2c-buffer-aref buf 11) 8)
           (logand #xFF (i2c-dev:i2c-buffer-aref buf 10)))
        (+ (ash (i2c-dev:i2c-buffer-aref buf 13) 8)
           (logand #xFF (i2c-dev:i2c-buffer-aref buf 12)))))

(defun read-temp-gyro-accel ()
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x20)
    (i2c-dev:i2c-read-register LSM6DS3TR_DEVADDR
                               *wbuf*
                               1
                               *temp-gyro-accel*
                               14)
    (parse-temp-gyro-accel *temp-gyro-accel*))

;
; Magnetometer
;
(defun wake-magnet ()
    ; CTRL_REG1 (20h)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x20)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #x30)
    (i2c-dev:i2c-write LIS3MDL_DEVADDR
                       *wbuf*
                       2)
    ; CTRL_REG3 (22h)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x22)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #x00)
    (i2c-dev:i2c-write LIS3MDL_DEVADDR
                       *wbuf*
                       2)
    ; CRTL_REG4 (23h)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x23)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #x04)
    (i2c-dev:i2c-write LIS3MDL_DEVADDR
                       *wbuf*
                       2))

(defun parse-magnet (buf)
    (values
        (+ (ash (i2c-dev:i2c-buffer-aref buf 1) 8)
           (logand #xFF (i2c-dev:i2c-buffer-aref buf 0)))
        (+ (ash (i2c-dev:i2c-buffer-aref buf 3) 8)
           (logand #xFF (i2c-dev:i2c-buffer-aref buf 2)))
        (+ (ash (i2c-dev:i2c-buffer-aref buf 5) 8)
           (logand #xFF (i2c-dev:i2c-buffer-aref buf 4)))))

(defun read-magnet ()
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x28)
    (i2c-dev:i2c-read-register LIS3MDL_DEVADDR
                               *wbuf*
                               1
                               *magnet*
                               6)
    (parse-magnet *magnet*))

