(defpackage :i2c-haptic
    (:use :cl)
    (:export
        :set-dev
        :enable-freq-track
        :set-operation-mode-idle
        :set-operation-mode-dro
        :set-override-val))

(in-package :i2c-haptic)

(defconstant DEVADDR #x4a)

(require :cffi)
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
                        (loop for x from 0 below 2 collect 0)))
(defparameter *wbuffer*
    (cffi:foreign-alloc :uint8
                        :initial-contents
                        (loop for x from 0 below 2 collect 0)))

(defun set-dev (device-path)
    (if *dev*
        (cffi:foreign-free *dev*))
    (setf *dev*
        (cffi:foreign-string-alloc device-path)))

(defun enable-freq-track (enable)
    (setf (cffi:mem-aref *wbuffer* :uint8 0)
          #x13)
    (setf (cffi:mem-aref *wbuffer* :uint8 0)
          (logior #x16
                  (if (= 0 enable) #x00 #x08)))
    (i2c-write
        *dev*
        DEVADDR
        *wbuffer*
        2))

(defun set-operation-mode-idle ()
    (setf (cffi:mem-aref *wbuffer* :uint8 0)
          #x22)
    (setf (cffi:mem-aref *wbuffer* :uint8 0)
          #x00)
    (i2c-write
        *dev*
        DEVADDR
        *wbuffer*
        2))

(defun set-operation-mode-dro ()
    (setf (cffi:mem-aref *wbuffer* :uint8 0)
          #x22)
    (setf (cffi:mem-aref *wbuffer* :uint8 0)
          #x01)
    (i2c-write
        *dev*
        DEVADDR
        *wbuffer*
        2))

(defun set-override-val (val)
    (setf (cffi:mem-aref *wbuffer* :uint8 0)
          #x23)
    (setf (cffi:mem-aref *wbuffer* :uint8 0)
          (logand #xFF
                  val))
    (i2c-write
        *dev*
        DEVADDR
        *wbuffer*
        2))

