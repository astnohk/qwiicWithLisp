(defpackage :i2c-mpr121
    (:use :cl)
    (:export
        :reset
        :start
        :read-touch-status
        :read-electrode-filtered
        :free-resources))
(in-package :i2c-mpr121)

(require :cffi)

(defconstant DEVADDR #x5a)

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
(defparameter *wbuffer*
    (cffi:foreign-alloc :uint8
                        :initial-contents
                        (loop for x from 0 below 2 collect 0)))
(defparameter *rbuffer*
    (cffi:foreign-alloc :uint8
                        :initial-contents
                        (loop for x from 0 below 26 collect 0)))

(defun reset ()
    (setf (cffi:mem-aref *wbuffer* :uint8 0) #x80)
    (setf (cffi:mem-aref *wbuffer* :uint8 1) #x63)
    (i2c-write *dev*
               DEVADDR
               *wbuffer*
               2))

(defun start ()
    (setf (cffi:mem-aref *wbuffer* :uint8 0) #x5e)
    (setf (cffi:mem-aref *wbuffer* :uint8 1)
          (logior (ash #x02 6)  ; CL (Calibration Lock)
                  (ash #x00 4)  ; ELEPROX_EN
                  #x0c))        ; ELE_EN
    (i2c-write *dev*
               DEVADDR
               *wbuffer*
               2))

(defun read-touch-status ()
    (setf (cffi:mem-aref *wbuffer* :uint8 0) #x00)
    (i2c-read-register *dev*
                       DEVADDR
                       *wbuffer*
                       1
                       *rbuffer*
                       2)
    (let ((data (logior (ash 8
                             (logand #x1f
                                     (cffi:mem-aref *rbuffer*
                                                    :uint8
                                                    1)))
                        (cffi:mem-aref *rbuffer*
                                       :uint8
                                       0))))
        (loop for i from 0 below 13 collect
            (logand #x01
                    (ash data (* -1 i))))))

(defun read-electrode-filtered ()
    (setf (cffi:mem-aref *wbuffer* :uint8 0) #x04)
    (i2c-read-register *dev*
                       DEVADDR
                       *wbuffer*
                       1
                       *rbuffer*
                       26)
    (loop for i from 0 below 13 collect
        (logior (ash 8
                     (logand #x03
                             (cffi:mem-aref *rbuffer*
                                            :uint8
                                            (+ 1 (* 2 i)))))
                (cffi:mem-aref *rbuffer*
                               :uint8
                               (* 2 i)))))

(defun free-resources ()
    (if *dev* (cffi:foreign-free *dev*))
    (if *wbuffer* (cffi:foreign-free *wbuffer*))
    (if *rbuffer* (cffi:foreign-free *rbuffer*)))
