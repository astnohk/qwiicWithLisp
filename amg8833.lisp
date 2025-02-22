(require :cffi)

;
; Generic Functions
;
(defun get-elapsed-time () (/ (get-internal-real-time) internal-time-units-per-second))
(defun zeros (N) (loop for i from 0 below N collect 0))

;
; Generic I2C Driver
;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AMG8833 Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defconstant AMG8833_SLAVE_ADDR #x68)
(defconstant AMG8833_SLAVE_ADDR #x69)

(defconstant AMG8833-STATUS-INTERRUPT-OUTBREAK-BIT 1)
(defconstant AMG8833-STATUS-TEMP-OVERFLOW-BIT 2)
(defconstant AMG8833-TEMP-COEFF 0.25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AMG8833 Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *dev*
    (cffi:foreign-string-alloc "/dev/i2c-1"))
(defparameter *amg8833-rbuf*
    (cffi:foreign-alloc :uint8
                        :initial-contents
                        (zeros (* 64 2))))
(defparameter *amg8833-wbuf*
    (cffi:foreign-alloc :uint8
                        :initial-contents
                        (zeros 10)))
(defparameter *amg8833-temperature*
    (make-array '(64) :initial-element 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AMG8833 Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun amg8833-reset ()
    (setf (cffi:mem-aref *amg8833-wbuf* :uint8 0) #x01)
    (setf (cffi:mem-aref *amg8833-wbuf* :uint8 1) #x30)
    (i2c-write *dev* AMG8833_SLAVE_ADDR *amg8833-wbuf* 2))

(defun amg8833-set-framerate (enable-high-framerate)
    (setf (cffi:mem-aref *amg8833-wbuf* :uint8 0) #x02)
    (setf (cffi:mem-aref *amg8833-wbuf* :uint8 1) (if enable-high-framerate #x00 #x01))
    (i2c-write *dev* AMG8833_SLAVE_ADDR *amg8833-wbuf* 2))

(defun amg8833-set-average (enable)
    (setf (cffi:mem-aref *amg8833-wbuf* :uint8 0) #x1F)
    (setf (cffi:mem-aref *amg8833-wbuf* :uint8 1) #x50)
    (setf (cffi:mem-aref *amg8833-wbuf* :uint8 2) #x1F)
    (setf (cffi:mem-aref *amg8833-wbuf* :uint8 3) #x45)
    (setf (cffi:mem-aref *amg8833-wbuf* :uint8 4) #x1F)
    (setf (cffi:mem-aref *amg8833-wbuf* :uint8 5) #x57)
    (setf (cffi:mem-aref *amg8833-wbuf* :uint8 6) #x07)
    (setf (cffi:mem-aref *amg8833-wbuf* :uint8 7) (if enable #x20 #x00))
    (setf (cffi:mem-aref *amg8833-wbuf* :uint8 8) #x1F)
    (setf (cffi:mem-aref *amg8833-wbuf* :uint8 9) #x00)
    (i2c-write *dev* AMG8833_SLAVE_ADDR *amg8833-wbuf* 10))

(defun amg8833-read-status ()
    (setf (cffi:mem-aref *amg8833-wbuf* :uint8 0) #x04)
    (i2c-read-register *dev* AMG8833_SLAVE_ADDR *amg8833-wbuf* 1 *amg8833-rbuf* 1)
    (cffi:mem-aref *amg8833-rbuf* :uint8 0))

(defun amg8833-read-temperature ()
    (setf (cffi:mem-aref *amg8833-wbuf* :uint8 0) #x80)
    (i2c-write *dev* AMG8833_SLAVE_ADDR *amg8833-wbuf* 1)
    (i2c-read-register *dev* AMG8833_SLAVE_ADDR *amg8833-wbuf* 1 *amg8833-rbuf* (* 64 2))
    (loop for i from 0 below 64 do
        (setf (aref *amg8833-temperature* i)
              (* AMG8833-TEMP-COEFF
                 (logior (cffi:mem-aref *amg8833-rbuf* :uint8 (* 2 i))
                         (ash (cffi:mem-aref *amg8833-rbuf* :int8 (+ (* 2 i) 1)) 8))))))



;
; MAIN
;

(amg8833-reset)
(sleep 0.25)
(amg8833-set-framerate t)
(amg8833-set-average t)
(sleep 0.5)
(format t "~a~%" (amg8833-read-status))
(loop
    (amg8833-read-temperature)
    (loop for y from 0 below 8 do
        (loop for x from 0 below 8 do
            (format t " ~d" (aref *amg8833-temperature*
                                  (+ (* y 8) x))))
        (format t "~%"))
    (format t "~%")
    (sleep 0.1))

