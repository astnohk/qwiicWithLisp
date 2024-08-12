(require :cffi)

;;;;;;;;;;;;;;;;
; CONFIG
;;;;;;;;;;;;;;;;
(defconstant AW9523_SLAVE_ADDR #x58)
;;;;;;;;;;;;;;;;

;
; Generic Functions
;
(defun get-elapsed-time () (/ (get-internal-real-time) internal-time-units-per-second))


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


;
; Constants
;
(defconstant AW9523-LED-P1_0-DIM #x20)
(defconstant AW9523-LED-P1_1-DIM #x21)
(defconstant AW9523-LED-P1_2-DIM #x22)
(defconstant AW9523-LED-P1_3-DIM #x23)
(defconstant AW9523-LED-P0_0-DIM #x24)
(defconstant AW9523-LED-P0_1-DIM #x25)
(defconstant AW9523-LED-P0_2-DIM #x26)
(defconstant AW9523-LED-P0_3-DIM #x27)
(defconstant AW9523-LED-P0_4-DIM #x28)
(defconstant AW9523-LED-P0_5-DIM #x29)
(defconstant AW9523-LED-P0_6-DIM #x2a)
(defconstant AW9523-LED-P0_7-DIM #x2b)
(defconstant AW9523-LED-P1_4-DIM #x2c)
(defconstant AW9523-LED-P1_5-DIM #x2d)
(defconstant AW9523-LED-P1_6-DIM #x2e)
(defconstant AW9523-LED-P1_7-DIM #x2f)


;
; Parameters
;
(defparameter *dev*
    (cffi:foreign-string-alloc "/dev/i2c-1"))
(defparameter *wbuf*
    (cffi:foreign-alloc :uint8
                        :initial-contents
                        '(0 0)))


;
; LED Controller
;
(defun set-led-mode ()
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x12)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #x00)
    (i2c-write *dev* AW9523_SLAVE_ADDR *wbuf* 2)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x13)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #x00)
    (i2c-write *dev* AW9523_SLAVE_ADDR *wbuf* 2))

(defun set-current-limit ()
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x11)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #x03)
    (i2c-write *dev* AW9523_SLAVE_ADDR *wbuf* 2))

(defun set-led-current (port current)
    (setf (cffi:mem-aref *wbuf* :uint8 0) port)
    (setf (cffi:mem-aref *wbuf* :uint8 1) current)
    (i2c-write *dev* AW9523_SLAVE_ADDR *wbuf* 2))


;
; MAIN
;

(set-led-mode)
(set-current-limit)
(loop
    (set-led-current AW9523-LED-P0_1-DIM #x50)
    (sleep 0.5)
    (set-led-current AW9523-LED-P0_1-DIM #x00)
    (sleep 0.5))

