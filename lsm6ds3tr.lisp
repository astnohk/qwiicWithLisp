(require :cffi)

;;;;;;;;;;;;;;;;
; CONFIG
;;;;;;;;;;;;;;;;
(defconstant LSM6DS3TR_SLAVE_ADDR #x6a)
(defconstant LIS3MDL_SLAVE_ADDR #x1c)
;;;;;;;;;;;;;;;;

;
; Generic Functions
;
(defun get-elapsed-time () (/ (get-internal-real-time) internal-time-units-per-second))
(defun length-3d (v) (sqrt (loop for i from 0 below (array-dimension v 0) sum (expt (aref v i) 2))))


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
; Parameters
;
(defparameter *dev*
    (cffi:foreign-string-alloc "/dev/i2c-1"))
(defparameter *wbuf*
    (cffi:foreign-alloc :uint8
                        :initial-contents
                        '(0 0)))
(defparameter *temp-gyro-accel*
    (cffi:foreign-alloc :int8
                        :initial-contents
                        (loop for x from 0 below 14 collect 0)))
(defparameter *magnet*
    (cffi:foreign-alloc :int8
                        :initial-contents
                        (loop for x from 0 below 6 collect 0)))


;
; Gyro Accelorometer
;
(defun wake-gyro-accel ()
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x10)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #x3c)
    (i2c-write *dev* LSM6DS3TR_SLAVE_ADDR *wbuf* 2)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x11)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #x30)
    (i2c-write *dev* LSM6DS3TR_SLAVE_ADDR *wbuf* 2))

(defun read-temp-gyro-accel ()
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x20)
    (i2c-read-register *dev*
                       LSM6DS3TR_SLAVE_ADDR
                       *wbuf*
                       1
                       *temp-gyro-accel*
                       14))

(defun parse-temp-gyro-accel (buf)
    (values
        ; temp
        (+ (ash (cffi:mem-aref buf :int8 1) 8)
           (logand #xFF (cffi:mem-aref buf :int8 0)))
        ; gyro
        (+ (ash (cffi:mem-aref buf :int8 3) 8)
           (logand #xFF (cffi:mem-aref buf :int8 2)))
        (+ (ash (cffi:mem-aref buf :int8 5) 8)
           (logand #xFF (cffi:mem-aref buf :int8 4)))
        (+ (ash (cffi:mem-aref buf :int8 7) 8)
           (logand #xFF (cffi:mem-aref buf :int8 6)))
        ; accel
        (+ (ash (cffi:mem-aref buf :int8 9) 8)
           (logand #xFF (cffi:mem-aref buf :int8 8)))
        (+ (ash (cffi:mem-aref buf :int8 11) 8)
           (logand #xFF (cffi:mem-aref buf :int8 10)))
        (+ (ash (cffi:mem-aref buf :int8 13) 8)
           (logand #xFF (cffi:mem-aref buf :int8 12)))))

;
; Magnetometer
;
(defun wake-magnet ()
    ; CTRL_REG1 (20h)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x20)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #x30)
    (i2c-write *dev* LIS3MDL_SLAVE_ADDR *wbuf* 2)
    ; CTRL_REG3 (22h)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x22)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #x00)
    (i2c-write *dev* LIS3MDL_SLAVE_ADDR *wbuf* 2)
    ; CRTL_REG4 (23h)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x23)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #x04)
    (i2c-write *dev* LIS3MDL_SLAVE_ADDR *wbuf* 2))

(defun read-magnet ()
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x28)
    (i2c-read-register *dev*
                       LIS3MDL_SLAVE_ADDR
                       *wbuf*
                       1
                       *magnet*
                       6))

(defun parse-magnet (buf)
    (values
        (+ (ash (cffi:mem-aref buf :int8 1) 8)
           (logand #xFF (cffi:mem-aref buf :int8 0)))
        (+ (ash (cffi:mem-aref buf :int8 3) 8)
           (logand #xFF (cffi:mem-aref buf :int8 2)))
        (+ (ash (cffi:mem-aref buf :int8 5) 8)
           (logand #xFF (cffi:mem-aref buf :int8 4)))))


;
; MAIN
;

(wake-gyro-accel)
(wake-magnet)
(loop
    (read-temp-gyro-accel)
    (read-magnet)
    (multiple-value-bind
        (temp gx gy gz ax ay az)
        (parse-temp-gyro-accel *temp-gyro-accel*)
        (format t "~d (~d, ~d, ~d) (~d, ~d, ~d)~%"
            temp gx gy gz ax ay az))
    (multiple-value-bind
        (mx my mz)
        (parse-magnet *magnet*)
        (format t "(~d, ~d, ~d)~%"
            mx my mz))
    (sleep 0.125))

