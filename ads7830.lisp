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
(defparameter *ads7830_rbuffer*
    (cffi:foreign-alloc :uint8
                        :initial-contents
                        (loop for x from 0 below 1 collect 0)))
(defparameter *ads7830_wbuffer*
    (cffi:foreign-alloc :uint8
                        :initial-contents
                        (loop for x from 0 below 1 collect 0)))
(defconstant ADS7830-DIFFERENTIAL-INPUTS 0)
(defconstant ADS7830-SINGLE-ENDED-INPUTS 1)
(defconstant ADS7830-PD-POWER-DOWN 0)
(defconstant ADS7830-PD-INTERNAL-OFF-AD-ON 1)
(defconstant ADS7830-PD-INTERNAL-ON-AD-OFF 2)
(defconstant ADS7830-PD-INTERNAL-ON-AD-ON 3)

(defun read-ads7830 (ch SD PD)
    (setf (cffi:mem-aref *ads7830_wbuffer* :uint8 0)
          (logior (ash SD 7)
                  (ash ch 4)
                  (ash PD 2)))
    (i2c-read-register
        *dev*
        #x48
        *ads7830_wbuffer*
        1
        *ads7830_rbuffer*
        1)
    (cffi:mem-aref *ads7830_rbuffer* :uint8 0))


(defun read-ads7830-single-ended (ch)
    (read-ads7830 ch
                  ADS7830-SINGLE-ENDED-INPUTS
                  ADS7830-PD-INTERNAL-ON-AD-ON))
(loop
    (format t "read all channels.~%")
    (format t "CH0: ~d~%"
        (read-ads7830-single-ended 0))
    (format t "CH1: ~d~%"
        (read-ads7830-single-ended 1))
    (format t "CH2: ~d~%"
        (read-ads7830-single-ended 2))
    (format t "CH3: ~d~%"
        (read-ads7830-single-ended 3))
    (format t "CH4: ~d~%"
        (read-ads7830-single-ended 4))
    (format t "CH5: ~d~%"
        (read-ads7830-single-ended 5))
    (format t "CH6: ~d~%"
        (read-ads7830-single-ended 6))
    (format t "CH7: ~d~%"
        (read-ads7830-single-ended 7))
    (format t "~%")
    (sleep 0.25))

(cffi:foreign-free *dev*)
(cffi:foreign-free *ads7830_rbuffer*)
(cffi:foreign-free *ads7830_wbuffer*)

