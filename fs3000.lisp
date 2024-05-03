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
(defparameter *fs3000_buffer*
    (cffi:foreign-alloc :uint8
                        :initial-contents
                        (loop for x from 0 below 5 collect 0)))

(defun read-fs3000 ()
    (i2c-read *dev* #x28 *fs3000_buffer* 5)
    (let ((count
        (+ (ash
                (logand #x0f
                        (cffi:mem-aref *fs3000_buffer* :uint8 1))
                8)
           (cffi:mem-aref *fs3000_buffer* :uint8 2))))
        (* 7.23
            (/ (max 0 (- count 409))
               (- 3686 409)))))

(loop
    (format t "~d~%" (read-fs3000))
    (sleep 0.125))

(cffi:foreign-free *dev*)
(cffi:foreign-free *fs3000_buffer*)

