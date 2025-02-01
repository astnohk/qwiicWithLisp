(require :cffi)
(require :local-time)

(defconstant DS3231_DEVADDR #x68)
(defconstant DS3231_REGISTERS_LENGTH 18)

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
(defparameter *ds3231_rbuffer*
    (cffi:foreign-alloc :uint8
                        :initial-contents
                        (loop for x from 0 below DS3231_REGISTERS_LENGTH collect 0)))
(defparameter *ds3231_wbuffer*
    (cffi:foreign-alloc :uint8
                        :initial-contents
                        (loop for x from 0 below 8 collect 0)))

;
; low-level functions for DS3231
;
(defun ds3231-read-all-registers ()
    (i2c-read
        *dev*
        DS3231_DEVADDR
        *ds3231_rbuffer*
        DS3231_REGISTERS_LENGTH)
    (loop for i from 0 below DS3231_REGISTERS_LENGTH collect
        (cffi:mem-aref *ds3231_rbuffer* :uint8 i)))

(defun ds3231-read-date()
    (let ((length 7)
          (value ()))
        (setf (cffi:mem-aref *ds3231_wbuffer* :uint8 0)
              0)
        (i2c-read-register
            *dev*
            DS3231_DEVADDR
            *ds3231_wbuffer*
            1
            *ds3231_rbuffer*
            length)
        (setf value
              (loop for x from 0 below length collect
                  (cffi:mem-aref *ds3231_rbuffer* :uint8 x)))
        (loop for x from 0 below length do
            (format t "~4,'0b ~4,'0b~%"
                (ash (nth x value) -4)
                (logand #x0f (nth x value))))
        value))

(defun ds3231-parse-value (value)
    (+ (* 10 (ash value -4))
       (logand value #x0f)))

(defun ds3231-parse-date (value)
    (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            (+ 2000 (ds3231-parse-value (nth 6 value)))
            (ds3231-parse-value (nth 5 value))
            (ds3231-parse-value (nth 4 value))
            (ds3231-parse-value (nth 2 value))
            (ds3231-parse-value (nth 1 value))
            (ds3231-parse-value (nth 0 value))))

(defun ds3231-write-date ()
    (let* ((length 7)
           (now (local-time:now))
           (second (local-time:timestamp-second now))
           (minute (local-time:timestamp-minute now))
           (hour (local-time:timestamp-hour now))
           (day (+ 1 (local-time:timestamp-day-of-week now)))
           (date (local-time:timestamp-day now))
           (month (local-time:timestamp-month now))
           (year (mod (local-time:timestamp-year now) 100)))
        (setf (cffi:mem-aref *ds3231_wbuffer* :uint8 0)
              0)
        (setf (cffi:mem-aref *ds3231_wbuffer* :uint8 1)
              (logior (ash (floor (/ second 10)) 4)
                      (mod second 10)))
        (setf (cffi:mem-aref *ds3231_wbuffer* :uint8 2)
              (logior (ash (floor (/ minute 10)) 4)
                      (mod minute 10)))
        (setf (cffi:mem-aref *ds3231_wbuffer* :uint8 3)
              (logior (ash (floor (/ hour 10)) 4)
                      (mod hour 10)))
        (setf (cffi:mem-aref *ds3231_wbuffer* :uint8 4)
              day)
        (setf (cffi:mem-aref *ds3231_wbuffer* :uint8 5)
              (logior (ash (floor (/ date 10)) 4)
                      (mod date 10)))
        (setf (cffi:mem-aref *ds3231_wbuffer* :uint8 6)
              (logior (ash (floor (/ month 10)) 4)
                      (mod month 10)))
        (setf (cffi:mem-aref *ds3231_wbuffer* :uint8 7)
              (logior (ash (floor (/ year 10)) 4)
                      (mod year 10)))
        (loop for x from 0 below length do
            (format t "~4,'0b ~4,'0b~%"
                (ash (cffi:mem-aref *ds3231_wbuffer* :uint8 (+ 1 x)) -4)
                (logand #x0f (cffi:mem-aref *ds3231_wbuffer* :uint8 (+ 1 x)))))
        (i2c-write
            *dev*
            DS3231_DEVADDR
            *ds3231_wbuffer*
            (+ 1 length))))
