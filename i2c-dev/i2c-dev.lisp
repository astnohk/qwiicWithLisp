(in-package :I2C-DEV)

(require :cffi)

(defparameter *dev*
              (cffi:foreign-string-alloc "/dev/i2c-1"))

;;;; Low level functions
(defun set-i2c-dev (device-path)
    (if *dev*
        (cffi:foreign-free *dev*))
    (setf *dev*
          (cffi:foreign-string-alloc device-path)))

(defun create-i2c-buffer (size)
    (cffi:foreign-alloc :uint8
                        :initial-contents (loop for x from 0 below size collect 0)))

(defun free-i2c-buffer (buf)
    (cffi:foreign-free buf))

(defmacro i2c-buffer-aref (buf index)
    `(cffi:mem-aref ,buf :uint8 ,index))

(defun i2c-read (addr read-buf read-buf-len)
    (cffi:with-foreign-objects ((msg '(:struct i2c-msg))
                                (packets '(:struct i2c-rdwr-ioctl-data)))
        (setf (cffi:foreign-slot-value msg '(:struct i2c-msg) 'addr) addr
              (cffi:foreign-slot-value msg '(:struct i2c-msg) 'flags) I2C-M-RD
              (cffi:foreign-slot-value msg '(:struct i2c-msg) 'buf) read-buf
              (cffi:foreign-slot-value msg '(:struct i2c-msg) 'len) read-buf-len)
        (setf (cffi:foreign-slot-value packets '(:struct i2c-rdwr-ioctl-data) 'msgs) msg
              (cffi:foreign-slot-value packets '(:struct i2c-rdwr-ioctl-data) 'nmsgs) 1)
        (let ((fd (cffi:foreign-funcall "open"
                                        (:pointer :char) *dev*
                                        :int O-RDWR
                                        :int)))
            (if (< fd 0)
                (progn
                    (format t "fail open()")
                    -1) ; ERROR
                (if (< (cffi:foreign-funcall "ioctl"
                                             :int fd
                                             :ulong I2C-RDWR
                                             (:pointer (:struct i2c-rdwr-ioctl-data)) packets
                                             :int)
                       0)
                    (progn
                        (format t "fail ioctl()")
                        -1) ; ERROR
                    (progn (cffi:foreign-funcall "close"
                                          :int fd
                                          :int)
                        0))))))

(defun i2c-read-register (addr write-buf write-buf-len read-buf read-buf-len)
    (cffi:with-foreign-objects ((msgs '(:struct i2c-msg) 2)
                                (packets '(:struct i2c-rdwr-ioctl-data)))
        ; write buf
        (let ((msg (cffi:mem-aptr msgs '(:struct i2c-msg) 0)))
            (setf (cffi:foreign-slot-value msg '(:struct i2c-msg) 'addr) addr
                  (cffi:foreign-slot-value msg '(:struct i2c-msg) 'flags) 0
                  (cffi:foreign-slot-value msg '(:struct i2c-msg) 'buf) write-buf
                  (cffi:foreign-slot-value msg '(:struct i2c-msg) 'len) write-buf-len))
        ; read buf
        (let ((msg (cffi:mem-aptr msgs '(:struct i2c-msg) 1)))
            (setf (cffi:foreign-slot-value msg '(:struct i2c-msg) 'addr) addr
                  (cffi:foreign-slot-value msg '(:struct i2c-msg) 'flags) I2C-M-RD
                  (cffi:foreign-slot-value msg '(:struct i2c-msg) 'buf) read-buf
                  (cffi:foreign-slot-value msg '(:struct i2c-msg) 'len) read-buf-len))
        ; Set packets
        (setf (cffi:foreign-slot-value packets '(:struct i2c-rdwr-ioctl-data) 'msgs) msgs
              (cffi:foreign-slot-value packets '(:struct i2c-rdwr-ioctl-data) 'nmsgs) 2)
        (let ((fd (cffi:foreign-funcall "open"
                                        (:pointer :char) *dev*
                                        :int O-RDWR
                                        :int)))
            (if (< fd 0)
                (progn
                    (format t "fail open()")
                    -1) ; ERROR
                (if (< (cffi:foreign-funcall "ioctl"
                                             :int fd
                                             :ulong I2C-RDWR
                                             (:pointer (:struct i2c-rdwr-ioctl-data)) packets
                                             :int)
                       0)
                    (progn
                        (format t "fail ioctl()")
                        -1) ; ERROR
                    (progn (cffi:foreign-funcall "close"
                                          :int fd
                                          :int)
                        0))))))

(defun i2c-write (addr write-buf write-buf-len)
    (cffi:with-foreign-objects ((msg '(:struct i2c-msg))
                                (packets '(:struct i2c-rdwr-ioctl-data)))
        (setf (cffi:foreign-slot-value msg '(:struct i2c-msg) 'addr) addr
              (cffi:foreign-slot-value msg '(:struct i2c-msg) 'flags) 0
              (cffi:foreign-slot-value msg '(:struct i2c-msg) 'buf) write-buf
              (cffi:foreign-slot-value msg '(:struct i2c-msg) 'len) write-buf-len)
        (setf (cffi:foreign-slot-value packets '(:struct i2c-rdwr-ioctl-data) 'msgs) msg
              (cffi:foreign-slot-value packets '(:struct i2c-rdwr-ioctl-data) 'nmsgs) 1)
        (let ((fd (cffi:foreign-funcall "open"
                                        (:pointer :char) *dev*
                                        :int O-RDWR
                                        :int)))
            (if (< fd 0)
                (progn
                    (format t "fail open()")
                    -1) ; ERROR
                (if (< (cffi:foreign-funcall "ioctl"
                                             :int fd
                                             :ulong I2C-RDWR
                                             (:pointer (:struct i2c-rdwr-ioctl-data)) packets
                                             :int)
                       0)
                    (progn
                        (format t "fail ioctl()")
                        -1) ; ERROR
                    (progn (cffi:foreign-funcall "close"
                                          :int fd
                                          :int)
                        0))))))

