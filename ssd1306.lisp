(require :cffi)


;;;;;;;;;;;;;;;;
; CONFIG
;;;;;;;;;;;;;;;;
(defconstant SSD1306_SLAVE_ADDR #x3d)
(defconstant SSD1306_WIDTH 128)
(defconstant SSD1306_HEIGHT 64)
(defconstant SSD1306_MEMORY_HORIZONTAL_ADDRESSING #x00)
(defconstant SSD1306_MEMORY_VERTICAL_ADDRESSING #x01)
(defconstant SSD1306_MEMORY_PAGE_ADDRESSING #x10)
;;;;;;;;;;;;;;;;

;
; Generic Functions
;
(defun get-elapsed-time () (/ (get-internal-real-time) internal-time-units-per-second))
(defun zeros (N)
    (loop for i from 0 below N collect 0))


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
(defparameter *rbuf*
    (cffi:foreign-alloc :uint8
                        :initial-contents
                        '(0)))
(defparameter *wbuf*
    (cffi:foreign-alloc :uint8
                        :initial-contents
                        (zeros 3)))

(defconstant PIXELBUF_LENGTH (+ 1 (/ (* SSD1306_WIDTH SSD1306_HEIGHT) 8)))
(defparameter *pixelbuf*
    (cffi:foreign-alloc :uint8
                        :initial-contents
                        (zeros PIXELBUF_LENGTH)))


;
; OLED Controller
;
(defun set-oled-contrast (contrast)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x00)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #x81)
    (setf (cffi:mem-aref *wbuf* :uint8 2) contrast)
    (i2c-write *dev* SSD1306_SLAVE_ADDR *wbuf* 3))

(defun set-oled-display-on (on)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x00)
    (setf (cffi:mem-aref *wbuf* :uint8 1)
          (logior #xae
                  (if on #x01 #x00)))
    (i2c-write *dev* SSD1306_SLAVE_ADDR *wbuf* 2))

(defun set-oled-inverse-display (inverse)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x00)
    (setf (cffi:mem-aref *wbuf* :uint8 1)
          (logior #xa6
                  (if inverse #x01 #x00)))
    (i2c-write *dev* SSD1306_SLAVE_ADDR *wbuf* 2))

(defun set-oled-entire-display-on (on)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x00)
    (setf (cffi:mem-aref *wbuf* :uint8 1)
          (logior #xa4
                  (if on #x01 #x00)))
    (i2c-write *dev* SSD1306_SLAVE_ADDR *wbuf* 2))

(defun set-oled-deactivate-scroll ()
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x00)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #x2e)
    (i2c-write *dev* SSD1306_SLAVE_ADDR *wbuf* 2))

(defun set-oled-activate-scroll ()
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x00)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #x2f)
    (i2c-write *dev* SSD1306_SLAVE_ADDR *wbuf* 2))

(defun set-oled-memory-addressing-mode (mode)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x00)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #x20)
    (setf (cffi:mem-aref *wbuf* :uint8 2) mode)
    (i2c-write *dev* SSD1306_SLAVE_ADDR *wbuf* 3))

(defun set-oled-display-start-line (line)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x00)
    (setf (cffi:mem-aref *wbuf* :uint8 1)
          (logior #x40 line))
    (i2c-write *dev* SSD1306_SLAVE_ADDR *wbuf* 2))

(defun set-oled-segment-remap (remap)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x00)
    (setf (cffi:mem-aref *wbuf* :uint8 1)
          (logior #xa0
                  (if remap #x01 #x00)))
    (i2c-write *dev* SSD1306_SLAVE_ADDR *wbuf* 2))

(defun set-oled-multiplex-ratio (ratio)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x00)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #xa8)
    (setf (cffi:mem-aref *wbuf* :uint8 2) ratio)
    (i2c-write *dev* SSD1306_SLAVE_ADDR *wbuf* 3))

(defun set-oled-com-scan-direction (remap)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x00)
    (setf (cffi:mem-aref *wbuf* :uint8 1)
          (logior #xc0
                  (if remap #x08 #x00)))
    (i2c-write *dev* SSD1306_SLAVE_ADDR *wbuf* 2))

(defun set-oled-display-offset (offset)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x00)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #xd3)
    (setf (cffi:mem-aref *wbuf* :uint8 2) offset)
    (i2c-write *dev* SSD1306_SLAVE_ADDR *wbuf* 3))

(defun set-oled-com-pins (alternative lr-remap)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x00)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #xda)
    (setf (cffi:mem-aref *wbuf* :uint8 2)
          (logior #x02
                  (if alternative #x10 #x00)
                  (if lr-remap #x20 #x00)))
    (i2c-write *dev* SSD1306_SLAVE_ADDR *wbuf* 3))

(defun set-oled-display-clock-div (div)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x00)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #xd5)
    (setf (cffi:mem-aref *wbuf* :uint8 2) div)
    (i2c-write *dev* SSD1306_SLAVE_ADDR *wbuf* 3))

(defun set-oled-pre-charge-period (period)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x00)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #xd9)
    (setf (cffi:mem-aref *wbuf* :uint8 2) period)
    (i2c-write *dev* SSD1306_SLAVE_ADDR *wbuf* 3))

(defun set-oled-v-comh-deselect-level (level)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x00)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #xdb)
    (setf (cffi:mem-aref *wbuf* :uint8 2)
          (ash level 4))
    (i2c-write *dev* SSD1306_SLAVE_ADDR *wbuf* 3))

(defun set-oled-charge-pump (enable)
    (setf (cffi:mem-aref *wbuf* :uint8 0) #x00)
    (setf (cffi:mem-aref *wbuf* :uint8 1) #x8d)
    (setf (cffi:mem-aref *wbuf* :uint8 2)
          (logior #x10
                  (if enable #x04 #x00)))
    (i2c-write *dev* SSD1306_SLAVE_ADDR *wbuf* 3))

(defun write-oled-pixels ()
    (let ((N (- PIXELBUF_LENGTH 1)))
        (setf (cffi:mem-aref *pixelbuf* :uint8 0) #x40)
        (loop for i from 0 below N do
            (setf (cffi:mem-aref *pixelbuf* :uint8 (+ 1 i)) (if (> (logand #x01 i) 0) #xaa #x00)))
        (i2c-write *dev* SSD1306_SLAVE_ADDR *pixelbuf* PIXELBUF_LENGTH)))


;
; MAIN
;

(format t "init...~%")
(set-oled-display-on nil)
(set-oled-display-clock-div #x80)
(set-oled-multiplex-ratio (- SSD1306_HEIGHT 1))
(set-oled-display-offset 0)
(set-oled-display-start-line 0)
(set-oled-charge-pump t)
(set-oled-memory-addressing-mode SSD1306_MEMORY_HORIZONTAL_ADDRESSING)
(set-oled-segment-remap t)
(set-oled-com-scan-direction nil)
(set-oled-com-pins t nil)
(set-oled-contrast #x9f)
(set-oled-pre-charge-period #x22)
(set-oled-v-comh-deselect-level 4)
(set-oled-entire-display-on nil)
(set-oled-inverse-display nil)
(set-oled-deactivate-scroll)
(set-oled-display-on t)
(format t "start~%")

; Drawing loop
(loop
    (write-oled-pixels)
    (sleep 0.5))

