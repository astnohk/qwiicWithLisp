;
; Qwiic Micro OLED (SSD1306)
;
(defpackage :i2c-ssd1306
    (:use :cl)
    (:export
        :WIDTH
        :HEIGHT
        :PAGE_SIZE
        :MEMORY_HORIZONTAL_ADDRESSING
        :MEMORY_VERTICAL_ADDRESSING
        :MEMORY_PAGE_ADDRESSING
        :MICRO_OLED_COLUMN_OFFSET
        :OLED_PIXELBUF_LENGTH
        :set-oled-contrast
        :set-oled-display-on
        :set-oled-inverse-display
        :set-oled-entire-display-on
        :set-oled-deactivate-scroll
        :set-oled-activate-scroll
        :set-oled-memory-addressing-mode
        :set-oled-display-start-line
        :set-oled-segment-remap
        :set-oled-multiplex-ratio
        :set-oled-com-scan-direction
        :set-oled-display-offset
        :set-oled-com-pins
        :set-oled-display-clock-div
        :set-oled-pre-charge-period
        :set-oled-v-comh-deselect-level
        :set-oled-charge-pump
        :set-oled-pixels
        :write-oled-pixels
        :draw-pixel
        :draw-slashes))
(in-package :i2c-ssd1306)

(require :i2c-dev)

;;;;;;;;;;;;;;;;
; CONFIG
;;;;;;;;;;;;;;;;
(defconstant DEVADDR #x3d)
(defconstant WIDTH 64)
(defconstant HEIGHT 48)
(defconstant PAGE_SIZE 6)
(defconstant MEMORY_HORIZONTAL_ADDRESSING #x00)
(defconstant MEMORY_VERTICAL_ADDRESSING #x01)
(defconstant MEMORY_PAGE_ADDRESSING #x10)
(defconstant MICRO_OLED_COLUMN_OFFSET 2)
(defconstant OLED_PIXELBUF_LENGTH (+ 1 (* WIDTH (/ HEIGHT 8))))
;;;;;;;;;;;;;;;;

;
; Parameters
;
(defparameter *wbuf* (i2c-dev:create-i2c-buffer 3))
(defparameter *oled-pixelbuf* (i2c-dev:create-i2c-buffer OLED_PIXELBUF_LENGTH))

(defparameter *oled-pixels* (make-array `(,(floor HEIGHT 8) ,WIDTH) :initial-element 0))

;
; OLED Controller
;
(defun set-oled-contrast (contrast)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x00)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #x81)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 2) contrast)
    (i2c-dev:i2c-write DEVADDR *wbuf* 3))

(defun set-oled-display-on (on)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x00)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1)
          (logior #xae
                  (if on #x01 #x00)))
    (i2c-dev:i2c-write DEVADDR *wbuf* 2))

(defun set-oled-inverse-display (inverse)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x00)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1)
          (logior #xa6
                  (if inverse #x01 #x00)))
    (i2c-dev:i2c-write DEVADDR *wbuf* 2))

(defun set-oled-entire-display-on (on)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x00)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1)
          (logior #xa4
                  (if on #x01 #x00)))
    (i2c-dev:i2c-write DEVADDR *wbuf* 2))

(defun set-oled-deactivate-scroll ()
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x00)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #x2e)
    (i2c-dev:i2c-write DEVADDR *wbuf* 2))

(defun set-oled-activate-scroll ()
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x00)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #x2f)
    (i2c-dev:i2c-write DEVADDR *wbuf* 2))

(defun set-oled-memory-addressing-mode (mode)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x00)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #x20)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 2) mode)
    (i2c-dev:i2c-write DEVADDR *wbuf* 3))

(defun set-oled-display-start-line (line)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x00)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1)
          (logior #x40 line))
    (i2c-dev:i2c-write DEVADDR *wbuf* 2))

(defun set-oled-segment-remap (remap)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x00)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1)
          (logior #xa0
                  (if remap #x01 #x00)))
    (i2c-dev:i2c-write DEVADDR *wbuf* 2))

(defun set-oled-multiplex-ratio (ratio)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x00)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #xa8)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 2) ratio)
    (i2c-dev:i2c-write DEVADDR *wbuf* 3))

(defun set-oled-com-scan-direction (remap)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x00)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1)
          (logior #xc0
                  (if remap #x08 #x00)))
    (i2c-dev:i2c-write DEVADDR *wbuf* 2))

(defun set-oled-display-offset (offset)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x00)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #xd3)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 2) offset)
    (i2c-dev:i2c-write DEVADDR *wbuf* 3))

(defun set-oled-com-pins (alternative lr-remap)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x00)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #xda)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 2)
          (logior #x02
                  (if alternative #x10 #x00)
                  (if lr-remap #x20 #x00)))
    (i2c-dev:i2c-write DEVADDR *wbuf* 3))

(defun set-oled-display-clock-div (div)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x00)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #xd5)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 2) div)
    (i2c-dev:i2c-write DEVADDR *wbuf* 3))

(defun set-oled-pre-charge-period (period)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x00)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #xd9)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 2) period)
    (i2c-dev:i2c-write DEVADDR *wbuf* 3))

(defun set-oled-v-comh-deselect-level (level)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x00)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #xdb)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 2)
          (ash level 4))
    (i2c-dev:i2c-write DEVADDR *wbuf* 3))

(defun set-oled-charge-pump (enable)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x00)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) #x8d)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 2)
          (logior #x10
                  (if enable #x04 #x00)))
    (i2c-dev:i2c-write DEVADDR *wbuf* 3))

(defun write-oled-pixels (page)
    ; PAGE ADDR
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x00)
    (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) (logior #xb0 page))
    (i2c-dev:i2c-write DEVADDR *wbuf* 2)
    ; COLUMN OFFSET
    (let ((x-offset MICRO_OLED_COLUMN_OFFSET)
          (col-start 0)) ; always send entier segment
        ; Top
        (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x00)
        (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) (logior #x10
                                                      (+ (ash col-start -4) x-offset)))
        (i2c-dev:i2c-write DEVADDR *wbuf* 2)
        ; Low
        (setf (i2c-dev:i2c-buffer-aref *wbuf* 0) #x00)
        (setf (i2c-dev:i2c-buffer-aref *wbuf* 1) (logand #x0f col-start))
        (i2c-dev:i2c-write DEVADDR *wbuf* 2))
    ; PIXELS
    (setf (i2c-dev:i2c-buffer-aref *oled-pixelbuf* 0) #x40)
    (loop for i from 0 below WIDTH do
        (setf (i2c-dev:i2c-buffer-aref *oled-pixelbuf* (+ 1 i)) (aref *oled-pixels* page i)))
    (i2c-dev:i2c-write DEVADDR *oled-pixelbuf* (+ 1 WIDTH)))


;
; DRAWING FUNCTIONS
;
(defun draw-pixel (x y color)
    (multiple-value-bind (i bits) (floor y 8)
        (let* ((bitmask (ash #x01 bits))
               (v (aref *oled-pixels* i x))
               (newval (logand #xff
                               (logior (logand v (lognot bitmask))
                                       (if (> color 0) bitmask 0)))))
            (if (and (< x WIDTH)
                     (< i PAGE_SIZE))
                (setf (aref *oled-pixels* i x)
                      newval)
                (format t "(draw-pixel): out of the bound")))))

(defun draw-slashes ()
    (loop for i from 0 below HEIGHT do
        (if (< i WIDTH)
            (draw-pixel i i 1))))

