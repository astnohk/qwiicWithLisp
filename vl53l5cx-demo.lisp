(load "vl53l5cx.lisp")

(require :bordeaux-threads)

;;;;;;;;;;;;;;;;
; CONFIG
;;;;;;;;;;;;;;;;
(defconstant TOF_FREQUENCY 10)
(defconstant TOF_RESOLUTION i2c-vl53l5cx:RESOLUTION_4X4)
(defconstant TOF_HISTORY_LENGTH 5)
(defconstant MAX_DISTANCE 4000)
;;;;;;;;;;;;;;;;

(defun log10 (x) (/ (log x) (log 10)))
(defun sum (x) (reduce #'+ x))
(defun mean (x) (if (> (length x) 0) (/ (sum x) (length x)) 0))

(defmacro shift-circular (place)
    `(let ((tmp (loop for i from 0 below (- (length ,place) 1) collect (nth i ,place))))
        (setf ,place (cons (car (last ,place)) tmp))))
(defmacro assign-array (dest val)
    `(loop for i from 0 below (array-dimension ,dest 0) do
        (setf (aref ,dest i) (aref ,val i))))

;
; Parameters
;
(defparameter *results_data* (i2c-vl53l5cx:create-results-buffer))
(defparameter *distance_mm* (make-array (list TOF_RESOLUTION) :initial-element (list 0)))
(defparameter *range_sigma_mm* (make-array (list TOF_RESOLUTION) :initial-element (list 0)))
(defparameter *distance_hist*
    (loop for i from 0 to TOF_HISTORY_LENGTH collect
        (make-array (list TOF_RESOLUTION) :initial-element (list 0))))
(defparameter *range_sigma_mm_hist*
    (loop for i from 0 to TOF_HISTORY_LENGTH collect
        (make-array (list TOF_RESOLUTION) :initial-element (list 0))))


;
; Sensor Thread
;
(defun detect (dev)
    (i2c-vl53l5cx:get-ranging-data dev *results_data*)
    (i2c-vl53l5cx:extract-distance-mm *distance_mm* *results_data* TOF_RESOLUTION)
    (format t "~a~%" *distance_mm*)
    (shift-circular *distance_hist*)
    (assign-array (nth 0 *distance_hist*) *distance_mm*)
    (i2c-vl53l5cx:extract-range-sigma-mm *range_sigma_mm* *results_data* TOF_RESOLUTION)
    (shift-circular *range_sigma_mm_hist*)
    (assign-array (nth 0 *range_sigma_mm_hist*) *range_sigma_mm*))

(defun main-loop (dev)
    (let ((is_ready 0))
        (loop
            (setf is_ready (i2c-vl53l5cx:check-data-ready dev))
            (format t "is_ready: ~d~%" is_ready)
            (if (> is_ready 0)
                (detect dev))
            (sleep 0.025))))

(defun init ()
    (let ((dev (i2c-vl53l5cx:create-dev)))
        ; Check sensor is alive
        (let ((is_alive (i2c-vl53l5cx:is-alive dev)))
            (format t "is_alive: ~d~%" is_alive))
        ; Initialize
        (format t "init: ~d~%"
            (i2c-vl53l5cx:init-device dev))
        (format t "set_resolution: ~d~%"
            (i2c-vl53l5cx:set-resolution dev TOF_RESOLUTION))
        (format t "set_ranging_mode: ~d~%"
            (i2c-vl53l5cx:set-ranging-mode dev i2c-vl53l5cx:RANGING_MODE_AUTONOMOUS))
        (format t "set_ranging_frequency_hz: ~d~%"
            (i2c-vl53l5cx:set-ranging-frequency-hz dev TOF_FREQUENCY))
        (format t "set_integration_time_ms: ~d~%"
            (i2c-vl53l5cx:set-integration-time-ms dev 70))
        (format t "set_target_order: ~d~%"
            (i2c-vl53l5cx:set-target-order dev i2c-vl53l5cx:TARGET_ORDER_STRONGEST))
        ; Start ranging
        (format t "start_ranging: ~d~%"
            (i2c-vl53l5cx:start-ranging dev))
        (main-loop dev)))

(init)
