(require :cffi)
(require :bordeaux-threads)

(defconstant VL53L5CX_RESOLUTION_4X4 16)
(defconstant VL53L5CX_RESOLUTION_8X8 64)

;;;;;;;;;;;;;;;;
; CONFIG
;;;;;;;;;;;;;;;;
(defconstant TOF_FREQUENCY 10)
(defconstant TOF_RESOLUTION VL53L5CX_RESOLUTION_4X4)
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
; VL53L5CX Driver
;
(cffi:load-foreign-library "./vl53l5cx_driver.so")

(defconstant VL53L5CX_NB_TARGET_PER_ZONE 2)

(defconstant VL53L5CX_TARGET_ORDER_CLOSEST 1)
(defconstant VL53L5CX_TARGET_ORDER_STRONGEST 2)
(defconstant VL53L5CX_RANGING_MODE_CONTINUOUS 1)
(defconstant VL53L5CX_RANGING_MODE_AUTONOMOUS 3)
(defconstant VL53L5CX_POWER_MODE_SLEEP 0)
(defconstant VL53L5CX_POWER_MODE_WAKEUP 1)

(defconstant VL53L5CX_NVM_DATA_SIZE 492)
(defconstant VL53L5CX_CONFIGURATION_SIZE 972)
(defconstant VL53L5CX_OFFSET_BUFFER_SIZE 488)
(defconstant VL53L5CX_XTALK_BUFFER_SIZE 776)

(cffi:defcstruct vl53l5cx_platform
    (address :uint16))

(cffi:defcstruct vl53l5cx_configuration
    (platform (:struct vl53l5cx_platform))
	(streamcount :uint8)
	(data_read_size :uint32)
	(default_configuration (:pointer :uint8))
	(default_xtalk (:pointer :uint8))
	(offset_data :uint8 :count 488)
	(xtalk_data :uint8 :count 776)
	(temp_buffer :uint8 :count 1452)
	(is_auto_stop_enabled :uint8))

(cffi:defcstruct vl53l5cx_motion_indicator
    (global_indicator_1 :uint32)
    (global_indicator_2 :uint32)
    (status :uint8)
    (nb_of_detected_aggregates :uint8)
    (nb_of_aggregates :uint8)
    (spare :uint8)
    (motion :uint32 :count 32))

(cffi:defcstruct vl53l5cx_results_data
	(silicon_temp_degc :int8)
	(ambient_per_spad :uint32 :count 64)
	(nb_target_detected :uint8 :count 64)
	(nb_spads_enabled :uint32 :count 64)
	(signal_per_spad :uint32 :count 128)
	(range_sigma_mm :uint16 :count 128)
	(distance_mm :int16 :count 128)
	(reflectance :uint8 :count 128)
	(target_status :uint8 :count 128)
    (motion_indicator (:struct vl53l5cx_motion_indicator)))

(cffi:defcfun vl53l5cx_is_alive :uint8
    (p_dev :pointer)
    (isAlive :pointer))

(cffi:defcfun vl53l5cx_init :uint8
    (p_dev :pointer))

(cffi:defcfun vl53l5cx_set_power_mode :uint8
    (p_dev :pointer)
    (power_mode :uint8))

(cffi:defcfun vl53l5cx_set_resolution :uint8
    (p_dev :pointer)
    (resolution :uint8))

(cffi:defcfun vl53l5cx_set_ranging_frequency_hz :uint8
    (p_dev :pointer)
    (frequency_hz :uint8))

(cffi:defcfun vl53l5cx_set_integration_time_ms :uint8
    (p_dev :pointer)
    (integration_time_ms :uint32))

(cffi:defcfun vl53l5cx_set_sharpener_percent :uint8
    (p_dev :pointer)
    (sharpener_percent :uint8))

(cffi:defcfun vl53l5cx_set_target_order :uint8
    (p_dev :pointer)
    (target_order :uint8))

(cffi:defcfun vl53l5cx_set_ranging_mode :uint8
    (p_dev :pointer)
    (ranging_mode :uint8))

(cffi:defcfun vl53l5cx_start_ranging :uint8
    (p_dev :pointer))

(cffi:defcfun vl53l5cx_stop_ranging :uint8
    (p_dev :pointer))

(cffi:defcfun vl53l5cx_check_data_ready :uint8
    (p_dev :pointer)
    (isReady :pointer))

(cffi:defcfun vl53l5cx_get_ranging_data :uint8
    (p_dev :pointer)
    (p_results :pointer))


;
; Parameters
;
(defparameter *results_data* (cffi:foreign-alloc '(:struct vl53l5cx_results_data)))
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
(defun extract_distance_mm (dest results)
    (let ((c_distance_mm (cffi:foreign-slot-pointer results '(:struct vl53l5cx_results_data) 'distance_mm)))
        (loop for ind from 0 below TOF_RESOLUTION do
            (setf (aref dest ind)
                (loop for tar from 0 below VL53L5CX_NB_TARGET_PER_ZONE collect
                    (cffi:mem-aref c_distance_mm :int16 (+ (* VL53L5CX_NB_TARGET_PER_ZONE ind) tar)))))))

(defun extract_range_sigma_mm (dest results)
    (let ((c_range_sigma_mm (cffi:foreign-slot-pointer results '(:struct vl53l5cx_results_data) 'range_sigma_mm)))
        (loop for ind from 0 below TOF_RESOLUTION do
            (setf (aref dest ind)
                (loop for tar from 0 below VL53L5CX_NB_TARGET_PER_ZONE collect
                    (cffi:mem-aref c_range_sigma_mm :int16 (+ (* VL53L5CX_NB_TARGET_PER_ZONE ind) tar)))))))

(defun detect (dev)
    (vl53l5cx_get_ranging_data dev *results_data*)
    (extract_distance_mm *distance_mm* *results_data*)
    (format t "~a~%" *distance_mm*)
    (shift-circular *distance_hist*)
    (assign-array (nth 0 *distance_hist*) *distance_mm*)
    (extract_range_sigma_mm *range_sigma_mm* *results_data*)
    (shift-circular *range_sigma_mm_hist*)
    (assign-array (nth 0 *range_sigma_mm_hist*) *range_sigma_mm*))

(defun main-loop (dev)
    (let ((is_ready (cffi:foreign-alloc :int8)))
        (loop
            (vl53l5cx_check_data_ready dev is_ready)
            (format t "is_ready: ~d~%" (cffi:mem-ref is_ready :uint8))
            (if (> (cffi:mem-ref is_ready :uint8) 0)
                (detect dev))
            (sleep 0.025))))

(defun init ()
    (let ((dev (cffi:foreign-alloc '(:struct vl53l5cx_configuration))))
        (setf (cffi:foreign-slot-value
            (cffi:foreign-slot-pointer dev '(:struct vl53l5cx_configuration) 'platform)
            '(:struct vl53l5cx_platform) 'address)
            #x29)
        ; Check sensor is alive
        (let ((is_alive (cffi:foreign-alloc :int8)))
            (vl53l5cx_is_alive dev is_alive)
            (format t "is_alive: ~d~%" (cffi:mem-ref is_alive :int8)))
        ; Initialize
        (format t "init: ~d~%"
            (vl53l5cx_init dev))
        (format t "set_resolution: ~d~%"
            (vl53l5cx_set_resolution dev TOF_RESOLUTION))
        (format t "set_ranging_mode: ~d~%"
            (vl53l5cx_set_ranging_mode dev VL53L5CX_RANGING_MODE_AUTONOMOUS))
        (format t "set_ranging_frequency_hz: ~d~%"
            (vl53l5cx_set_ranging_frequency_hz dev TOF_FREQUENCY))
        (format t "set_integration_time_ms: ~d~%"
            (vl53l5cx_set_integration_time_ms dev 70))
        (format t "set_target_order: ~d~%"
            (vl53l5cx_set_target_order dev VL53L5CX_TARGET_ORDER_STRONGEST))
        ; Start ranging
        (format t "start_ranging: ~d~%"
            (vl53l5cx_start_ranging dev))
        (main-loop dev)))

(init)
