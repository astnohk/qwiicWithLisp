(defpackage :i2c-vl53l5cx
    (:use :cl)
    (:export
        :RESOLUTION_4X4
        :RESOLUTION_8X8

        :NB_TARGET_PER_ZONE
        :TARGET_ORDER_CLOSEST
        :TARGET_ORDER_STRONGEST
        :RANGING_MODE_CONTINUOUS
        :RANGING_MODE_AUTONOMOUS
        :POWER_MODE_SLEEP
        :POWER_MODE_WAKEUP

        :create-dev
        :create-results-buffer
        :init-device
        :set-integration-time-ms
        :set-ranging-frequency-hz
        :set-ranging-mode
        :set-resolution
        :set-target-order
        :is-alive
        :check-data-ready
        :start-ranging
        :stop-ranging
        :get-ranging-data
        :extract-distance-mm
        :extract-range-sigma-mm))
(in-package :i2c-vl53l5cx)

(require :i2c-dev)

(defconstant DEVADDR #x29)
(defconstant RESOLUTION_4X4 16)
(defconstant RESOLUTION_8X8 64)

(defparameter *is_alive_buffer*
              (cffi:foreign-alloc :int8))
(defparameter *is_ready_buffer*
              (cffi:foreign-alloc :int8))

;
; VL53L5CX Driver
;
(cffi:load-foreign-library "./vl53l5cx_driver.so")

(defconstant NB_TARGET_PER_ZONE 2)

(defconstant TARGET_ORDER_CLOSEST 1)
(defconstant TARGET_ORDER_STRONGEST 2)
(defconstant RANGING_MODE_CONTINUOUS 1)
(defconstant RANGING_MODE_AUTONOMOUS 3)
(defconstant POWER_MODE_SLEEP 0)
(defconstant POWER_MODE_WAKEUP 1)

(defconstant NVM_DATA_SIZE 492)
(defconstant CONFIGURATION_SIZE 972)
(defconstant OFFSET_BUFFER_SIZE 488)
(defconstant XTALK_BUFFER_SIZE 776)

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
; Sensor Functions
;
(defun create-dev ()
    (let ((dev (cffi:foreign-alloc '(:struct vl53l5cx_configuration))))
        (setf (cffi:foreign-slot-value (cffi:foreign-slot-pointer dev '(:struct vl53l5cx_configuration) 'platform)
                                       '(:struct vl53l5cx_platform) 'address)
                                       DEVADDR)
        dev))

(defun create-results-buffer ()
    (cffi:foreign-alloc '(:struct vl53l5cx_results_data)))

(defun init-device (dev)
    (vl53l5cx_init dev))

(defun set-integration-time-ms (dev integration-time-ms)
    (vl53l5cx_set_integration_time_ms dev integration-time-ms))

(defun set-ranging-frequency-hz (dev frequency-hz)
    (vl53l5cx_set_ranging_frequency_hz dev frequency-hz))

(defun set-ranging-mode (dev ranging-mode)
    (vl53l5cx_set_ranging_mode dev ranging-mode))

(defun set-resolution (dev resolution)
    (vl53l5cx_set_resolution dev resolution))

(defun set-target-order (dev target-order)
    (vl53l5cx_set_target_order dev target-order))

(defun is-alive (dev)
    (vl53l5cx_is_alive dev *is_alive_buffer*)
    (cffi:mem-ref *is_alive_buffer* :int8))

(defun check-data-ready (dev)
    (vl53l5cx_check_data_ready dev *is_ready_buffer*)
    (cffi:mem-ref *is_ready_buffer* :int8))

(defun start-ranging (dev)
    (vl53l5cx_start_ranging dev))

(defun stop-ranging (dev)
    (vl53l5cx_stop_ranging dev))

(defun get-ranging-data (dev results_buffer)
    (vl53l5cx_get_ranging_data dev results_buffer))

(defun extract-distance-mm (dest results resolution)
    (let ((c_distance_mm (cffi:foreign-slot-pointer results '(:struct vl53l5cx_results_data) 'distance_mm)))
        (loop for ind from 0 below resolution do
            (setf (aref dest ind)
                (loop for tar from 0 below NB_TARGET_PER_ZONE collect
                    (cffi:mem-aref c_distance_mm :int16 (+ (* NB_TARGET_PER_ZONE ind) tar)))))))

(defun extract-range-sigma-mm (dest results resolution)
    (let ((c_range_sigma_mm (cffi:foreign-slot-pointer results '(:struct vl53l5cx_results_data) 'range_sigma_mm)))
        (loop for ind from 0 below resolution do
            (setf (aref dest ind)
                (loop for tar from 0 below NB_TARGET_PER_ZONE collect
                    (cffi:mem-aref c_range_sigma_mm :int16 (+ (* NB_TARGET_PER_ZONE ind) tar)))))))

