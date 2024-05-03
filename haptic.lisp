(require :cffi)


(defun log10 (x) (/ (log x) (log 10)))
(defun get-elapsed-time () (/ (get-internal-real-time) internal-time-units-per-second))


;
; Haptic Driver
;
(cffi:load-foreign-library "./libhaptic.so")
(cffi:foreign-funcall "hapticEnableFreqTrack" :int8 0 :uint8)
(cffi:foreign-funcall "hapticSetOperationModeDRO" :uint8)
(cffi:defcfun ("hapticSetOverrideVal" haptic-set-override-val) :uint8
    (val :int8))


;
; Haptic Thread
;
(haptic-set-override-val #x00)
(defun haptic-loop ()
    (let ((last-beep-time (get-elapsed-time)))
        (loop
            (let ((duration 0.5)
                  (now (get-elapsed-time)))
                (cond ((> (- now last-beep-time) duration)
                           (setf last-beep-time now)
                           (haptic-set-override-val #x35)
                           (sleep 0.02)))
                (haptic-set-override-val #x00)
                (sleep 0.1)))))

(haptic-loop)
