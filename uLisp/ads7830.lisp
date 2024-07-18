(defvar ADS7830-ADDR #x48)
(defvar ADS7830-DIFFERENTIAL-INPUTS 0)
(defvar ADS7830-SINGLE-ENDED-INPUTS 1)
(defvar ADS7830-PD-POWER-DOWN 0)
(defvar ADS7830-PD-INTERNAL-OFF-AD-ON 1)
(defvar ADS7830-PD-INTERNAL-ON-AD-OFF 2)
(defvar ADS7830-PD-INTERNAL-ON-AD-ON 3)

(defun ads7830-read (ch sd pd)
    (with-i2c (str #x48)
        (write-byte
            (logior (ash sd 7)
                    (ash ch 4)
                    (ash pd 2))
            str)
        (restart-i2c str 1)
        (read-byte str)))

(let ((th 1)
      (prev 0))
    (loop
        (let ((x (ads7830-read
                     0
                     ADS7830-SINGLE-ENDED-INPUTS
                     ADS7830-PD-INTERNAL-OFF-AD-ON)))
            (for-millis (50)
                (format t "~d~%"
                    (if (> (- x prev) th)
                        "*"
                        (if (< (- x prev) (* -1 th))
                            "     *"
                            "  *"))))
            (setq prev x))))
