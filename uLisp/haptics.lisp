(defun hap-enable-freqtrack ()
    (with-i2c (str #x4a)
              (write-byte #x13 str)
              (write-byte #x1e)))

(defun hap-set-idle ()
    (with-i2c (str #x4a)
              (write-byte #x22 str)
              (write-byte #x00 str)))

(defun hap-set-opmode-dro ()
    (with-i2c (str #x4a)
              (write-byte #x22 str)
              (write-byte #x01 str)))

(defun hap-set-override-val (x)
    (with-i2c (str #x4a)
              (write-byte #x23 str)
              (write-byte x str)))

(defun run-hap-loop ()
    (hap-enable-freqtrack)
    (hap-set-idle)
    (hap-set-opmode-dro)
    (loop
        (for-millis (500) (hap-set-override-val 0))
        (for-millis (500) (hap-set-override-val 30))))
