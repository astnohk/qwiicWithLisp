(defvar MCP3425-ADDR #x68) ; You may need to change the device address
(defvar MCP3425-CONV-ONE-SHOT 0)
(defvar MCP3425-CONV-CONTINUOUS 1)
(defvar MCP3425-SAMPLE-RATE-240 0)
(defvar MCP3425-SAMPLE-RATE-60 1)
(defvar MCP3425-SAMPLE-RATE-15 2)
(defvar MCP3425-PGA-GAIN-1 0)
(defvar MCP3425-PGA-GAIN-2 1)
(defvar MCP3425-PGA-GAIN-4 2)
(defvar MCP3425-PGA-GAIN-8 3)

(defun mcp3425-set (conv rate gain)
    (let ((con (logior
                   (ash 1 7)
                   (ash 0 5)
                   (ash conv 4)
                   (ash rate 2)
                    gain)))
        (with-i2c (str MCP3425-ADDR)
            (write-byte con str))))

(defun mcp3425-read ()
    (with-i2c (str MCP3425-ADDR 2)
        (logior (ash (read-byte str) 8)
                (read-byte str))))

(defun get-wave (x n)
    (if (> n 47)
        nil
        (append
            (if (= x n) '("*") '(" "))
            (get-wave x (+ n 1)))))

(defun show-wave (s)
    (if (car s)
        (progn
            (format t "~d" (car s))
            (show-wave (cdr s)))))
