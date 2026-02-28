;
; Qwiic Micro OLED (SSD1306)
;
(load "ssd1306.lisp")

(format t "init...~%")
(i2c-ssd1306:set-oled-display-on nil)
(i2c-ssd1306:set-oled-display-clock-div #x80)
(i2c-ssd1306:set-oled-multiplex-ratio (- i2c-ssd1306:HEIGHT 1))
(i2c-ssd1306:set-oled-display-offset 0)
(i2c-ssd1306:set-oled-display-start-line 0)
(i2c-ssd1306:set-oled-charge-pump t)
(i2c-ssd1306:set-oled-memory-addressing-mode i2c-ssd1306:MEMORY_PAGE_ADDRESSING)
(i2c-ssd1306:set-oled-segment-remap t)
(i2c-ssd1306:set-oled-com-scan-direction nil)
(i2c-ssd1306:set-oled-com-pins t nil)
(i2c-ssd1306:set-oled-contrast #xcf)
(i2c-ssd1306:set-oled-pre-charge-period #xf1)
(i2c-ssd1306:set-oled-v-comh-deselect-level 4)
(i2c-ssd1306:set-oled-entire-display-on nil)
(i2c-ssd1306:set-oled-inverse-display nil)
(i2c-ssd1306:set-oled-deactivate-scroll)
(i2c-ssd1306:set-oled-display-on t)
(format t "start~%")

(loop for i from 0 below i2c-ssd1306:HEIGHT do
    (loop for j from 0 below i2c-ssd1306:WIDTH do
        (i2c-ssd1306:draw-pixel j i 0)))

; Drawing loop
(let ((i 0))
    (loop
        ; Clear all pixels
        (loop for y from 0 below i2c-ssd1306:HEIGHT do
            (loop for x from 0 below i2c-ssd1306:WIDTH do
                (i2c-ssd1306:draw-pixel x y 0)))
        ; Draw lines
        (i2c-ssd1306:draw-slashes)
        ; Send all pixels data to the device
        (loop for page from 0 below i2c-ssd1306:PAGE_SIZE do
            (i2c-ssd1306:write-oled-pixels page))
        ; Increment i
        (setq i (mod (+ i 1) (* i2c-ssd1306:WIDTH i2c-ssd1306:HEIGHT)))
        (sleep 0.01)))

