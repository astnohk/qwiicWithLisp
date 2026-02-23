(load "htu21d.lisp")

(format t "temperature: ~d Celsius degree~%" (i2c-htu21d:read-temperature))
(format t "humidity: ~d%~%" (i2c-htu21d:read-humidity))
