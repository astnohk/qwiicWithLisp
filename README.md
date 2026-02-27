# qwiicWithLisp
Use Qwiic / STEMMA QT modules with Common Lisp.
Here some simple examples of Common Lisp code for comunicating with some sensors via I2C.


## Getting Started

### Prerequisites

Install `sbcl` or some other Common Lisp implementaions (multi-threading support is needed).

### Build i2c_lib.c

Install `gcc` and build C libraries:

```
$ make
```

### Install i2c-dev

Make `~/common-lisp/` and put `i2c-dev/` into the directory.

```
$ mkdir ~/common-lisp
$ cp -r ./i2c-dev ~/common-lisp/
```

Check whether you can load the installed package.

```
$ sbcl
* (asdf:load-system "i2c-dev")
```

If some error occured at `asdf:load-system` then please check error messages and solve problems.

### Run Common Lisp code

Run some of Common Lisp code.

```
$ sbcl --load amg8833.lisp
$ sbcl --load htu21d.lisp
$ sbcl --load fs3000.lisp
$ sbcl --load lsm6ds3tr.lisp
$ sbcl --load vl53l5cx.lisp
$ sbcl --load haptic.lisp
$ sbcl --load mpr121.lisp
$ sbcl --load rv8803.lisp
```

## Source Code

### `i2c_lib`

It is very simple wrapper for `linux/i2c.h` and `linux/i2c-dev.h`.
These libraries are available on Linux.
LISP programs use this simple wrapper to communicate with I2C.

### ADS7830 8ch 8-bit AD Converter

[ADS7830 ADC](https://www.adafruit.com/product/5836) is 8-ch 8-bit AD converter.

### AMG8833 Grid-EYE Infrared Array Sensor

[AMG8833 Grid-EYE](https://www.sparkfun.com/sparkfun-grid-eye-infrared-array-breakout-amg8833-qwiic.html) is infrared array temperature sensor.
It could measure temperature distant from targets with 8x8 resolution.
The details of this sensor is available at [Grid-EYE](https://industrial.panasonic.com/jp/products/pt/grid-eye/models/AMG8833).

### AW9523 LED Driver

[AW9523 LED Driver](https://www.adafruit.com/product/4886) is LED driver breakout board with 16 I/O ports.

### DS3231 Precision RTC

[DS3231 Precision RTC](https://www.adafruit.com/product/5188) is the high precision RTC.
This board has CR1220 battery holder for backup power source.

### HTU21D Temperature and Humidity Sensor

[HTU21D-F Temperature & Humidity Sensor Breakout Board](https://www.adafruit.com/product/1899) is an accurate simple humidity and temperature sensor with a PTFE filter which keeps the sensor clean.

### FS3000

[FS3000](https://www.sparkfun.com/products/18377)  is Air Velocity Sensor.

### Haptic Driver with DA7280

[Haptic Driver](https://www.sparkfun.com/products/17590) includes Linear Resonant Actuator (LRA) vibrator and motor driver DA7280.
It could control LRA within a few milliseconds so you can produce arbitrary vibration such as simple vibration or knock like single shot vibrations.

### LSM6DS3TR and LIS3MDL

[Adafruit LSM6DS3TR-C + LIS3MDL](https://www.adafruit.com/product/5543) is 9 DoF IMU STEMMA QT/Qwiic module.
LSM6DS3TR is 6 DoF IMU accelerometer + gyro.
LIS3MDL is 3-axis magnetometer.

### MPR121 12-Key Capacitive Touch Sensor

[12-Key Touch Sensor](https://www.adafruit.com/product/1982) is capacitive touch sensor module.
It could get 12-Key touch state via I2C.
The function `read-mpr121-touch-status` returns each key's touch state as a list of `1` (on) or `0` (off).

### RV8803-C RTC

[Real Time Clock Module - RV-8803](https://www.sparkfun.com/sparkfun-real-time-clock-module-rv-8803-qwiic.html) is RTC module.
It has alarm and countdown interrupt.

```
* (i2c-rv8803:write-date)
* (i2c-rv8803:isoformat (i2c-rv8803:read-date))
```

### SSD1306 Micro OLED

[SparkFun Micro OLED Breakout](https://www.sparkfun.com/products/22495) is micro OLED breakout board.
It has OLED with resolution of 64x48.

### VL53L5CX

[VL53L5CX](https://www.sparkfun.com/products/18642) is 4x4 dToF (direct Time of Flight) sensor array upto 8x8.
It emit laser pulses from VCSEL and receive signals on SPAD array.
It could detect distances to targets upto 4000mm.


## LICENSE

Codes written by me distributed under MIT License.
This repository includes STMicroelectronics's `VL53L5CX ULD driver`.
You need to read STM license files before using them.
