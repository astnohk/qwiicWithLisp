# qwiicWithLisp
Use Qwiic / STEMMA QT modules with Common Lisp.
Here some simple examples of Common Lisp code for comunicating with some sensors via I2C.


## Getting Started

### Prerequisites

Install `sbcl` or some other Common Lisp implementaions (multi-threading support is needed).

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
Some of Common Lisp programs use this simple wrapper to communicate with I2C.

### `i2c-dev Common Lisp wrapper`

It is very simple wrapper for `linux/i2c.h` and `linux/i2c-dev.h`.
These libraries are available on Linux.
This is purely written in Common Lisp with CFFI and CFFI-Grovel so you can create some projects with almost pure Common Lisp source code.

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


## Old info

### `i2c_lib.c`

`i2c_lib.c` is very simple wrapper of i2c-dev for Common Lisp.
It provide easy way to import combined features of ioctl and i2c-dev.
If you want CFFI without CFFI-Grovel it's hard to import ioctl's functions.
`i2c_lib.c` is only need to define some arguments with valid types.

#### Build `i2c_lib.c`

Install `gcc` and build it.

```
$ make
```

#### Import `i2c_lib.c` in Common Lisp

Put a built shared object `i2c_lib.so` in your project and load with CFFI.

```
(require :cffi)

(cffi:load-foreign-library "./i2c_lib.so")
```

Define functions by `cffi:defcfun`.

```
(cffi:defcfun ("i2c_write" i2c-write) :uint8
              (dev (:pointer :char))
              (addr :uint8)
              (buf :pointer)
              (buf_len :size))

(cffi:defcfun ("i2c_read" i2c-read) :uint8
              (dev (:pointer :char))
              (addr :uint8)
              (read_buf :pointer)
              (read_buf_len :size))

(cffi:defcfun ("i2c_read_register" i2c-read-register) :uint8
              (dev (:pointer :char))
              (addr :uint8)
              (write_buf :pointer)
              (write_buf_len :size)
              (read_buf :pointer)
              (read_buf_len :size))
```

Allocate C char* constant of path string.
In almost all environment the main I2C port is assigned to "/dev/i2c-1".

```
(defparameter *dev* (cffi:foreign-string-alloc "/dev/i2c-1"))
```

Allocate buffer for send/receive data from I2C.
The data type is `:uint8`.

```
(defparameter *rbuffer*
    (cffi:foreign-alloc :uint8
                        :initial-contents '(0 0)))

(defparameter *wbuffer*
    (cffi:foreign-alloc :uint8
                        :initial-contents '(0 0)))
```

#### Use `i2c_lib.c` in Common Lisp

If you want to write some data to device via I2C then you need assign some value to `*wbuffer*`.
And just write it by `i2c-write`.
Here is very simple example to write `0xFF` to device of address `0x40`.

```
(setf (cffi:mem-aref *wbuffer* :uint8 0) #xFF)
(i2c-write *dev* #x40 *wbuffer* 1)
```

And this is the example of read two values from `0x40`.
You should check boundary in practical use.
You need to set count within the size of `*rbuffer*`.

```
(i2c-read *dev* #x40 *rbuffer* 2)
(loop for i from 0 below 2 collect
    (cffi:mem-aref *rbuffer* :uint8 i))
```

Also you may need to read some values from the specified register address.
You can use `i2c-read-register` for such situations.
At first you should set register address in `*wbuffer*` because `i2c-read-register` will write *wbuffer* at first then next read values and store them to `*rbuffer*`.

```
; Read from register at 0x10
(setf (cffi:mem-aref *wbuffer* :uint8 0) #x10)

; Read 2 values from 0x10 from the device with I2C address 0x40
(i2c-read-register *dev*
                   #x40
                   *wbuffer*
                   1
                   *rbuffer*
                   2)

; Get values as Common Lisp list
(loop for i from 0 below 2 collect
    (cffi:mem-aref *rbuffer* :uint8 i))
```


## LICENSE

Codes written by me distributed under MIT License.
This repository includes STMicroelectronics's `VL53L5CX ULD driver`.
You need to read STM license files before using them.
