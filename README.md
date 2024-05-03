# qwiicWithLisp
Use Qwiic / STEMMA QT modules on LISP


## Getting Started

Install `sbcl` or some other Common Lisp implementaions (multi-threading support is needed).
Install `gcc` and build C libraries:

```
$ make
```

Run some of LISP code.

```
$ sbcl --load vl53l5cx.lisp
$ sbcl --load lsm6ds3tr.lisp
$ sbcl --load fs3000.lisp
```

## Source Code

### `i2c_lib`

It is very simple wrapper for `linux/i2c.h` and `linux/i2c-dev.h`.
These libraries are available on Linux.
LISP programs use this simple wrapper to communicate with I2C.

### FS3000

[FS3000](https://www.sparkfun.com/products/18377)  is Air Velocity Sensor.

### LSM6DS3TR and LIS3MDL

[Adafruit LSM6DS3TR-C + LIS3MDL](https://www.adafruit.com/product/5543) is 9 DoF IMU STEMMA QT/Qwiic module.
LSM6DS3TR is 6 DoF IMU accelerometer + gyro.
LIS3MDL is 3-axis magnetometer.

### VL53L5CX

[VL53L5CX](https://www.sparkfun.com/products/18642) is 4x4 dToF (direct Time of Flight) sensor array upto 8x8.
It emit laser pulses from VCSEL and receive signals on SPAD array.
It could detect distances to targets upto 4000mm.

### Haptic Driver with DA7280

[Haptic Driver](https://www.sparkfun.com/products/17590) includes Linear Resonant Actuator (LRA) vibrator and motor driver DA7280.
It could control LRA within a few milliseconds so you can produce arbitrary vibration such as simple vibration or knock like single shot vibrations.


## LICENSE

Codes written by me distributed under MIT License.
This repository includes STMicroelectronics's `VL53L5CX ULD driver`.
You need to read STM license files before using them.
