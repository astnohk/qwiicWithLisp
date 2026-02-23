(in-package :I2C-DEV)

(include "fcntl.h")
(include "sys/ioctl.h")
(include "linux/i2c-dev.h")
(include "linux/i2c.h")

(constant (I2C-RDWR "I2C_RDWR"))
(constant (I2C-M-RD "I2C_M_RD"))
(constant (O-RDWR "O_RDWR"))

(cstruct i2c-msg "struct i2c_msg"
    (addr "addr" :type :uint16)
    (flags "flags" :type :uint16)
    (len "len" :type :uint16)
    (buf "buf" :type :pointer))
(cstruct i2c-rdwr-ioctl-data "struct i2c_rdwr_ioctl_data"
    (msgs "msgs" :type :pointer)
    (nmsgs "nmsgs" :type :uint32))

