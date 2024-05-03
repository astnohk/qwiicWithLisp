#include <stdio.h>
#include <stdint.h>
#include <errno.h>

#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <linux/i2c-dev.h>
#include <linux/i2c.h>

uint8_t
i2c_write(char *dev, uint8_t addr, void *buf, size_t buf_len)
{
    struct i2c_msg msg;
    struct i2c_rdwr_ioctl_data packets;
    int8_t ret;
    uint8_t status = 0;
    int32_t fd;

    msg.addr = addr;
    msg.flags = 0;
    msg.buf = buf;
    msg.len = buf_len;

    packets.msgs = &msg;
    packets.nmsgs = 1;

    fd = open(dev, O_RDWR);
    if (fd < 0)
    {
        perror(dev);
        status = 255;
        return status;
    }
    ret = ioctl(fd, I2C_RDWR, &packets);
    if (ret < 0)
    {
        perror(dev);
        status = -1;
        return status;
    }

    close(fd);

    return status;
}

uint8_t
i2c_read(char *dev, uint8_t addr, void *read_buf, size_t read_buf_len)
{
    struct i2c_msg msg;
    struct i2c_rdwr_ioctl_data packets;
    int8_t ret;
    uint8_t status = 0;
    int32_t fd;

    msg.addr = addr;
    msg.flags = I2C_M_RD;
    msg.buf = read_buf;
    msg.len = read_buf_len;

    packets.msgs = &msg;
    packets.nmsgs = 1;

    fd = open(dev, O_RDWR);
    if (fd < 0)
    {
        perror(dev);
        status = 255;
        return status;
    }
    ret = ioctl(fd, I2C_RDWR, &packets);
    if (ret < 0)
    {
        perror(dev);
        status = -1;
        return status;
    }

    close(fd);

    return status;
}

uint8_t
i2c_read_register(char *dev, uint8_t addr, void *write_buf, size_t write_buf_len, void *read_buf, size_t read_buf_len)
{
    struct i2c_msg msgs[2];
    struct i2c_rdwr_ioctl_data packets;
    int8_t ret;
    uint8_t status = 0;
    int32_t fd;

    msgs[0].addr = addr;
    msgs[0].flags = 0;
    msgs[0].buf = write_buf;
    msgs[0].len = write_buf_len;
    msgs[1].addr = addr;
    msgs[1].flags = I2C_M_RD;
    msgs[1].buf = read_buf;
    msgs[1].len = read_buf_len;

    packets.msgs = msgs;
    packets.nmsgs = 2;

    fd = open(dev, O_RDWR);
    if (fd < 0)
    {
        perror(dev);
        status = 255;
        return status;
    }
    ret = ioctl(fd, I2C_RDWR, &packets);
    if (ret < 0)
    {
        perror(dev);
        status = -1;
        return status;
    }

    close(fd);

    return status;
}

