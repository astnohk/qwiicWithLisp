#include <stdio.h>
#include <stdint.h>
#include <errno.h>

#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <linux/i2c-dev.h>
#include <linux/i2c.h>

#define I2C_DEVICE ("/dev/i2c-1")
#define I2C_DA7280_ADDR 0x4a

uint8_t
haptic_i2c_write(uint8_t addr, void *buf, size_t buf_len)
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

    fd = open(I2C_DEVICE, O_RDWR);
    if (fd < 0)
    {
        perror(I2C_DEVICE);
        status = -1;
        return status;
    }
    ret = ioctl(fd, I2C_RDWR, &packets);
    if (ret < 0)
    {
        perror(I2C_DEVICE);
        status = -1;
        return status;
    }

    close(fd);

    return status;
}

uint8_t
haptic_i2c_read(uint8_t addr, void *buf, size_t buf_len)
{
    struct i2c_msg msg;
    struct i2c_rdwr_ioctl_data packets;
    int8_t ret;
    uint8_t status = 0;
    int32_t fd;

    msg.addr = addr;
    msg.flags = I2C_M_RD;
    msg.buf = buf;
    msg.len = buf_len;

    packets.msgs = &msg;
    packets.nmsgs = 1;

    fd = open(I2C_DEVICE, O_RDWR);
    if (fd < 0)
    {
        perror(I2C_DEVICE);
        status = -1;
        return status;
    }
    ret = ioctl(fd, I2C_RDWR, &packets);
    if (ret < 0)
    {
        perror(I2C_DEVICE);
        status = -1;
    }

    close(fd);

    return status;
}

uint8_t
hapticEnableFreqTrack(int8_t enable)
{
    char data[2];
    data[0] = 0x13;
    data[1] = 0x16 | (enable == 0 ? 0x00 : 0x08);
    return haptic_i2c_write(I2C_DA7280_ADDR, data, 2);
}

uint8_t
hapticSetIdle(void)
{
    char data[2];
    data[0] = 0x22;
    data[1] = 0x00;
    return haptic_i2c_write(I2C_DA7280_ADDR, data, 2);
}

uint8_t
hapticSetOperationModeDRO(void)
{
    char data[2];
    data[0] = 0x22;
    data[1] = 0x01; // Set DRO
    return haptic_i2c_write(I2C_DA7280_ADDR, data, 2);
}

uint8_t
hapticSetOverrideVal(int8_t val)
{
    char data[2];
    data[0] = 0x23;
    data[1] = val;
    return haptic_i2c_write(I2C_DA7280_ADDR, data, 2);
}

int
main(void)
{
    int ret;
    ret = hapticEnableFreqTrack(0);
    if (ret < -1)
    {
        perror(I2C_DEVICE);
        return -1;
    }
    ret = hapticSetIdle();
    if (ret < -1)
    {
        perror(I2C_DEVICE);
        return -1;
    }
    ret = hapticSetOperationModeDRO();
    if (ret < -1)
    {
        perror(I2C_DEVICE);
        return -1;
    }
    // 
    for (int i = 0; i < 5; ++i)
    {
        hapticSetOverrideVal(0x7f);
        usleep(10 * 1000);
        hapticSetOverrideVal(0);
        usleep(100 * 1000);
    }

    return 0;
}

