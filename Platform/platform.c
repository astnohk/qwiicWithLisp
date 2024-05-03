/**
  *
  * Copyright (c) 2021 STMicroelectronics.
  * All rights reserved.
  *
  * This software is licensed under terms that can be found in the LICENSE file
  * in the root directory of this software component.
  * If no LICENSE file comes with this software, it is provided AS-IS.
  *
  ******************************************************************************
  */


#include <stdio.h>
#include <stdint.h>
#include <errno.h>

#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <linux/i2c-dev.h>
#include <linux/i2c.h>

#define I2C_DEVICE ("/dev/i2c-1")

#include "platform.h"

uint8_t
vl53l5cx_i2c_write(uint8_t addr, void *buf, size_t buf_len)
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
        status = 255;
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
vl53l5cx_i2c_read(uint8_t addr, void *write_buf, size_t write_buf_len, void *read_buf, size_t read_buf_len)
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

    fd = open(I2C_DEVICE, O_RDWR);
    if (fd < 0)
    {
        perror(I2C_DEVICE);
        status = 255;
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

uint8_t RdByte(
		VL53L5CX_Platform *p_platform,
		uint16_t RegisterAdress,
		uint8_t *p_value)
{
	uint8_t status = 0;
    uint8_t buf[2];

    buf[0] = (RegisterAdress >> 8) & 0xff;
    buf[1] = RegisterAdress & 0xff;
    status = vl53l5cx_i2c_read(p_platform->address, buf, 2, p_value, 1);

	return status;
}

uint8_t WrByte(
		VL53L5CX_Platform *p_platform,
		uint16_t RegisterAdress,
		uint8_t value)
{
	uint8_t status = 0;
    uint8_t buf[3];

    buf[0] = (RegisterAdress >> 8) & 0xff;
    buf[1] = RegisterAdress & 0xff;
    buf[2] = value;
    status = vl53l5cx_i2c_write(p_platform->address, buf, 3);

	return status;
}

#define I2C_CHUNK_SIZE 2048
uint8_t WrMulti(
		VL53L5CX_Platform *p_platform,
		uint16_t RegisterAdress,
		uint8_t *p_values,
		uint32_t size)
{
	uint8_t status = 0;
    size_t i, N;
    uint8_t buf[2 + I2C_CHUNK_SIZE];

    for (i = 0; i < size; i += I2C_CHUNK_SIZE)
    {
        N = size - i;
        if (N > I2C_CHUNK_SIZE)
        {
            N = I2C_CHUNK_SIZE;
        }
        buf[0] = ((RegisterAdress + i) >> 8) & 0xff;
        buf[1] = (RegisterAdress + i) & 0xff;
        memcpy(buf + 2, p_values + i, N);
        status |= vl53l5cx_i2c_write(p_platform->address, buf, 2 + N);
    }

	return status;
}

uint8_t RdMulti(
		VL53L5CX_Platform *p_platform,
		uint16_t RegisterAdress,
		uint8_t *p_values,
		uint32_t size)
{
	uint8_t status = 0;
    uint8_t buf[2];

    buf[0] = (RegisterAdress >> 8) & 0xff;
    buf[1] = RegisterAdress & 0xff;
    status = vl53l5cx_i2c_read(p_platform->address, buf, 2, p_values, size);
	
	return status;
}

uint8_t Reset_Sensor(
		VL53L5CX_Platform *p_platform)
{
	uint8_t status = 0;
	
	/* (Optional) Need to be implemented by customer. This function returns 0 if OK */
	
	/* Set pin LPN to LOW */
	/* Set pin AVDD to LOW */
	/* Set pin VDDIO  to LOW */
	WaitMs(p_platform, 100);

	/* Set pin LPN of to HIGH */
	/* Set pin AVDD of to HIGH */
	/* Set pin VDDIO of  to HIGH */
	WaitMs(p_platform, 100);

	return status;
}

void SwapBuffer(
		uint8_t 		*buffer,
		uint16_t 	 	 size)
{
	uint32_t i, tmp;
	
	/* Example of possible implementation using <string.h> */
	for(i = 0; i < size; i = i + 4) 
	{
		tmp = (
		  buffer[i]<<24)
		|(buffer[i+1]<<16)
		|(buffer[i+2]<<8)
		|(buffer[i+3]);
		
		memcpy(&(buffer[i]), &tmp, 4);
	}
}	

uint8_t WaitMs(
		VL53L5CX_Platform *p_platform,
		uint32_t TimeMs)
{
	uint8_t status = 0;

    usleep(TimeMs * 1000);
	
	return status;
}
