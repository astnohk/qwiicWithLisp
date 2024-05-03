CC=gcc
WARNING=-Wall -Wextra
OPTION=-O2
OPTION_SHARED=-O2 -shared -fPIC

PLATFORM_DIR=Platform
INCLUDES=VL53L5CX_ULD_driver/VL53L5CX_ULD_API/inc
SRCDIR=VL53L5CX_ULD_driver/VL53L5CX_ULD_API/src


all: vl53l5cx_driver.so libhaptic.so i2c_lib.so

vl53l5cx_driver.so: platform.o vl53l5cx_api.o 
	$(CC) $(WARNING) $(OPTION_SHARED) -I $(PLATFORM_DIR) -I $(INCLUDES) -o $@ $^

libhaptic.so: haptic_da7280.c
	$(CC) $(WARNING) $(OPTION_SHARED) -o $@ $<

i2c_lib.so: i2c_lib.c
	$(CC) $(WARNING) $(OPTION_SHARED) -o $@ $<

platform.o: $(PLATFORM_DIR)/platform.c
	$(CC) $(WARNING) $(OPTION_SHARED) -I $(PLATFORM_DIR) -I $(INCLUDES) -c $^

vl53l5cx_api.o: $(SRCDIR)/vl53l5cx_api.c
	$(CC) $(WARNING) $(OPTION_SHARED) -I $(PLATFORM_DIR) -I $(INCLUDES) -c $^


.PHONY: clean
clean:
	rm -f platform.o vl53l5cx_api.o 
