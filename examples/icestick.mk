# Makefile includes for IceStick FPGA programming with the icestorm toolchain
#
# Copyright (C) 2024--2025, Simon Dobson
#
# This is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this software. If not, see <http://www.gnu.org/licenses/gpl.html>.

# ---------- Device ----------

# Target device
FPGA_DEVICE = hx1k
FPGA_PACKAGE = tq144

# Docker container tag
# Leave this blank to run the toolchain native; provide a tag to run in a container
CONTAINER_TAG = icestorm


# ---------- Tools ----------

# Tools
VHDSLC = ../../../bin/vhdslc
SYNTH = yosys
PNR = nextpnr-ice40
PACK = icepack
PROGRAM = iceprog
DOCKER = docker
RM = rm -fr

# Tool options
VHDSLC_OPTS =
SYNTH_OPTS = -q
PNR_OPTS = -q
PROGRAM_OPTS =

# Top module defaults to the stem of the name of the target bitstream
TARGET_BASENAME = $(shell basename $(TARGET) .bin)
ifeq ($(TOPMODULE),)
TOPMODULE = $(TARGET_BASENAME)
endif

# Generated files
GENERATED_STEMS = $(foreach fn,$(SOURCES), $(shell basename $(fn) .lisp))
GENERATED += $(foreach stem,$(GENERATED_STEMS),$(stem).v $(stem).asc $(stem).bin $(stem).json)

# Command to run tools in a container (if requested)
# (Container must run privileged to be able to upload to the device.)
ifneq ($(CONTAINER_TAG),)
IN_CONTAINER = $(DOCKER) run -it --rm --privileged --mount type=bind,source=`pwd`,target=/work $(CONTAINER_TAG)
endif

# ---------- Implicit rules ----------

.SUFFIXES: .v .pcf .json .asc .bin

%.v: $(SOURCES) $(CONFIG)
	$(VHDSLC) $(VHDSLC_OPTS) -o $*.v $(SOURCES)

.v.json:
	$(IN_CONTAINER) $(SYNTH) $(SYNTH_OPTS) -p "synth_ice40 -top $(TOPMODULE) -json $*.json" $<

.json.asc:
	$(IN_CONTAINER) $(PNR) $(PNR_OPTS) --$(FPGA_DEVICE) --package $(FPGA_PACKAGE) --json $*.json --pcf $*.pcf --asc $*.asc

.asc.bin:
	$(IN_CONTAINER) $(PACK) $< $*.bin
