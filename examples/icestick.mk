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

# ---------- Tools ----------

# Tools
VHDSLC = ../../bin/vhdslc
SYNTH = yosys
PNR = nextpnr-ice40
PACK = icepack
PROGRAM = iceprog
RM = rm -fr

# Tool options
VHDSLC_OPTS = $(SOURCES_COMMON)
SYNTH_OPTS = -q
PNR_OPTS = -q
PROGRAM_OPTS =

# Top module defaults to the stem of the name of the target bitstream
ifeq ($(TOPMODULE),)
TOPMODULE = $(shell basename $(TARGET) .bin)
endif

# Generated files
GENERATED_STEMS = $(foreach fn,$(SOURCES), $(shell basename $(fn) .v))
GENERATED = $(foreach stem,$(GENERATED_STEMS),$(stem).asc $(stem).bin $(stem).json)


# ---------- Implicit rules ----------

%.v: %.lisp
	$(VHDSLC) $(VHDSLC_OPTS) $*.lisp

%.json: %.v
	$(SYNTH) $(SYNTH_OPTS) -p "synth_ice40 -top $(TOPMODULE) -json $*.json" $<

%.asc: %.json %.pcf
	$(PNR) $(PNR_OPTS) --$(FPGA_DEVICE) --package $(FPGA_PACKAGE) --json $*.json --pcf $*.pcf --asc $*.asc

%.bin: %.asc
	$(PACK) $< $*.bin
