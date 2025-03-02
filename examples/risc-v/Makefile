# Makefile for RISC-V core in RTLisp
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

include ../icestick.mk


# ---------- Target code ----------

# Bitstream
TARGET = core.bin
TOPMODULE = soc

# Sources
SOURCES = package.lisp clockworks.lisp core.lisp
CONFIG = core.pcf
OBJECT = core.v

# Assembly code
ASM_SOURCES = firmware.s
ASM_OBJECT = firmware.hex


# ---------- Tools ----------

# Tools
RM = rm -fr

# ---------- Top-level targets ----------

# Build the target bitstream
$(TARGET): $(ASM_OBJECT) $(SOURCES) $(CONFIG)


# Upload the target bitstream to the device
upload: $(TARGET)
	$(PROGRAM) $(PROGRAM_OPTS) $<


# Clean-up the build directory
clean:
	$(RM) $(GENERATED)


# Print a help message
help:
	@make usage


# ---------- Lisp to Verilog rules ----------

LISP = sbcl

LISP_OPTS =

.SUFFIXES: .lisp .v

.lisp.v:
	$(LISP) $(LISP_OPTS) -o $*.v $(SOURCES)


# ---------- Assembler rules ----------

AS = riscv64-linux-gnu-as
LD = riscv64-linux-gnu-ld
OBJDUMP = riscv64-linux-gnu-objdump
GREP = grep
BASH = bash
SED = sed
AWK = awk
DD = dd
OD = od

AS_OPTS = -march=rv32i -mabi=ilp32 -mno-relax
LD_OPTS = -T bram.ld -m elf32lriscv -nostdlib

.SUFFIXES: .s .o .hex

.s.o:
	$(AS) $(AS_OPTS) $*.s -o $*.o

.o.hex:
	$(OBJDUMP) -h $*.o | $(GREP) .text | $(SED) -e 's|\s\+|\t|g' -e 's|^\s\+||g'| $(AWK) '{print "$(DD) if=$*.o bs=1 count=$$((0x"$$3")) skip=$$((0x"$$6")) status=none";}' | $(BASH) | $(OD) -Anone -tx4 >$*.hex


# ----- Usage -----

define HELP_MESSAGE
Available targets:
   make              make the target
   make upload       upload the bitstream to the FPGA
   make clean        clean-up the build directory

endef
export HELP_MESSAGE

usage:
	@echo "$$HELP_MESSAGE"
