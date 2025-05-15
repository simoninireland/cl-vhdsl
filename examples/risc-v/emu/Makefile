# Makefile for emulated RISC-V core
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

# ---------- Target code ----------

# Sources
SOURCES = package.lisp utils.lisp straight.isp

# Assembly code
ASM_SOURCES = count.s boundedcount.s


# ---------- Tools ----------

# Tools
RM = rm -fr


# ---------- Derived filenames ----------

ASM_STEMS = $(foreach fn,$(ASM_SOURCES), $(shell basename $(fn) .s))
ASM_GENERATED = $(foreach stem,$(ASM_STEMS),$(stem).o $(stem).hex)
ASM_OBJECTS = $(foreach stem,$(ASM_STEMS),$(stem).o)
ASM_HEXES = $(foreach stem,$(ASM_STEMS),$(stem).hex)


# ---------- Top-level targets ----------

# Build the assembler test files
asm: $(ASM_HEXES) $(CONFIG)


# Clean-up the build directory
clean:
	$(RM) $(ASM_GENERATED)


# Print a help message
help:
	@make usage


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
	$(OBJDUMP) -h $*.o | \
	$(GREP) .text |\
	$(SED) -e 's|\s\+|\t|g' -e 's|^\s\+||g'| \
	$(AWK) '{print "$(DD) if=$*.o bs=1 count=$$((0x"$$3")) skip=$$((0x"$$6")) status=none";}' | \
	$(BASH) | \
	$(OD) -Anone -tx4 >$*.hex


# ----- Usage -----

define HELP_MESSAGE
Available targets:
   make asm          make the target
   make clean        clean-up the build directory

endef
export HELP_MESSAGE

usage:
	@echo "$$HELP_MESSAGE"
