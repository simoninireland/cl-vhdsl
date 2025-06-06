# Makefile for the RTLisp UART demo
#
# Copyright (C) 2023--2025 Simon Dobson
#
# This file is part of verilisp, a very Lisp approach to hardware synthesis
#
# verilisp is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# verilisp is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

include ../icestick.mk


# ---------- Target code ----------

# Bitstream
TARGET = uart.bin

# Sources
SOURCES = package.lisp uart.lisp io.lisp
TOPMODULE = io
CONFIG = uart.pcf
OBJECT = uart.v


# ---------- Tools ----------

# Tools
RM = rm -fr

# ---------- Top-level targets ----------

# Build the target bitstream
$(TARGET): $(SOURCES) $(CONFIG)


# Upload the target bitstream to the device
upload: $(TARGET)
	$(PROGRAM) $(PROGRAM_OPTS) $<


# Clean-up the build directory
clean:
	$(RM) $(GENERATED)


# Print a help message
help:
	@make usage


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
