# Docker file for Lattice icestorm tools container
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
ARG BASE_IMAGE="alpine:3.17"

FROM $BASE_IMAGE
LABEL description="Icestorm tools for Lattice FPGA development"
LABEL maintainer="Simon Dobson <simoninireland@gmail.com>"

USER root

# Add edge repositories that contain the FGPA packages
RUN <<EOF
for branch in "main" "community" "testing"; do
    echo "https://dl-cdn.alpinelinux.org/alpine/edge/$branch" >>/etc/apk/repositories
done
apk update
apk upgrade
EOF

# Install the tooling
RUN apk add yosys nextpnr icestorm

# Create a working directory
RUN mkdir /work
WORKDIR /work
