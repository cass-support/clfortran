# -----------------------------------------------------------------------------
# CLFORTRAN - OpenCL bindings module for Fortran.
#
# This is the main module file and contains all OpenCL API definitions to be
# invoked from Fortran programs.
#
# -----------------------------------------------------------------------------
#
# Copyright (C) 2013 Company for Advanced Supercomputing Solutions LTD
# Bosmat 2a St.
# Shoham
# Israel 60850
# http://www.cass-hpc.com
#
# Author: Mordechai Butrashvily <support@cass-hpc.com>
#
# -----------------------------------------------------------------------------
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# -----------------------------------------------------------------------------

# ----- Compiler -----
FC = gfortran
#FC = ifort

# OpenCL library folder (Modify for local path).
CL_LIB = /usr/lib64/catalyst

# ----- Build targets -----
examples: clfortran
	$(FC) examples/query_platforms_devices.f90 -L$(CL_LIB) -lOpenCL -o query_platforms_devices
	$(FC) examples/create_device_context.f90 -L$(CL_LIB) -lOpenCL -o create_device_context
	$(FC) examples/basic_device_io.f90 -L$(CL_LIB) -lOpenCL -o basic_device_io

clfortran: clfortran.f90
	$(FC) -c clfortran.f90

clean:
	rm -f query_platforms_devices create_device_context
	rm -f *.o *.mod
