# ----- Compiler. -----
FC = gfortran
#FC = ifort

# OpenCL library folder (Modify for local path).
CL_LIB = /usr/lib64/catalyst

# Build targets.
examples: clfortran
	$(FC) examples/query_platforms_devices.f90 -L$(CL_LIB) -lOpenCL -o query_platforms_devices

clfortran: clfortran.f90
	$(FC) -c clfortran.f90

clean:
	rm -f query_platforms_devices
	rm -f *.o *.mod
