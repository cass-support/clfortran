---------------
1. Introduction
---------------
CLFORTRAN is a Fortran interface to OpenCL, written in pure Fortran and 
compatible with OpenCL 1.2.

The API CLFORTRAN exposes is identical to what developers will find with the C
interface (read below for more specific details).
For any question about parameters meaning, it is better to consult the official
OpenCL documentation available at http://www.khronos.org.

-------------------
2. Compiler Support
-------------------
To use CLFORTRAN compiler should support Fortran 2002-2003 extensions, 
especially ISO_C_BINDING.

Most current compilers have support for this feature, including GNU, Intel and 
IBM.
CLFORTRAN was compiled and tested with the following compilers:
    - gfortran (4.8.2)
    - ifort (13.1.1)
But previous versions conforming to Fortran 2003 should work as well.

-----------------------
3. OpenCL Compatibility
-----------------------
CLFORTRAN was written to provide the full API exposed by C OpenCL.

However, since Fortran is case insensitive compared with C, there is a naming 
conflict defining constants and structures regarding the image support.

In order to overcome this issue, the CL_IMAGE_* constants were renamed to begin
with the following prefix: CL_IMAGE_INFO_*.
Otherwise, a conflict would occur between CL_IMAGE_FORMAT and the 
cl_image_format structure.

This is the only difference from the standard.

-------------------
4. Examples & Build
-------------------

Example files are provided to demonstrate the use of CLFORTRAN and writing 
Fortran programs that utilize OpenCL.

It is advised to consult the makefile for an example of building applications
with CLFORTRAN.

--------
5. Legal
--------
CLFORTRAN is licensed under LGPL, attached is a matching license.

----------------------
6. Contact Information
----------------------
For any other issue, contact us at: support@cass-hpc.com

Copyright (C) 2013-2014 Company for Advanced Supercomputing Solutions LTD

Company for Advanced Supercomputing Solutions LTD
Bosmat 2a St.
Shoham
Israel 60850
http://www.cass-hpc.com

Author: Mordechai Butrashvily <support@cass-hpc.com>
