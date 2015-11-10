! -----------------------------------------------------------------------------
! CLFORTRAN - OpenCL bindings module for Fortran.
!
! This is an example file that demonstrates using CLFORTRAN to perform basic 
! device I/O with OpenCL API.
!
! Last Update: 18 March, 2014
!
! -----------------------------------------------------------------------------
!
! Copyright (C) 2013-2014 Company for Advanced Supercomputing Solutions LTD
! Bosmat 2a St.
! Shoham
! Israel 60850
! http://www.cass-hpc.com
!
! Author: Mordechai Butrashvily <support@cass-hpc.com>
!
! -----------------------------------------------------------------------------
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
! -----------------------------------------------------------------------------

! Main entry point.
!
! Queries available platforms, chooses the first and same for its devices.
! After selection is made, a context is created with command queue.
! Next, a Fortran array is copied back & forth and compared for validity.
program create_device_context
    use clfortran
    use ISO_C_BINDING
    implicit none

    ! Variable definitions for OpenCL API and in general.
    integer(c_int32_t) :: err
    integer(c_size_t) :: zero_size = 0
    integer(c_size_t) :: temp_size
    integer(c_int) :: num_platforms
    integer(c_int) :: num_devices
    integer(c_intptr_t), allocatable, target :: platform_ids(:)
    integer(c_intptr_t), allocatable, target :: device_ids(:)

    integer(c_intptr_t), target :: ctx_props(3)
    integer(c_intptr_t), target :: context
    integer(c_int64_t) :: cmd_queue_props
    integer(c_intptr_t), target :: cmd_queue

    integer(c_intptr_t), target :: d_buffer
    integer(c_size_t) :: d_buffer_offset
    real(c_float), allocatable, target :: array1(:, :)
    real(c_float), allocatable, target :: array2(:, :)
    integer(c_size_t) :: array_size_x
    integer(c_size_t) :: array_size_y

    print '(A)', 'OpenCL sample for creating context and queue for GPU device'

    ! Get the number of platforms, prior to allocating an array.
    err = clGetPlatformIDs(0, C_NULL_PTR, num_platforms)
    if (err /= CL_SUCCESS) then
        print *, 'Error quering platforms: ', err
        call exit(1)
    end if

    if (num_platforms == 0) then
        print *, 'No platforms found'
        call exit(0)
    end if

    ! Allocate an array to hold platform handles.
    allocate(platform_ids(num_platforms))

    ! Get platforms IDs.
    err = clGetPlatformIDs(num_platforms, C_LOC(platform_ids), num_platforms)
    if (err /= CL_SUCCESS) then
        print *, 'Error getting platforms: ', err
        call exit(1)
    end if

    !
    ! Check number of devices for first platform.
    !
    err = clGetDeviceIDs(platform_ids(1), CL_DEVICE_TYPE_GPU, 0, C_NULL_PTR, &
            num_devices)
    if (err /= CL_SUCCESS) then
        print *, 'Error quering devices: ', err
        call exit(1)
    end if

    if (num_devices == 0) then
        print *, 'No GPU devices found'
        call exit(0)
    end if

    ! Allocate an array to hold device handles.
    allocate(device_ids(num_devices))

    ! Get device IDs.
    err = clGetDeviceIDs(platform_ids(1), CL_DEVICE_TYPE_GPU, num_devices, &
            C_LOC(device_ids), num_devices)
    if (err /= CL_SUCCESS) then
        print *, 'Error gettings devices: ', err
        call exit(1)
    end if

    !
    ! Create a context and a command queue.
    !

    ! Context.
    num_devices = 1
    ctx_props(1) = CL_CONTEXT_PLATFORM
    ctx_props(2) = platform_ids(1)
    ctx_props(3) = 0

    context = clCreateContext(C_LOC(ctx_props), num_devices, &
                C_LOC(device_ids), C_NULL_FUNPTR, C_NULL_PTR, err)

    if (err /= CL_SUCCESS) then
        print *, 'Error creating context: ', err
        call exit(1)
    end if

    ! Command queue.
    cmd_queue_props = 0
    cmd_queue = clCreateCommandQueue(context, device_ids(1), cmd_queue_props, err)

    if (err /= CL_SUCCESS) then
        print *, 'Error creating command queue: ', err
        call exit(1)
    end if

    print '(A)', 'Successfuly create OpenCL context and queue for GPU device'

    !
    ! Start OpenCL logic here.
    !

    ! Allocate arrays of size 256x256 (float, 32 bit).
    array_size_x = 256
    array_size_y = 256
    allocate(array1(array_size_x, array_size_y))
    allocate(array2(array_size_x, array_size_y))

    ! Initialize first array with ones.
    array1(:, :) = 1.0

    ! Allocate device buffer.
    d_buffer = clCreateBuffer(context, CL_MEM_READ_WRITE, array_size_x * &
        array_size_y * c_float, C_NULL_PTR, err)

    if (err /= CL_SUCCESS) then
        print *, 'Error allocating device buffer: ', err
        call exit(1)
    end if

    print '(A,I10,A)', 'Successfuly allocated GPU device buffer, length: ',&
        array_size_x * array_size_y * c_float, ' bytes'

    ! Copy CPU data to device buffer.
    d_buffer_offset = 0
    err = clEnqueueWriteBuffer(cmd_queue, d_buffer, CL_TRUE, d_buffer_offset, &
        array_size_x * array_size_y * c_float, C_LOC(array1), 0, C_NULL_PTR, &
        C_NULL_PTR)

    if (err /= CL_SUCCESS) then
        print *, 'Error copying to device buffer: ', err
        call exit(1)
    end if

    ! Copy device buffer back to different CPU array.
    err = clEnqueueReadBuffer(cmd_queue, d_buffer, CL_TRUE, d_buffer_offset, &
        array_size_x * array_size_y * c_float, C_LOC(array2), 0, C_NULL_PTR, &
        C_NULL_PTR)

    if (err /= CL_SUCCESS) then
        print *, 'Error copying from device buffer: ', err
        call exit(1)
    end if

    ! Compare values.
    if (any(array1 /= array2)) then
        print *, 'Error, arrays are not equal!'
        call exit(1)
    end if

    print '(A)', 'Successfuly copied data to OpenCL device and back!'

    !
    ! End of OpenCL logic.
    !

    !
    ! Release allocated handles.
    !
    err = clReleaseMemObject(d_buffer)
    err = clReleaseCommandQueue(cmd_queue)
    err = clReleaseContext(context)
end program create_device_context

