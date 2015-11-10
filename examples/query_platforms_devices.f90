! -----------------------------------------------------------------------------
! CLFORTRAN - OpenCL bindings module for Fortran.
!
! This is an example file that demonstrates using CLFORTRAN to query available
! platforms and devices in the system, using the OpenCL API directly.
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
! Checks the number of available platforms, and calls a specific query
! subroutine to print additional details on each platform (and attached
! devices).
program query_platforms_devices
    use clfortran
    use ISO_C_BINDING
    implicit none

    ! Variable definitions for OpenCL API and in general.
    integer(c_int32_t) :: err
    integer(c_size_t) :: zero_size = 0
    integer(c_size_t) :: temp_size
    integer(c_int) :: num_platforms
    integer(c_int) :: i
    integer(c_intptr_t), allocatable, target :: platform_ids(:)

    ! Get the number of platforms, prior to allocating an array.
    err = clGetPlatformIDs(0, C_NULL_PTR, num_platforms)
    if (err /= CL_SUCCESS) then
        print *, 'Error quering platforms: ', err
        call exit(1)
    end if

    print '(A, I2)', 'Num Platforms: ', num_platforms

    ! Allocate an array to hold platform handles.
    allocate(platform_ids(num_platforms))

    ! Get platforms IDs.
    err = clGetPlatformIDs(num_platforms, C_LOC(platform_ids), num_platforms)
    if (err /= CL_SUCCESS) then
        print *, 'Error quering platforms: ', err
        call exit(1)
    end if

    !
    ! Header for platform details and devices.
    !
    print *
    print '(A)', 'Platform Details:'
    print '(A)', '-----------------'

    ! Loop over platforms and print information.
    do i = 1, num_platforms
        ! Iterate over platforms and get number of devices.
        print '(A, I2)', 'Platform: ', i
        print *

        ! Query platform information.
        call query_platform_info(platform_ids(i))

        ! Print separator between platforms, half size.
        print '(A)', '--------'
    end do
end program query_platforms_devices

! Queries OpenCL platform for additional details (see OpenCL manual for more
! options) and included devices.
!
! Parameters:
! @platform_id - OpenCL handle of platform to query.
!
subroutine query_platform_info(platform_id)
    use clfortran
    use ISO_C_BINDING
    implicit none

    ! Input variable.
    integer(c_intptr_t), intent(in)         :: platform_id

    ! Helper variables to work with OpenCL API.
    integer(c_int32_t) :: err
    integer(c_size_t) :: zero_size = 0
    integer(c_size_t) :: temp_size
    ! For quering devices.
    integer(c_int64_t) :: device_type
    integer(c_int32_t) :: num_devices
    integer(c_int) :: i
    integer(c_intptr_t), allocatable, target :: device_ids(:)

    ! String arrays for holding platform details.
    character, allocatable, target :: platform_profile(:)
    character, allocatable, target :: platform_version(:)
    character, allocatable, target :: platform_name(:)
    character, allocatable, target :: platform_vendor(:)
    character, allocatable, target :: platform_extensions(:)

    ! String array for holding device name.
    character, allocatable, target :: device_name(:)
    ! Maximum compute units for device.
    integer(c_int32_t), target :: device_cu

    ! Profile.
    err = clGetPlatformInfo(platform_id, CL_PLATFORM_PROFILE, zero_size, C_NULL_PTR, temp_size)
    allocate(platform_profile(temp_size))
    err = clGetPlatformInfo(platform_id, CL_PLATFORM_PROFILE, temp_size, C_LOC(platform_profile), temp_size)
    print *, 'Profile: ', platform_profile
    deallocate(platform_profile)

    ! Version.
    err = clGetPlatformInfo(platform_id, CL_PLATFORM_VERSION, zero_size, C_NULL_PTR, temp_size)
    allocate(platform_version(temp_size))
    err = clGetPlatformInfo(platform_id, CL_PLATFORM_VERSION, temp_size, C_LOC(platform_version), temp_size)
    print *, 'Version: ', platform_version
    deallocate(platform_version)

    ! Name.
    err = clGetPlatformInfo(platform_id, CL_PLATFORM_NAME, zero_size, C_NULL_PTR, temp_size)
    allocate(platform_name(temp_size))
    err = clGetPlatformInfo(platform_id, CL_PLATFORM_NAME, temp_size, C_LOC(platform_name), temp_size)
    print *, 'Name: ', platform_name
    deallocate(platform_name)

    ! Vendor.
    err = clGetPlatformInfo(platform_id, CL_PLATFORM_VENDOR, zero_size, C_NULL_PTR, temp_size)
    allocate(platform_vendor(temp_size))
    err = clGetPlatformInfo(platform_id, CL_PLATFORM_VENDOR, temp_size, C_LOC(platform_vendor), temp_size)
    print *, 'Vendor: ', platform_vendor
    deallocate(platform_vendor)

    ! Extensions.
    err = clGetPlatformInfo(platform_id, CL_PLATFORM_EXTENSIONS, zero_size, C_NULL_PTR, temp_size)
    allocate(platform_extensions(temp_size))
    err = clGetPlatformInfo(platform_id, CL_PLATFORM_EXTENSIONS, temp_size, C_LOC(platform_extensions), temp_size)
    print *, 'Extensions: ', platform_extensions
    deallocate(platform_extensions)

    !
    ! Print device information for this platform.
    !
    ! Get device count.
    !device_type = CL_DEVICE_TYPE_ALL
    err = clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_ALL, 0, C_NULL_PTR, num_devices)

    if (err /= CL_SUCCESS .or. num_devices < 1) then
        print *, 'No devices found: ', err
        return
    end if

    print *
    print '(A, I2)', 'Num Devices: ', num_devices

    ! Allocate an array to hold device handles.
    allocate(device_ids(num_devices))

    ! Get device IDs.
    err = clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_ALL, num_devices, C_LOC(device_ids), num_devices)
    if (err /= CL_SUCCESS) then
        print *, 'Error quering devices: ', err
        return
    end if

    ! Loop over devices and print information.
    do i = 1, num_devices
        ! Maximum compute units.
        temp_size = 4
        err = clGetDeviceInfo(device_ids(i), CL_DEVICE_MAX_COMPUTE_UNITS, temp_size, C_LOC(device_cu), temp_size)

        ! Name.
        err = clGetDeviceInfo(device_ids(i), CL_DEVICE_NAME, zero_size, C_NULL_PTR, temp_size)
        allocate(device_name(temp_size))
        err = clGetDeviceInfo(device_ids(i), CL_DEVICE_NAME, temp_size, C_LOC(device_name), temp_size)

        ! Print brief device details.
        write (*, '(A,I2,A,I3,A)', advance='no') ' Device (#', i, ', Compute Units: ', device_cu, ') - '
        print *, device_name

        deallocate(device_name)
    end do
end subroutine query_platform_info
