! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the ai_wavelength_grid module

!> Test module for the wavelength_grid_t type and related functions
program test_wavelength_grid

  implicit none

  call test_wavelength_grid_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Tests for the wavelength_grid_t type
  subroutine test_wavelength_grid_t( )

    use ai_wavelength_grid
    use musica_constants,              only : dk => musica_dk
    use musica_assert,                 only : assert, almost_equal
    use musica_grid,                   only : grid_iterator_t

    type(wavelength_grid_t) :: base_grid, grid
    class(grid_iterator_t), pointer :: base_iter, iter
    integer :: i_section
    integer, parameter :: kNumberOfSections = 3
    real(kind=dk) :: lower_bounds(kNumberOfSections) =                        &
                       (/ 10.0_dk, 100.0_dk, 250.0_dk /)
    real(kind=dk) :: upper_bounds(kNumberOfSections) =                        &
                       (/ 50.0_dk, 250.0_dk, 400.0_dk /)

    ! default grid (in wavelengths [m])
    grid = wavelength_grid_t( lower_bounds, upper_bounds )
    iter => grid%iterator( )
    i_section = 0
    do while( iter%next( ) )
      i_section = i_section + 1
      call assert( 365591108, i_section .le. kNumberOfSections )
      call assert( 753667156, grid%lower_bound( iter ) .eq.                   &
                              lower_bounds( i_section ) )
      call assert( 415258882, grid%upper_bound( iter ) .eq.                   &
                              upper_bounds( i_section ) )
    end do
    call assert( 181999112, i_section .eq. kNumberOfSections )
    deallocate( iter )

    base_grid = grid
    base_iter => base_grid%iterator( )

    ! grid in wavelengths [m]
    grid = wavelength_grid_t( lower_bounds, upper_bounds,                     &
                              bounds_in = kWavelength,                        &
                              base_unit = kMeter )
    call assert( 284980556, grid .eq. base_grid )

    ! grid in wavelengths [cm]
    grid = wavelength_grid_t( lower_bounds, upper_bounds,                     &
                              bounds_in = kWavelength,                        &
                              base_unit = kCentimeter )
    iter => grid%iterator( )
    call base_iter%reset( )
    do while( iter%next( ) )
      call assert( 601782953, base_iter%next( ) )
      call assert( 151056334, almost_equal(                                   &
                                  grid%lower_bound( iter ) * 100.0_dk,        &
                                  base_grid%lower_bound( base_iter ) ) )
      call assert( 363711171, almost_equal(                                   &
                                  grid%upper_bound( iter ) * 100.0_dk,        &
                                  base_grid%upper_bound( base_iter ) ) )
    end do
    call assert( 130451401, .not. base_iter%next( ) )
    deallocate( iter )

    ! grid in wavelengths [nm]
    grid = wavelength_grid_t( lower_bounds, upper_bounds,                     &
                              bounds_in = kWavelength,                        &
                              base_unit = kNanometer )
    iter => grid%iterator( )
    call base_iter%reset( )
    do while( iter%next( ) )
      call assert( 765535031, base_iter%next( ) )
      call assert( 312902878, almost_equal(                                   &
                                  grid%lower_bound( iter ) * 1.0e9_dk,        &
                                  base_grid%lower_bound( base_iter ) ) )
      call assert( 142745974, almost_equal(                                   &
                                  grid%upper_bound( iter ) * 1.0e9_dk,        &
                                  base_grid%upper_bound( base_iter ) ) )
    end do
    call assert( 709602006, .not. base_iter%next( ) )
    deallocate( iter )

    ! grid in wavenumber [m]
    grid = wavelength_grid_t( lower_bounds, upper_bounds,                     &
                              bounds_in = kWavenumber,                        &
                              base_unit = kMeter )
    iter => grid%iterator( )
    call base_iter%reset( )
    do while( iter%next( ) )
      call assert( 821920351, base_iter%next( ) )
      call assert( 369288198, almost_equal(                                   &
                                  1.0_dk / grid%lower_bound( iter ),          &
                                  base_grid%lower_bound( base_iter ) ) )
      call assert( 481606543, almost_equal(                                   &
                                  1.0_dk / grid%upper_bound( iter ),          &
                                  base_grid%upper_bound( base_iter ) ) )
    end do
    call assert( 593924888, .not. base_iter%next( ) )
    deallocate( iter )

    ! grid in wavenumber [cm]
    grid = wavelength_grid_t( lower_bounds, upper_bounds,                     &
                              bounds_in = kWavenumber,                        &
                              base_unit = kCentimeter )
    iter => grid%iterator( )
    call base_iter%reset( )
    do while( iter%next( ) )
      call assert( 988718482, base_iter%next( ) )
      call assert( 818561578, almost_equal(                                   &
                             ( 1.0_dk / grid%lower_bound( iter ) ) * 100.0_dk,&
                             base_grid%lower_bound( base_iter ) ) )
      call assert( 365929425, almost_equal(                                   &
                             ( 1.0_dk / grid%upper_bound( iter ) ) * 100.0_dk,&
                             base_grid%upper_bound( base_iter ) ) )
    end do
    call assert( 760723019, .not. base_iter%next( ) )
    deallocate( iter )

    ! grid in wavenumber [nm]
    grid = wavelength_grid_t( lower_bounds, upper_bounds,                     &
                              bounds_in = kWavenumber,                        &
                              base_unit = kNanometer )
    iter => grid%iterator( )
    call base_iter%reset( )
    do while( iter%next( ) )
      call assert( 976736629, base_iter%next( ) )
      call assert( 806579725, almost_equal(                                   &
                             ( 1.0_dk / grid%lower_bound( iter ) ) * 1.0e9_dk,&
                             base_grid%lower_bound( base_iter ) ) )
      call assert( 301373320, almost_equal(                                   &
                             ( 1.0_dk / grid%upper_bound( iter ) ) * 1.0e9_dk,&
                             base_grid%upper_bound( base_iter ) ) )
    end do
    call assert( 696166914, .not. base_iter%next( ) )
    deallocate( iter )

    deallocate( base_iter )

  end subroutine test_wavelength_grid_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_wavelength_grid
