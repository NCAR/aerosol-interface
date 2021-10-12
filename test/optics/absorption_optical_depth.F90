! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the ai_optic_absorption_optical_depth  module

!> Test module for the optics_absorption_optical_depth_t type and related functions
program test_optics_absorption_optical_depth

  use ai_accessor,                     only : accessor_t
  use ai_optics,                       only : optics_t
  use ai_optics_absorption_optical_depth
  use ai_wavelength_grid,              only : wavelength_grid_t
  use musica_assert,                   only : assert, die
  use musica_constants,                only : dk => musica_dk

  implicit none

  class(optics_t),   pointer :: foo
  class(accessor_t), pointer :: optics_accessor
  type(wavelength_grid_t)    :: from_grid, to_grid
  real(kind=dk)              :: values(3)
  real(kind=dk)              :: from_lower(3) = (/  0.0_dk, 20.0_dk, 200.0_dk /)
  real(kind=dk)              :: from_upper(3) = (/ 10.0_dk, 50.0_dk, 300.0_dk /)
  real(kind=dk)              :: to_lower(3)   = (/  5.0_dk, 20.0_dk, 200.0_dk /)
  real(kind=dk)              :: to_upper(3)   = (/ 20.0_dk, 50.0_dk, 400.0_dk /)

  from_grid = wavelength_grid_t( from_lower, from_upper )
  to_grid   = wavelength_grid_t(   to_lower,   to_upper )

  ! standard test, default interpolation
  foo => optics_absorption_optical_depth_t( from_grid, to_grid )
  call assert( 262808423, foo%native_grid( ) .eq. from_grid )
  call assert( 992651518, foo%output_grid( ) .eq.   to_grid )
  call foo%accessor( optics_accessor )
  call assert( 822494614, .not. associated( optics_accessor ) )
  call foo%set_values( (/ 1.0_dk, 20.0_dk, 4.0_dk /) )
  call foo%get_values( values )
  call assert( 652337710, values(1) .eq.  0.5_dk )
  call assert( 482180806, values(2) .eq. 20.0_dk )
  call assert( 377032302, values(3) .eq.  4.0_dk )
  call foo%add_values( (/ 12.0_dk, 10.0_dk, 30.0_dk /) )
  call foo%get_values( values )
  call assert( 824400148, values(1) .eq.  6.5_dk )
  call assert( 371767995, values(2) .eq. 30.0_dk )
  call assert( 201611091, values(3) .eq. 34.0_dk )
  call foo%reset_values( )
  call foo%get_values( values )
  call assert( 931454186, values(1) .eq. 0.0_dk )
  call assert( 478822033, values(2) .eq. 0.0_dk )
  call assert( 373673529, values(3) .eq. 0.0_dk )
  call assert( 821041375, foo%name( )  .eq. "absorption optical depth" )
  call assert( 368409222, foo%units( ) .eq. "unitless" )

end program test_optics_absorption_optical_depth
