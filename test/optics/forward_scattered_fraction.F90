! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the ai_optic_forward_scattered_fraction  module

!> Test module for the optics_forward_scattered_fraction_t type and related functions
program test_optics_forward_scattered_fraction

  use ai_accessor,                     only : accessor_t
  use ai_optics,                       only : optics_t
  use ai_optics_forward_scattered_fraction
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
  foo => optics_forward_scattered_fraction_t( from_grid, to_grid )
  call assert( 371793592, foo%native_grid( ) .eq. from_grid )
  call assert( 136628288, foo%output_grid( ) .eq.   to_grid )
  call foo%accessor( optics_accessor )
  call assert( 313955033, .not. associated( optics_accessor ) )
  call foo%set_values( (/ 1.0_dk, 20.0_dk, 4.0_dk /) )
  call foo%get_values( values )
  call assert( 426273378, values(1) .eq.  0.5_dk )
  call assert( 873641224, values(2) .eq. 20.0_dk )
  call assert( 703484320, values(3) .eq.  4.0_dk )
  call foo%add_values( (/ 12.0_dk, 10.0_dk, 30.0_dk /) )
  call foo%get_values( values )
  call assert( 533327416, values(1) .eq.  6.5_dk )
  call assert( 980695262, values(2) .eq. 30.0_dk )
  call assert( 810538358, values(3) .eq. 34.0_dk )
  call foo%reset_values( )
  call foo%get_values( values )
  call assert( 922856703, values(1) .eq. 0.0_dk )
  call assert( 752699799, values(2) .eq. 0.0_dk )
  call assert( 300067646, values(3) .eq. 0.0_dk )
  call assert( 129910742, foo%name( )  .eq. "forward scattered fraction" )
  call assert( 924762237, foo%units( ) .eq. "unitless" )

end program test_optics_forward_scattered_fraction
