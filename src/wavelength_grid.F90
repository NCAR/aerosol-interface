! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The ai_wavelength_grid module

!> The wavelength_grid_t type and related functions
module ai_wavelength_grid

  use musica_grid,                     only : grid_t

  implicit none
  private

  public :: wavelength_grid_t

  !> @name Constants for building wavelength grids
  !! @{
  integer, parameter, public :: kWavelength = 1
  integer, parameter, public :: kWavenumber = 2
  integer, parameter, public :: kCentimeter = 10
  integer, parameter, public :: kMeter      = 11
  integer, parameter, public :: kNanometer  = 12
  !> @}

  !> A wavelength grid
  type, extends(grid_t) :: wavelength_grid_t
  contains
  end type wavelength_grid_t

  interface wavelength_grid_t
    module procedure :: constructor_bounds
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of wavelength_grid_t objects from lower and upper bin bounds
  function constructor_bounds( lower_bounds, upper_bounds, bounds_in,         &
      base_unit ) result( new_grid )

    use musica_assert,                 only : assert_msg, die_msg
    use musica_constants,              only : musica_dk
    use musica_string,                 only : string_t

    type(wavelength_grid_t)        :: new_grid
    !> Lower wavelength bounds for each bin
    real(kind=musica_dk), intent(in) :: lower_bounds(:)
    !> Upper wavelength bounds for each bin
    real(kind=musica_dk), intent(in) :: upper_bounds(:)
    !> How bounds are specified (kWavelength or kWavenumber)
    !! defaults to wavelength
    integer, optional,  intent(in) :: bounds_in
    !> Base length units (kCentimeter, kMeter, kNanometer)
    !! defaults to [m]
    integer, optional,  intent(in) :: base_unit

    real(kind=musica_dk), allocatable :: adjusted_lower_bounds(:)
    real(kind=musica_dk), allocatable :: adjusted_upper_bounds(:)
    real(kind=musica_dk) :: scale_factor
    integer              :: bound_type
    type(string_t)       :: units

    scale_factor = 1.0
    if( present( base_unit ) ) then
      select case( base_unit )
      case( kCentimeter )
        scale_factor = 1.0e-2_musica_dk
      case( kMeter )
        scale_factor = 1.0_musica_dk
      case( kNanometer )
        scale_factor = 1.0e-9_musica_dk
      case default
        call die_msg( 531934235, "Invalid units for wavelength grid" )
      end select
    end if

    allocate( adjusted_lower_bounds( size( lower_bounds ) ) )
    allocate( adjusted_upper_bounds( size( upper_bounds ) ) )
    bound_type = kWavelength
    if( present( bounds_in ) ) bound_type = bounds_in
    select case( bound_type )
    case( kWavelength )
      adjusted_lower_bounds = lower_bounds(:) * scale_factor
      adjusted_upper_bounds = upper_bounds(:) * scale_factor
    case( kWavenumber )
      adjusted_lower_bounds = 1.0_musica_dk/( lower_bounds(:) * scale_factor )
      adjusted_upper_bounds = 1.0_musica_dk/( upper_bounds(:) * scale_factor )
    case default
      call die_msg( 135687402, "Invalid wavelength grid bounds" )
    end select
    units = "m"
    call new_grid%private_constructor_bounds( adjusted_lower_bounds,          &
                                              adjusted_upper_bounds, units )

  end function constructor_bounds

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module ai_wavelength_grid
