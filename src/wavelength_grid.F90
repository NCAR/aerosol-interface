! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The ai_wavelength_grid module

!> The wavelength_grid_t type and related functions
module ai_wavelength_grid

  use ai_constants,                    only : kDouble

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

  !> A wavelength range
  type wavelength_range_t
    private
    real(kind=kDouble) :: lower_bound__m_
    real(kind=kDouble) :: upper_bound__m_
  contains
    procedure, private :: range_equals_range
    generic :: operator(==) => range_equals_range
    procedure, private :: range_not_equals_range
    generic :: operator(/=) => range_not_equals_range
  end type wavelength_range_t

  !> A wavelength grid
  type wavelength_grid_t
    private
    type(wavelength_range_t), allocatable :: bins_(:)
  contains
    procedure :: number_of_bins
    procedure :: lower_bounds__m
    procedure :: upper_bounds__m
    procedure, private :: grid_equals_grid
    generic :: operator(==) => grid_equals_grid
    procedure, private :: grid_not_equals_grid
    generic :: operator(/=) => grid_not_equals_grid
  end type wavelength_grid_t

  interface wavelength_grid_t
    module procedure :: constructor_bounds
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compares wavelength_rate_t objects for equality
  logical elemental function range_equals_range( a, b ) result( equals )

    class(wavelength_range_t), intent(in) :: a
    type(wavelength_range_t),  intent(in) :: b

    equals = a%lower_bound__m_ .eq. b%lower_bound__m_ .and.                   &
             a%upper_bound__m_ .eq. b%upper_bound__m_

  end function range_equals_range

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compares wavelength_range_t objects for inequality
  logical elemental function range_not_equals_range( a, b )                   &
      result( not_equals )

    class(wavelength_range_t), intent(in) :: a
    type(wavelength_range_t),  intent(in) :: b

    not_equals = .not. a .eq. b

  end function range_not_equals_range

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of wavelength_grid_t objects from lower and upper bin bounds
  function constructor_bounds( lower_bounds, upper_bounds, bounds_in,         &
      base_unit ) result( new_obj )

    use ai_util,                       only : assert_msg, die_msg

    type(wavelength_grid_t)        :: new_obj
    !> Lower wavelength bounds for each bin
    real(kind=kDouble), intent(in) :: lower_bounds(:)
    !> Upper wavelength bounds for each bin
    real(kind=kDouble), intent(in) :: upper_bounds(:)
    !> How bounds are specified (kWavelength or kWavenumber)
    !! defaults to wavelength
    integer, optional,  intent(in) :: bounds_in
    !> Base length units (kCentimeter, kMeter, kNanometer)
    !! defaults to [m]
    integer, optional,  intent(in) :: base_unit

    real(kind=kDouble) :: scale_factor
    integer            :: bound_type

    call assert_msg( 584221364,                                               &
                     size( lower_bounds ) .eq. size( upper_bounds ),          &
                     "Bad wavelength specification" )
    allocate( new_obj%bins_( size( lower_bounds ) ) )

    scale_factor = 1.0
    if( present( base_unit ) ) then
      select case( base_unit )
      case( kCentimeter )
        scale_factor = 1.0e-2
      case( kMeter )
        scale_factor = 1.0
      case( kNanometer )
        scale_factor = 1.0e-9
      case default
        call die_msg( 531934235, "Invalid units for wavelength grid" )
      end select
    end if

    bound_type = kWavelength
    if( present( bounds_in ) ) bound_type = bounds_in
    select case( bound_type )
    case( kWavelength )
      new_obj%bins_(:)%lower_bound__m_ = lower_bounds(:) * scale_factor
      new_obj%bins_(:)%upper_bound__m_ = upper_bounds(:) * scale_factor
    case( kWavenumber )
      new_obj%bins_(:)%lower_bound__m_ = 1.0/lower_bounds(:) * scale_factor
      new_obj%bins_(:)%upper_bound__m_ = 1.0/upper_bounds(:) * scale_factor
    case default
      call die_msg( 135687402, "Invalid wavelength grid bounds" )
    end select

  end function constructor_bounds

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the number of bins in the wavelength grid
  integer elemental function number_of_bins( this )

    class(wavelength_grid_t), intent(in) :: this

    number_of_bins = size( this%bins_ )

  end function number_of_bins

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the lower bounds for each wavelength grid [m]
  function lower_bounds__m( this )

    real(kind=kDouble), allocatable     :: lower_bounds__m(:)
    class(wavelength_grid_t), intent(in) :: this

    allocate( lower_bounds__m( size( this%bins_ ) ) )
    lower_bounds__m(:) = this%bins_(:)%lower_bound__m_

  end function lower_bounds__m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the upper bounds for each wavelength grid [m]
  function upper_bounds__m( this )

    real(kind=kDouble), allocatable :: upper_bounds__m(:)
    class(wavelength_grid_t), intent(in) :: this

    allocate( upper_bounds__m( size( this%bins_ ) ) )
    upper_bounds__m(:) = this%bins_(:)%upper_bound__m_

  end function upper_bounds__m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compares wavelength_grid_t objects for equality
  logical elemental function grid_equals_grid( a, b ) result( equals )

    class(wavelength_grid_t), intent(in) :: a
    type(wavelength_grid_t),  intent(in) :: b

    integer :: i_bin

    equals = allocated( a%bins_ ) .and. allocated( b%bins_ )
    if( .not. equals ) return
    equals = size( a%bins_ ) .eq. size( b%bins_ )
    if( .not. equals ) return
    do i_bin = 1, size( a%bins_ )
      equals = equals .and. a%bins_( i_bin ) .eq. b%bins_( i_bin )
    end do

  end function grid_equals_grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compares wavelength_grid_t objects for inequality
  logical elemental function grid_not_equals_grid( a, b ) result( not_equals )

    class(wavelength_grid_t), intent(in) :: a
    type(wavelength_grid_t),  intent(in) :: b

    not_equals = .not. a .eq. b

  end function grid_not_equals_grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module ai_wavelength_grid
