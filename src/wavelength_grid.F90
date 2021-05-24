! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The ai_wavelength_grid module

!> The wavelength_grid_t type and related functions
module ai_wavelength_grid

  implicit none
  private

  public :: wavelength_grid_t

  !> A wavelength range
  type wavelength_range_t
    private
    real(kind=kDouble) :: lower_bound__m_
    real(kind=kDouble) :: upper_bound__m_
  end type wavelength_range_t

  !> A wavelength grid
  type wavelength_grid_t
    private
    type(wavelength_grid_t), allocatable :: bins_(:)
  contains
    procedure :: number_of_bins
    procedure :: lower_bounds__m
    procedure :: upper_bounds__m
  end type wavelength_grid_t

  interface wavelength_grid_t
    module procedure :: constructor_bounds
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of wavelength_grid_t objects from lower and upper bin bounds
  function constructor_bounds( lower_bounds, upper_bounds ) result( new_obj )

    type(wavelength_grid_t)        :: new_obj
    !> Lower wavelength bounds for each bin [m]
    real(kind=kDouble), intent(in) :: lower_bounds(:)
    !> Upper wavelength bounds for each bin [m]
    real(kind=kDouble), intent(in) :: upper_bounds(:)

    !> \todo How to add checks for function argument validity?
    allocate( new_obj%bins_( size( lower_bounds ) ) )
    new_obj%bins_(:)%lower_bound__m_ = lower_bounds(:)
    new_obj%bins_(:)%upper_bound__m_ = upper_bounds(:)

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

    real(kiind=kDouble), allocatable     :: lower_bounds__m
    class(wavelength_grid_t), intent(in) :: this

    allocate( lower_bounds__m( size( this%bins_ ) ) )
    lower_bounds__m(:) = this%bins_(:)%lower_bound__m_

  end function lower_bounds__m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the upper bounds for each wavelength grid [m]
  function upper_bounds__m( this )

    real(kind=kDouble), allocatable :: upper_bounds__m
    class(wavelength_grid_t), intent(in) :: this

    allocate( upper_bounds__m( size( this%bins_ ) ) )
    upper_bounds__m(:) = this%bins_(:)%upper_bound__m_

  end function upper_bounds__m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module ai_wavelength_grid
