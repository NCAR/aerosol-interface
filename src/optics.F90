! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The ai_optics module

!> The optics_t type and related functions
module ai_optics

  use ai_accessor,                     only : accessor_t
  use ai_wavelength_grid,              only : wavelength_grid_t
  use musica_constants,                only : musica_dk
  use musica_interpolator,             only : interpolator_t

  implicit none
  private

  public :: optics_t, optics_ptr

  !> An optical property
  type, abstract :: optics_t
    private
    !> Wavelength grid on which the optical property is calculated
    type(wavelength_grid_t) :: native_grid_
    !> Wavelength grid used by the owner of the \c optics_t object
    type(wavelength_grid_t) :: output_grid_
    !> Flag indicating whether interpolation between grids is needed
    logical :: do_interpolation_ = .false.
    !> Interpolator from native grid to output grid
    type(interpolator_t) :: interpolator_
    !> Optical property values on the native grid (wavelength bin)
    real(kind=musica_dk), allocatable :: values_(:)
    !> Aerosol-model-specific information for calculating the optical
    !! property
    class(accessor_t), pointer :: accessor_ => null( )
  contains
    procedure(property_name), deferred :: name
    procedure(units),         deferred :: units
    procedure :: native_grid
    procedure :: output_grid
    procedure :: accessor
    procedure :: reset_values
    procedure :: add_values
    procedure :: set_values
    procedure :: get_values
    !> Private constructor - should only be called by extending types
    procedure :: private_constructor
  end type optics_t

  !> Pointer to optical property objects
  type :: optics_ptr
    class(optics_t), pointer :: ptr_ => null( )
  contains
    final :: optics_ptr_finalize
  end type optics_ptr

interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the name of the optical property
  type(string_t) function property_name( this )
    use musica_string,                 only : string_t
    import optics_t
    class(optics_t), intent(in) :: this
  end function property_name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the units for the optical property
  type(string_t) function units( this )
    use musica_string,                 only : string_t
    import optics_t
    class(optics_t), intent(in) :: this
  end function units

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the grid on which the optical property is calculated
  type(wavelength_grid_t) function native_grid( this )

    class(optics_t), intent(in) :: this

    native_grid = this%native_grid_

  end function native_grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the grid on which the optical property is output
  type(wavelength_grid_t) function output_grid( this )

    class(optics_t), intent(in) :: this

    output_grid = this%output_grid_

  end function output_grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns a pointer to the aerosol-model-specific accessor information
  function accessor( this )

    class(accessor_t), pointer    :: accessor
    class(optics_t),   intent(in) :: this

    accessor => this%accessor_

  end function accessor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Resets the values of the optical property
  subroutine reset_values( this )

    class(optics_t), intent(inout) :: this

    this%values_(:) = 0.0

  end subroutine reset_values

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds values to the existing set of values for the optical property
  subroutine add_values( this, values )

    class(optics_t),      intent(inout) :: this
    real(kind=musica_dk), intent(in)    :: values(:)

    this%values_(:) = this%values_(:) + values(:)

  end subroutine add_values

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Sets the values of the optical property on the native grid
  subroutine set_values( this, values )

    class(optics_t),      intent(inout) :: this
    real(kind=musica_dk), intent(in)    :: values(:)

    this%values_(:) = values(:)

  end subroutine set_values

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the values of the optical property on the output grid
  subroutine get_values( this, values )

    class(optics_t),      intent(in)  :: this
    real(kind=musica_dk), intent(out) :: values(:)

    if( this%do_interpolation_ ) then
      call this%interpolator_%interpolate( this%values_, values )
    else
      values(:) = this%values_(:)
    end if

  end subroutine get_values

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> General constructor of optics_t objects
  subroutine private_constructor( this, native_grid, output_grid,             &
      accessor, interpolation_strategy )

    use musica_interpolator,           only : interpolation_strategy_i
    use musica_interpolator_linear_1D, only : linear => strategy

    class(optics_t),                     intent(inout) :: this
    type(wavelength_grid_t),             intent(in)    :: native_grid
    type(wavelength_grid_t),             intent(in)    :: output_grid
    class(accessor_t),       optional,   intent(in)    :: accessor
    procedure(interpolation_strategy_i), optional :: interpolation_strategy

    this%native_grid_ = native_grid
    this%output_grid_ = output_grid
    allocate( this%values_( native_grid%number_of_sections( ) ) )
    this%do_interpolation_ = native_grid .ne. output_grid
    if( present( accessor ) ) then
      allocate( this%accessor_, source = accessor )
    end if
    if( present( interpolation_strategy ) ) then
      this%interpolator_ =                                                    &
          interpolator_t( interpolation_strategy, native_grid, output_grid )
    else
      this%interpolator_ = interpolator_t( linear, native_grid, output_grid )
    end if

  end subroutine private_constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalization for optics_t pointer
  subroutine optics_ptr_finalize( this )

    type(optics_ptr), intent(inout) :: this

    if( associated( this%ptr_ ) ) then
      deallocate( this%ptr_ )
    end if

  end subroutine optics_ptr_finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module ai_optics
