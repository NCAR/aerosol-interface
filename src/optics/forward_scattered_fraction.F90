! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The ai_optics_forward_scattered_fraction module

!> The optics_forward_scattered_fraction_t type and related functions
module ai_optics_forward_scattered_fraction

  use ai_optics,                       only : optics_t

  implicit none
  private

  public :: optics_forward_scattered_fraction_t

  !> \todo is "forward scattered fraction" the correct name/units for the
  !!       shortwave optical property returned to radiation?
  type, extends(optics_t) :: optics_forward_scattered_fraction_t
  contains
    procedure :: name => property_name
    procedure :: units
    final :: finalize
  end type optics_forward_scattered_fraction_t

  interface optics_forward_scattered_fraction_t
    procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of optics_forward_scattered_fraction_t object
  function constructor( native_grid, output_grid, accessor,                   &
      interpolation_strategy ) result( new_optics )

    use ai_accessor,                   only : accessor_t
    use ai_wavelength_grid,            only : wavelength_grid_t
    use musica_interpolator,           only : interpolation_strategy_i

    type(optics_forward_scattered_fraction_t), pointer :: new_optics
    type(wavelength_grid_t),           intent(in) :: native_grid
    type(wavelength_grid_t),           intent(in) :: output_grid
    class(accessor_t),       optional, intent(in) :: accessor
    procedure(interpolation_strategy_i), optional :: interpolation_strategy

    allocate( new_optics )
    call new_optics%private_constructor( native_grid, output_grid,            &
                                         accessor, interpolation_strategy )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the name of the optical property
  type(string_t) function property_name( this )

    use musica_string,                 only : string_t

    class(optics_forward_scattered_fraction_t), intent(in) :: this

    property_name = "forward scattered fraction"

  end function property_name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the units of the optical property
  type(string_t) function units( this )

    use musica_string,                 only : string_t

    class(optics_forward_scattered_fraction_t), intent(in) :: this

    units = "unitless"

  end function units

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize the optics object
  subroutine finalize( this )

    type(optics_forward_scattered_fraction_t), intent(inout) :: this

    call this%private_finalize( )

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module ai_optics_forward_scattered_fraction
