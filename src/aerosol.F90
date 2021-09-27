! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The ai_aerosol module

!> The aerosol_t type and related functions
module ai_aerosol

  implicit none
  private

  public :: aerosol_t

  !> An abstract aerosol
  !!
  !! Maintains the aerosol state for a configurable number of independent
  !! systems in an arbitrary way and exposes functionality required by
  !! atmospheric models of aerosol models.
  type, abstract :: aerosol_t
    private
  contains
    procedure(new_state),  deferred :: new_state
    procedure(new_optics), deferred :: new_optics
    procedure(shortwave_optics_scalar), private, deferred ::                  &
        shortwave_optics_scalar
    procedure(shortwave_optics_array),  private, deferred ::                  &
        shortwave_optics_array
    generic :: shortwave_optics => shortwave_optics_scalar,                   &
                                   shortwave_optics_array
    procedure(longwave_optics_scalar), private, deferred ::                   &
        longwave_optics_scalar
    procedure(longwave_optics_array),  private, deferred ::                   &
        longwave_optics_array
    generic :: longwave_optics => longwave_optics_scalar,                     &
                                  longwave_optics_array
    procedure(print_state),      deferred :: print_state
  end type aerosol_t

interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates and returns a new aerosol state object for this aerosol
  function new_state( this )
    use ai_aerosol_state,              only : aerosol_state_t
    import aerosol_t
    class(aerosol_state_t), pointer :: new_state
    class(aerosol_t),       intent(in) :: this
  end function new_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates an optics_t object for a given optical property
  !!
  !! The aerosol module should pack any information needed to return the
  !! requested optics on the grid specified in the optics_t object into the
  !! returned optics_t object's accessor member, such that given the optics_t
  !! object and the envrionmental conditions, the requested optics can be
  !! returned on the specified grid.
  !!
  !! If the grid or optical properties cannot be calculated, the aerosol
  !! model should fail the run.
  function new_optics( this, property, output_grid, interpolation_strategy )
    use ai_optics,                     only : optics_t
    use ai_wavelength_grid,            only : wavelength_grid_t
    use musica_interpolator,           only : interpolation_strategy_i
    use musica_property,               only : property_t
    import aerosol_t
    class(optics_t),                   pointer    :: new_optics
    class(aerosol_t),                  intent(in) :: this
    class(property_t),                 intent(in) :: property
    type(wavelength_grid_t),           intent(in) :: output_grid
    procedure(interpolation_strategy_i), optional :: interpolation_strategy
  end function new_optics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns shortwave optical properties
  subroutine shortwave_optics_scalar( this, environmental_state,             &
      aerosol_state, optics )
    use ai_aerosol_state,              only : aerosol_state_t
    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_t
    import aerosol_t
    class(aerosol_t),             intent(in)    :: this
    class(environmental_state_t), intent(in)    :: environmental_state
    class(aerosol_state_t),       intent(in)    :: aerosol_state
    class(optics_t), target,      intent(inout) :: optics
  end subroutine shortwave_optics_scalar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns shortwave optical properties
  subroutine shortwave_optics_array( this, environmental_state,               &
      aerosol_state, optics )
    use ai_aerosol_state,              only : aerosol_state_t
    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_ptr
    import aerosol_t
    class(aerosol_t),             intent(in)    :: this
    class(environmental_state_t), intent(in)    :: environmental_state
    class(aerosol_state_t),       intent(in)    :: aerosol_state
    type(optics_ptr),             intent(inout) :: optics(:)
  end subroutine shortwave_optics_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns longwave optical properties
  subroutine longwave_optics_scalar( this, environmental_state,               &
      aerosol_state, optics )
    use ai_aerosol_state,              only : aerosol_state_t
    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_t
    import aerosol_t
    class(aerosol_t),             intent(in)    :: this
    class(environmental_state_t), intent(in)    :: environmental_state
    class(aerosol_state_t),       intent(in)    :: aerosol_state
    class(optics_t), target,      intent(inout) :: optics
  end subroutine longwave_optics_scalar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns longwave optical properties
  subroutine longwave_optics_array( this, environmental_state,                &
      aerosol_state, optics )
    use ai_aerosol_state,              only : aerosol_state_t
    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_ptr
    import aerosol_t
    class(aerosol_t),             intent(in)    :: this
    class(environmental_state_t), intent(in)    :: environmental_state
    class(aerosol_state_t),       intent(in)    :: aerosol_state
    type(optics_ptr),             intent(inout) :: optics(:)
  end subroutine longwave_optics_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Outputs text describing the current aerosol state
  subroutine print_state( this, aerosol_state, io_unit )
    use ai_aerosol_state,              only : aerosol_state_t
    import aerosol_t
    class(aerosol_t),       intent(in) :: this
    class(aerosol_state_t), intent(in) :: aerosol_state
    !> Optional output unit (defaults to 6)
    integer, optional,      intent(in) :: io_unit
  end subroutine print_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end interface

end module ai_aerosol
