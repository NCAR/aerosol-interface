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
    procedure(get_new_state),   deferred :: get_new_state
    procedure(optics_accessor), deferred :: optics_accessor
    procedure(get_optics),      deferred :: get_optics
    procedure(print_state),     deferred :: print_state
  end type aerosol_t

interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates and returns a new aerosol state object for this aerosol
  function get_new_state( this ) result( new_state )
    use ai_aerosol_state,              only : aerosol_state_t
    import aerosol_t
    class(aerosol_state_t), pointer :: new_state
    class(aerosol_t),       intent(in) :: this
  end function get_new_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Provides an accessor for a specific set of optical properties
  !!
  !! The aerosol module should pack any information needed to return the
  !! requested optics on the grid specified in the optics_t object into the
  !! returned accessor, such that given the accessor and the envrionmental
  !! conditions, the requested optics can be returned on the specified grid.
  !!
  !! If the grid or optical properties cannot be calculated, the aerosol
  !! model should fail the run.
  function optics_accessor( this, optics )
    use ai_accessor,                   only : accessor_t
    use ai_optics,                     only : optics_t
    import aerosol_t
    class(accessor_t), pointer    :: optics_accessor
    class(aerosol_t),  intent(in) :: this
    class(optics_t),   intent(in) :: optics
  end function optics_accessor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns optical properties on the grid specified in the accessor.
  subroutine get_optics( this, optics_accessor, environmental_state,          &
      aerosol_state, optics )
    use ai_accessor,                   only : accessor_t
    use ai_aerosol_state,              only : aerosol_state_t
    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_t
    import aerosol_t
    class(aerosol_t),             intent(in)    :: this
    class(accessor_t),            intent(in)    :: optics_accessor
    class(environmental_state_t), intent(in)    :: environmental_state
    class(aerosol_state_t),       intent(in)    :: aerosol_state
    class(optics_t),              intent(inout) :: optics
  end subroutine get_optics

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
