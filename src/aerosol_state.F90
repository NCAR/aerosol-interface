! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The ai_aerosol_state module

!> The aerosol_state_t type and related functions
module ai_aerosol_state

  implicit none
  private

  public :: aerosol_state_t

  !> A collection of independent aerosol states
  type, abstract :: aerosol_state_t
    private
  contains
    procedure(raw_size),   deferred :: raw_size
    procedure(load_state), deferred :: load_state
    procedure(dump_state), deferred :: dump_state
    procedure(randomize),  deferred :: randomize
  end type aerosol_state_t

interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the number of doubles needed to hold the raw aerosol state
  integer function raw_size( this )
    import aerosol_state_t
    class(aerosol_state_t), intent(in) :: this
  end function raw_size

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Loads raw state data into the aerosol_state_t object
  !!
  !! Aerosol schemes should copy the raw state into their own internal data
  !! structures.
  subroutine load_state( this, raw_state, index )
    use musica_constants,              only : musica_dk
    import aerosol_state_t
    class(aerosol_state_t),           intent(inout) :: this
    real(kind=musica_dk),             intent(in)    :: raw_state(:)
    !> The index argument can be used to specify the starting index in
    !! raw_state to use for the aerosol state data and will be advanced by the
    !! size of the aerosol state. If it is not included the starting index
    !! will be assumed to be 1.
    integer, optional,                intent(inout) :: index
  end subroutine load_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Dumps the raw state data to a double array
  !!
  !! Aerosol schemes should copy data from their own internal data
  !! structures to the raw state array
  subroutine dump_state( this, raw_state, index )
    use musica_constants,              only : musica_dk
    import aerosol_state_t
    class(aerosol_state_t), intent(inout) :: this
    real(kind=musica_dk),   intent(inout) :: raw_state(:)
    !> The index argument can be used to specify the starting index in
    !! raw_state to dump the aerosol state data and will be advanced by the
    !! size of the aerosol state. If it is not included the starting index
    !! will be assumed to be 1.
    integer, optional,      intent(inout) :: index
  end subroutine dump_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Sets the aerosol state to a random, but reasonable, state.
  !!
  !! For testing only. This subroutine should only accept initialized states
  !! to randomize (i.e. those created by an aerosol_t object's get_new_state()
  !! function).
  subroutine randomize( this )
    import aerosol_state_t
    class(aerosol_state_t), intent(inout) :: this
  end subroutine randomize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end interface

end module ai_aerosol_state
