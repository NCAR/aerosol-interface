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
  !! An aerosol scheme may operate on the raw data in place or copy it to
  !! internal data structures.
  subroutine load_state( this, raw_state, index )
    use musica_constants,              only : musica_dk
    import aerosol_state_t
    class(aerosol_state_t),           intent(inout) :: this
    real(kind=musica_dk),     target, intent(inout) :: raw_state(:)
    !> The index argument can be used to specify the starting index in
    !! raw_state to use for the aerosol state data and will be advanced by the
    !! size of the aerosol state. If it is not included the starting index
    !! will be assumed to be 1.
    integer, optional,                intent(inout) :: index
  end subroutine load_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Dumps the raw state data to a double array
  !!
  !! If the aerosol module operates on the raw data in place, pointer
  !! associations should be dropped. If the aerosol state is contained in
  !! internal data structures, it should be copied to the raw data array.
  !!
  !! \todo consider dropping copy option and removing raw_state from this
  !!       function. If the calling function passes a different array or
  !!       slice than it passed to the load_state( ) function, pointer-
  !!       associated aerosol_state_t classes will not update this raw_state
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
