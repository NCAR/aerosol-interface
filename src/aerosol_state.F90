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
  !! The index argument specifies the start of the state data in raw_state
  !! and will be advanced by the size of the aerosol state.
  !!
  !! An aerosol scheme may operate on the raw data in place or copy it to
  !! internal data structures.
  subroutine load_state( this, raw_state, index )
    use ai_constants,                  only : kDouble
    import aerosol_state_t
    class(aerosol_state_t),         intent(inout) :: this
    real(kind=kDouble),     target, intent(inout) :: raw_state(:)
    integer,                        intent(inout) :: index
  end subroutine load_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Dumps the raw state data to a double array
  !!
  !! The index argument specifies the starting index in raw_state to dump the
  !! aerosol state data and will be advanced by the size of the aerosol state.
  !!
  !! If the aerosol module operates on the raw data in place, pointer
  !! associations should be dropped. If the aerosol state is contained in
  !! internal data structures, it should be copied to the raw data array.
  subroutine dump_state( this, raw_state, index )
    use ai_constants,                  only : kDouble
    import aerosol_state_t
    class(aerosol_state_t), intent(inout) :: this
    real(kind=kDouble),     intent(inout) :: raw_state(:)
    integer,                intent(inout) :: index
  end subroutine dump_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end interface

end module ai_aerosol_state
