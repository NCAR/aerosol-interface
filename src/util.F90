! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The ai_util module

!> Utility functions for aerosol models
module ai_util

  implicit none
  private

  public :: assert_msg, assert, die_msg, die

  !> Unit for error output
  !! \todo how to handle error output?
  integer, parameter :: kErrorId = 0

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Asserts condition to be true or fails with provided message
  subroutine assert_msg( code, condition, error_message )

    !> Unique code for the assertion
    integer, intent(in) :: code
    !> Condition to evaluate
    logical, intent(in) :: condition
    !> Message to display on failure
    character(len=*), intent(in) :: error_message

    character(len=50) :: str_code

    if( .not. condition ) then
      write(str_code, '(i30)') code
      write(kErrorId,*) "ERROR (Aerosol-"//trim( adjustl( str_code ) )//"): " &
                        //error_message
      stop 3
    end if

  end subroutine assert_msg

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Asserts condition to be true or fails
  subroutine assert( code, condition )

    !> Unique code for the assertion
    integer, intent(in) :: code
    !> Condition to evaluate
    logical, intent(in) :: condition

    call assert_msg( code, condition, "Internal error" )

  end subroutine assert

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Fails the run with a provided message
  subroutine die_msg( code, error_message )

    !> Unique code for the failure
    integer, intent(in) :: code
    !> Failure message
    character(len=*), intent(in) :: error_message

    call assert_msg( code, .false., error_message )

  end subroutine die_msg

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Fails the run
  subroutine die( code )

    !> Unique code for the failure
    integer, intent(in) :: code

    call assert_msg( code, .false., "Internal error" )

  end subroutine die

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module ai_util
