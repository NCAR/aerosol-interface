! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the ai_environmental_state module

!> Test module for the environmental_state_t type and related functions
program test_environmental_state

  implicit none

  call test_environmental_state_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Tests of the environmental_statte_t type
  subroutine test_environmental_state_t( )

    use ai_environmental_state,        only : environmental_state_t
    use musica_assert,                 only : assert
    use musica_constants,              only : dk => musica_dk

    type(environmental_state_t) :: env_state

    call env_state%set_layer_thickness__Pa( 101325.0_dk )
    call assert( 211175931, env_state%layer_thickness__Pa( ) .eq. 101325.0_dk )

    call env_state%randomize( )
    call assert( 932395946, env_state%layer_thickness__Pa( ) .ge.  50.0_dk )
    call assert( 199194078, env_state%layer_thickness__Pa( ) .le. 100.0_dk )

  end subroutine test_environmental_state_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_environmental_state
