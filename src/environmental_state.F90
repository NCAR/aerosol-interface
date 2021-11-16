! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The ai_environmental_state module

!> The environmental_state_t type and related functions
module ai_environmental_state

  use musica_constants,                only : musica_dk

  implicit none
  private

  public :: environmental_state_t

  !> A collection of indenendent environmental states
  type :: environmental_state_t
    private
    real(kind=musica_dk) :: layer_thickness__Pa_ = 0.0_musica_dk
  contains
    procedure :: set_layer_thickness__Pa
    procedure :: layer_thickness__Pa
    procedure :: randomize
  end type environmental_state_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Sets the layer thickness [Pa]
  subroutine set_layer_thickness__Pa( this, layer_thickness__Pa )

    class(environmental_state_t), intent(inout) :: this
    real(kind=musica_dk),         intent(in)    :: layer_thickness__Pa

    this%layer_thickness__Pa_ = layer_thickness__Pa

  end subroutine set_layer_thickness__Pa

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the layer thickness [Pa]
  real(kind=musica_dk) elemental function layer_thickness__Pa( this )

    class(environmental_state_t), intent(in) :: this

    layer_thickness__Pa = this%layer_thickness__Pa_

  end function layer_thickness__Pa

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Set the environmental state to random, but reasonable, values
  subroutine randomize( this )

    class(environmental_state_t), intent(inout) :: this

    real(kind=musica_dk) :: rand_val

    !> \todo make sure ranges of random environmental values are reasonable
    call random_number( rand_val )
    this%layer_thickness__Pa_ = rand_val * 50.0 + 50.0

  end subroutine randomize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module ai_environmental_state
