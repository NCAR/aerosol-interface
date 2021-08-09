! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The ai_environmental_state module

!> The environmental_state_t type and related functions
module ai_environmental_state

  use ai_constants,                    only : kDouble

  implicit none
  private

  public :: environmental_state_t

  !> A collection of indenendent environmental states
  type :: environmental_state_t
    private
    real(kind=kDouble) :: layer_thickness__Pa_ = 0.0_kDouble
  contains
    procedure :: set_layer_thickness__Pa
    procedure :: layer_thickness__Pa
  end type environmental_state_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Sets the layer thickness [Pa]
  subroutine set_layer_thickness__Pa( this, layer_thickness__Pa )

    class(environmental_state_t), intent(inout) :: this
    real(kind=kDouble),           intent(in)    :: layer_thickness__Pa

    this%layer_thickness__Pa_ = layer_thickness__Pa

  end subroutine set_layer_thickness__Pa

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the layer thickness [Pa]
  real(kind=kDouble) elemental function layer_thickness__Pa( this )

    class(environmental_state_t), intent(in) :: this

    layer_thickness__Pa = this%layer_thickness__Pa_

  end function layer_thickness__Pa

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module ai_environmental_state
