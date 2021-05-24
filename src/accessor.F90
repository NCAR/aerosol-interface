! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The ai_acessor module

!> The accessor_t type and related functions
module ai_accessor

  implicit none
  private

  public :: accessor_t

  !> Abstract accessor for physical/model properties
  type :: accessor_t
  end type accessor_t

end module ai_accessor
