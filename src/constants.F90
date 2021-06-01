! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The ai_constants module

!> Physical and other constants
module ai_constants

  implicit none
  private

  !> @name Primitive type kinds
  !! @{
  integer, parameter, public :: kFloat  = kind( 1.0e0 )
  integer, parameter, public :: kDouble = kind( 1.0d0 )
  !> @}

end module ai_constants
