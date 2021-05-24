! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The ai_constants module

!> Physical and other constants
module ai_constants

  implicit none
  private

  parameter, public :: kFloat  = kind( 1.0e0 )
  parameter, public :: kDouble = kind( 1.0d0 )

end module ai_constants
