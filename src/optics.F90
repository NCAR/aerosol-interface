! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The ai_optics module

!> The optics_t type and related functions
module ai_optics

  use ai_constants,                    only : kDouble
  use ai_optical_property,             only : optical_property_t
  use ai_wavelength_grid,              only : wavelength_grid_t

  implicit none
  private

  public :: optics_t

  !> Optical properties on a wavelength grid
  type :: optics_t
    private
    class(wavelength_grid_t) :: grid_
    class(optical_property_t), allocatable :: properties_(:)
    real(kind=kDouble), allocatable :: values_(:,:)
  end type optics_t

end module ai_optics
