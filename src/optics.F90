! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The ai_optics module

!> The optics_t type and related functions
module ai_optics

  use musica_constants,                only : musica_dk
  use musica_property_set,             only : property_set_t
  use musica_wavelength_grid,          only : wavelength_grid_t

  implicit none
  private

  public :: optics_t

  !> Optical properties on a wavelength grid
  type :: optics_t
    private
    !> Wavelength grid to store optical properties on
    type(wavelength_grid_t) :: grid_
    !> Set of optical properties
    type(property_set_t), pointer :: properties_
    !> Optical property values (wavelength bin, property)
    real(kind=musica_dk), allocatable, public :: values_(:,:)
  contains
    procedure :: property_set
    procedure :: grid
  end type optics_t

  interface optics_t
    module procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructs optics_t objects for a given grid and property set
  function constructor( properties, grid ) result( new_obj )

    type(optics_t)                      :: new_obj
    type(property_set_t),    intent(in) :: properties
    type(wavelength_grid_t), intent(in) :: grid

    new_obj%grid_ = grid
    allocate( new_obj%properties_ )
    new_obj%properties_ = properties
    allocate( new_obj%values_( grid%number_of_bins( ),                        &
                               new_obj%properties_%size( ) ) )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the set of optics properties
  function property_set( this )

    type(property_set_t), pointer    :: property_set
    class(optics_t),      intent(in) :: this

    allocate( property_set )
    property_set = this%properties_

  end function property_set

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the wavelength grid used for the optics
  function grid( this )

    type(wavelength_grid_t)     :: grid
    class(optics_t), intent(in) :: this

    grid = this%grid_

  end function grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module ai_optics
