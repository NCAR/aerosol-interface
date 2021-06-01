! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The ai_optics module

!> The optics_t type and related functions
module ai_optics

  use ai_constants,                    only : kDouble
  use ai_property,                     only : property_t
  use ai_wavelength_grid,              only : wavelength_grid_t

  implicit none
  private

  public :: optics_t

  !> Optical properties on a wavelength grid
  type :: optics_t
    private
    !> Wavelength grid to store optical properties on
    type(wavelength_grid_t) :: grid_
    !> Set of optical properties
    type(property_t), allocatable :: properties_(:)
    !> Optical property values (wavelength bin, property, layer, column)
    real(kind=kDouble), allocatable, public :: values_(:,:,:,:)
  contains
    procedure :: number_of_properties
    procedure :: property_name
    procedure :: grid
  end type optics_t

  interface optics_t
    module procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructs optics_t objects for a given grid and property set
  function constructor( properties, grid, number_of_columns,                  &
      number_of_layers ) result( new_obj )

    type(optics_t)                      :: new_obj
    type(property_t),        intent(in) :: properties(:)
    type(wavelength_grid_t), intent(in) :: grid
    integer,                 intent(in) :: number_of_columns
    integer,                 intent(in) :: number_of_layers

    new_obj%grid_ = grid
    new_obj%properties_ = properties
    allocate( new_obj%values_( grid%number_of_bins( ),                        &
                               size( new_obj%properties_ ),                   &
                               number_of_layers,                              &
                               number_of_columns ) )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the number of properties in the set
  integer function number_of_properties( this )

    class(optics_t), intent(in) :: this

    number_of_properties = size( this%properties_ )

  end function number_of_properties

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the name of the property at a given index
  function property_name( this, property_index )

    character(len=:), allocatable :: property_name
    class(optics_t),  intent(in)  :: this
    integer,          intent(in)  :: property_index

    if( property_index .lt. 1 .or.                                            &
        property_index .gt. size( this%properties_ ) ) then
      property_name = "undefined"
    else
      property_name = this%properties_( property_index )%name( )
    end if

  end function property_name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the wavelength grid used for the optics
  function grid( this )

    type(wavelength_grid_t)     :: grid
    class(optics_t), intent(in) :: this

    grid = this%grid_

  end function grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module ai_optics
