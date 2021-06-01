! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The ai_property module

!> The property_t type and related functions
module ai_property

  implicit none
  private

  public :: property_t

  !> A named physical or other property
  !!
  !! A property is defined by a name and units
  type property_t
    private
    character(len=:), allocatable :: name_
    character(len=:), allocatable :: units_
  contains
    procedure, private, pass(a) :: equals_property
    procedure, private, pass(a) :: equals_string
    procedure, private, pass(b) :: string_equals
    generic :: operator(==) => equals_property, equals_string, string_equals
    procedure, private, pass(a) :: not_equals_property
    procedure, private, pass(a) :: not_equals_string
    procedure, private, pass(b) :: string_not_equals
    generic :: operator(/=) => not_equals_property, not_equals_string,        &
                               string_not_equals
    procedure :: has_units
    procedure :: name => get_name
    procedure :: units
  end type property_t

  interface property_t
    module procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructs an property_t object by property name
  function constructor( property_name, units ) result( new_obj )

    type(property_t)             :: new_obj
    character(len=*), intent(in) :: property_name
    character(len=*), intent(in) :: units

    new_obj%name_  = property_name
    new_obj%units_ = units

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare two properties for equality based on the property name
  logical elemental function equals_property( a, b )

    class(property_t), intent(in) :: a
    class(property_t), intent(in) :: b

    equals_property = a%name_ .eq. b%name_

  end function equals_property

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compares a property to a character array by name for equality
  logical elemental function equals_string( a, b )

    class(property_t), intent(in) :: a
    character(len=*),  intent(in) :: b

    equals_string = a%name_ .eq. b

  end function equals_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compares a character array to a property by name for equality
  logical elemental function string_equals( a, b )

    character(len=*),  intent(in) :: a
    class(property_t), intent(in) :: b

    string_equals = a .eq. b%name_

  end function string_equals

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare two properties for inequality
  logical elemental function not_equals_property( a, b )

    class(property_t), intent(in) :: a
    class(property_t), intent(in) :: b

    not_equals_property = .not. a .eq. b

  end function not_equals_property

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compares a property to a character array by name for inequality
  logical elemental function not_equals_string( a, b )

    class(property_t), intent(in) :: a
    character(len=*),  intent(in) :: b

    not_equals_string = .not. a .eq. b

  end function not_equals_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compares a character array to a property by name for inequality
  logical elemental function string_not_equals( a, b )

    character(len=*),  intent(in) :: a
    class(property_t), intent(in) :: b

    string_not_equals = .not. a .eq. b

  end function string_not_equals

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns whether the property has given units
  logical elemental function has_units( this, units )

    class(property_t), intent(in) :: this
    character(len=*),  intent(in) :: units

    has_units = this%units_ .eq. units

  end function has_units

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the name of the property
  function get_name( this )

    character(len=:), allocatable :: get_name
    class(property_t), intent(in) :: this

    get_name = this%name_

  end function get_name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the units for the property
  function units( this )

    character(len=:), allocatable :: units
    class(property_t), intent(in) :: this

    units = this%units_

  end function units

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module ai_property
