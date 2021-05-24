! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The ai_optical_property module

!> The optical_property_t type and related functions
module ai_optical_property

  implicit none
  private

  public

  integer, parameter :: kMaxStringLen = 50

  !> Names for each available optical property
  character(len=kMaxStringLen) :: kPropertyNames =                            &
      (/ "undefined                                         ",                &
         "layer optical depth                               ",                &
         "layer scattering optical depth                    ",                &
         "layer asymmetric scattering optical depth         ",                &
         "layer absorption optical depth                    "                 &
      /)

  !> Units for each available optical property
  character(len=kMaxStringLen) :: kUnits =                                    &
      (/ "undefined                                         ",                &
         "unitless                                          ",                &
         "unitless                                          ",                &
         "unitless                                          ",                &
         "unitless                                          "                 &
      /)

  !> A named optical property
  type optical_property_t
    private
    integer :: property_ = kUndefined
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
  end type optical_property_t

  interface optical_property_t
    module procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructs an optical_property_t object by property name
  function constructor( property_name ) result( new_obj )

    type(optical_property_t)     :: new_obj
    character(len=*), intent(in) :: property_name

    integer :: i_prop

    do i_prop = 2, len( kPropertyNames )
      if( trim( kPropertyNames( i_prop ) ) .eq. property_name ) then
        new_obj%property_ = i_prop - 1
        exit
      end if
    end do

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare two properties for equality
  logical elemental function equals_property( a, b )

    class(optical_property_t), intent(in) :: a
    class(optical_property_t), intent(in) :: b

    equals_property = a%property_ .eq. b%property_

  end function equals_property

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compares a property to a character array by name for equality
  logical elemental function equals_string( a, b )

    class(optical_property_t), intent(in) :: a
    character(len=*),          intent(in) :: b

    equals_string = trim( kPropertyNames( a%property_ ) ) .eq. b

  end function equals_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compares a character array to a property by name for equality
  logical elemental function string_equals( a, b )

    character(len=*),          intent(in) :: a
    class(optical_property_t), intent(in) :: b

    string_equals = a .eq. trim( kPropertyNames( b%property_ ) )

  end function string_equals

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare two properties for inequality
  logical elemental function not_equals_property( a, b )

    class(optical_property_t), intent(in) :: a
    class(optical_property_t), intent(in) :: b

    not_equals_property = .not. a .eq. b

  end function not_equals_property

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compares a property to a character array by name for inequality
  logical elemental function not_equals_string( a, b )

    class(optical_property_t), intent(in) :: a
    character(len=*),          intent(in) :: b

    not_equals_string = .not. a .eq. b

  end function not_equals_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compares a character array to a property by name for inequality
  logical elemental function string_not_equals( a, b )

    character(len=*),          intent(in) :: a
    class(optical_property_t), intent(in) :: b

    string_not_equals = .not. a .eq. b

  end function string_not_equals

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns whether the property has given units
  logical elemental function has_units( this, units )

    class(optical_property_t), intent(in) :: this
    character(len=*),          intent(in) :: units

    has_units = trim( kUnits( this%property_ ) ) .eq. units

  end function has_units

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module ai_optical_property
