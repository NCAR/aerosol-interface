! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the ai_optics module

!> Test module for the abstract optics_t type and related functions
module optics_foo

  use ai_accessor,                     only : accessor_t
  use ai_optics,                       only : optics_t

  implicit none
  private

  public :: optics_foo_t, foo_strategy, foo_accessor_t

  type, extends(optics_t) :: optics_foo_t
  contains
    procedure :: name => property_name
    procedure :: units
  end type optics_foo_t

  interface optics_foo_t
    procedure :: constructor
  end interface

  type, extends(accessor_t) :: foo_accessor_t
    integer :: bar_ = 12
  end type foo_accessor_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function constructor( native_grid, output_grid, accessor, strategy )        &
      result( optics )

    use ai_wavelength_grid,            only : wavelength_grid_t
    use musica_interpolator,           only : interpolation_strategy_i

    type(optics_foo_t)                            :: optics
    type(wavelength_grid_t),           intent(in) :: native_grid
    type(wavelength_grid_t),           intent(in) :: output_grid
    class(accessor_t), optional,       intent(in) :: accessor
    procedure(interpolation_strategy_i), optional :: strategy

    call optics%private_constructor( native_grid, output_grid,                &
                                     accessor = accessor,                     &
                                     interpolation_strategy = strategy )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  type(string_t) function property_name( this )

    use musica_string,                 only : string_t

    class(optics_foo_t), intent(in) :: this

    property_name = "foo optics"

  end function property_name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  type(string_t) function units( this )

    use musica_string,                 only : string_t

    class(optics_foo_t), intent(in) :: this

    units = "foobits"

  end function units

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function foo_strategy( from_grid, to_grid ) result( map )

    use musica_assert,                 only : assert
    use musica_constants,              only : musica_dk
    use musica_grid,                   only : grid_t, grid_iterator_t
    use musica_interpolator,           only : interpolator_element_t

    class(interpolator_element_t), allocatable :: map(:)
    class(grid_t),                 intent(in)  :: from_grid
    class(grid_t),                 intent(in)  :: to_grid

    type(interpolator_element_t), allocatable :: local_map(:)
    type(interpolator_element_t) :: element
    class(grid_iterator_t), pointer :: from_iter, to_iter
    integer :: i_element, j_element

    ! set up a map where from[1] => to[last] ... from[last] => to[1]
    allocate( local_map( 0 ) )
    from_iter => from_grid%iterator( )
    to_iter   => to_grid%iterator( )
    i_element = 0
    do while( from_iter%next( ) )
      i_element = i_element + 1
      call to_iter%reset( )
      j_element = 1
      do while( j_element .le. to_grid%number_of_sections( ) - i_element + 1 )
        j_element = j_element + 1
        call assert( 603310583, to_iter%next( ) )
      end do
      element = interpolator_element_t( from_iter, to_iter, 1.0_musica_dk )
      local_map = [ local_map, element ]
    end do
    allocate( map, source = local_map )
    deallocate( from_iter )
    deallocate(   to_iter )

  end function foo_strategy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module optics_foo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program test_optics

  use ai_accessor,                     only : accessor_t
  use ai_wavelength_grid,              only : wavelength_grid_t
  use musica_assert,                   only : assert, die
  use musica_constants,                only : dk => musica_dk
  use optics_foo,                      only : optics_foo_t, foo_strategy,     &
                                              foo_accessor_t

  type(optics_foo_t)         :: foo
  type(foo_accessor_t)       :: foo_accessor
  class(accessor_t), pointer :: optics_accessor
  type(wavelength_grid_t)    :: from_grid, to_grid
  real(kind=dk)              :: values(3)
  real(kind=dk)        :: from_lower(3) = (/  0.0_dk, 20.0_dk, 200.0_dk /)
  real(kind=dk)        :: from_upper(3) = (/ 10.0_dk, 50.0_dk, 300.0_dk /)
  real(kind=dk)        :: to_lower(3)   = (/  5.0_dk, 20.0_dk, 200.0_dk /)
  real(kind=dk)        :: to_upper(3)   = (/ 20.0_dk, 50.0_dk, 400.0_dk /)

  from_grid = wavelength_grid_t( from_lower, from_upper )
  to_grid   = wavelength_grid_t(   to_lower,   to_upper )

  ! standard test, no interpolation
  foo = optics_foo_t( from_grid, from_grid )
  call assert( 236464908, foo%native_grid( ) .eq. from_grid )
  call assert( 343518946, foo%output_grid( ) .eq. from_grid )
  call foo%accessor( optics_accessor )
  call assert( 175267576, .not. associated( optics_accessor ) )
  call foo%set_values( (/ 1.0_dk, 20.0_dk, 4.0_dk /) )
  call foo%get_values( values )
  call assert( 226388589, values(1) .eq.  1.0_dk )
  call assert( 338706934, values(2) .eq. 20.0_dk )
  call assert( 168550030, values(3) .eq.  4.0_dk )
  call foo%add_values( (/ 12.0_dk, 10.0_dk, 30.0_dk /) )
  call foo%get_values( values )
  call assert( 898393125, values(1) .eq. 13.0_dk )
  call assert( 445760972, values(2) .eq. 30.0_dk )
  call assert( 275604068, values(3) .eq. 34.0_dk )
  call foo%reset_values( )
  call foo%get_values( values )
  call assert( 387922413, values(1) .eq. 0.0_dk )
  call assert( 217765509, values(2) .eq. 0.0_dk )
  call assert( 112617005, values(3) .eq. 0.0_dk )

  ! standard test, default interpolation
  foo = optics_foo_t( from_grid, to_grid )
  call assert( 709224102, foo%native_grid( ) .eq. from_grid )
  call assert( 169263851, foo%output_grid( ) .eq.   to_grid )
  call foo%accessor( optics_accessor )
  call assert( 781976589, .not. associated( optics_accessor ) )
  call foo%set_values( (/ 1.0_dk, 20.0_dk, 4.0_dk /) )
  call foo%get_values( values )
  call assert( 200232226, values(1) .eq.  0.5_dk )
  call assert( 484613009, values(2) .eq. 20.0_dk )
  call assert( 314456105, values(3) .eq.  4.0_dk )
  call foo%add_values( (/ 12.0_dk, 10.0_dk, 30.0_dk /) )
  call foo%get_values( values )
  call assert( 867424750, values(1) .eq.  6.5_dk )
  call assert( 639429287, values(2) .eq. 30.0_dk )
  call assert( 751747632, values(3) .eq. 34.0_dk )
  call foo%reset_values( )
  call foo%get_values( values )
  call assert( 695814607, values(1) .eq. 0.0_dk )
  call assert( 802868645, values(2) .eq. 0.0_dk )
  call assert( 350236492, values(3) .eq. 0.0_dk )

  ! custom interpolation strategy, accessor
  foo = optics_foo_t( from_grid, to_grid, accessor = foo_accessor,            &
                      strategy = foo_strategy )
  call assert( 823635133, foo%native_grid( ) .eq. from_grid )
  call assert( 378172821, foo%output_grid( ) .eq.   to_grid )
  call foo%accessor( optics_accessor )
  call assert( 709863549, associated( optics_accessor ) )
  select type( optics_accessor )
  type is( foo_accessor_t )
    call assert( 357567888, optics_accessor%bar_ .eq. 12 )
  class default
    call die( 306899170 )
  end select
  call foo%set_values( (/ 1.0_dk, 20.0_dk, 4.0_dk /) )
  call foo%get_values( values )
  call assert( 468432994, values(1) .eq.  4.0_dk )
  call assert( 245701838, values(2) .eq. 20.0_dk )
  call assert( 358020183, values(3) .eq.  1.0_dk )
  call foo%add_values( (/ 12.0_dk, 10.0_dk, 30.0_dk /) )
  call foo%get_values( values )
  call assert( 805388029, values(1) .eq. 34.0_dk )
  call assert( 635231125, values(2) .eq. 30.0_dk )
  call assert( 465074221, values(3) .eq. 13.0_dk )
  call foo%reset_values( )
  call foo%get_values( values )
  call assert( 294917317, values(1) .eq. 0.0_dk )
  call assert( 124760413, values(2) .eq. 0.0_dk )
  call assert( 854603508, values(3) .eq. 0.0_dk )

end program test_optics
