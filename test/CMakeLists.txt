################################################################################
# Link aerosollib to a test and add it to the suite

macro(add_std_test test_name)
  target_include_directories(${test_name} PUBLIC ${CMAKE_BINARY_DIR}/src)
  target_link_libraries(${test_name} aerosollib)
  add_test(NAME ${test_name} COMMAND ${test_name})
endmacro(add_std_test)

################################################################################
# aerosol tests

add_executable(environmental_state environmental_state.F90)
add_std_test(environmental_state)

add_executable(optics optics.F90)
add_std_test(optics)

add_executable(wavelength_grid wavelength_grid.F90)
add_std_test(wavelength_grid)

################################################################################

