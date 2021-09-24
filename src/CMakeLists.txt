################################################################################
# Aerosol interface

set( AI_SRC accessor.F90
            aerosol.F90
            aerosol_state.F90
            environmental_state.F90
            optics.F90
            optics/absorption_optical_depth.F90
            optics/asymmetry_factor.F90
            optics/extinction_optical_depth.F90
            optics/forward_scattered_fraction.F90
            optics/single_scatter_albedo.F90
            wavelength_grid.F90
   )

add_library(aerosollib ${AI_SRC})

target_link_libraries(aerosollib musicalib)

set_target_properties(aerosollib PROPERTIES OUTPUT_NAME aerosol)

################################################################################
