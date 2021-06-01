################################################################################
# Aerosol interface

set( AI_SRC accessor.F90
            aerosol.F90
            constants.F90
            environmental_state.F90
            optics.F90
            property.F90
            util.F90
            wavelength_grid.F90
   )

add_library(aerosollib ${AI_SRC})

set_target_properties(aerosollib PROPERTIES OUTPUT_NAME aerosol)

################################################################################
