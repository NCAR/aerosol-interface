################################################################################
# Aerosol interface

set( AI_SRC accessor.F90
            aerosol.F90
            aerosol_state.F90
            environmental_state.F90
            optics.F90
   )

add_library(aerosollib ${AI_SRC})

target_link_libraries(aerosollib musicalib)

set_target_properties(aerosollib PROPERTIES OUTPUT_NAME aerosol)

################################################################################
