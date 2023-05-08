add_library(fvcom)

target_sources(fvcom
  PRIVATE
    mod_prec.F
    sinter.F
    mod_types.F
    mod_time.F
    mod_main.F
    mod_spherical.F
    mod_utils.F
    mod_clock.F
    eqs_of_state.F
    mod_bulk.F
    mod_interp.F
    mod_par.F
    mod_par_special.F
    mod_ncll.F
    mod_nctools.F
    mod_wd.F
    mod_sng.F
    mod_solar.F
    mod_input.F
    mod_force.F
    mod_obcs.F
    mod_petsc.F
    mod_semi_implicit.F
    mod_non_hydro.F
    mod_set_time.F
    ice_kinds_mod.F
    ice_model_size.F
    ice_domain.F
    ice_constants.F
    ice_fileunits.F
    ice_state.F
    ice_work.F
    ice_grid.F
    ice_albedo.F
    ice_calendar.F
    ice_flux.F
    ice_flux_in.F
    ice_itd.F
    ice_mechred.F
    ice_itd_linear.F
    ice_scaling.F
    ice_atmo.F
    ice_ocean.F
    ice_therm_vertical.F
    ice_init.F
    ice_therm_itd.F
    mod_ice2d.F
    mod_ice.F
    mod_startup.F
    mod_wqm.F
    mod_ncdio.F
    mod_setup.F
    mod_newinp.F
    particle.F
    linklist.F
    mod_lag.F
    mod_northpole.F
    mod_pwp.F
    mod_optimal_interpolation.F
    mod_report.F
    mod_probe.F
    mod_gotm.F
    mod_balance_2d.F
    mod_tridiag.F
    mod_scal.F
    mod_meanflow.F
    mod_obcs2.F
    mod_obcs3.F
    mod_sed.F
    mod_enkf.F
    mod_etkf.F
    mod_rrk.F
    mod_rrkf_obs.F
    mod_rrkassim.F
    mod_enkf_ncd.F
    enkf_ncdio.F
    mod_enkf_obs.F
    mod_enkfassim.F
    mod_assim.F
    mod_nesting.F
    mod_visit.F
    mod_plbc.F
    mod_dam.F
    mod_station_timeseries.F
    mod_sparse_timeseries.F
    mod_boundschk.F
    mod_esmf_nesting.F
    mod_cstms_vars.F
    mod_flocmod.F
    mod_sed_cstms.F
    mod_fluid_mud.F
    mod_main_wave.F
    swmod1.F
    swmod2.F
    swmod3.F
    mod_action_im.F
    mod_action_ex.F
    mod_wavesetup.F
    mod_bbl.F
    genmap.F
    tge.F
    longshore_flow.F
    cell_area.F
    open_all.F
    load_grid.F
    allocate_all.F
    setup_domain.F
    genmap_obc.F
    genmap_lsf.F
    print_vals.F
    coords_n_const.F
    shape_coef_gcn.F
    shape_coef_gcy.F
    depth_grad.F
    grid_metrics.F
    cntrl_prmtrs.F
    init_sed.F
    internal_step.F
    bcond_gcn.F
    bcond_gcy.F
    adjust2d3d.F
    brough.F
    advection_edge_gcn.F
    advection_edge_gcy.F
    ghostuv.F
    advave_edge_gcn.F
    advave_edge_gcy.F
    phy_baropg.F
    baropg.F
    external_step.F
    extel_edge.F
    extuv_edge.F
    depth_check.F
    vertvl_edge.F
    adv_uv_edge_gcn.F
    adv_uv_edge_gcy.F
    vdif_uv.F
    extelpf_edge.F
    wreal.F
    viscofh.F
    adv_q.F
    fct_q2.F
    fct_q2l.F
    vdif_q.F
    adv_t.F
    adv_s.F
    fct_t.F
    vdif_ts.F
    vdif_ts_gom.F
    fct_s.F
    bcond_ts.F
    adjust_ts.F
    conv_over.F
    visitsim.F
    startup_type.F
    edge_len.F
    adcor.F
    icing.F
    rho_pmean.F
    calc_vort.F
    namelist.F
    nh_set_nesting.F
    ocpmix.F
    ocpcre.F
    ocpids.F
    swanpre1.F
    swanpre2.F
    swanser.F
    swanmain.F
    swancom1.F
    swancom2.F
    swancom3.F
    swancom4.F
    swancom5.F
    w3part.F
    func_pointer.c
    mod_bio_3D.F
    mod_onedtide.F
  )


find_package(NetCDF COMPONENTS Fortran REQUIRED)
target_link_libraries(fvcom PUBLIC NetCDF::NetCDF_Fortran)

# we need to do this differently but now just happy that this links
include(ExternalProject)
foreach(_name julian metis)
  ExternalProject_Add(${_name}
    URL ${CMAKE_CURRENT_SOURCE_DIR}/libs/${_name}.tgz
    DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
    SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/${_name}
    BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/${_name}
    PATCH_COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/libs/patch-${_name}.sh
       ${CMAKE_CURRENT_SOURCE_DIR}
       ${CMAKE_CURRENT_SOURCE_DIR}/libs/${_name}
       ${CMAKE_CURRENT_BINARY_DIR}/${_name}
    CONFIGURE_COMMAND ""
    INSTALL_DIR ${CMAKE_CURRENT_SOURCE_DIR}/libs/${_name}
    )
endforeach()

# we need to do this differently but now just happy that this links
target_include_directories(fvcom PUBLIC libs/julian/include)
target_link_libraries(fvcom PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/julian/libjulian.a)
add_dependencies(fvcom julian)

# we need to do this differently but now just happy that this links
target_link_libraries(fvcom PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/metis/libmetis.a)
add_dependencies(fvcom metis)


add_executable(FVCOM_exe fvcom.F)
target_link_libraries(FVCOM_exe PRIVATE fvcom)


#-------------------------------------------------------------------------------
# options


option(ENABLE_DOUBLE_PRECISION "Enable DOUBLE_PRECISION" OFF)
if(ENABLE_DOUBLE_PRECISION)
  target_compile_definitions(fvcom PUBLIC DOUBLE_PRECISION)
endif()


option(ENABLE_MULTIPROCESSOR "Enable MULTIPROCESSOR" ON)
if(ENABLE_MULTIPROCESSOR)
  find_package(MPI REQUIRED COMPONENTS Fortran)
  target_compile_definitions(fvcom PUBLIC MULTIPROCESSOR)
  target_link_libraries(fvcom PUBLIC MPI::MPI_Fortran)
endif()


#-------------------------------------------------------------------------------
# features which require dependencies


option(ENABLE_FABM "Enable FABM" OFF)
if(ENABLE_FABM)
  include(FetchContent)

  FetchContent_Declare(fabm
    GIT_REPOSITORY https://github.com/fabm-model/fabm.git
    GIT_TAG v0.96.0
    )

  FetchContent_GetProperties(fabm)
  if(NOT fabm_POPULATED)
    FetchContent_Populate(fabm)

    set(FABM_FORCED_HOST fvcom)
    set(CMAKE_POSITION_INDEPENDENT_CODE ON)
    if(ENABLE_DOUBLE_PRECISION)
      set(FABM_FORCED_REAL_KIND "selected_real_kind(12)")
    else()
      set(FABM_FORCED_REAL_KIND "selected_real_kind(6)")
    endif()

    add_subdirectory(${fabm_SOURCE_DIR} ${fabm_BINARY_DIR})
  endif()

  target_sources(fvcom
    PRIVATE
      mod_fabm_3D.F
      mod_fabm_data.F
    )

  target_compile_definitions(fvcom PUBLIC FABM)
  target_link_libraries(fvcom PUBLIC fabm yaml)
endif()


option(ENABLE_NH "Enable NH" OFF)
if(ENABLE_NH)
  message(FATAL_ERROR "NH requires PETSc but for this we need to write a FindPETSc.cmake first")
  target_compile_definitions(fvcom PUBLIC NH)
endif()


option(ENABLE_PROJ "Enable PROJ" OFF)
if(ENABLE_PROJ)
  set(_name proj)
  ExternalProject_Add(${_name}
    URL ${CMAKE_CURRENT_SOURCE_DIR}/libs/${_name}.tgz
    DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
    SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/${_name}
    BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/${_name}
    CONFIGURE_COMMAND
      ${CMAKE_CURRENT_BINARY_DIR}/${_name}/configure
        CC=${CMAKE_C_COMPILER}
        CXX=${CMAKE_CXX_COMPILER}
        F77=${CMAKE_Fortran_COMPILER}
        F90=${CMAKE_Fortran_COMPILER}
        FC=${CMAKE_Fortran_COMPILER}
        --prefix=${CMAKE_CURRENT_BINARY_DIR}/install/${_name}
    )

  set(_name fproj)
  ExternalProject_Add(${_name}
    URL ${CMAKE_CURRENT_SOURCE_DIR}/libs/${_name}.tgz
    DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
    SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/${_name}
    BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/${_name}
    CONFIGURE_COMMAND
      ${CMAKE_CURRENT_BINARY_DIR}/${_name}/configure
        CC=${CMAKE_C_COMPILER}
        CXX=${CMAKE_CXX_COMPILER}
        F77=${CMAKE_Fortran_COMPILER}
        F90=${CMAKE_Fortran_COMPILER}
        FC=${CMAKE_Fortran_COMPILER}
        --prefix=${CMAKE_CURRENT_BINARY_DIR}/install/${_name}
        --with-proj4=${CMAKE_CURRENT_BINARY_DIR}/install/proj
      ${CMAKE_CURRENT_SOURCE_DIR}/libs/patch-${_name}.sh ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_CURRENT_BINARY_DIR}/${_name}
    )
  ExternalProject_Add_Step(${_name} post-conf-patch
    COMMAND
      ${CMAKE_CURRENT_SOURCE_DIR}/libs/patch-${_name}.sh
      ${CMAKE_Fortran_COMPILER_ID}
      ${CMAKE_CURRENT_BINARY_DIR}/${_name}
      ${CMAKE_CURRENT_SOURCE_DIR}/${_name}
    COMMENT "post-configure patch"
    DEPENDEES configure
    )

  target_link_libraries(fvcom
    PUBLIC
      ${CMAKE_CURRENT_BINARY_DIR}/fproj/libfproj4.a
      ${CMAKE_CURRENT_BINARY_DIR}/install/proj/lib/libproj.a
    )
  target_compile_definitions(fvcom PUBLIC PROJ)
  target_include_directories(fvcom PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/install/fproj/include)

  add_dependencies(fvcom fproj)
  add_dependencies(fproj proj)
endif()


#-------------------------------------------------------------------------------
# features which activate additional sources


option(ENABLE_HEATING_CALCULATED "Enable HEATING_CALCULATED" OFF)
if(ENABLE_HEATING_CALCULATED)
  target_sources(fvcom PRIVATE mod_heatflux.F)
  target_compile_definitions(fvcom PUBLIC HEATING_CALCULATED)
endif()


option(ENABLE_DYE_RELEASE "Enable DYE_RELEASE" OFF)
if(ENABLE_DYE_RELEASE)
  target_sources(fvcom PRIVATE mod_dye.F)
  target_compile_definitions(fvcom PUBLIC DYE_RELEASE)
endif()


option(ENABLE_WAVE_CURRENT_INTERACTION "Enable WAVE_CURRENT_INTERACTION" OFF)
if(ENABLE_WAVE_CURRENT_INTERACTION)
  target_sources(fvcom PRIVATE mod_wave_current_interaction.F)
  target_compile_definitions(fvcom PUBLIC WAVE_CURRENT_INTERACTION)
endif()


#-------------------------------------------------------------------------------
# features which are by default ON


foreach(feature
    WET_DRY
    LIMITED_NO
    GCN
    OLD_PETSC
  )
  option(ENABLE_${feature} "Enable ${feature}" ON)
  if(ENABLE_${feature})
    target_compile_definitions(fvcom PUBLIC ${feature})
  endif()
endforeach()


#-------------------------------------------------------------------------------
# features which are by default OFF


foreach(feature
    AIR_PRESSURE
    ATMO_TIDE
    BALANCE_2D
    BioGen
    CSTMS
    DATA_ASSIM
    ENKF
    EQUI_TIDE
    EVAPORATION_CALCULATED
    EXPLICIT
    GCY1
    GCY2
    GOTM
    HEATING_CALCULATED
    ICE
    LAG_PARTICLE
    LIMITED_1
    LIMITED_2
    LIMITER_VER_ADV
    MEAN_FLOW
    MPDATA
    OFFLINE_SEDIMENT
    ONE_D_MODEL
    ORIGINAL SEDIMENT
    PLBC
    PWP
    RIVER_FLOAT
    RRKF
    SALTLAKE
    SEDIMENT
    SEMI_IMPLICIT
    SPHERICAL
    THIN_DAM
    TIDE_OUTPUT
    TURBINES
    TWO_D_MODEL
    VISIT
    WATER_QUALITY
    WAVE_OFFLINE
    WAVE_ONLY
    WAVE_ROLLER
  )
  option(ENABLE_${feature} "Enable ${feature}" OFF)
  if(ENABLE_${feature})
    target_compile_definitions(fvcom PUBLIC ${feature})
  endif()
endforeach()
