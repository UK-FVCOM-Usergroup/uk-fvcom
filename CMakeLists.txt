project(FVCOM Fortran C)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/")

set(ROOT "${CMAKE_CURRENT_SOURCE_DIR}/FVCOM_source")

find_path(JULIAN_BASE fjulian.inc HINTS "${ROOT}/libs/julian" DOC "Source directory of julian")

include_directories(${ROOT} ${JULIAN_BASE})

add_library(FVCOM_C ${ROOT}/func_pointer.c)

add_definitions(-DWET_DRY -DLIMITED_2 -DGCY1 -DMPDATA -DFABM)

find_package(NetCDF REQUIRED)
include_directories("${NetCDF_INCLUDE_DIRS}")
if (NetCDF_STATIC_MSVC_BUILD)
  # On Windows with a statically-compiled NetCDF library - compile all code against static runtime.
  # This MUST be done before any targets are added.
  add_compile_options("/libs:static")
endif()

add_library(julian
   ${JULIAN_BASE}/format.c
   ${JULIAN_BASE}/juldates.c
   ${JULIAN_BASE}/leapsecs.c
   ${JULIAN_BASE}/parse.c
   ${JULIAN_BASE}/seconds.c
   ${JULIAN_BASE}/tai_et.c
   ${JULIAN_BASE}/utc_tai.c
   ${JULIAN_BASE}/fortran.c
   ${JULIAN_BASE}/rlerrors.c
   ${JULIAN_BASE}/rlmemory.c
)
target_compile_definitions(julian PRIVATE -D__STDC__)

add_library(fjulian
   ${JULIAN_BASE}/fjulian.for
   ${JULIAN_BASE}/fstrings.for
)

find_path(FABM_BASE src/fabm.F90 DOC "Path to FABM source directory.")
set(FABM_FORCED_HOST fvcom)
add_subdirectory(${FABM_BASE}/src fabm)

add_executable(FVCOM_exe
${ROOT}/adcor.F
${ROOT}/adjust2d3d.F
${ROOT}/adjust_ts.F
${ROOT}/advave_edge_gcn.F
${ROOT}/advave_edge_gcy.F
${ROOT}/advection_edge_gcn.F
${ROOT}/advection_edge_gcy.F
${ROOT}/adv_q.F
${ROOT}/adv_s.F
${ROOT}/adv_t.F
${ROOT}/adv_uv_edge_gcn.F
${ROOT}/adv_uv_edge_gcy.F
${ROOT}/allocate_all.F
${ROOT}/baropg.F
${ROOT}/bcond_gcn.F
${ROOT}/bcond_gcy.F
${ROOT}/bcond_ts.F
${ROOT}/brough.F
${ROOT}/calc_vort.F
${ROOT}/cell_area.F
${ROOT}/cntrl_prmtrs.F
${ROOT}/conv_over.F
${ROOT}/coords_n_const.F
${ROOT}/depth_check.F
${ROOT}/depth_grad.F
${ROOT}/edge_len.F
${ROOT}/enkf_ncdio.F
${ROOT}/eqs_of_state.F
${ROOT}/extelpf_edge.F
${ROOT}/extel_edge.F
${ROOT}/external_step.F
${ROOT}/extuv_edge.F
${ROOT}/fct_q2.F
${ROOT}/fct_q2l.F
${ROOT}/fct_s.F
${ROOT}/fct_t.F
${ROOT}/fvcom.F
${ROOT}/genmap.F
${ROOT}/genmap_lsf.F
${ROOT}/genmap_obc.F
${ROOT}/ghostuv.F
${ROOT}/grid_metrics.F
${ROOT}/ice_albedo.F
${ROOT}/ice_atmo.F
${ROOT}/ice_calendar.F
${ROOT}/ice_constants.F
#${ROOT}/ice_coupling.F
${ROOT}/ice_domain.F
${ROOT}/ice_fileunits.F
${ROOT}/ice_flux.F
${ROOT}/ice_flux_in.F
${ROOT}/ice_grid.F
${ROOT}/ice_init.F
${ROOT}/ice_itd.F
${ROOT}/ice_itd_linear.F
${ROOT}/ice_kinds_mod.F
${ROOT}/ice_mechred.F
${ROOT}/ice_model_size.F
${ROOT}/ice_ocean.F
${ROOT}/ice_scaling.F
${ROOT}/ice_state.F
${ROOT}/ice_therm_itd.F
${ROOT}/ice_therm_vertical.F
${ROOT}/ice_work.F
${ROOT}/icing.F
${ROOT}/init_sed.F
${ROOT}/internal_step.F
${ROOT}/linklist.F
${ROOT}/load_grid.F
${ROOT}/longshore_flow.F
${ROOT}/mod_action_ex.F
${ROOT}/mod_action_im.F
${ROOT}/mod_assim.F
${ROOT}/mod_balance_2d.F
${ROOT}/mod_bbl.F
${ROOT}/mod_bio_3D.F
${ROOT}/mod_boundschk.F
${ROOT}/mod_bulk.F
${ROOT}/mod_clock.F
${ROOT}/mod_dam.F
${ROOT}/mod_dye.F
${ROOT}/mod_enkf.F
${ROOT}/mod_enkfassim.F
${ROOT}/mod_enkf_ncd.F
${ROOT}/mod_enkf_obs.F
${ROOT}/mod_etkf.F
${ROOT}/mod_fabm_3D.F
${ROOT}/mod_force.F
${ROOT}/mod_gotm.F
${ROOT}/mod_heatflux.F
${ROOT}/mod_heatflux_gl.F
${ROOT}/mod_ice.F
${ROOT}/mod_ice2d.F
${ROOT}/mod_input.F
${ROOT}/mod_interp.F
${ROOT}/mod_lag.F
${ROOT}/mod_main.F
${ROOT}/mod_main_wave.F
${ROOT}/mod_meanflow.F
${ROOT}/mod_ncdio.F
${ROOT}/mod_ncll.F
${ROOT}/mod_nctools.F
${ROOT}/mod_nesting.F
${ROOT}/mod_newinp.F
${ROOT}/mod_non_hydro.F
${ROOT}/mod_northpole.F
${ROOT}/mod_obcs.F
${ROOT}/mod_obcs2.F
${ROOT}/mod_obcs3.F
${ROOT}/mod_onedtide.F
${ROOT}/mod_optimal_interpolation.F
${ROOT}/mod_par.F
${ROOT}/mod_petsc.F
${ROOT}/mod_plbc.F
${ROOT}/mod_prec.F
${ROOT}/mod_probe.F
${ROOT}/mod_pwp.F
${ROOT}/mod_report.F
${ROOT}/mod_rrk.F
${ROOT}/mod_rrkassim.F
${ROOT}/mod_rrkf_obs.F
${ROOT}/mod_scal.F
${ROOT}/mod_sed.F
${ROOT}/mod_semi_implicit.F
${ROOT}/mod_setup.F
${ROOT}/mod_set_time.F
${ROOT}/mod_sng.F
${ROOT}/mod_solar.F
${ROOT}/mod_spherical.F
${ROOT}/mod_startup.F
${ROOT}/mod_station_timeseries.F
${ROOT}/mod_time.F
${ROOT}/mod_tridiag.F
${ROOT}/mod_types.F
${ROOT}/mod_utils.F
${ROOT}/mod_visit.F
${ROOT}/mod_wavesetup.F
${ROOT}/mod_wave_current_interaction.F
${ROOT}/mod_wd.F
${ROOT}/mod_wqm.F
${ROOT}/namelist.F
${ROOT}/nh_set_nesting.F
${ROOT}/ocpcre.F
${ROOT}/ocpids.F
${ROOT}/ocpmix.F
${ROOT}/open_all.F
${ROOT}/particle.F
${ROOT}/phy_baropg.F
${ROOT}/print_vals.F
${ROOT}/rho_pmean.F
#${ROOT}/sectinf.F
${ROOT}/setup_domain.F
${ROOT}/shape_coef_gcn.F
${ROOT}/shape_coef_gcy.F
${ROOT}/sinter.F
${ROOT}/startup_type.F
${ROOT}/swancom1.F
${ROOT}/swancom2.F
${ROOT}/swancom3.F
${ROOT}/swancom4.F
${ROOT}/swancom5.F
${ROOT}/swanmain.F
${ROOT}/swanpre1.F
${ROOT}/swanpre2.F
${ROOT}/swanser.F
${ROOT}/swmod1.F
${ROOT}/swmod2.F
${ROOT}/swmod3.F
${ROOT}/tge.F
${ROOT}/vdif_q.F
${ROOT}/vdif_ts.F
${ROOT}/vdif_ts_gom.F
${ROOT}/vdif_uv.F
${ROOT}/vertvl_edge.F
${ROOT}/viscofh.F
${ROOT}/visitsim.F
${ROOT}/wreal.F
)
if(MSVC)
  # Force Visual Studio to interpret .F as Fortran 90
  target_compile_options(FVCOM_exe PRIVATE /free)
endif()
target_include_directories(FVCOM_exe PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/fabm/modules)
target_link_libraries(FVCOM_exe julian fjulian)
target_link_libraries(FVCOM_exe fabm)
target_link_libraries(FVCOM_exe FVCOM_C)
target_link_libraries(FVCOM_exe "${NetCDF_LIBRARIES}")
if (NetCDF_STATIC_MSVC_BUILD)
  set_property(TARGET FVCOM_exe PROPERTY LINK_FLAGS_DEBUG "/NODEFAULTLIB:\"libcmt\"")
endif()