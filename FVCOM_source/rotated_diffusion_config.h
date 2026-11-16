/* Phase A supports fixed wet Cartesian/spherical geometry, explicit tracer time stepping,
 * both standard and MPDATA advection, and serial/MPI assembly. */
#if defined(ROTATED_DIFFUSION_FLUX)
#if defined(WET_DRY) || defined(THIN_DAM) || defined(ONE_D_MODEL) || defined(TWO_D_MODEL) || defined(NH) || defined(SEMI_IMPLICIT) || defined(ICESHELF)
#error ROTATED_DIFFUSION_FLUX: unsupported geometry or time stepping; see docs/rotated_diffusion.md
#endif
#endif
