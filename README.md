FVCOM
-----

The main FVCOM website is http://fvcom.smast.umassd.edu/fvcom.

This repository is for the UK FVCOM Users' code development.

Current version has been updated to include the official 4.42 version from http://code.fvcom.org/medm/fvcom441
There are a number of changes in 441, particularly with respect to advection schemes that require some additions to the namelists. 
There is a new RK for 3D solution. This has not been implemented in FABM. A movement in that direction would require testing the MPDATA/TVD, Semi-implicit,  standard scheme and the RK_3D option to determine a cost-benefit analysis. 
Other updates: [for a more in-depth list check the Doc folder]
SST Assimilation treatment of MLD
Updates to MPI IO (maybe now it works!)
Use Netcdf4 without compression.

Register
--------

Users of FVCOM must [register first](http://fvcom.smast.umassd.edu/wp-login.php?action=register) with Dr Changsheng Chen at the University of Massachusetts School of Marine Science. Users must agree to the [licence terms](FVCOM_source/LICENCE).

Download
--------

Once registered, the official releases of FVCOM are available for download from http://fvcom.smast.umassd.edu/download.

Support
-------

The official FVCOM Forum can be accessed at http://fvcom.smast.umassd.edu/bbs/. There is another (user written) wiki available at https://wiki.fvcom.pml.ac.uk.

The examples bundled with the official FVCOM release can be found in this gitlab 
```bash 
git clone git@gitlab.ecosystem-modelling.pml.ac.uk:fvcom/fvcom-examples.git ./fvcom-examples
```



FABM-ERSEM
----------

The main FABM website is http://fabm.sourceforge.net.

The ERSEM biogeochemical model can be requested from the Shelf Seas Biogeochemistry website: http://www.shelfseasmodelling.org.

Development of FABM coupling for the ERSEM biogeochemical model is taking place in this repository, under the [FABM-ERSEM](https://gitlab.ecosystem-modelling.pml.ac.uk/fvcom/uk-fvcom/tree/FABM-ERSEM) branch.

We always aim to maintain this branch in sink with the latest FVCOM release. FVCOM4.3 is now included in [FABM-ERSEM 4.3](https://gitlab.ecosystem-modelling.pml.ac.uk/fvcom/uk-fvcom/tree/FABM-ERSEM_v4.3) branch. This branch includes all the developments that have been carried out by partners of the uk-FVCOM group.

LOG
-------
18-10-2021 c9e464f9e4440d1bba Akvaplan elevation gradient in wetting and drying treatment. rho_pmean.F (double precission broadcasting)
18-10-2021 c9e464f9e4440d1bba mod_obcs.F to include Jianzhong Ge fixes for real time surface tidal elevations with eleven harmonics. Updates from Jianzhong to CSTMS original (bed thickness limits, thresholds to erodability, vertical settling options [i.e. hindered], semi_implicit for FABM. Kept spatially variable CBCMIN from Jianzhong Ge. 
15-10-2021 f3e3840ba96cc Nullifying and initialising all variables that weren't included in the latest version
15-10-2021 be7cb30853f7d Updates to include FABM on bcond_gcn/bcond_gcy, internal_step, mod_main, mod_input, mod_force,  cntrl_prmtrs(real time used in tidal forcing), left out Karsten H changes to COARE26z, WET_DRY on extel_edge.F,  
14-10-2021 f3e3840ba96cc7 Starting to merge with FABMv1_v43 to transfer uk-fvcom changes and FABM coupler. 



TO CHECK
----------
That corrections in mod_onetide.F make sense. 
SEDIMENT MIXING RATIO... 
Original CSTMS --- addition of thickness threshold and Fixed layer proximity factor. 
heat flux parameters from ROSA and intertidal areas (in vdif_ts.F) have not been transferred across to this version. 
