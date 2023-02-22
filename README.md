FVCOM
-----

The main FVCOM website is http://fvcom.smast.umassd.edu/fvcom.

This repository is for the UK FVCOM Users' code development.

This branch is a work in progress merging of FVCOM441 from http://code.fvcom.org/medm/fvcom441 

Register
--------

Users of FVCOM are encouraged to  [register first](http://fvcom.smast.umassd.edu/wp-login.php?action=register) with Dr Changsheng Chen at the University of Massachusetts School of Marine Science. Users must agree to the [licence terms](FVCOM_source/LICENCE).

Download
--------

Once registered, the official releases of FVCOM are available for download from http://code.fvcom.org/medm


Support
-------

The official FVCOM Forum can be accessed at http://fvcom.smast.umassd.edu/bbs/. There is another (user written) wiki available at https://wiki.fvcom.pml.ac.uk.

The examples bundled with the official FVCOM release can be found in this gitlab 
```bash 
git clone git@gitlab.ecosystem-modelling.pml.ac.uk:fvcom/fvcom-examples.git ./fvcom-examples
```


History
----------
2021-10-05 - Initial upload to branch FVCOM441
2021-10-05 - compilation check in Ceto (PML HPC) : No changes done to code. Make.inc copied directly from FVCOM43 code. Libs compiled in Ceto from code in FVCOM43 from 2016. It required addition of NML_ML in namelist file and USE_NETCDF4 flag. 
2021-10-05 - test simulation of Lake Erie setup



FABM-ERSEM
----------

The main FABM website is http://fabm.sourceforge.net.

The ERSEM biogeochemical model can be requested from the Shelf Seas Biogeochemistry website: http://www.shelfseasmodelling.org.

Development of FABM coupling for the ERSEM biogeochemical model is taking place in this repository, under the [FABM-ERSEM](https://gitlab.ecosystem-modelling.pml.ac.uk/fvcom/uk-fvcom/tree/FABM-ERSEM) branch.

We always aim to maintain this branch in sink with the latest FVCOM release. FVCOM4.3 is now included in [FABM-ERSEM 4.3](https://gitlab.ecosystem-modelling.pml.ac.uk/fvcom/uk-fvcom/tree/FABM-ERSEM_v4.3) branch. This branch includes all the developments that have been carried out by partners of the uk-FVCOM group.


EnergyVeg Branch
----------------

This has additional elements to represent vertically resolved momentum sink which has been used to represent tidal turbines and mangrove forests.

As used in De Dominicis et al 2023 https://doi.org/10.1038/s43247-022-00672-7 
