FVCOM
-----

The main FVCOM website is http://fvcom.smast.umassd.edu/fvcom.

This repository is for the UK FVCOM Users' code development and includes some changes with respect to the official v4.3 release contributed by a variety of partners. 
See the commit history for a flavour of those updates. 

Register
--------

Users of FVCOM must [register first](http://fvcom.smast.umassd.edu/wp-login.php?action=register) with Dr Changsheng Chen at the University of Massachusetts School of Marine Science. Users must agree to the [licence terms](FVCOM_source/LICENCE).

Download
--------

Once registered, the official releases of FVCOM are available for download as tarballs from http://fvcom.smast.umassd.edu/download.

Support
-------

The official FVCOM Forum can be accessed at http://fvcom.smast.umassd.edu/bbs/. There is a wiki available at https://wiki.fvcom.pml.ac.uk.

FABM
----------

The main FABM website is https://github.com/fabm-model/fabm/. While this branch has been updated to work with FABM v1 it has not been tested in a realistic application. Consider this branch a beta version. 

FABM-ERSEM
----------

The ERSEM biogeochemical model can be requested from the Shelf Seas Biogeochemistry website: http://www.shelfseasmodelling.org.

## Todo list:

- [x] Remove vectorised advection from FABM coupler 
- [ ] Enable combined nesting and OBC approach
- [ ] Look at river_dilution behaviour (non conservation when non dilution is active)
- [ ] Enable interaction between sediments and spectrally resolved light
- [ ] Remove any hard coded links between sediments and FABM that are domain specific
- [ ] Identify and solve offline conservation issues on first time step. 

## Fixes Log:

21/09/2020 e7eceb0e621b3eb565737027f6991d2a4959342c Remove option for vectorised advection in FABM coupler as we have not seen performance benefits yet. If it needs bringing back with newer chips it can be rescued from FABM-ERSEM_v4.3 branch. 

14/07/2020 aafa66ee0f5ccbbcfccf8d5f325a983b9dde7090 Last update towards FABM v1 compatibililty

03/06/2020 f25bd36fa077f01b8fb50a1c7c5aabc16da1e9e8 Changes associated with offline capabilities. These include commits from 16/03/2020 db4fd01f015eb7690fa6560620b26cddf2d328a1. Re-organisation of statements when offline forcing data is read. Addition to offline file object of other required variables used in the advection and OBC routines. These include such variables such as DTFA (adjusted depth for mass conservation), UARD_OBCN (for determining direction of normal velocity to the OBC elements). 

16/03/2020 28f5e82647e841cd64b765cdc476bc4bb9ee2c0b. Change of order in which Nesting update is called with respect to OBC call in FABM_UPDATE. 

02/05/2019 c0d500159c7f6ce69645484cc8a8545e51566ec6 Addition of FABM coupler to released FVCOM version 4.3 downloaded from UMASSD gitlab server. Check commit history for details of all changes in V4.3. 

30/04/2019 0945742489131d41c51ab6b7a1dbfba945aff345 Addition of Akvaplan/NIVA fixes to wetting/drying. Consist of calculation of pressure gradients under long dry periods. These became apparent when modelling lake URMIA over timescales of decades as the lake underwent irreversible drying. 

22/01/2019 277076608c45dac75446c3802b8ee3e91e72023a Merged of Akvaplan TVD advection and observed ice model to FABM coupler. 

21/12/2018 d5f4cb0b04645d6e28bc0f16ee30b8e096b5c64f Merged of FABM-ERSEM branch into FABM-ERSEM_v4.3

19/09/2018 595236a260c87168b45d7018687948012ee4e342 Addition of sed_frac variable to sediment restart netcdf file. 







