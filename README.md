# FVCOM


The main FVCOM website is http://fvcom.smast.umassd.edu/fvcom.

This repository is for the UK FVCOM Users' code development.

# Register


Users of FVCOM must [register first](http://fvcom.smast.umassd.edu/wp-login.php?action=register) with Dr Changsheng Chen at the University of Massachusetts School of Marine Science. Users must agree to the [licence terms](FVCOM_source/LICENCE).

# Download


Once registered, the official releases of FVCOM are available for download as tarballs from http://fvcom.smast.umassd.edu/download.

## Support


The official FVCOM Forum can be accessed at http://fvcom.smast.umassd.edu/bbs/. There is a wiki available at https://wiki.fvcom.pml.ac.uk.

## FABM


The main FABM website is https://github.com/fabm-model/fabm. This FVCOM branch requires FABM version v0.96 (https://github.com/fabm-model/fabm/tree/v0.96.0). Version v1 of FABM is being introduced in FVCOM in branch [FABMv1-v4.3](https://gitlab.ecosystem-modelling.pml.ac.uk/fvcom/uk-fvcom/-/tree/FABMv1_v4.3).  


## FABM-ERSEM

The ERSEM biogeochemical model can be requested from the Shelf Seas Biogeochemistry website: http://www.shelfseasmodelling.org.

Initial development of FABM coupling for the ERSEM biogeochemical model started in this repository, under the [FABM-ERSEM](https://gitlab.ecosystem-modelling.pml.ac.uk/fvcom/uk-fvcom/tree/FABM-ERSEM) branch.

This branch is diverging from FABMv1_v4.3 and they will need to be merged soon.
## Todo list:
- [x] Add horizontal turbulence variable to offline_file object
- [x] Remove vectorised advection from FABM coupler 
- [x] Enable combined nesting and OBC approach
- [x] Look at river_dilution behaviour (non conservation when non dilution is active)
- [ ] Enable interaction between sediments and spectrally resolved light
- [ ] Remove any hard coded links between sediments and FABM that are domain specific
- [ ] Identify and solve offline conservation issues on first time step. 

## Fixes Log:
25/04/2021 9d54af025b125a3f173487722cebc1398968754b Added horizontal turbulence to offline file object

12/01/2021 a71806eddca992d07cc3038bd343c14cd6c95c3c Moved river dilution logic of FABM variables to the advection routine

01/12/2020 8b640af007b866afa16cf525236714222d6bbabe Combined nesting and OBC approaches for subsets of FABM variables. It can ran with i.e. direct nesting of some FABM variables while using zero gradient at boundaries for others. 

21/09/2020 e7eceb0e621b3eb565737027f6991d2a4959342c Remove option for vectorised advection in FABM coupler as we have not seen performance benefits yet. If it needs bringing back with newer chips it can be rescued from FABM-ERSEM_v4.3 branch. 

14/07/2020 aafa66ee0f5ccbbcfccf8d5f325a983b9dde7090 Last update towards FABM v1 compatibililty

03/06/2020 f25bd36fa077f01b8fb50a1c7c5aabc16da1e9e8 Changes associated with offline capabilities. These include commits from 16/03/2020 db4fd01f015eb7690fa6560620b26cddf2d328a1. Re-organisation of statements when offline forcing data is read. Addition to offline file object of other required variables used in the advection and OBC routines. These include such variables such as DTFA (adjusted depth for mass conservation), UARD_OBCN (for determining direction of normal velocity to the OBC elements). 

16/03/2020 28f5e82647e841cd64b765cdc476bc4bb9ee2c0b. Change of order in which Nesting update is called with respect to OBC call in FABM_UPDATE. 

02/05/2019 c0d500159c7f6ce69645484cc8a8545e51566ec6 Addition of FABM coupler to released FVCOM version 4.3 downloaded from UMASSD gitlab server. Check commit history for details of all changes in V4.3. 

30/04/2019 0945742489131d41c51ab6b7a1dbfba945aff345 Addition of Akvaplan/NIVA fixes to wetting/drying. Consist of calculation of pressure gradients under long dry periods. These became apparent when modelling lake URMIA over timescales of decades as the lake underwent irreversible drying. 

22/01/2019 277076608c45dac75446c3802b8ee3e91e72023a Merged of Akvaplan TVD advection and observed ice model to FABM coupler. 

21/12/2018 d5f4cb0b04645d6e28bc0f16ee30b8e096b5c64f Merged of FABM-ERSEM branch into FABM-ERSEM_v4.3

