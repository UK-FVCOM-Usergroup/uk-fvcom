!
!            REAL, INTEGER FOR HQI SEDIMENT MODEL
!                                      (RCA VERSION 3.0)
!
      MODULE MOD_SED
      IMPLICIT NONE
      REAL,ALLOCATABLE,DIMENSION(:,:) ::                              &
        CPOP,FLXPOP,CPON,FLXPON,CPOC,FLXPOC
      REAL,ALLOCATABLE,DIMENSION(:) ::                                &
        CPOS,FLXPOS,FLXPO4T2S,FLXSIT2S,CTEMP,CDTEMP,BSVOL             &
       ,HSED,CCH4,CSO4,VSED,VPMIX,VDMIX,M1,M2,JDIAGP,JDIAGN           &
       ,JDIAGC,JDIAGS,PPFWS,PNFWS,PCFWS,PSFWS,SSFWS                   &
       ,PIE1SI,PIE2SI,PIE1PO4M,PIE1PO4N

      REAL      FRPPH1(3),     FRPPH2(3),    FRPPH3(3),    FRPOP(3),  &
                FRNPH1(3),     FRNPH2(3),    FRNPH3(3),    FRPON(3),  &
                FRCPH1(3),     FRCPH2(3),    FRCPH3(3),    FRPOC(3)
      REAL      RESTART_IN,FLXPO4T2,FLXSIT2

!______________________________________________________________________
!- Input parameters
      real ::                                                            &
           m1g,m2g,Dp,w2,Dd,thtaDp,thtaDd,                               &
           Dd0,thtaDd0,                                                  &
           kappnh4f,kappnh4s,pienh4,thtanh4,kmnh4,thtakmnh4,kmnh4o2,     &
           kapp1no3f,kapp1no3s,k2no3f,k2no3s,thtano3,                    &
           kappd1,kappp1,pie1s,pie2s,thtapd1,kmhso2,                     &
           ksi,csisat,pie1sig,pie2sig,h20,thtasi,kmpsi,o2critsi,jsidetr, &
           pie1po4mg,pie1po4ng,o2crit,kmo2Dp,tempbnth,                   &
           kbnthstr,klbnth,Dpmin,                                        &
           kappch4,thtach4,                                              &
           kmch4o2,kmso4,                                                &
           kpdiag(3),dpthta(3),                                          &
           kndiag(3),dnthta(3),                                          &
           kcdiag(3),dcthta(3),                                          &
           thtanh4s,thtanh4f,thtano3s,thtano3f
!______________________________________________________________________
!- concentrations
      REAL,ALLOCATABLE,DIMENSION(:) ::                                   &
        pon1tm1s,pon2tm1s,pon3tm1s,poc1tm1s,poc2tm1s,poc3tm1s,           &
        pop1tm1s,pop2tm1s,pop3tm1s,psitm1s,bnthstr1s,bformaxs,iswbnths

      REAL,ALLOCATABLE,DIMENSION(:) ::                                   &
          nh41tm1s,nh4t1tm1s,nh4t2tm1s,                                  &
          no31tm1s,no3t1tm1s,no3t2tm1s,                                  &
          hs1tm1s,hst1tm1s,hst2tm1s,                                     &
          si1tm1s,sit1tm1s,sit2tm1s,                                     &
          po41tm1s,po4t1tm1s,po4t2tm1s,                                  &
          ch41tm1s,ch4t2tm1s,ch4t1tm1s,                                  &
          so41tm1s,so4t2tm1s
      real ::                                                            &   
          nh40,nh41,nh42,nh42av,nh4t1,nh4t2,nh4t2av,                     &
          nh41tm1,nh4t1tm1,nh4t2tm1,jnh4,nh4t2sav,                       &
          no30,no31,no32,no32av,no3t1,no3t2,no3t2av,                     &
          no31tm1,no3t1tm1,no3t2tm1,jno3,no3t2sav,                       &
          hs0,hs1,hs2,hs2av,hst1,hst2,hst2av,                            &
          hs1tm1,hst1tm1,hst2tm1,jhs,hst2sav,                            &
          si0,si1,si2,si2av,sit1,sit2,sit2av,                            &
          si1tm1,sit1tm1,sit2tm1,jsi,sit2sav,                            &
          po40,po41,po42,po42av,po4t1,po4t2,po4t2av,                     &
          po41tm1,po4t1tm1,po4t2tm1,jpo4,po4t2sav,                       &
          jch4aq,jch4g,                                                  &
          ch40,     ch41,      ch42,                                     &
          ch42av,   ch4t1,     ch4t2,    ch4t2av,  ch41tm1,              &
          ch4t2tm1, jch4,      ch4t2sav, jch4gaso2,                      &
          jgas,     so40,      so41,     so42,                           &
          so4t1,    so4t2,     so4t2av,  so40mg,   so41tm1,              &
          so4t2tm1, jso4,      so4t2sav

!______________________________________________________________________
!- diagenesis
      real ::                                                           &
        pon1,pon1tm1,pon2,pon2tm1,pon3,pon3tm1,                         &
        poc1,poc1tm1,poc2,poc2tm1,poc3,poc3tm1,                         &
        pop1,pop1tm1,pop2,pop2tm1,pop3,pop3tm1,                         &
        pon1av,pon2av,pon3av,                                           &
        poc1av,poc2av,poc3av,                                           &
        pop1av,pop2av,pop3av,                                           &
        jnin,xjn,jn,jcin,xjc,jc,jpin,xjp,jp,                            &
        psi,psitm1,psisav,psiav,jsiin,                                  &
        xjcno3,xjco2,xjc1,jo2nh4,xjco2av,xjc1av

!______________________________________________________________________
!- n layer arguments
      real ::                                                           &
          k0h1d,k0h1p,k1h1d,k1h1p,k2h2d,k2h2p,k3,                       &
          pie1,pie2,j1,j2,kmc1,w12,kl12,tempd,o20,                      &
          ch4sat,sal0

!______________________________________________________________________
!- h1 variables
      REAL :: h1,h1dot,h2,h2dot,sodtm1,o20tm1,Dd0tm1,Dd0o20dot
      REAL,ALLOCATABLE,DIMENSION(:) :: sodtm1s,o20tm1s,Dd0tm1s

!______________________________________________________________________
!- time and space parameters
      integer :: ised,nsedlyr
      real :: saltsw,deltat,deltatin,xnsedlyr,deltaz,delta

!______________________________________________________________________
!- sedf storage
      integer :: isw12,id,ibnth,iswbnth 
      real ::                                                          &
           xappnh4,xkmnh4,xappd1,xappp1,xapp1no3,xk2no3,xDd0,          &
           xksi,temp20,temp202,xkjsi,                                  &
           fd1,fp1,fd2,fp2,sod,csod,s,kl12nom,                         &
           kl12so4,csodhs,csodch4,hso4,ddso4,vjch4g,                   &
           fp1so4,fp2so4,khs_1,dch4t2,dhst2,fluxhs,                    &
           w12nom,w12decay,w12min,tw12,bnthstr,bnthstrs,bnthstr1,      &
           bformax,vjch4

!_______________________________________________________________________
!- theta dependency storage
      REAL, DIMENSION(350) ::                                          &
           zhtaDd0,zhtanh4s,zhtanh4f,zhtakmnh4,                        &
           zhtano3s,zhtano3f,zhta2no3s,zhta2no3f,                      &
           zhtad1,zhtap1,zhtasi,                                       &
           zl12nom,zw12nom,                                            &
           zhtapon1,zhtapon2,zhtapon3,                                 &
           zhtapoc1,zhtapoc2,zhtapoc3,                                 &
           zhtapop1,zhtapop2,zhtapop3,                                 &
           zhtach4,zhtajsi

!_______________________________________________________________________
!- sediment mass balances
      real*8 ::                                                        &
           totorgp,totorgn,totorgc,totbsi                              &
          ,totpo4,totnh4,totno3,totsi,toth2s,totch4,totso4             &
          ,jpop,jpon,jpoc,jbsi,jpo4ss,jsiss                            &
          ,burialorgp,burialorgn,burialorgc,burialbsi                  &
          ,burialpo4,burialnh4,burialno3,burialsi                      &
          ,burialh2s,burialch4,burialso4                               &
          ,totjpo4,totjnh4,totjno3,totjn2,totjsi                       &
          ,totjh2s,totjch4aq,totjch4g,totsod

      END MODULE MOD_SED
 
