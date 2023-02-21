!OPTION CROSSREF ON
!OPTION SYMTABLE ON
!PAGEWIDTH 132
      SUBROUTINE DIAGEN
!-- Diagenesis conputation
!-    frpon1 = G1 fraction of jnin
!-    frpon2 = G2 fraction of jnin
!-    frpon3 = G3 fraction of jnin
!-    pon1(nl) = G1 pon concentration
!-    pon2(nl) = G2 pon concentration
!-    pon3(nl) = G3 pon concentration
!-    kpon1 = G1 reaction rate
!-    kpon2 = G2 reaction rate
!-    kpon3 = G3 reaction rate
!-    thtapon1 = theta for kpon1
!-    thtapon2 = theta for kpon2
!-    thtapon3 = theta for kpon3
!-- Similarly for poc and pop
!-    psi(nl) = biogenic si
!-    ksi = specific reaction rate for dissolution
!-    thtaksi = theta for ksi

      USE MOD_RCA
      USE MOD_SED
      IMPLICIT NONE
      SAVE                                                              
!LIST OFF
!      INCLUDE 'RCACM'
!      INCLUDE 'SEDCM'
!LIST ON

!JQI      REAL
!JQI     .     kpop1  , kpop2  , kpop3
!JQI     .   , kpon1  , kpon2  , kpon3
!JQI     .   , kpoc1  , kpoc2  , kpoc3
!JQI     .   , thtapop1,thtapop2,thtapop3
!JQI     .   , thtapon1,thtapon2,thtapon3
!JQI     .   , thtapoc1,thtapoc2,thtapoc3
!JQI      equivalence
!JQI     .     (kpdiag(1),kpop1) , (kpdiag(2),kpop2) , (kpdiag(3),kpop3)
!JQI     .   , (kndiag(1),kpon1) , (kndiag(2),kpon2) , (kndiag(3),kpon3)
!JQI     .   , (kcdiag(1),kpoc1) , (kcdiag(2),kpoc2) , (kcdiag(3),kpoc3)
!JQI     .  , (dpthta(1),thtapop1),(dpthta(2),thtapop2),(dpthta(3),thtapop3)
!JQI     .  , (dnthta(1),thtapon1),(dnthta(2),thtapon2),(dnthta(3),thtapon3)
!JQI     .  , (dcthta(1),thtapoc1),(dcthta(2),thtapoc2),(dcthta(3),thtapoc3)

      integer itemp,iprt
      real :: xkpop1,xkpop2,xkpop3,xkpon1,xkpon2,xkpon3, xkpoc1,xkpoc2,xkpoc3
      REAL csodmx,sechxc   !Tian
     
      itemp = 10.*tempd + 1
      xkpop1=zhtapop1(itemp)*deltaz
      xkpop2=zhtapop2(itemp)*deltaz
      xkpop3=zhtapop3(itemp)*deltaz
      xkpon1=zhtapon1(itemp)*deltaz
      xkpon2=zhtapon2(itemp)*deltaz
      xkpon3=zhtapon3(itemp)*deltaz
      xkpoc1=zhtapoc1(itemp)*deltaz
      xkpoc2=zhtapoc2(itemp)*deltaz
      xkpoc3=zhtapoc3(itemp)*deltaz
      xksi=zhtasi(itemp)*deltaz
      xkjsi=zhtajsi(itemp)

!- pon1
      pon1=(flxpon(IXY,1)*deltat/h20+pon1tm1)/(1.0+(xkpon1+w2)*deltat/h20)
      pon1av=pon1

!- pon2
      pon2=(flxpon(IXY,2)*deltat/h20+pon2tm1)/(1.0+(xkpon2+w2)*deltat/h20)
      pon2av=pon2

!- pon3
      pon3=(flxpon(IXY,3)*deltat/h20+pon3tm1)/(1.0+(xkpon3+w2)*deltat/h20)
      pon3av=pon3

      iprt=0

!- jn
      xjn=xkpon1*pon1+xkpon2*pon2+xkpon3*pon3
      jn=xjn

!- poc1
      poc1=(flxpoc(IXY,1)*deltat/h20+poc1tm1)/(1.0+(xkpoc1+w2)*deltat/h20)
      poc1av=poc1

!- poc2
      poc2=(flxpoc(IXY,2)*deltat/h20+poc2tm1)/(1.0+(xkpoc2+w2)*deltat/h20)
      poc2av=poc2

!- poc3
      poc3=(flxpoc(IXY,3)*deltat/h20+poc3tm1)/(1.0+(xkpoc3+w2)*deltat/h20)
      poc3av=poc3
      iprt=0

!- jc
      xjc=xkpoc1*poc1+xkpoc2*poc2+xkpoc3*poc3
      jc=xjc

!- pop1
      pop1=(flxpop(IXY,1)*deltat/h20+pop1tm1)/(1.0+(xkpop1+w2)*deltat/h20)
      pop1av=pop1

!- pop2
      pop2=(flxpop(IXY,2)*deltat/h20+pop2tm1)/(1.0+(xkpop2+w2)*deltat/h20)
      pop2av=pop2

!- pop3
      pop3=(flxpop(IXY,3)*deltat/h20+pop3tm1)/(1.0+(xkpop3+w2)*deltat/h20)
      pop3av=pop3

      iprt=0

!- jp
      xjp=xkpop1*pop1+xkpop2*pop2+xkpop3*pop3
      jp=xjp

!- psi
!     Reaction kinetics
!     d psi/ dt  = -xksi*psi/(psi+kmpsi)*(csisat-si2) + jsiin + jsidetr
!
!     Two sources: jsiin computed from C/Si ratio, and jsidetr, detritial
!     source of Si.

      fd2=1.0/(1.0+m2(ixy)*pie2si(ixy))
      k3=xksi*(csisat-fd2*sit2tm1)/(psitm1+kmpsi)
      psi=((flxpos(IXY)+jsidetr)*deltat/h20+psitm1)/(1.0+(k3+w2)*deltat/h20)

      iprt=0
!d     if(idisk.ge.1)  iprt=0
!d     ixp=4
!d     iyp=6

!d       if(iprt.eq.1.and.IX.eq.ixp.and.IY.eq.iyp)then
!d         print *,'DIAGEN ix,iy',ix,iy
!d         print *,'    '
!d         print *,'psi,flxpos,deltat,h2,psitm1,k3,w2'
!d         print *, psi,flxpos(ix,iy),deltat,h2,psitm1,k3,w2 
!d         print *,'    '
!d       endif

      psiav=psi
      
      RETURN
      END SUBROUTINE DIAGEN

      FUNCTION SEDF(SOD1,IUSE)

      USE MOD_RCA
      USE MOD_SED
      IMPLICIT NONE
      SAVE                                                              
!LIST OFF
!      include 'RCACM'
!      include 'SEDCM'
      REAL*8 AD(4,4), BX(4), G(2), HX(2,2)                       !CH4-SO4 Code
      REAL*8 DBLSO41, DBLSO42, RA0, RA1, RA2, R1, R2, DISC, SN1  !CH4-SO4 Code
      REAL SEDF,SOD1
      INTEGER IUSE
      INTEGER IPRT,ITEMP
      REAL XAPPCH4,BFOR,BNTH,A0,A1,A2,XJCNO31,SO42AV,XJ2,XJ2CH4,X1J2      &
          ,CH4T1TM1,FLUXHSCH4,SODERR,PSOD,DSOD,SODJHS,SODPLD,SODPLP
      REAL csodmx,sechxc  !Tian	  
!LIST ON
      SEDF = 0. 
      iprt=0
!d     if(idisk.ge.1)  iprt=0
!d     ixp=4
!d     iyp=6
!d     if(iprt.eq.1.and.ix.eq.ixp.and.iy.eq.iyp .and. iuse.eq.1)  
!d    .   print *,'  SOD iteration'
!d     if(iprt.eq.1.and.ix.eq.ixp.and.iy.eq.iyp .and. iuse.ne.1) 
!d    .   print *,'  FLUX iteration'

!-- if iuse=1 then this is an SOD iteration
!   if iuse<>1 then this is a full evaluation of all the fluxes

      if(iuse.eq.0) then
!-- Temperature corrections
      itemp = 10.*tempd + 1
      xkmnh4=zhtakmnh4(itemp)
      xappd1=zhtad1(itemp)
      xappp1=zhtap1(itemp)

      if(sal0.ge.saltsw)  then
         xappnh4=zhtanh4s(itemp)
         xapp1no3=zhtano3s(itemp)
         xk2no3=zhta2no3s(itemp)*DELTAZ
      else
         xappnh4=zhtanh4f(itemp)
         xapp1no3=zhtano3f(itemp)
         xk2no3=zhta2no3f(itemp)*DELTAZ
      endif

      xksi=zhtasi(itemp)*deltaz
      xDd0=zhtaDd0(itemp)
      xappch4=zhtach4(itemp)
!- layer 1-2 transport
      kl12nom=Dd/deltaz*zl12nom(itemp)
!- include organic carbon term to simulate benthic biomass
      w12nom=Dp/deltaz*zw12nom(itemp)*poc1av/1.0e5
!-- Benthic stress equation
!--           iswbnth=0 -> low temperature period
!--           iswbnth=1 -> high temperature period
      if(iswbnth.eq.0) then
!-- low temp
              if(tempd.ge.tempbnth) then
!-- change to high temp
              iswbnth=1
              bformax=0.0
              endif
         bfor=kmo2Dp/(kmo2Dp+o20)
         else
!-- high temp
              if(tempd.lt.tempbnth) then
!-- change to low temp
              iswbnth=0
              endif
         bformax=amax1(kmo2Dp/(kmo2Dp+o20),bformax)
         bfor=bformax
         endif

         bnthstr=(bnthstr1+deltat*bfor)/(1.0+kbnthstr*deltat)
         bnth=(1.0-kbnthstr*bnthstr)
!-- w12min= Dpmin/deltaz is physical mixing
         w12min= Dpmin/deltaz
         w12=w12nom*bnth + w12min
 
!*-add in the enhancement due to benthic activity
         kl12=kl12nom + klbnth*w12

!-*****
      return
      endif


!-- Entry for computing the NH4,NO3, and SOD fluxes
      if(iuse.eq.1) then

!-- surface mass transfer
      s = sod1/o20

!-- h1dot evaluation
      h1=xDd0*o20/sod1
!   limit to the full depth
      h1=amin1(h1,h20)
      h2=h20-h1
!        prevent h2 from going to 0
     ! h2=amax1(h2,(0.001*h1)) !Tian should be open,just for to similar to V2
      h1=h20-h2
      Dd0o20dot=(xDd0*o20-Dd0tm1*o20tm1)/deltat
      h1dot=(-h1*(sod1-sodtm1)/deltat + Dd0o20dot)/sod1
      h2dot=-h1dot

!- ammonia flux

      k0h1p=0.
      k1h1p=0.
      k2h2d=0.
      k2h2p=0.
      if(kmnh4.ne.0.0) then
         k0h1d=xappnh4**2/s*xkmnh4*(o20/(kmnh4o2+o20))
         k1h1d=s
      else
         k1h1d=xappnh4**2/s*(o20/(kmnh4o2+o20))+s
         k0h1d=0.
      endif

      j1=s*nh40
      k3=0.0
      j2=jn
      pie1=pienh4
      pie2=pienh4
      kmc1=xkmnh4
!d       if(iprt.eq.1.and.IX.eq.ixp.and.IY.eq.iyp)then
!d          print *,' k0h1d,k1h1d,j1,j2,pie1,pie2,kmc1'
!d          print *,  k0h1d,k1h1d,j1,j2,pie1,pie2,kmc1 
!d       endif

      call sedtsfnl(nh41,nh42,nh42av,nh4t1,nh4t2,nh4t2av,nh41tm1,nh4t1tm1,nh4t2tm1,1)
      jnh4=s*(nh41-nh40)
!d       if(iprt.eq.1.and.IX.eq.ixp.and.IY.eq.iyp)then
!d         print *,' nh41,nh42,nh42av'
!d         print *,  nh41,nh42,nh42av
!d         print *,' nh4t1,nh4t2,nh4t2av'
!d         print *,  nh4t1,nh4t2,nh4t2av 
!d         print *,' nh41tm1,nh4t2tm1'
!d         print *,  nh41tm1,nh4t2tm1 
!d         print *,' jnh4=s*(nh41-nh40)'
!d         print *,  jnh4,s,nh41,nh40
!d       endif

!- Oxygen consumed by nitrification
!        a1 = 64/14 * 1/1000 - mole ratio and mg/m2-day to gm/m2-day
         a1 = 0.004571429
      if(kmnh4.ne.0.0) then
         jo2nh4=a1*k0h1d*nh41/(xkmnh4+nh41tm1)
      else
         jo2nh4=a1*(k1h1d-s)*nh41
      endif

!- denitrification

      k0h1d=0.
      k0h1p=0.
      kmc1=0.0
      k1h1d=xapp1no3**2/s+s
      k1h1p=0.
      k2h2d=xk2no3
      k2h2p=0.
      if(xkmnh4.ne.0.0) then
        j1=s*no30+xappnh4**2/s*xkmnh4*(o20/(kmnh4o2+o20))*nh41/(xkmnh4+nh41tm1)
      else
        j1=s*no30+xappnh4**2/s*(o20/(kmnh4o2+o20))*nh41
      endif
      k3=0.0
      j2=0.0
      pie1=0.
      pie2=0.

      call sedtsfnl(no31,no32,no32av,no3t1,no3t2,no3t2av,no31tm1,no3t1tm1,no3t2tm1,1)
    
      jno3=s*(no31-no30)

!- Sulfide/methane oxidation
!        diagenesis consumed by denitrification
!        a2 = 10/8 * 32/14 * 1/1000
         a2 = 0.00285714
         xjcno31 = a2*xapp1no3**2/s*no31
         xjcno3 = a2*xk2no3*no32
!- add the aerobic and first anaerobic layer to keep mass balance
         xjcno3=xjcno31+xjcno3

!d       if(iprt.eq.1.and.IX.eq.ixp.and.IY.eq.iyp)then
!d         print *,'ixp,iyp,xjcno31,a2,xapp1no3,s,no31'
!d         print *, ixp,iyp,xjcno31,a2,xapp1no3,s,no31 
!d         print *,'ixp,iyp,xjcno3 ,a2,xk2no3,no32'
!d         print *, ixp,iyp,xjcno3 ,a2,xk2no3,no32 
!d       endif

!- Convert carbon diagenesis flux to o2*  units and decrement jco2

!        a0= 32./12./1000.
         a0=2.666666666e-3
         xjco2av=0.0
         xjc1av=0.0
         xjco2 = a0*jc
         xjc1 = amax1(xjco2-xjcno3,0.0)
         xjco2av=xjco2av+xjco2
         xjc1av=xjc1av+xjc1

!d       if(iprt.eq.1.and.IX.eq.ixp.and.IY.eq.iyp)then
!d         print *,'ixp,iyp',ixp,iyp
!d         print *,'xjco2,a0,xjc,xjc1,xjcno3,xjco2av,xjco2av,xjc1av'
!d         print *, xjco2,a0,xjc,xjc1,xjcno3,xjco2av,xjco2av,xjc1av 
!d       endif

!**** **********************************************************
!**** New code for methane formation.  CH4 starts forming
!**** once all sulfate is used up.
!**** **********************************************************

!**** Sulfide and sulfate in O2 equivalents
!**** units: so4 in o2 equivalents
!     SO4 (mg so4/L)* 1 mmol SO4 /98 mg SO4 * 2 mmol O2/ 1 mmol SO4
!     . 32 mg O2 / mmol O2= 0.65306122
!#######################C   FROM VERSION 2 to LINE 426
!c        marine or freshwater  ==>  sulfide or methane formulation
      if(sal0.ge.saltsw)  then
!c           sulfide
         k0h1d=0.
         k0h1p=0.
         kmc1=0.0
         k1h1d=xappd1**2/s*(o20/kmhso2) +s
         k1h1p=xappp1**2/s*(o20/kmhso2)
         k2h2d=0.
         k2h2p=0.
         j1=0.
         k3=0.0
         j2=xjc1
         pie1=pie1s
         pie2=pie2s

!d       if(iprt.eq.1.and.IR.eq.irp.and.IC.eq.icp)then
!d         print *,'irp,icp',irp,icp
!d         print *,'k1h1d,xappd1,s,o20,kmhso2,k1h1p,xappp1,pie1,pie2'
!d         print *, k1h1d,xappd1,s,o20,kmhso2,k1h1p,xappp1,pie1,pie2 
!d       endif

         call sedtsfnl   &
     &      (hs1,hs2,hs2av,hst1,hst2,hst2av,hs1tm1,hst1tm1,hst2tm1,1)
         jhs=s*hs1
         csod=(xappd1**2/s*fd1 + xappp1**2/s*fp1)*(o20/kmhso2)*hst1
!       write(*,*)'sed 390  ',csod,xappd1,fd1,xappp1,
!     &    fp1,o20,kmhso2,hst1,s
         jch4aq=0.0
         jch4g=0.0

!d       if(iprt.eq.1.and.IR.eq.irp.and.IC.eq.icp)then
!d         print *,'irp,icp',irp,icp
!d         print *,'   after sedtsfnl'
!d         print *,'time,hs1,hs2,hs2av'
!d         print *, time,hs1,hs2,hs2av 
!d         print *,'hst1,hst2,hst2av'
!d         print *, hst1,hst2,hst2av 
!d         print *,'hs1tm1,hst1tm1,hst2tm1'
!d         print *, hs1tm1,hst1tm1,hst2tm1 
!d         print *,'jhs,s,hs1,csod,xappd1,fd1,xappp1,fp1,o20,kmhso2,hst1'
!d         print *, jhs,s,hs1,csod,xappd1,fd1,xappp1,fp1,o20,kmhso2,hst1 
!d       endif

      else

!c           methane
         csodmx = sqrt(2.*kl12*ch4sat*xjc1av)
         if(csodmx.gt.xjc1av)   csodmx=xjc1av
         if( (xappch4/s).lt.80)  then
            sechxc = 2./(exp(xappch4/s)+exp(-xappch4/s))
         else
            sechxc = 0.0
         endif
         csod = csodmx*(1.-sechxc)
         jch4aq = csodmx*sechxc
         jch4g = xjc1av - jch4aq - csod
         jhs = 0.0

!d       if(iprt.eq.1.and.IR.eq.irp.and.IC.eq.icp)then
!d         print *,'irp,icp',irp,icp
!d         print *,'csodmx,xjc1av,xappch4,s,sechxc,csod,jch4aq,jch4g'
!d         print *, csodmx,xjc1av,xappch4,s,sechxc,csod,jch4aq,jch4g 
!d       endif

      endif
!######################  END OF CODE FROM VERSION 2
!- SOD function - Evaluate the error
      sod = csod + jo2nh4
      soderr=sod-sod1
      sedf = sod - sod1

 
!d       if(iprt.eq.1.and.IX.eq.ixp.and.IY.eq.iyp)then
!d         print *,'ixp,iyp',ixp,iyp
!d         print *,'sod,csod,jo2nh4,soderr,sod1,sedf'
!d         print *, sod,csod,jo2nh4,soderr,sod1,sedf 
!d       endif

!-- if this is sod iteration then return
      return
      endif

!-- Entry for Silica and PO4 flux

!-  Silica
      if(iuse == 2) then
      k0h1d=0.
      k0h1p=0.
      kmc1=0.0
      k1h1d=s
      k1h1p=0.
      k2h2d=0.
      k2h2p=0.
      j1=s*si0

!-- Oxygen dependency of pie1
      if(o20.lt.o2critsi) then
         pie1=pie2si(ixy)*pie1si(ixy)**(o20/o2critsi)
      else
         pie1=pie2si(ixy)*pie1si(ixy)
      endif
      pie2=pie2si(ixy)
!- Silica dissolution kinetics
!     d psi/ dt  = -xksi*psi/(psi+kmpsi)*(csisat-si2)
!     d sit2/ dt = +xksi*psi/(psi+kmpsi)*csisat
!                  -xksi*psi/(psi+kmpsi)*si2
      fd2=1./(1.+m2(ixy)*pie2)
      k3=xksi*psi/(psitm1+kmpsi)*fd2
      j2=xksi*psi/(psitm1+kmpsi)*csisat + flxsit2

      call sedtsfnl(si1,si2,si2av,sit1,sit2,sit2av,si1tm1,sit1tm1,sit2tm1,1)
      jsi=s*(si1-si0)

!-- Phosphate

      k0h1d=0.
      k0h1p=0.
      kmc1=0.0
      k1h1d=s
      k1h1p=0.
      k2h2d=0.
      k2h2p=0.
      j1=s*po40
      k3=0.0
      j2=jp + flxpo4t2
!-- Oxygen dependency of pie1
      if(o20.lt.o2crit) then
         pie1=pie1po4n(ixy)*pie1po4m(ixy)**(o20/o2crit)
      else
         pie1=pie1po4n(ixy)*pie1po4m(ixy)
      endif
      pie2=pie1po4n(ixy)
   
      call sedtsfnl(po41,po42,po42av,po4t1,po4t2,po4t2av,po41tm1,po4t1tm1,po4t2tm1,1)
      jpo4=s*(po41-po40)

!-- printing for mixing output

      if(ibnth.eq.1)  then
        k1h1d=xappd1**2/s
        k1h1p=xappp1**2/s*(o20/kmhso2)
        psod=w12*(hst2av-hst1)
        dsod=kl12*(hs2av-hs1)
        sodjhs=jhs
        sodpld=sodjhs+dsod
        sodplp=sodpld+psod
      endif
      return

      endif

      return
      END FUNCTION SEDF

      SUBROUTINE SEDTSFNL(C1,C2,C2AV,CT1,CT2,CT2AV,C1TM1,CT1TM1,CT2TM1,ITYPE)
      
      USE MOD_RCA
      USE MOD_SED
      IMPLICIT NONE
      SAVE                                                              
!LIST OFF
!      INCLUDE 'RCACM'
!      INCLUDE 'SEDCM'
!LIST ON
      REAL :: ans(2),c1,c2,c2av,ct1,ct2,ct2av,c1tm1,ct2tm1
      REAL :: F12,F21,XK0,XK1,XK2,H1DOTP,H1DOTM,A11,A12,A21,A22      &
             ,B1,B2,CT1TM1 
      INTEGER :: ITYPE
!-  Notation
!     c0 = olw concentration
!     c1 = layer 1 dissolved conc.
!     c2(i) = layer 2-nsedlyr dissolved conc.
!     c2av = layer 2-nsedlyr average conc. for output
!     ct1 = layer 1 total conc.
!     ct2(i) = layer 2-nsedlyr total conc.
!     ct2av = later 2-nsedlyr average conc. for output
!     c1tm1 = c1 at time level t - deltat
!     ct1tm1 = ct1 at time level t - deltat
!     ct2tm1 = ct2 at time level t - deltat
!     h1 =  aerobic layer depth
!     h2 =  anaerobic layer depth
!     h1dot =  d(h1)dt
!     h2dot =  d(h2)dt
!- For c = nh4,no3,hs,po4,si
!-    if itype=1 then return all variables,
!        else return only ct1 and ct2 - for diagenesis computation
 
      fd1=1./(1.+m1(ixy)*pie1)
      fp1=m1(ixy)*pie1/(1.+m1(ixy)*pie1)
      fd2=1./(1.+m2(ixy)*pie2)
      fp2=m2(ixy)*pie2/(1.+m2(ixy)*pie2)

!- Transport and Decay terms
      f12 = w12*fp1 + kl12*fd1
      f21 = w12*fp2 + kl12*fd2
!     evaluate the MM term at time level t-1
      if(kmc1 /= 0.0) then
       xk0 = (k0h1d*fd1 + k0h1p*fp1)/(kmc1+c1tm1)
      else
       xk0=0.0
      endif
      xk1 = xk0 + k1h1d*fd1 + k1h1p*fp1
      xk2 = k2h2d*fd2 + k2h2p*fp2

!-- aerobic layer displacement flux
      h1dotp=0.5*(h1dot+abs(h1dot))
      h1dotm=-0.5*(h1dot-abs(h1dot))

!-- linear equation coefficients
!     a11 = -f12 -xk1 - w2 + h1dotm - h1dot - h1/deltat
      a11 = -h1dotm -h1dot -h1/deltat -f12 -xk1 -w2
      a12 = f21 + h1dotp
      a21 = f12 + w2 + h1dotm
      b1 = -j1 - h1/deltat*ct1tm1
!     a22= -f21 -xk2 - w2 - k3 -h1dotp -h2dot - h2/deltat
      a22= -h1dotp -h2dot -h2/deltat -f21 -xk2 -w2 -k3
      b2= -j2 - h2/deltat*ct2tm1

!-- Solve the 2x2 set of linear equations
      delta=a11*a22-a12*a21
  
      if(delta == 0.0) then
       print *,'twod is singular: a11,a12,a21,a22'
       print *,a11,a12,a21,a22
       stop
      endif
      ans(1)=(b1*a22-b2*a12)/delta
      ans(2)=(b2*a11-b1*a21)/delta
!        print *,' ans(1),ans(2)...',ans(1),ans(2)

      ct2=ans(2)
      ct2av=ct2

!-- Evaluate other terms if not diagenesis solution
      IF(ITYPE == 1) THEN
       CT1  = ANS(1)
       C1   = FD1*CT1
       C2   = FD2*ANS(2)
       C2AV = C2
      ENDIF

      RETURN 
      END SUBROUTINE SEDTSFNL

      SUBROUTINE SEDTS2TH
      USE MOD_RCA
      USE MOD_SED
      IMPLICIT NONE
      SAVE                                                              
!LIST OFF
!      INCLUDE 'RCACM'
!      INCLUDE 'SEDCM'
!LIST ON

!JQI      REAL
!JQI     .    kpop1    ,kpop2    ,kpop3
!JQI     .   ,kpon1    ,kpon2    ,kpon3
!JQI     .   ,kpoc1    ,kpoc2    ,kpoc3
!JQI     .   ,thtapop1 ,thtapop2 ,thtapop3
!JQI     .   ,thtapon1 ,thtapon2 ,thtapon3
!JQI     .   ,thtapoc1 ,thtapoc2 ,thtapoc3
!JQI      EQUIVALENCE 
!JQI     .     (kpdiag(1),kpop1) , (kpdiag(2),kpop2) , (kpdiag(3),kpop3)
!JQI     .   , (kndiag(1),kpon1) , (kndiag(2),kpon2) , (kndiag(3),kpon3)
!JQI     .   , (kcdiag(1),kpoc1) , (kcdiag(2),kpoc2) , (kcdiag(3),kpoc3)
!JQI     .  , (dpthta(1),thtapop1),(dpthta(2),thtapop2),(dpthta(3),thtapop3)
!JQI     .  , (dnthta(1),thtapon1),(dnthta(2),thtapon2),(dnthta(3),thtapon3)
!JQI     .  , (dcthta(1),thtapoc1),(dcthta(2),thtapoc2),(dcthta(3),thtapoc3)

      INTEGER :: ITEMP
      REAL    :: TEMP

      DO ITEMP=1,350
       temp=float(itemp-1)/10.0 + 0.05
       temp20=temp-20.0
       temp202=temp20/2.0
       zhtanh4s(itemp)=kappnh4s*thtanh4s**temp202
       zhtanh4f(itemp)=kappnh4f*thtanh4f**temp202
       zhtakmnh4(itemp)=kmnh4*thtakmnh4**temp202
       zhtad1(itemp)=kappd1*thtapd1**temp202
       zhtap1(itemp)=kappp1*thtapd1**temp202
       zhtano3s(itemp)=kapp1no3s*thtano3s**temp202
       zhta2no3s(itemp)=k2no3s*thtano3s**temp20
       zhtano3f(itemp)=kapp1no3f*thtano3f**temp202
       zhta2no3f(itemp)=k2no3f*thtano3f**temp20
       zhtaDd0(itemp)=Dd0*thtaDd0**temp20
       zl12nom(itemp)=thtaDd**temp20
       zw12nom(itemp)=thtaDp**temp20
!JQI       zhtapon1(itemp)=kpon1*thtapon1**temp20
       zhtapon1(itemp)=kndiag(1)*dnthta(1)**temp20
!JQI       zhtapon2(itemp)=kpon2*thtapon2**temp20
       zhtapon2(itemp)=kndiag(2)*dnthta(2)**temp20
!JQI       zhtapon3(itemp)=kpon3*thtapon3**temp20
       zhtapon3(itemp)=kndiag(3)*dnthta(3)**temp20
!JQI       zhtapoc1(itemp)=kpoc1*thtapoc1**temp20
!JQI       zhtapoc2(itemp)=kpoc2*thtapoc2**temp20
!JQI       zhtapoc3(itemp)=kpoc3*thtapoc3**temp20
       zhtapoc1(itemp)=kcdiag(1)*dcthta(1)**temp20
       zhtapoc2(itemp)=kcdiag(2)*dcthta(2)**temp20
       zhtapoc3(itemp)=kcdiag(3)*dcthta(3)**temp20
!JQI       zhtapop1(itemp)=kpop1*thtapop1**temp20
       zhtapop1(itemp)=kpdiag(1)*dpthta(1)**temp20
!JQI       zhtapop2(itemp)=kpop2*thtapop2**temp20
       zhtapop2(itemp)=kpdiag(2)*dpthta(2)**temp20
!JQI       zhtapop3(itemp)=kpop3*thtapop3**temp20
       zhtapop3(itemp)=kpdiag(3)*dpthta(3)**temp20
       zhtasi(itemp)=ksi*thtasi**temp20
       zhtajsi(itemp)=thtasi**temp20
       zhtach4(itemp) = kappch4*thtach4**temp202
      END DO
      
      RETURN
      END SUBROUTINE SEDTS2TH

      FUNCTION ZBRENT(FUNC,X1,X2,TOL,IERR)
      USE MOD_RCA  !Tian
      USE MOD_SED  !Tian

      IMPLICIT NONE
      REAL,EXTERNAL :: FUNC
!-- modified for two argument FUNC to accomodate sedf
      INTEGER,PARAMETER :: ITMAX=100
      INTEGER,PARAMETER :: EPS=3.E-8
      REAL ZBRENT
      REAL,INTENT(IN) :: TOL,X1,X2
      INTEGER,INTENT(OUT) :: IERR
     ! REAL A,B,C,D,E,FA,FB,FC,TOL1,XM,S,P,Q,R
      REAL A,B,C,D,E,FA,FB,FC,TOL1,XM,P,Q,R      
      INTEGER ITER
      ZBRENT = 0.
      IERR=0
      A=X1
      B=X2
     
      FA=FUNC(A,1)
      FB=FUNC(B,1)

!        Check --- Root must bracket ZBRENT
      IF(FB*FA > 0.) THEN
       IERR=1
       RETURN
      ENDIF
      FC=FB
      DO ITER=1,ITMAX
       IF(FB*FC > 0.) THEN
        C=A
        FC=FA
        D=B-A
        E=D
       ENDIF
       IF(ABS(FC) < ABS(FB)) THEN
        A=B
        B=C
        C=A
        FA=FB
        FB=FC
        FC=FA
       ENDIF
       TOL1=2.*EPS*ABS(B)+0.5*TOL
       XM=.5*(C-B)
       IF(ABS(XM) <= TOL1 .OR. FB == 0.)THEN
        ZBRENT=B
        ! FB=FUNC(B,1) !Tian deleted FOR v2
        RETURN
       ENDIF
       IF(ABS(E) >= TOL1 .AND. ABS(FA) > ABS(FB)) THEN
        S=FB/FA
        IF(A == C) THEN
         P=2.*XM*S
         Q=1.-S
        ELSE
         Q=FA/FC
         R=FB/FC
         P=S*(2.*XM*Q*(Q-R)-(B-A)*(R-1.))
         Q=(Q-1.)*(R-1.)*(S-1.)
        ENDIF
        IF(P > 0.) Q=-Q
        P=ABS(P)
        IF(2.*P < MIN(3.*XM*Q-ABS(TOL1*Q),ABS(E*Q))) THEN
         E=D
         D=P/Q
        ELSE
         D=XM
         E=D
        ENDIF
       ELSE
        D=XM
        E=D
       ENDIF
       A=B
       FA=FB
       IF(ABS(D) > TOL1) THEN
        B=B+D
       ELSE
        B=B+SIGN(TOL1,XM)
       ENDIF
       FB=FUNC(B,1)
      END DO
!        ZBRENT exceeded maximum iterations
      IERR=2
      ZBRENT=B
      
      RETURN
      END FUNCTION ZBRENT

      SUBROUTINE STORETM1
      USE MOD_RCA
      USE MOD_SED
      IMPLICIT NONE
      SAVE                                                              
!LIST OFF
!      include 'RCACM'
!      include 'SEDCM'
!LIST ON

!        dissolved concentrations in layer 1
      nh41tm1s(IXY)=nh41
      no31tm1s(IXY)=no31
      hs1tm1s(IXY)=hs1
      si1tm1s(IXY)=si1
      po41tm1s(IXY)=po41
!        benthic stress
      bnthstr1s(IXY) = bnthstr
!        total concentrations in layer 1
      nh4t1tm1s(IXY)=nh4t1
      no3t1tm1s(IXY)=no3t1
      hst1tm1s(IXY)=hst1
      sit1tm1s(IXY)=sit1
      po4t1tm1s(IXY)=po4t1
!        total concentrations in layer 2
      nh4t2tm1s(IXY)=nh4t2
      no3t2tm1s(IXY)=no3t2
      hst2tm1s(IXY)=hst2
      sit2tm1s(IXY)=sit2
      po4t2tm1s(IXY)=po4t2
!        POM concentrations in layer 2
      pon1tm1s(IXY)=pon1
      pon2tm1s(IXY)=pon2
      pon3tm1s(IXY)=pon3
      poc1tm1s(IXY)=poc1
      poc2tm1s(IXY)=poc2
      poc3tm1s(IXY)=poc3
      pop1tm1s(IXY)=pop1
      pop2tm1s(IXY)=pop2
      pop3tm1s(IXY)=pop3
      psitm1s(IXY)=psi
!        h1 variables
      sodtm1s(IXY)=sod
      o20tm1s(IXY)=o20
      Dd0tm1s(IXY)=xDd0
      CH4T1TM1S(IXY) = CH4T1
      CH4T2TM1S(IXY) = CH4T2
      CH41TM1S(IXY)  = CH41
      SO4T2TM1S(IXY) = SO4T2

!d     iprt=0
!d     ixp=4
!d     iyp=6
!d     if(idisk.ge.0)  iprt=0
!d     if(iprt.eq.1 .and. ix.eq.ixp .and. iy.eq.iyp)  then
!d        print *,' storetm1...poc1tm1s=poc1',poc1tm1s(ixp,iyp),poc1
!d        print *,' storetm1...pon1tm1s=pon1',pon1tm1s(ixp,iyp),pon1
!d        print *,' storetm1...pop1tm1s=pop1',pop1tm1s(ixp,iyp),pop1
!d        print *,' storetm1...psitm1s=psi',psitm1s(ixp,iyp),psi
!d     endif

! NEW
      BFORMAXS(IXY)=BFORMAX
      ISWBNTHS(IXY)=ISWBNTH
! END NEW

      RETURN
      END SUBROUTINE STORETM1

      SUBROUTINE LOADTM1
      USE MOD_RCA
      USE MOD_SED
      IMPLICIT NONE
      SAVE                                                              
!LIST OFF
!      include 'RCACM'
!      include 'SEDCM'
!LIST ON
      REAL CH4T1TM1
      
      nh41tm1=nh41tm1s(IXY)
      no31tm1=no31tm1s(IXY)
      hs1tm1=hs1tm1s(IXY)
      si1tm1=si1tm1s(IXY)
      po41tm1=po41tm1s(IXY)
      bnthstr1 = bnthstr1s(IXY)
      nh4t1tm1=nh4t1tm1s(IXY)
      no3t1tm1=no3t1tm1s(IXY)
      hst1tm1=hst1tm1s(IXY)
      sit1tm1=sit1tm1s(IXY)
      po4t1tm1=po4t1tm1s(IXY)
      nh4t2tm1=nh4t2tm1s(IXY)
      no3t2tm1=no3t2tm1s(IXY)
      hst2tm1=hst2tm1s(IXY)
      sit2tm1=sit2tm1s(IXY)
      po4t2tm1=po4t2tm1s(IXY)
      pon1tm1=pon1tm1s(IXY)
      pon2tm1=pon2tm1s(IXY)
      pon3tm1=pon3tm1s(IXY)
      poc1tm1=poc1tm1s(IXY)
      poc2tm1=poc2tm1s(IXY)
      poc3tm1=poc3tm1s(IXY)
      pop1tm1=pop1tm1s(IXY)
      pop2tm1=pop2tm1s(IXY)
      pop3tm1=pop3tm1s(IXY)
      psitm1=psitm1s(IXY)
      sodtm1=sodtm1s(IXY)
      o20tm1=o20tm1s(IXY)
      Dd0tm1=Dd0tm1s(IXY)
      CH4T1TM1 = CH4T1TM1S(IXY)
      CH4T2TM1 = CH4T2TM1S(IXY)
      CH41TM1  = CH41TM1S(IXY)
      SO4T2TM1 = SO4T2TM1S(IXY)

!d     iprt=0
!d     ixp=4
!d     iyp=6
!d     if(idisk.ge.0)  iprt=0
!d     if(iprt.eq.1 .and. ix.eq.ixp .and. iy.eq.iyp)  then
!d      print *,' loadtm1...poc1tm1=poc1tm1s',poc1tm1,poc1tm1s(ixp,iyp)
!d      print *,' loadtm1...pon1tm1=pon1tm1s',pon1tm1,pon1tm1s(ixp,iyp)
!d      print *,' loadtm1...pop1tm1=pop1tm1s',pop1tm1,pop1tm1s(ixp,iyp)
!d      print *,' loadtm1...psitm1=psitm1s',psitm1,psitm1s(ixp,iyp)
!d     endif

! NEW
      BFORMAX=BFORMAXS(IXY)
      ISWBNTH=ISWBNTHS(IXY)
! END NEW

      RETURN
      END SUBROUTINE LOADTM1

      SUBROUTINE STORESED
      USE MOD_RCA
      USE MOD_SED
      IMPLICIT NONE
      SAVE                                                              
!LIST OFF
!      include 'RCACM'
!      include 'SEDCM'
!LIST ON

!     print *,' entry storesed...idisk',idisk

! CPOC ETC NOW IN MG/M**3
      cpop(:,1)=pop1tm1s(:)
      cpop(:,2)=pop2tm1s(:)
      cpop(:,3)=pop3tm1s(:)
      cpon(:,1)=pon1tm1s(:)
      cpon(:,2)=pon2tm1s(:)
      cpon(:,3)=pon3tm1s(:)
      cpoc(:,1)=poc1tm1s(:)
      cpoc(:,2)=poc2tm1s(:)
      cpoc(:,3)=poc3tm1s(:)
      cpos(:)=psitm1s(:)

!      DO IXY=1,NXY
!d     iprt=0
!d     ixp=4
!d     iyp=6
!d     if(idisk.ge.0)  iprt=0
!d     if(iprt.eq.1 .and.ixx.eq.ixp .and. iyy.eq.iyp)  then
!d        print *,' storesed...poc(ix,iy)=poc1tm1s',cpoc(ixx,iyy,1)
!d    .          ,poc1tm1s(ixx,iyy)
!d        print *,' storesed...pon(ix,iy)=pon1tm1s',cpon(ixx,iyy,1)
!d    .          ,pon1tm1s(ixx,iyy)
!d        print *,' storesed...pop(ix,iy)=pop1tm1s',cpop(ixx,iyy,1)
!d    .          ,pop1tm1s(ixx,iyy)
!d        print *,' storesed...pos(ix,iy)=psitm1s',cpos(ixx,iyy)
!d    .          ,psitm1s(ixx,iyy)
!d     endif

!      END DO
      
      RETURN
      END SUBROUTINE STORESED

      SUBROUTINE LOADSED
      USE MOD_RCA
      USE MOD_SED
      IMPLICIT NONE
      SAVE                                                              
!LIST OFF
!      include 'RCACM'
!      include 'SEDCM'
!LIST ON

!d     print *,' entry loadsed...idisk',idisk
      pop1tm1s(:)=cpop(:,1)
      pop2tm1s(:)=cpop(:,2)
      pop3tm1s(:)=cpop(:,3)
      pon1tm1s(:)=cpon(:,1)
      pon2tm1s(:)=cpon(:,2)
      pon3tm1s(:)=cpon(:,3)
      poc1tm1s(:)=cpoc(:,1)
      poc2tm1s(:)=cpoc(:,2)
      poc3tm1s(:)=cpoc(:,3)
      psitm1s(:)=cpos(:)

!      DO IXY=1,NXY
!d     iprt=0
!d     ixp=4
!d     iyp=6
!d     if(idisk.ge.0)  iprt=0
!d     if(iprt.eq.1 .and.ixx.eq.ixp .and. iyy.eq.iyp)  then
!d        print *,' loadsed...poc1tm1s=poc(ix,iy)',poc1tm1s(ixx,iyy)
!d    .          ,cpoc(ixx,iyy,1)
!d        print *,' loadsed...pon1tm1s=pon(ix,iy)',pon1tm1s(ixx,iyy)
!d    .          ,cpon(ixx,iyy,1)
!d        print *,' loadsed...pop1tm1s=pop(ix,iy)',pop1tm1s(ixx,iyy)
!d    .          ,cpop(ixx,iyy,1)
!d        print *,' loadsed...psitm1s=pos(ix,iy)',psitm1s(ixx,iyy)
!d    .          ,cpos(ixx,iyy)
!d     endif

!      END DO
      
      RETURN
      END SUBROUTINE LOADSED

      SUBROUTINE ICREAD(NXY,IOUT,ISEDPRNT,ARRAY,SSNAME)
      
      USE ALL_VARS, ONLY : M,MT,SERIAL,PAR,MSR
# if defined (MULTIPROCESSOR)
      USE MOD_PAR
# endif
      IMPLICIT NONE
      CHARACTER(LEN=25) SSNAME
      CHARACTER(LEN=80) COMMENT
      REAL  ARRAY(MT),ARRAY_TMP(NXY)
      INTEGER NXY,IOUT,ISEDPRNT
      INTEGER IXY
      REAL SCALE
   
      READ(40,'(A80)')  COMMENT
      READ(40,'(8F10.0)')  SCALE

      READ(40,'(8F10.0)')   (ARRAY_TMP(IXY),IXY=1,NXY)
      
      IF(MSR)THEN
       WRITE(IOUT,1200)  SSNAME,SCALE
       IF(ISEDPRNT > 0) WRITE(IOUT,1300)  (ARRAY_TMP(IXY),IXY=1,NXY)
      END IF 
 1200 FORMAT(//50X,'SEDIMENT INITIAL CONDITIONS FOR',/53X,A25/         &
         53X,'SCALE FACTOR =',E10.3//3X,'COLUMN      ROW -->'/         &
         10X,7X,'1',11X,'2',11X,'3',11X,'4',11X,'5',11X,'6',11X,'7'    &
         11X,'8',11X,'9',10X,'10'/)
 1300 FORMAT(10X,10E12.3,/(10X,10E12.3))

      IF(SERIAL)THEN
       DO IXY=1,NXY
        ARRAY(IXY)=SCALE*ARRAY_TMP(IXY)
       END DO
      END IF 

# if defined (MULTIPROCESSOR)
      IF(PAR)THEN
       DO IXY=1,M
        ARRAY(IXY)=SCALE*ARRAY_TMP(NGID(IXY))
       END DO
       DO IXY=1,NHN
        ARRAY(IXY+M)=SCALE*ARRAY_TMP(HN_LST(IXY))
       END DO
      END IF	
# endif

      RETURN
      END SUBROUTINE ICREAD

      SUBROUTINE VREAD(NXY,IOUT,ISEDPRNT,ARRAY,INUM)

      USE ALL_VARS,ONLY : M,MT,SERIAL,PAR,MSR
# if defined (MULTIPROCESSOR)
      USE MOD_PAR
# endif
      IMPLICIT NONE
      CHARACTER   COMMENT*80
      REAL  ARRAY(MT),ARRAY_TMP(NXY)
      REAL  SCALE
      INTEGER IXY
      INTEGER NXY,IOUT,ISEDPRNT,INUM
   
      READ(40,'(A80)') COMMENT
      READ(40,'(8F10.0)') SCALE

      READ(40,'(8F10.0)')   (ARRAY_TMP(IXY),IXY=1,NXY)

      IF(MSR)THEN
       IF(INUM == 1) WRITE(IOUT,'(//52X,"SEDIMENTATION RATES (CM/YR)"/)')
       IF(INUM == 2) WRITE(IOUT,'(//43X,"SEDIMENT SOLID-PHASE MIXING RATES (M**2/DAY)"/)')
       IF(INUM == 3) WRITE(IOUT,'(//41X,"SEDIMENT DISSOLVED-PHASE MIXING RATES (M**2/DAY)"/)')
       IF(INUM == 4) WRITE(IOUT,'(//41X,"SEDIMENT SOLIDS CONCENTRATION - AEROBIC (KG/L)"/)')
       IF(INUM == 5) WRITE(IOUT,'(//41X,"SEDIMENT SOLIDS CONCENTRATION - ANAEROBIC (KG/L)"/)')
       IF(INUM == 6) WRITE(IOUT,'(//41X,"AEROBIC LAYER SI PARTITION COEFFICIENT (L/KG)"/)')
       IF(INUM == 7) WRITE(IOUT,'(//41X,"ANAEROBIC LAYER SI PARTITION COEFFICIENT (L/KG)"/)')
       IF(INUM == 8) WRITE(IOUT,'(//41X,"AEROBIC LAYER PO4 PARTITION COEFFICIENT (L/KG)"/)')
       IF(INUM == 9) WRITE(IOUT,'(//41X,"ANAEROBIC LAYER PO4 PARTITION COEFFICIENT (L/KG)"/)')
       WRITE(IOUT,1300)  SCALE
       IF(ISEDPRNT > 0) WRITE(IOUT,1350) (ARRAY_TMP(IXY),IXY=1,NXY)
      END IF 
 1300 FORMAT(53X,'SCALE FACTOR =',E10.3//3X,'COLUMN      ROW -->'/       &
         10X,7X,'1',11X,'2',11X,'3',11X,'4',11X,'5',11X,'6',11X,'7'      &
         11X,'8',11X,'9',10X,'10'/)
 1350 FORMAT(10X,10E12.3,/(10X,10E12.3))

      IF(SERIAL)THEN
       DO IXY=1,NXY
        ARRAY(IXY)=SCALE*ARRAY_TMP(IXY)
       END DO
      END IF 

# if defined (MULTIPROCESSOR)
      IF(PAR)THEN
       DO IXY=1,M
        ARRAY(IXY)=SCALE*ARRAY_TMP(NGID(IXY))
       END DO
       DO IXY=1,NHN
        ARRAY(IXY+M)=SCALE*ARRAY_TMP(HN_LST(IXY))
       END DO
      END IF	
# endif

      RETURN
      END SUBROUTINE VREAD
