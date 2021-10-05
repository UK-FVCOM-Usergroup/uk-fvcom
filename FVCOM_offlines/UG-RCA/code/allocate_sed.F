!==============================================================================|
!    Allocate and Initialize SED Arrays                                       !
!==============================================================================|

      SUBROUTINE ALLOCATE_SED
      USE ALL_VARS,ONLY : MT,NT
      USE MOD_SED
      IMPLICIT NONE
      
      ALLOCATE(CPOP(MT,3))    ;CPOP = 0.0
      ALLOCATE(FLXPOP(MT,3))  ;FLXPOP = 0.0
      ALLOCATE(CPON(MT,3))    ;CPON = 0.0
      ALLOCATE(FLXPON(MT,3))  ;FLXPON = 0.0
      ALLOCATE(CPOC(MT,3))    ;CPOC = 0.0
      ALLOCATE(FLXPOC(MT,3))  ;FLXPOC = 0.0
      ALLOCATE(CPOS(MT))      ;CPOS = 0.0
      ALLOCATE(FLXPOS(MT))    ;FLXPOS = 0.0
      ALLOCATE(FLXPO4T2S(MT)) ;FLXPO4T2S = 0.0
      ALLOCATE(FLXSIT2S(MT))  ;FLXSIT2S = 0.0
      ALLOCATE(CTEMP(MT))     ;CTEMP = 0.0
      ALLOCATE(CDTEMP(MT))    ;CDTEMP = 0.0
      ALLOCATE(BSVOL(MT))     ;BSVOL = 0.0
      ALLOCATE(HSED(MT))      ;HSED = 0.0
      ALLOCATE(CCH4(MT))      ;CCH4 = 0.0
      ALLOCATE(CSO4(MT))      ;CSO4 = 0.0
      
      ALLOCATE(VSED(MT))      ;VSED = 0.0
      ALLOCATE(VPMIX(MT))     ;VPMIX = 0.0
      ALLOCATE(VDMIX(MT))     ;VDMIX = 0.0
      ALLOCATE(M1(MT))        ;M1 = 0.0
      ALLOCATE(M2(MT))        ;M2 = 0.0
      ALLOCATE(JDIAGP(MT))    ;JDIAGP = 0.0
      ALLOCATE(JDIAGN(MT))    ;JDIAGN = 0.0
      ALLOCATE(JDIAGC(MT))    ;JDIAGC = 0.0
      ALLOCATE(JDIAGS(MT))    ;JDIAGS = 0.0
      
      ALLOCATE(PPFWS(MT))     ;PPFWS = 0.0
      ALLOCATE(PNFWS(MT))     ;PNFWS = 0.0
      ALLOCATE(PCFWS(MT))     ;PCFWS = 0.0
      ALLOCATE(PSFWS(MT))     ;PSFWS = 0.0
      ALLOCATE(SSFWS(MT))     ;SSFWS = 0.0
      
      ALLOCATE(PIE1SI(MT))    ;PIE1SI = 0.0
      ALLOCATE(PIE2SI(MT))    ;PIE2SI = 0.0
      ALLOCATE(PIE1PO4M(MT))  ;PIE1PO4M = 0.0
      ALLOCATE(PIE1PO4N(MT))  ;PIE1PO4N = 0.0
  
      ALLOCATE(pon1tm1s(MT))  ;pon1tm1s = 0.0
      ALLOCATE(pon2tm1s(MT))  ;pon2tm1s = 0.0
      ALLOCATE(pon3tm1s(MT))  ;pon3tm1s = 0.0
      ALLOCATE(poc1tm1s(MT))  ;poc1tm1s = 0.0
      ALLOCATE(poc2tm1s(MT))  ;poc2tm1s = 0.0
      ALLOCATE(poc3tm1s(MT))  ;poc3tm1s = 0.0
      ALLOCATE(pop1tm1s(MT))  ;pop1tm1s = 0.0
      ALLOCATE(pop2tm1s(MT))  ;pop2tm1s = 0.0
      ALLOCATE(pop3tm1s(MT))  ;pop3tm1s = 0.0
      ALLOCATE(psitm1s(MT))   ;psitm1s = 0.0
      ALLOCATE(bnthstr1s(MT)) ;bnthstr1s = 0.0
      ALLOCATE(bformaxs(MT))  ;bformaxs = 0.0
      ALLOCATE(iswbnths(MT))  ;iswbnths = 0.0

      ALLOCATE(nh41tm1s(MT))  ;nh41tm1s = 0.0
      ALLOCATE(nh4t1tm1s(MT)) ;nh4t1tm1s = 0.0
      ALLOCATE(nh4t2tm1s(MT)) ;nh4t2tm1s = 0.0
      ALLOCATE(no31tm1s(MT))  ;no31tm1s = 0.0
      ALLOCATE(no3t1tm1s(MT)) ;no3t1tm1s = 0.0
      ALLOCATE(no3t2tm1s(MT)) ;no3t2tm1s = 0.0
      ALLOCATE(hs1tm1s(MT))   ;hs1tm1s = 0.0
      ALLOCATE(hst1tm1s(MT))  ;hst1tm1s = 0.0
      ALLOCATE(hst2tm1s(MT))  ;hst2tm1s = 0.0
      ALLOCATE(si1tm1s(MT))   ;si1tm1s = 0.0
      ALLOCATE(sit1tm1s(MT))  ;sit1tm1s = 0.0
      ALLOCATE(sit2tm1s(MT))  ;sit2tm1s = 0.0
      ALLOCATE(po41tm1s(MT))  ;po41tm1s = 0.0
      ALLOCATE(po4t1tm1s(MT)) ;po4t1tm1s = 0.0
      ALLOCATE(po4t2tm1s(MT)) ;po4t2tm1s = 0.0
      ALLOCATE(ch41tm1s(MT))  ;ch41tm1s = 0.0
      ALLOCATE(ch4t2tm1s(MT)) ;ch4t2tm1s = 0.0
      ALLOCATE(ch4t1tm1s(MT)) ;ch4t1tm1s = 0.0
      ALLOCATE(so41tm1s(MT))  ;so41tm1s = 0.0
      ALLOCATE(so4t2tm1s(MT)) ;so4t2tm1s = 0.0
      
      ALLOCATE(sodtm1s(MT))   ;sodtm1s = 0.0
      ALLOCATE(o20tm1s(MT))   ;o20tm1s = 0.0
      ALLOCATE(Dd0tm1s(MT))   ;Dd0tm1s = 0.0

      END SUBROUTINE ALLOCATE_SED
