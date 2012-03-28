DPTDN ; IHS/TUCSON/JCM - COMPARES NAMES ;
 ;;1.0;PATIENT MERGE;;FEB 02, 1994
 ;
 ; Calls: SOU^DICM1
 ;
START ;
 D INIT
 D NAME
 I $O(^DPT(XDRCD,.01,0)) D OTHER
END D EOJ
 Q
 ;
EN ; EP - Entry Point for any routines comparing names
 ;
 D INIT1
 D COMPARE
 D EOJ
 Q
 ;
INIT ;
 D EOJ
 S DPTDN("MATCH")=$P(XDRDTEST(XDRDTO),U,6)
 S DPTDN("NO MATCH")=$P(XDRDTEST(XDRDTO),U,7)
 S DPTDN=$G(XDRCD(XDRFL,XDRCD,.01,"I")),DPTDN2=$G(XDRCD2(XDRFL,XDRCD2,.01,"I"))
 ;
INIT1 S DPTDNL=$P(DPTDN,","),DPTDNF=$P($P(DPTDN,",",2)," "),DPTDNFI=$E(DPTDNF),DPTDNM=$P($P(DPTDN,",",2)," ",2),DPTDNMI=$E(DPTDNM)
 ;
INIT2 S DPTDNL2=$P(DPTDN2,","),DPTDNF2=$P($P(DPTDN2,",",2)," "),DPTDNFI2=$E(DPTDNF2),DPTDNM2=$P($P(DPTDN2,",",2)," ",2),DPTDNMI2=$E(DPTDNM2)
 Q
 ;
NAME ;
 D COMPARE
 D:$O(^DPT(XDRCD2,.01,0)) OTHER2
 Q
 ;
OTHER ;
 F DPTDNO=0:0 S DPTDNO=$O(^DPT(XDRCD,.01,DPTDNO)) Q:'DPTDNO  S DPTDN=$P(^DPT(XDRCD,.01,DPTDNO,0),U,1) S:'$D(DPTDN2) DPTDN2=XDRCD2(XDRFL,XDRCD2,.01,"I") D INIT1,NAME
 Q
 ;
OTHER2 ;
 F DPTDNO2=0:0 S DPTDNO2=$O(^DPT(XDRCD2,.01,DPTDNO2)) Q:'DPTDNO2  S DPTDN2=$P(^DPT(XDRCD2,.01,DPTDNO2,0),U,1) D INIT2,COMPARE
 Q
 ;
COMPARE ;
 S:'$D(DPTDN("TEST SCORE")) DPTDN("TEST SCORE")=DPTDN("NO MATCH")
 I DPTDN=DPTDN2 S DPTDN("TEST SCORE2")=DPTDN("MATCH") G COMPAREX
 I DPTDNF=DPTDNF2,DPTDNL=DPTDNL2 S DPTDN("TEST SCORE2")=DPTDN("MATCH")*.8 G COMPAREX
 I DPTDNFI=DPTDNFI2,DPTDNL=DPTDNL2 S DPTDN("TEST SCORE2")=DPTDN("MATCH")*.6 G COMPAREX
 I DPTDNL=DPTDNL2 S DPTDN("TEST SCORE2")=DPTDN("MATCH")*.4 G COMPAREX
 S X=DPTDNL D SOU^DICM1 S DPTDNLS=X S X=DPTDNL2 D SOU^DICM1 S DPTDNL2S=X
 S X=DPTDNF D SOU^DICM1 S DPTDNFS=X S X=DPTDNF2 D SOU^DICM1 S DPTDNF2S=X
 I DPTDNLS=DPTDNL2S,DPTDNFS=DPTDNF2S S DPTDN("TEST SCORE2")=DPTDN("MATCH")*.6 G COMPAREX
 I DPTDNFS=DPTDNF2S S DPTDN("TEST SCORE2")=DPTDN("MATCH")*.2 G COMPAREX
 S DPTDN("TEST SCORE2")=DPTDN("NO MATCH")
COMPAREX ;
 S:DPTDN("TEST SCORE2")>(DPTDN("TEST SCORE")) DPTDN("TEST SCORE")=DPTDN("TEST SCORE2")
 K X,DPTDNLS,DPTDNL2S,DPTDNFS,DPTDNF2S,DPTDN("TEST SCORE2")
 Q
 ;
EOJ ;
 S:$D(DPTDN("TEST SCORE")) XDRD("TEST SCORE")=DPTDN("TEST SCORE")
 K DPTDN,DPTDN2,DPTDNF,DPTDNF2,DPTDNL,DPTDNL2,DPTDNM,DPTDNM2
 K DPTDNMI,DPTDNMI2,DPTDNFI,DPTDNFI2,DPTDNO,DPTDNO2
 Q
