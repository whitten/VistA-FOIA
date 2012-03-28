BGP0CP2 ; IHS/CMI/LAB - IHS gpra print 02 Nov 2008 10:40 AM ;
 ;;10.0;IHS CLINICAL REPORTING;;JUN 18, 2010
 ;
 ;
AMI5W ;EP
 I $Y>(BGPIOSL-3) D HDR^BGP0CP Q:BGPQUIT  D L1H^BGP0CP
 D WDOD^BGP0CPU(DFN)
 Q:BGPQUIT
 D WDT^BGP0CPU(BGPVINP)
 Q:BGPQUIT
 I $$TRANS^BGP0CU(BGPVINP) D WTT^BGP0CPU(BGPVINP)
 Q:BGPQUIT
 I $$TRANS^BGP0CU(BGPVINP) D TRANSN Q:BGPQUIT
 D WPPDPOV^BGP0CPU(BGPVSIT)
 Q:BGPQUIT
 D OTHDPOVS^BGP0CPU(BGPVSIT)
 Q:BGPQUIT
 S X=$$COMFORT^BGP0CU(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$P($P(^AUPNVINP(BGPVINP,0),U),"."))
 D WCOMFORT^BGP0CPU(X)
 Q:BGPQUIT
 K Z
 S X=$$BETAALEG^BGP0CU2(DFN,$$DOB^AUPNPAT(DFN),$$DSCH^BGP0CU(BGPVINP))
 I X S X=$P(X,U,2)
 D WBETAAL^BGP0CPU
 Q:BGPQUIT
 K BGPBRADY
 D ALLDXS^BGP0CU2(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$$DSCH^BGP0CU(BGPVINP),.BGPBRADY,0,"BGP CMS BRADYCARDIA DXS")
 ;on active med for beta blocker?
 K BGPBETA
 D BETARX^BGP0CU2(DFN,$$FMADD^XLFDT($P($P(^AUPNVSIT(BGPVSIT,0),U),"."),-180),$$DSCH^BGP0CU(BGPVINP),1,.BGPBETA)
 D WBRADY5^BGP0CPU2
 Q:BGPQUIT
 K BGPDATA
 K BGP23RD
 D ALLDXS^BGP0CU2(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$$DSCH^BGP0CU(BGPVINP),.BGP23RD,0,"BGP CMS 2/3 HEART BLOCK DXS")
 S BGPPACE=$$PACE^BGP0CU2(DFN,$$DOB^AUPNPAT(DFN),$$DSCH^BGP0CU(BGPVINP))
 D W23RD^BGP0CPU2
 K BGPY
 D NMIDRUG^BGP0CU1(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$$DSCH^BGP0CU(BGPVINP),.BGPDATA,"BGP CMS BETA BLOCKER MEDS",0)
 S E=+$$CODEN^ICPTCOD("G8011")
 S Z=$$CPTI^BGP0DU(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$$DSCH^BGP0CU(BGPVINP),E) I Z]"" S Z=$$DATE^BGP0UTL($P(Z,U,2))_" [G8011]"
 D WNMIBETA^BGP0CPU
 Q:BGPQUIT
 S X=$$LASTMED^BGP0CU1(DFN,$$FMADD^XLFDT($P($P(^AUPNVSIT(BGPVSIT,0),U),"."),-365),$$FMADD^XLFDT($P($P(^AUPNVINP(BGPVINP,0),U),"."),30),"BGP CMS BETA BLOCKER MEDS","BGP CMS BETA BLOCKER NDC","BGP CMS BETA BLOCKER CLASS")
 S E=+$$CODEN^ICPTCOD("G8009")
 S Z=$$CPTI^BGP0DU(DFN,$$FMADD^XLFDT($P($P(^AUPNVSIT(BGPVSIT,0),U),"."),-365),$$FMADD^XLFDT($P($P(^AUPNVINP(BGPVINP,0),U),"."),30),E) I Z]"" S Z=$$DATE^BGP0UTL($P(Z,U,2))_" [G8009]"
 D WLASTBB^BGP0CPU
 Q:BGPQUIT
 D ALLALG1^BGP0CU1(DFN,DT,$$DSCH^BGP0CU(BGPVINP),.BGPDATA)
 D WALLALG^BGP0CPU
 Q:BGPQUIT
 K BGPDATA
 D ALLALGA1^BGP0CU1(DFN,DT,.BGPDATA)
 D WALLALGT^BGP0CPU
 K BGPDATA
 D IVUD^BGP0CU1(DFN,$P($P(BGPVSIT0,U),"."),$$DSCH^BGP0CU(BGPVINP),,.BGPDATA)
 D WIVUD^BGP0CPU
 Q
 ;
AMI5 ;EP
 I $Y>(BGPIOSL-3) D HDR^BGP0CP Q:BGPQUIT  D L1H^BGP0CP
 D WDT^BGP0CPU(BGPVINP)
 Q:BGPQUIT
 I $$TRANS^BGP0CU(BGPVINP) D WTT^BGP0CPU(BGPVINP)
 Q:BGPQUIT
 I $$TRANS^BGP0CU(BGPVINP) D TRANSN Q:BGPQUIT
 D WPPDPOV^BGP0CPU(BGPVSIT)
 Q:BGPQUIT
 D OTHDPOVS^BGP0CPU(BGPVSIT)
 Q:BGPQUIT
 S X=$$COMFORT^BGP0CU(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$P($P(^AUPNVINP(BGPVINP,0),U),"."))
 D WCOMFORT^BGP0CPU(X)
 Q:BGPQUIT
 K BGPBRADY
 D ALLDXS^BGP0CU2(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$$DSCH^BGP0CU(BGPVINP),.BGPBRADY,0,"BGP CMS BRADYCARDIA DXS")
 ;on active med for beta blocker?
 K BGPBETA
 D BETARX^BGP0CU2(DFN,$$FMADD^XLFDT($P($P(^AUPNVSIT(BGPVSIT,0),U),"."),-180),$$DSCH^BGP0CU(BGPVINP),1,.BGPBETA)
 D WBRADY5^BGP0CPU2
 Q:BGPQUIT
 K BGPDATA
 K BGP23RD
 D ALLDXS^BGP0CU2(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$$DSCH^BGP0CU(BGPVINP),.BGP23RD,0,"BGP CMS 2/3 HEART BLOCK DXS")
 S BGPPACE=$$PACE^BGP0CU2(DFN,$$DOB^AUPNPAT(DFN),$$DSCH^BGP0CU(BGPVINP))
 D W23RD^BGP0CPU2
 K BGPY
 D NMIDRUG^BGP0CU1(DFN,$$VD^APCLV(BGPVSIT),$$DSCH^BGP0CU(BGPVINP),.BGPDATA,"BGP CMS BETA BLOCKER MEDS",0)
 S E=+$$CODEN^ICPTCOD("G8011")
 S Z=$$CPTI^BGP0DU(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$$DSCH^BGP0CU(BGPVINP),E) I Z]"" S Z=$$DATE^BGP0UTL($P(Z,U,2))_" [G8011]"
 D WNMIBETA^BGP0CPU
 Q:BGPQUIT
 S X=$$LASTMED^BGP0CU1(DFN,$$FMADD^XLFDT($P($P(^AUPNVSIT(BGPVSIT,0),U),"."),-365),$$FMADD^XLFDT($P($P(^AUPNVINP(BGPVINP,0),U),"."),30),"BGP CMS BETA BLOCKER MEDS","BGP CMS BETA BLOCKER NDC","BGP CMS BETA BLOCKER CLASS")
 S E=+$$CODEN^ICPTCOD("G8009")
 S Z=$$CPTI^BGP0DU(DFN,$$FMADD^XLFDT($P($P(^AUPNVSIT(BGPVSIT,0),U),"."),-365),$$FMADD^XLFDT($P($P(^AUPNVINP(BGPVINP,0),U),"."),30),E) I Z]"" S Z=$$DATE^BGP0UTL($P(Z,U,2))_" [G8009]"
 D WLASTBB^BGP0CPU
 Q:BGPQUIT
 D ALLALG1^BGP0CU1(DFN,DT,$$DSCH^BGP0CU(BGPVINP),.BGPDATA)
 D WALLALG^BGP0CPU
 Q:BGPQUIT
 K BGPDATA
 D ALLALGA1^BGP0CU1(DFN,DT,.BGPDATA)
 D WALLALGT^BGP0CPU
 K BGPDATA
 D IVUD^BGP0CU1(DFN,$P($P(BGPVSIT0,U),"."),$$DSCH^BGP0CU(BGPVINP),,.BGPDATA)
 D WIVUD^BGP0CPU
 Q
 ;
AMI6 ;EP
 I $Y>(BGPIOSL-3) D HDR^BGP0CP Q:BGPQUIT  D L1H^BGP0CP
 I $$TRANSIN^BGP0CU(BGPVINP) D TRANSIN Q:BGPQUIT
 D WDT^BGP0CPU(BGPVINP)
 Q:BGPQUIT
 I $$TRANS^BGP0CU(BGPVINP) D WTT^BGP0CPU(BGPVINP)
 Q:BGPQUIT
 I $$TRANS^BGP0CU(BGPVINP) D TRANSN Q:BGPQUIT
 D WPPDPOV^BGP0CPU(BGPVSIT)
 Q:BGPQUIT
 D OTHDPOVS^BGP0CPU(BGPVSIT)
 Q:BGPQUIT
 S X=$$COMFORT^BGP0CU(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$P($P(^AUPNVINP(BGPVINP,0),U),"."))
 D WCOMFORT^BGP0CPU(X)
 Q:BGPQUIT
 K BGPBRADY
 D ALLDXS^BGP0CU2(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$$DSCH^BGP0CU(BGPVINP),.BGPBRADY,0,"BGP CMS BRADYCARDIA DXS")
 ;on active med for beta blocker?
 K BGPBETA
 D BETARX^BGP0CU2(DFN,$$FMADD^XLFDT($P($P(^AUPNVSIT(BGPVSIT,0),U),"."),-180),$$DSCH^BGP0CU(BGPVINP),1,.BGPBETA)
 S BGPASTER=1 D WBRADY6^BGP0CPU2 S BGPASTER=0
 Q:BGPQUIT
 K BGPDATA
A6WHF1 ;
 D ALLDXS^BGP0CU2(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$$DSCH^BGP0CU(BGPVINP),.BGPDATA,0,"BGP CMS HEART FAILURE DXS")
 D WHF^BGP0CPU2
 K BGPDATA
 Q:BGPQUIT
 K BGP23RD
 D ALLDXS^BGP0CU2(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$$DSCH^BGP0CU(BGPVINP),.BGP23RD,0,"BGP CMS 2/3 HEART BLOCK DXS")
 S BGPPACE=$$PACE^BGP0CU2(DFN,$$DOB^AUPNPAT(DFN),$$DSCH^BGP0CU(BGPVINP))
 D W23RD^BGP0CPU2
 K BGPY
 Q:BGPQUIT
 K BGPDATA
 D ALLDXS^BGP0CU2(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$$DSCH^BGP0CU(BGPVINP),.BGPDATA,0,"BGP CMS CIRCULATORY SHOCK DXS")
 D WCS^BGP0CPU2
 K BGPDATA
 Q:BGPQUIT
 K BGPY
 D NMIDRUG^BGP0CU1(DFN,$$VD^APCLV(BGPVSIT),$$DSCH^BGP0CU(BGPVINP),.BGPDATA,"BGP CMS BETA BLOCKER MEDS",0)
 S E=+$$CODEN^ICPTCOD("G8011")
 S Z=$$CPTI^BGP0DU(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$$DSCH^BGP0CU(BGPVINP),E) I Z]"" S Z=$$DATE^BGP0UTL($P(Z,U,2))_" [G8011]"
 D WNMIBETA^BGP0CPU
 Q:BGPQUIT
 S X=$$LASTMED^BGP0CU1(DFN,$$FMADD^XLFDT($P($P(^AUPNVSIT(BGPVSIT,0),U),"."),-365),$$FMADD^XLFDT($P($P(^AUPNVINP(BGPVINP,0),U),"."),30),"BGP CMS BETA BLOCKER MEDS","BGP CMS BETA BLOCKER NDC","BGP CMS BETA BLOCKER CLASS")
 S E=+$$CODEN^ICPTCOD("G8009")
 S Z=$$CPTI^BGP0DU(DFN,$$FMADD^XLFDT($P($P(^AUPNVSIT(BGPVSIT,0),U),"."),-365),$$FMADD^XLFDT($P($P(^AUPNVINP(BGPVINP,0),U),"."),30),E) I Z]"" S Z=$$DATE^BGP0UTL($P(Z,U,2))_" [G8009]"
 D WLASTBB^BGP0CPU
 Q:BGPQUIT
 D ALLALG1^BGP0CU1(DFN,DT,$$DSCH^BGP0CU(BGPVINP),.BGPDATA)
 D WALLALG^BGP0CPU
 Q:BGPQUIT
 K BGPDATA
 D ALLALGA1^BGP0CU1(DFN,DT,.BGPDATA)
 D WALLALGT^BGP0CPU
 K BGPDATA
 D IVUD^BGP0CU1(DFN,$P($P(BGPVSIT0,U),"."),$$DSCH^BGP0CU(BGPVINP),,.BGPDATA)
 D WIVUD^BGP0CPU
 Q
 ;
AMI6W ;EP
 S BGPASTER=0
 I $Y>(BGPIOSL-3) D HDR^BGP0CP Q:BGPQUIT  D L1H^BGP0CP
 I $$TRANSIN^BGP0CU(BGPVINP) D TRANSIN Q:BGPQUIT
 D WDOD^BGP0CPU(DFN)
 Q:BGPQUIT
 D WDT^BGP0CPU(BGPVINP)
 Q:BGPQUIT
 I $$TRANS^BGP0CU(BGPVINP) D WTT^BGP0CPU(BGPVINP)
 Q:BGPQUIT
 I $$TRANS^BGP0CU(BGPVINP) D TRANSN Q:BGPQUIT
 D WPPDPOV^BGP0CPU(BGPVSIT)
 Q:BGPQUIT
 D OTHDPOVS^BGP0CPU(BGPVSIT)
 Q:BGPQUIT
 S X=$$COMFORT^BGP0CU(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$P($P(^AUPNVINP(BGPVINP,0),U),"."))
 D WCOMFORT^BGP0CPU(X)
 Q:BGPQUIT
 S X=$$BETAALEG^BGP0CU2(DFN,$$DOB^AUPNPAT(DFN),$$DSCH^BGP0CU(BGPVINP))
 I X S X=$P(X,U,2)
 D WBETAAL^BGP0CPU
 Q:BGPQUIT
 K BGPBRADY
 D ALLDXS^BGP0CU2(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$$DSCH^BGP0CU(BGPVINP),.BGPBRADY,0,"BGP CMS BRADYCARDIA DXS")
 ;on active med for beta blocker?
 K BGPBETA
 D BETARX^BGP0CU2(DFN,$$FMADD^XLFDT($P($P(^AUPNVSIT(BGPVSIT,0),U),"."),-180),$$DSCH^BGP0CU(BGPVINP),1,.BGPBETA)
 S BGPASTER=1 D WBRADY6^BGP0CPU2 S BGPASTER=0
 Q:BGPQUIT
 K BGPDATA
A6WHF ;
 D ALLDXS^BGP0CU2(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$$DSCH^BGP0CU(BGPVINP),.BGPDATA,0,"BGP CMS HEART FAILURE DXS")
 D WHF^BGP0CPU2
 K BGPDATA
 Q:BGPQUIT
 K BGP23RD
 D ALLDXS^BGP0CU2(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$$DSCH^BGP0CU(BGPVINP),.BGP23RD,0,"BGP CMS 2/3 HEART BLOCK DXS")
 S BGPPACE=$$PACE^BGP0CU2(DFN,$$DOB^AUPNPAT(DFN),$$DSCH^BGP0CU(BGPVINP))
 D W23RD^BGP0CPU2
 K BGPY
 Q:BGPQUIT
 K BGPDATA
 D ALLDXS^BGP0CU2(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$$DSCH^BGP0CU(BGPVINP),.BGPDATA,0,"BGP CMS CIRCULATORY SHOCK DXS")
 D WCS^BGP0CPU2
 K BGPDATA
 Q:BGPQUIT
 D NMIDRUG^BGP0CU1(DFN,$$VD^APCLV(BGPVSIT),$$DSCH^BGP0CU(BGPVINP),.BGPDATA,"BGP CMS BETA BLOCKER MEDS",0)
 S E=+$$CODEN^ICPTCOD("G8011")
 S Z=$$CPTI^BGP0DU(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$$DSCH^BGP0CU(BGPVINP),E) I Z]"" S Z=$$DATE^BGP0UTL($P(Z,U,2))_" [G8011]"
 D WNMIBETA^BGP0CPU
 Q:BGPQUIT
 S X=$$LASTMED^BGP0CU1(DFN,$$FMADD^XLFDT($P($P(^AUPNVSIT(BGPVSIT,0),U),"."),-365),$$FMADD^XLFDT($P($P(^AUPNVINP(BGPVINP,0),U),"."),30),"BGP CMS BETA BLOCKER MEDS","BGP CMS BETA BLOCKER NDC","BGP CMS BETA BLOCKER CLASS")
 S E=+$$CODEN^ICPTCOD("G8009")
 S Z=$$CPTI^BGP0DU(DFN,$$FMADD^XLFDT($P($P(^AUPNVSIT(BGPVSIT,0),U),"."),-365),$$FMADD^XLFDT($P($P(^AUPNVINP(BGPVINP,0),U),"."),30),E) I Z]"" S Z=$$DATE^BGP0UTL($P(Z,U,2))_" [G8009]"
 D WLASTBB^BGP0CPU
 Q:BGPQUIT
 D ALLALG1^BGP0CU1(DFN,DT,$$DSCH^BGP0CU(BGPVINP),.BGPDATA)
 D WALLALG^BGP0CPU
 Q:BGPQUIT
 K BGPDATA
 D ALLALGA1^BGP0CU1(DFN,DT,.BGPDATA)
 D WALLALGT^BGP0CPU
 K BGPDATA
 D IVUD^BGP0CU1(DFN,$P($P(BGPVSIT0,U),"."),$$DSCH^BGP0CU(BGPVINP),,.BGPDATA)
 D WIVUD^BGP0CPU
 Q
 ;
AMI7AW ;EP
 S BGPASTER=0
 I $Y>(BGPIOSL-3) D HDR^BGP0CP Q:BGPQUIT  D L1H^BGP0CP
 I $$TRANSIN^BGP0CU(BGPVINP) D TRANSIN Q:BGPQUIT
 D WDT^BGP0CPU(BGPVINP)
 Q:BGPQUIT
 I $$TRANS^BGP0CU(BGPVINP) D WTT^BGP0CPU(BGPVINP)
 Q:BGPQUIT
 I $$TRANS^BGP0CU(BGPVINP) D TRANSN Q:BGPQUIT
 D WPPDPOV^BGP0CPU(BGPVSIT)
 Q:BGPQUIT
 D OTHDPOVS^BGP0CPU(BGPVSIT)
 Q:BGPQUIT
 S X=$$COMFORT^BGP0CU(DFN,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),$P($P(^AUPNVINP(BGPVINP,0),U),"."))
 D WCOMFORT^BGP0CPU(X)
 Q:BGPQUIT
 K BGPST1
 S BGPST1=$$LASTDX^BGP0UTL1(DFN,"BGP ST ELEVATION DX",$$FMADD^XLFDT($P($P(BGPVSIT0,U),"."),-1),$$DSCH^BGP0CU(BGPVINP))
 I BGPST1 S BGPST1=$$DATE^BGP0UTL($P(BGPST1,U,3))_"  ["_$P(BGPST1,U,2)_"]  "_$$VAL^XBDIQ1(9000010.07,$P(BGPST1,U,5),.04)
 D WST^BGP0CPU2
 ;LBBB ON ECG
 K BGPLBPC,BGPLBDX
 S BGPLBDX=$$LBBBDX^BGP0CU2(DFN,$$FMADD^XLFDT($P($P(BGPVSIT0,U),"."),-1),$$DSCH^BGP0CU(BGPVINP)) I BGPLBDX S BGPLBDX=$$DATE^BGP0UTL($P(BGPLBDX,U,3))_"  ["_$P(BGPLBDX,U,2)_"]  "_$$VAL^XBDIQ1(9000010.07,$P(BGPLBDX,U,5),.04)
 D LBBBPROC^BGP0CU2(DFN,$$FMADD^XLFDT($P($P(BGPVSIT0,U),"."),-1),$$DSCH^BGP0CU(BGPVINP),.BGPLBPC)
 D WLBBB^BGP0CPU2
 ;FIB MEDS
 S BGPFIB=""
 K BGPDATA
 D TARX^BGP0CU2(DFN,$$FMADD^XLFDT($P($P(^AUPNVSIT(BGPVSIT,0),U),"."),-365),$$FMADD^XLFDT($$DSCH^BGP0CU(BGPVINP),30),0,$P($P(^AUPNVSIT(BGPVSIT,0),U),"."),.BGPDATA)
 K BGPUD
 D IVUD^BGP0CU1(DFN,$P($P(BGPVSIT0,U),"."),$$DSCH^BGP0CU(BGPVINP),$O(^ATXAX("B","BGP CMS THROMBOLYTIC MEDS",0)),.BGPUD,"",$O(^ATXAX("B","BGP THROMBOLYTIC AGENTS CLASS",0)))
 S BGPTAPRO=$$LASTPRCI^BGP0UTL1(DFN,"99.10",$P($P(BGPVSIT0,U),"."),$$DSCH^BGP0CU(BGPVINP)) I BGPTAPRO S BGPTAPRO=$$DATE^BGP0UTL($P(BGPTAPRO,U,3))_"  ["_$P(BGPTAPRO,U,2)_"]  "_$$VAL^XBDIQ1(9000010.08,$P(BGPTAPRO,U,5),.04)
 D WFIB^BGP0CPU2
 Q:BGPQUIT
 D ALLALG1^BGP0CU1(DFN,DT,$$DSCH^BGP0CU(BGPVINP),.BGPDATA)
 D WALLALG^BGP0CPU
 Q:BGPQUIT
 K BGPDATA
 D ALLALGA1^BGP0CU1(DFN,DT,.BGPDATA)
 D WALLALGT^BGP0CPU
 K BGPDATA
 D IVUD^BGP0CU1(DFN,$P($P(BGPVSIT0,U),"."),$$DSCH^BGP0CU(BGPVINP),,.BGPDATA)
 D WIVUD^BGP0CPU
 Q
 ;
TRANSIN ;
 I $Y>(BGPIOSL-4) D HDR^BGP0CP Q:BGPQUIT  D L1H^BGP0CP
 W !!?3,"NOTE:  Since Admission Type was ","""","Transferred,",""""," review patient's chart"
 W !,"to determine if patient should be excluded if transfer was from another"
 W !,"acute care hospital, including ER from another hospital.",!
 Q
 ;
TRANSN ;EP
 I $Y>(BGPIOSL-4) D HDR^BGP0CP Q:BGPQUIT  D L1H^BGP0CP
 W !!?3,"NOTE:  Since Discharge Type was ","""","Transferred,",""""," review patient's chart"
 W !,"to determine if patient should be excluded if transferred to another"
 W !,"acute care hospital or federal hospital."
 Q
 ;
