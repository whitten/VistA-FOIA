DGPASS ;ALB/JDS - ABSENCE LIST ; 01 JAN 86 @0800 [ 03/17/2004  1:55 PM ]
 ;;5.3;Registration;**162**;Aug 13, 1993
 ;
EN D QUIT S %DT="AEPT",%DT("A")="Enter date of Absence: " D ^%DT G:Y'>0 QUIT G EN:+Y>(DT+1) S DGT=+Y,DG2=DGT,DGT=$S(DGT[".":DGT,1:DGT_".2400"),DG2=DGT
 S DGVAR="DGT^DG2",DGPGM="START^DGPASS" D ZIS^DGUTQ I 'POP U IO D START^DGPASS
QUIT D CLOSE^DGUTQ G QUIT1^DGOASIH
START D NOW^%DTC S Y=$E(%,1,12) S DGTIME=$$FMTE^XLFDT(Y,1) S $P(DGCL,"-",81)=""
 S X1=DGT,X2=-32 D C^%DTC S DGSTART=X
 S Y=DGT\1 X ^DD("DD") S DGDAY=Y
 S DGFL=0 D EEN I '$D(^UTILITY($J,"DG")) W !!?8,"*** THERE ARE NO PATIENTS OUT ON ABSENCE FOR "_DGDAY_" ***" G QUIT
 D WR G QUIT
 ;
EEN S DGA="^25^26^43^44^45^13^1^2^3^" D NOW^%DTC S DGNOW=%
 S DGDAT=DGT
 F DGDT=DGSTART:0 S DGDT=$O(^DGPM("AMV2",DGDT)) Q:'DGDT!(DGDT>DGDAT)  F DFN=0:0 S DFN=$O(^DGPM("AMV2",DGDT,DFN)) Q:'DFN  F DGIFN=0:0 S DGIFN=$O(^DGPM("AMV2",DGDT,DFN,DGIFN)) Q:'DGIFN  D CK
 Q
CK ;check to see if xfr to absence
 Q:'$D(^DGPM(+DGIFN,0))  S DGX=^(0),DGCA=$P(DGX,"^",14)
 Q:'$D(^DGPM(+DGCA,0))
 I DGA'[("^"_$P(DGX,"^",18)_"^") Q  ;not mvt to absence
 S DGFL=0 ;DGFL=1 if patient returned from absence
 S DGFL=0 F DGI=DGDT+.0000005:0 S DGI=$O(^DGPM("APCA",DFN,DGCA,DGI)) Q:'DGI!(DGI>DGDAT)  S DGJ=$O(^DGPM("APCA",DFN,DGCA,+DGI,0)) I $D(^DGPM(+DGJ,0)) S DGY=^(0) D OUT
 I 'DGFL D SET
 Q
SET S VAIP("D")=DGDAT D IN5^VADPT S:VAIP(1)']"" DGFL=1 Q:VAIP(1)']""  S DGADM=VAIP(1) I $D(^DGPM(DGADM,0)) S $P(DGX,"^",5)=$P(^DGPM(DGADM,0),"^",5),$P(DGX,"^",6)=$P(^DGPM(DGADM,0),"^",6),$P(DGX,"^",13)=$P(^DGPM(DGADM,0),"^",13)
 Q:$P(^DGPM(DGADM,0),"^",6)']""  S DGDV=$P(^DIC(42,+$P(^DGPM(DGADM,0),"^",6),0),"^",11) S DGDV=$S(DGDV]"":$P(^DG(40.8,DGDV,0),"^"),1:"ZUNKNOWN"),DGP=$P(^DPT(DFN,0),"^",1)
 D PID^VADPT6
 S DGTYP=$P(DGX,"^",18),DGTP=$S(DGTYP=1:"PASS",DGTYP=2!(DGTYP=26):"AA",DGTYP=3!(DGTYP=25):"UA",DGTYP=13!(DGTYP=43)!(DGTYP=44)!(DGTYP=45):"ASIH",1:""),DGCD=0,X=+DGX\1 D:X]"" DAT S:DGCD DGLV=DGCD
 I "^25^26^"[("^"_+VAIP(4)_"^") S DGTP=$S(+VAIP(4)=25:"UA",1:"AA")
 S DGCD=0 S X=$S($P(DGX,"^",13):$P(DGX,"^",13),1:"") D:X]"" DAT S DGRT=$S(DGCD:DGCD,1:"")
 ;IHS/ITSC/WAR 3/17/04 Changed to $$GET1^DIQ
 ;S DGW=$S($P(DGX,"^",5)]""&($P(DGX,"^",5)'?.N):$P(DGX,"^",5),$P(DGX,"^",5)]""&($P(DGX,"^",5)?.N):$P(^DIC(4,$P(DGX,"^",5),0),"^",1),1:$P(^DIC(42,$P(DGX,"^",6),0),"^"))
 S DGW=$S($P(DGX,"^",5)]""&($P(DGX,"^",5)'?.N):$P(DGX,"^",5),$P(DGX,"^",5)]""&($P(DGX,"^",5)?.N):$$GET1^DIQ(405,+DGIFN,.05),1:$P(^DIC(42,$P(DGX,"^",6),0),"^"))
 I DGFL Q  ;already returned...don't store
 I $D(DGDFN(DFN)) Q  ;patient already stored once
 I $D(DGASIH) S DGDFN(DFN)=DFN,^UTILITY($J,"DG",DGDV,DGP,DFN)=VA("PID")_"^"_DGLV_"^"_DGW Q
 S DGDFN(DFN)=DFN,^UTILITY($J,"DG",DGDV,DGP,DFN)=VA("PID")_"^"_DGTP_"^"_DGLV_"^"_DGRT_"^"_DGW S (VA("PID"),DGTP,DGLV,DGRT,DGW)=""
 Q
 ;
OUT ;check to see if patient returned from absence - DGFL=1 if yes
 S DGTYP=$P(DGY,"^",18)
 I $P(DGY,"^",2)=3,(DGTYP'=42),+DGY<DGDAT S DGFL=1 Q
 I $P(DGY,"^",2)=3,(DGTYP'=41),+DGY<DGDAT S DGFL=1 Q
 I "^14^22^23^24^"[("^"_DGTYP_"^"),+DGY<DGDAT S DGFL=1 Q
 I DGNOW<+DGY Q  ;future movement...ignore
 I DGTYP=42,+DGY<DGDAT S DGFL=1 Q  ;if WHILE ASIH and not future...
 Q
DAT S DGCD=$E(X,4,5)_"-"_$E(X,6,7)_"-"_(1700+$E(X,1,3)) Q
HEAD S DGPG=DGPG+1 W @IOF,!,"ABSENCE LIST FOR ",DGDAY,?40,"PRINTED: ",DGTIME,?72," PAGE: "_DGPG
 W !!,"NAME",?22,"PT ID",?36,"TYPE",?42,"LEAVE",?54,"RETURN",?66,"WARD"
 W !,DGCL,!!,?10,"DIVISION: ",$S(DGDV="ZUNKNOWN":"UNKNOWN",1:DGDV),!
 Q
WR S (DGU,DGDV,DGFL,DGP)=0
 F DGD=0:0 S DGDV=$O(^UTILITY($J,"DG",DGDV)) Q:DGDV=""!(DGU)  S DGPG=0 D:DGFL RT Q:DGU  D HEAD F M=0:0 S DGP=$O(^UTILITY($J,"DG",DGDV,DGP)) Q:DGP=""!(DGU)  D WRCNT
 W ! Q
WRCNT F DFN=0:0 S DFN=$O(^UTILITY($J,"DG",DGDV,DGP,DFN)) Q:'DFN!(DGU)  S DGNO=^UTILITY($J,"DG",DGDV,DGP,DFN) D WR1 S DGFL=1
 Q
WR1 I $Y+4>IOSL D:IOST?1"C-".E RT Q:DGU  D HEAD
 W !,$E(DGP,1,20),?22,$P(DGNO,"^"),?36,$P(DGNO,"^",2),?42,$P(DGNO,"^",3),?54,$P(DGNO,"^",4),?66,$E($P(DGNO,"^",5),1,14) Q
RT Q:IOST'?1"C-".E
 F X=$Y:1:(IOSL-2) W !
 R !?22,"Enter <RET> to continue or ^ to Quit",X:DTIME S:X["^"!('$T) DGU=1 Q:DGU=1
