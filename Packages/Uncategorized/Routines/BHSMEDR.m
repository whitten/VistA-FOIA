BHSMEDR ;IHS/MSC/MGH - Health Summary for MED RECONCILIATION ;04-Aug-2011 14:37;MGH
 ;;1.0;HEALTH SUMMARY COMPONENTS;**4,6**;March 17, 2006;Build 5
 ;;---------------------------------------------------------------
 ; IHS/CMI/LAB - APCHS7R -- SUMMARY PRODUCTION COMPONENTS ;
 ;;2.0;IHS PCC SUITE;**2**;MAY 14, 2009
 ;
 ;Patch 6 for non-VA meds
SET ;
 S BHSRX=P,BHSREF=0 D REF
 S BHSTAT(C,N,D,(9999999-F),M)=$S(P:$P(^PSRX(P,0),U),1:"")_U_BHSREF
 S BHMDSP(D,N,F)=""
 Q
MEDRCON ; ************* MEDS BY PRESCRIPTION STATUS *************
 ;
CONT ; <SETUP>
 N BHSPAT,BHSQ
 S BHSPAT=DFN
 I '$D(^AUPNVMED("AC",BHSPAT)),'$D(^PS(52.41,"P",BHSPAT)) Q
 D CKP^GMTSUP Q:$D(GMTSQIT)
 ; <BUILD>
 NEW BHMEDS,BHSTAT,BHCNT,BHMDSP,BHDI,BHDT,BHM,BHN,BHSSGY,BHSIG,BHZ,BHSN,BHST,BHI,BHD
 NEW X,F,M,V,D,N,P,C,E,S,R,J,BHSITE,BHSORT,BHSP,BHSREF,BHSRFL,BHSRX,BHSVDF,BHT
 S BHCNT=0
 K BHMEDS,BHMDSP
 D GETMEDS^APCHSMU1(BHSPAT,$$FMADD^XLFDT(DT,-395),DT,,,,,.BHMEDS)
 ;I '$D(APCHMEDS) D MEDX Q
 ;NOW REORDER THEM BY STATUS
 K BHSTAT
 S X=0 F  S X=$O(BHMEDS(X)) Q:X'=+X  D
 .S P=""
 .S F=$P(BHMEDS(X),U,1)  ;FILL DATE
 .S M=$P(BHMEDS(X),U,4)  ;vmed ien
 .S V=$P(BHMEDS(X),U,5)  ;visit ien
 .S D=$P(^AUPNVMED(M,0),U)  ;drug ien
 .S N=$P(^AUPNVMED(M,0),U,4) ;non table drug name
 .I N="" S N=$P(^PSDRUG(D,0),U)  ;drug name
 .I $P($G(^AUPNVSIT(X,0)),U,7)="E" S C="OUTSIDE MEDICATIONS" D SET Q
 .I $P($G(^AUPNVSIT(X,11)),U,8) S C="OUTSIDE MEDICATIONS" D SET Q
 .S P=$O(^PSRX("APCC",M,0))
 .I 'P S C="OUTSIDE MEDICATIONS" D SET Q
 .I '$D(^PSRX(P,0)) S P="",C="OUTSIDE MEDICATIONS" D SET Q
 .S S=$$VALI^XBDIQ1(52,P,100)  ;GET STATUS
 .I S=0 S C="ACTIVE MEDICATIONS" D SET Q
 .I S=3 S C="HOLD" D SET Q
 .I S=5 S C="SUSPENDED" D SET Q
 .I S=11 D  Q
 ..;get expiration date
 ..S E=$P($G(^PSRX(P,3)),U,6)
 ..S R=$$CHRONIC^APCHS72(M)  ;chronic flag
 ..I 'R D  Q
 ...;not chronic, check to see if expired in past 14 days, if not quit
 ...S J=$$FMDIFF^XLFDT(DT,E)
 ...Q:J>14  ;more than 14 days ago so don't display
 ...;check to see if same drug is already listed somewhere
 ...Q:$O(BHMDSP(D,N,F))  ;another of same drug after this date
 ...S C="EXPIRED" D SET Q
 ..;chronic = check 120 days
 ..S J=$$FMDIFF^XLFDT(DT,E)
 ..Q:J>120  ;expired more than 120 days ago
 ..Q:$O(BHMDSP(D,N,F))  ;another one there so don't display this one
 ..S C="EXPIRED" D SET Q
 .I S=12!(S=14) D
 ..S E=$P(^AUPNVMED(M,0),U,8)  ;discontinued date in v med
 ..I E="" S E=$P($G(^PSRX(P,3)),U,5)  ;canceled date in 52
 ..I $$FMDIFF^XLFDT(DT,E)>30 Q  ;only discontinueds in past 30 days
 ..Q:$O(BHMDSP(D,N,F))
 ..S C="DISCONTINUED MEDICATIONS" D SET Q
GETNVA ;NVA from file 55
 N L,D,N,X
 S X=0 F  S X=$O(^PS(55,BHSPAT,"NVA",X)) Q:X'=+X  D
 .I $P($G(^PS(55,BHSPAT,"NVA",X,999999911)),U,1),$D(^AUPNVMED($P(^PS(55,BHSPAT,"NVA",X,999999911),U,1),0)) Q  ;got this with V MED
 .S L=$P($P($G(^PS(55,BHSPAT,"NVA",X,0)),U,10),".")
 .S L=9999999-L
 .S D=$P(^PS(55,BHSPAT,"NVA",X,0),U,2)
 .I D="" S D="NO DRUG IEN"
 .S N=$S(D:$P(^PSDRUG(D,0),U,1),1:$P(^PS(50.7,$P(^PS(55,BHSPAT,"NVA",X,0),U,1),0),U,1))
 .S BHSTAT("NVA",N,D,(9999999-L))=U_"N",$P(BHSTAT("NVA",N,D,(9999999-L)),U,8)=$P(^PS(55,BHSPAT,"NVA",X,0),U,4)_" "_$P(^PS(55,BHSPAT,"NVA",X,0),U,5)_U_$P(^PS(55,BHSPAT,"NVA",X,0),U,7)
GETPEND ;
 NEW PEN,ORD
 F PEN=0:0 S PEN=$O(^PS(52.41,"P",BHSPAT,PEN)) Q:'PEN  S ORD=^PS(52.41,PEN,0),BHI=$P(ORD,"^",8),BHD=+$P(ORD,"^",9) D:$P(ORD,"^",3)'="DC"&($P(ORD,"^",3)'="DE")&($P(ORD,"^",3)'="HD")
 .S BHN=$S(BHD:$P($G(^PSDRUG(BHD,0)),"^"),+BHI&('BHD):$P(^PS(50.7,BHI,0),"^")_" "_$P(^PS(50.606,$P(^PS(50.7,BHI,0),"^",2),0),"^"),1:"") Q:BHN']""
 .S BHSTAT("PENDING",BHN,PEN)=$$VAL^XBDIQ1(52.41,PEN,13)
 .S C=0,X="" F  S C=$O(^PS(52.41,PEN,"SIG",C)) Q:'C  S X=X_$S(X]"":" ",1:"")_^PS(52.41,PEN,"SIG",C,0)
 .S $P(BHSTAT("PENDING",BHN,PEN),U,2)=X
DISP ;DISPLAY MEDS
 ;ACTIVE MEDS FIRST - ALL OF THEM
 D CKP^GMTSUP Q:$D(GMTSQIT)
 I '$D(BHSTAT("ACTIVE MEDICATIONS")) G OUT
 W "ACTIVE MEDICATIONS",!
 S BHCNT=0
 S BHT=1
 S BHN="" F  S BHN=$O(BHSTAT("ACTIVE MEDICATIONS",BHN)) Q:BHN=""!($D(GMTSQIT))  D
 .S BHDI="" F  S BHDI=$O(BHSTAT("ACTIVE MEDICATIONS",BHN,BHDI)) Q:BHDI=""!($D(GMTSQIT))  D
 ..S BHDT=0 F  S BHDT=$O(BHSTAT("ACTIVE MEDICATIONS",BHN,BHDI,BHDT)) Q:BHDT'=+BHDT!($D(GMTSQIT))  D
 ...S BHM=0 F  S BHM=$O(BHSTAT("ACTIVE MEDICATIONS",BHN,BHDI,BHDT,BHM)) Q:BHM'=+BHM!($D(GMTSQIT))  S BHZ=BHSTAT("ACTIVE MEDICATIONS",BHN,BHDI,BHDT,BHM) D MEDDSP
OUT ;OUTSIDE MEDICATIONS
 I '$D(BHSTAT("OUTSIDE MEDICATIONS")) G HOLD
 W "--------------------",!
 W "OUTSIDE MEDICATIONS",!
 S BHN="" F  S BHN=$O(BHSTAT("OUTSIDE MEDICATIONS",BHN)) Q:BHN=""!($D(GMTSQIT))  D
 .S BHDI="" F  S BHDI=$O(BHSTAT("OUTSIDE MEDICATIONS",BHN,BHDI)) Q:BHDI=""!($D(GMTSQIT))  D
 ..S BHDT=0 S BHDT=$O(BHSTAT("OUTSIDE MEDICATIONS",BHN,BHDI,BHDT)) Q:BHDT'=+BHDT!($D(GMTSQIT))  D
 ...S BHM=0 S BHM=$O(BHSTAT("OUTSIDE MEDICATIONS",BHN,BHDI,BHDT,BHM)) Q:BHM'=+BHM!($D(GMTSQIT))  S BHZ=BHSTAT("OUTSIDE MEDICATIONS",BHN,BHDI,BHDT,BHM) D MEDDSPO
 ;now display nva
 S BHN="" F  S BHN=$O(BHSTAT("NVA",BHN)) Q:BHN=""!($D(GMTSQIT))  D
 .S BHDI="" F  S BHDI=$O(BHSTAT("NVA",BHN,BHDI)) Q:BHDI=""!($D(GMTSQIT))  D
 ..S BHDT=0 S BHDT=$O(BHSTAT("NVA",BHN,BHDI,BHDT)) Q:BHDT'=+BHDT!($D(GMTSQIT))  D
 ...S BHZ=BHSTAT("NVA",BHN,BHDI,BHDT) D MEDDSPN
HOLD ;HOLD MEDICATIONS
 I '$D(BHSTAT("HOLD MEDICATIONS")) G SUSPEND
 S BHT=3
 W "--------------------",!
 W "ACTIVE NOT DISPENSED MEDICATIONS",!
 S BHN="" F  S BHN=$O(BHSTAT("HOLD MEDICATIONS",BHN)) Q:BHN=""!($D(GMTSQIT))  D
 .S BHDI="" F  S BHDI=$O(BHSTAT("HOLD MEDICATIONS",BHN,BHDI)) Q:BHDI=""!($D(GMTSQIT))  D
 ..S BHDT=0 F  S BHDT=$O(BHSTAT("HOLD MEDICATIONS",BHN,BHDI,BHDT)) Q:BHDT'=+BHDT!($D(GMTSQIT))  D
 ...S BHM=0 F  S BHM=$O(BHSTAT("HOLD MEDICATIONS",BHN,BHDI,BHDT,BHM)) Q:BHM'=+BHM!($D(GMTSQIT))  S BHZ=BHSTAT("HOLD MEDICATIONS",BHN,BHDI,BHDT,BHM) D MEDDSP
SUSPEND ;
 I '$D(BHSTAT("SUSPEND MEDICATIONS")) G PENDING
 S BHT=4
 W !,"--------------------",!
 W "SUSPENDED MEDICATIONS",!
 S BHN="" F  S BHN=$O(BHSTAT("SUSPEND MEDICATIONS",BHN)) Q:BHN=""!($D(GMTSQIT))  D
 .S BHDI="" F  S BHDI=$O(BHSTAT("SUSPEND MEDICATIONS",BHN,BHDI)) Q:BHDI=""!($D(GMTSQIT))  D
 ..S BHDT=0 F  S BHDT=$O(BHSTAT("SUSPEND MEDICATIONS",BHN,BHDI,BHDT)) Q:BHDT'=+BHDT!($D(GMTSQIT))  D
 ...S BHM=0 F  S BHM=$O(BHSTAT("SUSPEND MEDICATIONS",BHN,BHDI,BHDT,BHM)) Q:BHM'=+BHM!($D(GMTSQIT))  S BHZ=BHSTAT("SUSPEND MEDICATIONS",BHN,BHDI,BHDT,BHM) D MEDDSP
PENDING ;
 I '$D(BHSTAT("PENDING")) G EXPIRED
 W "--------------------",!
 W "PENDING MEDICATIONS",!
 S BHN="" F  S BHN=$O(BHSTAT("PENDING",BHN)) Q:BHN=""!($D(GMTSQIT))  D
 .S BHDI="" F  S BHDI=$O(BHSTAT("PENDING",BHN,BHDI)) Q:BHDI=""!($D(GMTSQIT))  D
 ..S BHZ=BHSTAT("PENDING",BHN,BHDI) D MEDDSPP
EXPIRED ;
 I '$D(BHSTAT("EXPIRED MEDICATIONS")) G DISCONT
 S BHT=6
 W "--------------------",!
 W "CHRONIC AND RECENTLY EXPIRED MEDICATIONS",!
 S BHN="" F  S BHN=$O(BHSTAT("EXPIRED MEDICATIONS",BHN)) Q:BHN=""!($D(GMTSQIT))  D
 .S BHDI="" F  S BHDI=$O(BHSTAT("EXPIRED MEDICATIONS",BHN,BHDI)) Q:BHDI=""!($D(GMTSQIT))  D
 ..S BHDT=0 F  S BHDT=$O(BHSTAT("EXPIRED MEDICATIONS",BHN,BHDI,BHDT)) Q:BHDT'=+BHDT!($D(GMTSQIT))  D
 ...S BHM=0 F  S BHM=$O(BHSTAT("EXPIRED MEDICATIONS",BHN,BHDI,BHDT,BHM)) Q:BHM'=+BHM!($D(GMTSQIT))  S BHZ=BHSTAT("EXPIRED MEDICATIONS",BHN,BHDI,BHDT,BHM) D MEDDSP
DISCONT ;
 I '$D(BHSTAT("DISCONTINUED MEDICATIONS")) G MEDX
 S BHT=7
 W "--------------------",!
 W "RECENTLY DISCONTINUED MEDICATIONS",!
 S BHN="" F  S BHN=$O(BHSTAT("DISCONTINUED MEDICATIONS",BHN)) Q:BHN=""!($D(GMTSQIT))  D
 .S BHDI="" F  S BHDI=$O(BHSTAT("DISCONTINUED MEDICATIONS",BHN,BHDI)) Q:BHDI=""!($D(GMTSQIT))  D
 ..S BHDT=0 F  S BHDT=$O(BHSTAT("DISCONTINUED MEDICATIONS",BHN,BHDI,BHDT)) Q:BHDT'=+BHDT!($D(GMTSQIT))  D
 ...S BHM=0 F  S BHM=$O(BHSTAT("DISCONTINUED MEDICATIONS",BHN,BHDI,BHDT,BHM)) Q:BHM'=+BHM!($D(GMTSQIT))  S BHZ=BHSTAT("DISCONTINUED MEDICATIONS",BHN,BHDI,BHDT,BHM) D MEDDSP
MEDX ;
 Q
MEDDSPP ;DISPLAY MEDICATION
 S BHCNT=BHCNT+1
 D CKP^GMTSUP Q:$D(GMTSQIT)
 W BHCNT,".",?6,BHN W:$P(BHZ,U,2) ?60,"Refills: ",$S('$P(BHZ,U,1):"NONE",1:$P(BHZ,U,1)) W !
 D CKP^GMTSUP Q:$D(GMTSQIT)
 K ^UTILITY($J,"W") S X=$P(BHZ,U,2),DIWL=0,DIWR=60 D ^DIWP
 W ?6,"Directions: "_$S($L($G(^UTILITY($J,"W",0,1,0)))>1:$G(^UTILITY($J,"W",0,1,0)),$L($G(^UTILITY($J,"W",0,1,0)))=1:"No directions on file",1:" "),!
 I $G(^UTILITY($J,"W",0))>1 F F=2:1:$G(^UTILITY($J,"W",0)) Q:$D(GMTSQIT)  D
 .D CKP^GMTSUP Q:$D(GMTSQIT)
 .W ?19,$G(^UTILITY($J,"W",0,F,0)),!
 K ^UTILITY($J,"W")
 Q
MEDDSPO ;DISPLAY MEDICATION
 S BHSN=^AUPNVMED(BHM,0)
 S BHCNT=BHCNT+1
 D CKP^GMTSUP Q:$D(GMTSQIT)
 W BHCNT,".",?6,BHN W:$P(BHZ,U,2) ?60,"Refills left: ",$S('$P(BHZ,U,2):"NONE",1:$P(BHZ,U,2)) W !
 D CKP^GMTSUP Q:$D(GMTSQIT)
 S BHSIG=$P(^AUPNVMED(BHM,0),U,5) D SIG
 S X=BHSSGY
 K ^UTILITY($J,"W") S DIWL=0,DIWR=60 D ^DIWP
 D CKP^GMTSUP Q:$D(GMTSQIT)
 W ?6,"Directions: "_$S($L($G(^UTILITY($J,"W",0,1,0)))>1:$G(^UTILITY($J,"W",0,1,0)),$L($G(^UTILITY($J,"W",0,1,0)))=1:"No directions on file",1:" "),!
 I $G(^UTILITY($J,"W",0))>1 F F=2:1:$G(^UTILITY($J,"W",0)) Q:$D(GMTSQIT)  D
 .D CKP^GMTSUP Q:$D(GMTSQIT)
 .W ?19,$G(^UTILITY($J,"W",0,F,0)),!
 K ^UTILITY($J,"W")
 Q
MEDDSP ;DISPLAY MEDICATION
 S BHSN=^AUPNVMED(BHM,0)
 S BHCNT=BHCNT+1
 D CKP^GMTSUP Q:$D(GMTSQIT)
 W BHCNT,".",?6,BHN,?40,"Rx #:",$P(BHZ,U,1),?60,"Refills left: ",$S('$P(BHZ,U,2):"NONE",1:$P(BHZ,U,2)),!
 D CKP^GMTSUP Q:$D(GMTSQIT)
 S BHSIG=$P(^AUPNVMED(BHM,0),U,5) D SIG
 S X=BHSSGY
 K ^UTILITY($J,"W") S DIWL=0,DIWR=60 D ^DIWP
 D CKP^GMTSUP Q:$D(GMTSQIT)
 W ?6,"Directions: "_$S($L($G(^UTILITY($J,"W",0,1,0)))>1:$G(^UTILITY($J,"W",0,1,0)),$L($G(^UTILITY($J,"W",0,1,0)))=1:"No directions on file",1:" "),!
 I $G(^UTILITY($J,"W",0))>1 F F=2:1:$G(^UTILITY($J,"W",0)) Q:$D(GMTSQIT)  D
 .D CKP^GMTSUP Q:$D(GMTSQIT)
 .W ?19,$G(^UTILITY($J,"W",0,F,0)),!
 K ^UTILITY($J,"W")
 D CKP^GMTSUP Q:$D(GMTSQIT)
 I BHT=1!(BHT=6) W ?6,"Last Filled: ",$$D(9999999-BHDT) D
 .S BHSORT="" I BHT=1 S BHSORT=$P($G(^AUPNVMED(BHM,11)),U)
 .I BHSORT["RETURNED TO STOCK" W "    ---",BHSORT,"  ",$$FMTE^XLFDT($P(^AUPNVMED(BHM,0),U,8),"2D")
 I BHT=6 I $P(BHZ,U,1) S E=$P($G(^PSRX($P(BHZ,U,1),3)),U,6) W ?30,"Expired: ",$$D(E)
 W !
 I BHT=3 W ?6,"Hold Reason: " I $P(BHZ,U,1) W $P($G(^PSRX($P(BHZ,U,1),"H")),U,1)
 I BHT=7 W ?6,"Discontinued: " D
 .S E=$P(^AUPNVMED(BHM,0),U,8)  ;discontinued date in v med
 .I E="",$P(BHZ,U,1) S E=$P($G(^PSRX($P(BHZ,U,1),3)),U,5)  ;canceled date in 52
 .W $$D(E),!
 Q
MEDDSPN ;
 S BHCNT=BHCNT+1
 D CKP^GMTSUP Q:$D(GMTSQIT)
 W BHCNT,".",?6,BHN,! ;W:$P(APCHZ,U,2) ?60,"Refills left: ",$S('$P(APCHZ,U,2):"NONE",1:$P(APCHZ,U,2)) W !
 D CKP^GMTSUP Q:$D(GMTSQIT)
 S BHSIG=$P(BHZ,U,8) D SIG
 S X=BHSSGY
 K ^UTILITY($J,"W") S DIWL=0,DIWR=60 D ^DIWP
 D CKP^GMTSUP Q:$D(GMTSQIT)
 W ?6,"Directions: "_$S($L($G(^UTILITY($J,"W",0,1,0)))>1:$G(^UTILITY($J,"W",0,1,0)),$L($G(^UTILITY($J,"W",0,1,0)))=1:"No directions on file",1:" "),!
 I $G(^UTILITY($J,"W",0))>1 F F=2:1:$G(^UTILITY($J,"W",0)) Q:$D(GMTSQIT)  D
 .D CKP^GMTSUP Q:$D(GMTSQIT)
 .W ?19,$G(^UTILITY($J,"W",0,F,0)),!
 I $P(BHZ,U,9) W !?19,"DATE DISCONTINUED: ",$$FMTE^XLFDT($P(BHZ,U,9))
 K ^UTILITY($J,"W")
 Q
D(D) ;
 I D="" Q ""
 Q $E(D,4,5)_"-"_$E(D,6,7)_"-"_$E(D,2,3)
 ;
SIG ;CONSTRUCT THE FULL TEXT FROM THE ENCODED SIG
 S BHSSGY="" F BHSP=1:1:$L(BHSIG," ") S X=$P(BHSIG," ",BHSP) I X]"" D
 . S Y=$O(^PS(51,"B",X,0)) I Y>0 S X=$P(^PS(51,Y,0),"^",2) I $D(^(9)) S Y=$P(BHSIG," ",BHSP-1),Y=$E(Y,$L(Y)) S:Y>1 X=$P(^(9),"^",1)
 . S BHSSGY=BHSSGY_X_" "
 Q
 ;
REF ;DETERMINE THE NUMBER OF REFILLS REMAINING
 I 'BHSRX S BHSREF=$P($G(^AUPNVMED(M,11)),U,7) S:BHSREF="" BHSREF=0 Q
 S BHSRFL=$P(^PSRX(BHSRX,0),U,9) S BHSREF=0 F  S BHSREF=$O(^PSRX(BHSRX,1,BHSREF)) Q:'BHSREF  S BHSRFL=BHSRFL-1
 S BHSREF=BHSRFL
 Q
 ;
 ;
SITE ;DETERMINE IF OUTSIDE LOCATION INFO PRESENT
 S BHSITE=""
 I $D(^AUPNVSIT(BHSVDF,21))#2 S BHSITE=$P(^(21),U) Q
 Q:$P(^AUPNVSIT(BHSVDF,0),U,6)=""
 I $P(^AUPNVSIT(BHSVDF,0),U,6)'=DUZ(2) S BHSITE=$E($P(^DIC(4,$P(^AUPNVSIT(BHSVDF,0),U,6),0),U),1,30)
 Q
 ;
CS(D) ;
 I $P(^PSDRUG(D,0),U,3)="" Q 0
 NEW Y S Y=$P(^PSDRUG(D,0),U,3)
 ;I Y[1 Q 1
 I Y[2 Q 1
 I Y[3 Q 1
 I Y[4 Q 1
 I Y[5 Q 1
 ;I Y["C" Q 1
 ;I Y["A" Q 1
 Q 0
 ;
CTR(X,Y) ;EP - Center X in a field Y wide.
 Q $J("",$S($D(Y):Y,1:IOM)-$L(X)\2)_X
