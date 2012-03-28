BMCRRNDA ; IHS/PHXAO/TMJ - NUMBER OF DAYS AUTHORIZED ; 
 ;;4.0;REFERRED CARE INFO SYSTEM;;JAN 09, 2006
 ;
START ;
 S BMCJOB=$J,BMCBTH=$H
 W !!,"This report will tally all in-house referrals by provider of service.",!!
D ;DATE RANGE
BD ;get beginning date
 W ! S DIR(0)="D^::EP",DIR("A")="Enter beginning Date of Modification" D ^DIR K DIR S:$D(DUOUT) DIRUT=1
 I $D(DIRUT) G XIT
 S BMCBD=Y
ED ;get ending date
 W ! S DIR(0)="D^"_BMCBD_"::EP",DIR("A")="Enter ending Date of Modification"  D ^DIR K DIR S:$D(DUOUT) DIRUT=1
 I $D(DIRUT) G BD
 S BMCED=Y
 S X1=BMCBD,X2=-1 D C^%DTC S BMCSD=X
 ;
PAT ;Get Patient
 S BMCPAT="" S DIR(0)="S^A:ALL Patients;O:ONE Patient",DIR("A")="Report should tally referrals for ",DIR("B")="A" K DA D ^DIR K DIR
 G:$D(DIRUT) D
 I Y="O" D GETPAT G:BMCPAT="" PAT
ZIS ;
 S XBRP="PRINT^BMCRRNDA",XBRC="PROC^BMCRRNDA",XBRX="XIT^BMCRRNDA",XBNS="BMC" ;IHS/TUCSON/LAB - changed XBNX to XBNS - 05/30/97
 D ^XBDBQUE
 D XIT
 Q
XIT ;EP
 K BMCPAT,BMCREF,BMCODAT,BMCBD,BMCED,BMCSD,BMCQUIT,BMCPG,BMCCLIN,BMCBT,BMCBTH,BMCET,BMCRREC,C,D,DFN,DIC,DIRUT,P,X,X1,X2,BMCSTOT
 D KILL^AUPNPAT
 Q
GETPAT ;
PATIENT ; GET PATIENT
 F  D PATIENT2 I BMCQ!($G(BMCDFN)) Q
 Q
 ;
PATIENT2 ; ASK FOR PATIENT UNTIL USER SELECTS OR QUITS
 S BMCQ=1
 S DIC="^AUPNPAT(",DIC(0)="AEMQ" D DIC^BMCFMC
 Q:Y<1
 S BMCDFN=+Y,BMCREC("PAT NAME")=$P(^DPT(+Y,0),U)
 S BMCQ=0
 I $$DOD^AUPNPAT(BMCDFN) D  I 'Y K BMCDFN,BMCREC("PAT NAME") Q
 . W !!,"This patient is deceased."
 . S DIR(0)="YO",DIR("A")="Are you sure you want this patient",DIR("B")="NO" K DA D ^DIR K DIR
 . W !
 . Q
 S BMCPAT=+Y
 Q
 ;
PROC ;EP called from xbdbque
 S BMCBT=$H
 S BMCODAT=$O(^BMCREF("B",BMCSD)) I BMCODAT="" S BMCET=$H Q
 S BMCODAT=BMCSD_".9999" F  S BMCODAT=$O(^BMCREF("B",BMCODAT)) Q:BMCODAT=""!((BMCODAT\1)>BMCED)  D R1
END ;
 S BMCET=$H
 Q
R1 ;
 S BMCREF="" F  S BMCREF=$O(^BMCREF("B",BMCODAT,BMCREF)) Q:BMCREF'=+BMCREF  I $P(^BMCREF(BMCREF,0),U,4)'="N" S BMCRREC=^BMCREF(BMCREF,0),DFN=$P(BMCRREC,U,3) D PROCR
 Q
PROCR ;
 ;I BMCPAT,$P(BMCRREC,U,3)'=BMCPAT Q
 S P=$P(BMCRREC,U,3)
 S BMCPAT=P
 ;S C=$S(C:$P(^DIC(40.7,C,0),U),1:"<UNKNOWN>")
 S P=$S(P:$P(^DPT(P,0),U),1:"<UNKNOWN>")
 ;S ^(C)=$S($D(^XTMP("BMCRRNDA",BMCJOB,BMCBTH,"PAT",P)):^(C)+1,1:1)
 Q:$P(BMCRREC,U,15)'="A"  ; QUIT IF NOT ACTIVE
 Q:$P($G(^BMCREF(BMCREF,11)),U,11)<2  ;QUIT IF NO ADDITIONAL VISITS AUTHORIZED
 Q:$P($G(^BMCREF(BMCREF,11)),U,26)=""  ; QUIT OF NO MODIFIED DT
 S ^XTMP("BMCRRNDA",BMCJOB,BMCBTH,"PATIENT",P,BMCPAT,BMCREF)=""
 Q
PRINT ;EP called from xbdbque
 S BMCPG=0 D @("HEAD"_(2-($E(IOST,1,2)="C-")))
 S BMCQUIT=0
 I '$D(^XTMP("BMCRRNDA",BMCJOB,BMCBTH)) W !!,"No # DAYS AUHTORIZED FOR THIS RUN",! G DONE
 S BMCPAT="" F  S BMCPAT=$O(^XTMP("BMCRRNDA",BMCJOB,BMCBTH,"PAT",BMCPAT)) Q:BMCPAT=""!(BMCQUIT)  D PATNAME
DONE ;
 K ^XTMP("BMCRRNDA",BMCJOB,BMCBTH)
 D DONE^BMCRLP2
 Q
PATNAME ;
 I $Y>(IOSL-5) D HEAD Q:BMCQUIT
 W !!,BMCPAT S BMCSTOT=0
 S BMCPATN="" F  S BMCPATN=$O(^XTMP("BMCRRNDA",BMCJOB,BMCBTH,"PAT",BMCPAT,BMCPATN)) Q:BMCPATN=""!(BMCQUIT)  D
 .I $Y>(IOSL-5) D HEAD Q:BMCQUIT
 .W !?25,BMCPATN,?55,$J(^XTMP("BMCRRNDA",BMCJOB,BMCBTH,"PAT",BMCPAT,BMCPATN),5) S BMCSTOT=BMCSTOT+^(BMCPATN)
 .Q
 I $Y>(IOSL-5) D HEAD Q:BMCQUIT
 W !!,"Total for ",BMCPAT,?55,$J(BMCSTOT,5)
 Q
HEAD ;
 I $E(IOST)="C",IO=IO(0) W ! S DIR(0)="EO" D ^DIR K DIR I Y=0!(Y="^")!($D(DTOUT)) S BMCQUIT=1 Q
HEAD1 ;
 W:$D(IOF) @IOF
HEAD2 ;
 S BMCPG=BMCPG+1
 W !?55,$$FMTE^XLFDT(DT),?72,"Page ",BMCPG,!
 W ?20,"IN-HOUSE REFERRALS BY PATIDER",!
 W ?14,"REFERRAL DATE RANGE: ",$$FMTE^XLFDT(BMCBD)," to ",$$FMTE^XLFDT(BMCED)
 W !!,"PATIDER",?25,"CLINIC REFERRED TO",?55,"NUMBER"
 W !,$TR($J(" ",80)," ","-")
 Q
