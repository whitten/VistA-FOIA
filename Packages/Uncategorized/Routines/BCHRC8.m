BCHRC8 ; IHS/TUCSON/LAB - CHRIS II Report 2 ;  [ 04/02/01  9:46 AM ]
 ;;1.0;IHS RPMS CHR SYSTEM;**7,12**;OCT 28, 1996
 ;IHS/CMI/LAB - tmp to xtmp
 ;
 I '$G(DUZ(2)) W $C(7),$C(7),!!,"SITE NOT SET IN DUZ(2) - NOTIFY SITE MANAGER!!",!! Q
 S BCHJOB=$J,BCHBTH=$H
 D INFORM
GETDATES ;
BD ;get beginning date
 W ! S DIR(0)="D^:DT:EP",DIR("A")="Enter BEGINNING Date of Service for Report" D ^DIR K DIR S:$D(DUOUT) DIRUT=1
 I $D(DIRUT) G XIT
 S BCHBD=Y
ED ;get ending date
 W ! S DIR(0)="D^"_BCHBD_":DT:EP",DIR("A")="Enter ENDING Date of Service for Report" S Y=BCHBD D DD^%DT S DIR("B")=Y,Y="" D ^DIR K DIR S:$D(DUOUT) DIRUT=1
 I $D(DIRUT) G BD
 S BCHED=Y
 S X1=BCHBD,X2=-1 D C^%DTC S BCHSD=X
 ;
TYPE ;
 S BCHRPT=""
 S DIR(0)="S^PG:By PROGRAM (Report 8);PR:By PROVIDER (Report 8.2)",DIR("A")="Which report do you wish to run",DIR("B")="PG" K DA D ^DIR K DIR
 I $D(DIRUT) G GETDATES
 S BCHRPT=Y
 D @Y
PROG ;IHS/CMI/LAB - added program screen
 S BCHPRG=""
 S DIR(0)="Y",DIR("A")="Include data from ALL CHR Programs",DIR("?")="If you wish to include visits from ALL programs answer Yes.  If you wish to tabulate for only one program enter NO." D ^DIR K DIR
 G:$D(DIRUT) TYPE
 I Y=1 S BCHPRG="" G ZIS
PROG1 ;enter program
 K X,DIC,DA,DD,DR,Y S DIC("A")="Which CHR Program: ",DIC="^BCHTPROG(",DIC(0)="AEMQ" D ^DIC K DIC,DA G:Y<0 PROG
 S BCHPRG=+Y
ZIS ;CALL TO XBDBQUE
 S XBRP="^BCHRC8P",XBRC="PROC^BCHRC8",XBRX="XIT^BCHRC8",XBNS="BCH"
 D ^XBDBQUE
 D XIT
 Q
ERR W $C(7),$C(7),!,"Must be a valid date and be Today or earlier. Time not allowed!" Q
XIT ;
 K BCHPRG,BCHNONE,BCHFT,BCHF,BCHT,BCHREF,BCHQUIT,BCHJOB,BCHBTH,BCHBT,BCHET,BCHBD,BCHED,BCHBDD,BCHEDD,BCHSD,BCHODAT,BCHPROG,BCHX,BCHR,BCHR0,BCHPG,BCHDT,BCHIDAT,BCHMON,BCHTOT,BCHCH,BCHITEM,BCHNAME,BCHRN,BCHRPT,BCHBRK,BCHEOJ
 K M,R,V,X,Y,I
 K X,Y
 Q
 ;
INFORM ;
 W:$D(IOF) @IOF
 W !?20,"**********  CHR REPORT NO. 8  **********"
 W !!?20,"HOURS (SERVICE+TRAVEL) BY MONTH",!!,"You must enter the time frame and the program for which the report",!,"will be run.",!!
 Q
 ;
 ;
PROC ;EP - PROCESS REFERRAL REPORT
 D XTMP^BCHUTIL("BCHRC8","CHR CHRIS II REPORT")
 S (BCHBT,BCHBTH)=$H,BCHJOB=$J
 D D,EOJ
 Q
 ;
EOJ ;
 S BCHET=$H
 Q
D ; Run by date of service
 S X1=BCHBD,X2=-1 D C^%DTC S BCHSD=X
 S BCHODAT=BCHSD_".9999" F  S BCHODAT=$O(^BCHR("B",BCHODAT)) Q:BCHODAT=""!((BCHODAT\1)>BCHED)  D D1
 Q
 ;
D1 ;
 S (BCHR,BCHRCNT)=0 F  S BCHR=$O(^BCHR("B",BCHODAT,BCHR)) Q:BCHR'=+BCHR  I $D(^BCHR(BCHR,0)),$P(^(0),U,2)]"",$P(^(0),U,3)]"" S BCHR0=^(0) D PROCESS
 Q
PROCESS ;
 S BCHPROG=$P(BCHR0,U,2)
 I BCHPRG,BCHPRG'=BCHPROG Q
 I BCHRPT="PR" S BCHITEM=$P(BCHR0,U,3)
 I BCHRPT="PG" S BCHITEM=BCHPROG
 S BCHDATE=$P(BCHR0,U) S Y=BCHDATE D DD^%DT S BCHMON=$P(Y," ")_$P(Y,",",2),X=BCHMON,%DT="" D ^%DT S BCHIDAT=Y
 S BCHTOT=$P(BCHR0,U,27)+$P(BCHR0,U,11),BCHS=$P(BCHR0,U,27),BCHT=$P(BCHR0,U,11)
 S $P(^(BCHMON),U)=$S($D(^XTMP("BCHRC8",BCHJOB,BCHBT,BCHITEM,"MONTHS",BCHIDAT,BCHMON)):$P(^(BCHMON),U)+BCHTOT,1:BCHTOT)
 S $P(^XTMP("BCHRC8",BCHJOB,BCHBT,BCHITEM,"MONTHS",BCHIDAT,BCHMON),U,2)=$P(^XTMP("BCHRC8",BCHJOB,BCHBT,BCHITEM,"MONTHS",BCHIDAT,BCHMON),U,2)+BCHS
 S $P(^XTMP("BCHRC8",BCHJOB,BCHBT,BCHITEM,"MONTHS",BCHIDAT,BCHMON),U,3)=$P(^XTMP("BCHRC8",BCHJOB,BCHBT,BCHITEM,"MONTHS",BCHIDAT,BCHMON),U,3)+BCHT
 S $P(^("TOTAL"),U)=$S($D(^XTMP("BCHRC8",BCHJOB,BCHBT,BCHITEM,"TOTAL")):$P(^("TOTAL"),U)+BCHTOT,1:BCHTOT)
 S $P(^XTMP("BCHRC8",BCHJOB,BCHBT,BCHITEM,"TOTAL"),U,2)=$P(^XTMP("BCHRC8",BCHJOB,BCHBT,BCHITEM,"TOTAL"),U,2)+BCHS
 S $P(^XTMP("BCHRC8",BCHJOB,BCHBT,BCHITEM,"TOTAL"),U,3)=$P(^XTMP("BCHRC8",BCHJOB,BCHBT,BCHITEM,"TOTAL"),U,3)+BCHT
 Q
PG ;
 S BCHRN=8,BCHCH="PROGRAM"
 Q
PR ;
 S BCHRN=8.2,BCHCH="PROVIDER"
 Q
