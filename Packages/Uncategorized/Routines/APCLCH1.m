APCLCH1 ; IHS/CMI/LAB - COMMUNITY HEALTH PROFILE ;
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
 ;
 ;
START ; 
 S APCLJOB=$J,APCLBTH=$H
 K ^XTMP("APCLCH1",APCLJOB,APCLBTH)
 D XTMP^APCLOSUT("APCLCH1","PCC - COMMUNITY HEALTH PROFILE")
 D INFORM
SU S B=$P(^AUTTLOC(DUZ(2),0),U,5) I B S S=$P(^AUTTSU(B,0),U),DIC("A")="Please Identify your Service Unit: "_S_"//"
 S DIC="^AUTTSU(",DIC(0)="AEMQZ" W ! D ^DIC K DIC
 I X="^" G XIT
 I X="" S (APCLSU,APCLSUF)=B G GETDATES
 G:Y=-1 GETDATES
 S (APCLSU,APCLSUF)=+Y
GETDATES ;
BD ;
 W !!,"Enter the time frame of interest.",! S DIR(0)="D^::EP",DIR("A")="Enter Beginning Visit Date",DIR("?")="Enter the beginning visit date for the search." D ^DIR K DIR S:$D(DUOUT) DIRUT=1
 G:$D(DIRUT) XIT
 S APCLBD=Y
ED ;
 S DIR(0)="DA^::EP",DIR("A")="Enter Ending Visit Date:  " D ^DIR K DIR S:$D(DUOUT) DIRUT=1
 G:$D(DIRUT) XIT
 I Y<APCLBD W !,"Ending date must be greater than or equal to beginning date!" G ED
 S APCLED=Y
 S X1=APCLBD,X2=-1 D C^%DTC S APCLSD=X
COMM ;
 S APCLCOMT="" K APCLQUIT,^XTMP("APCLCH1",APCLJOB,APCLBTH,"COMMUNITIES")
 K DIR S DIR(0)="S^O:ONE Particular Community;S:All Communities within the "_$P(^AUTTSU(APCLSU,0),U)_" SERVICE UNIT;T:A TAXONOMY or selected set of Communities"
 S DIR("A")="Enter a code indicating what COMMUNITIES of RESIDENCE are of interest",DIR("B")="O" K DA D ^DIR K DIR,DA
 G:$D(DIRUT) GETDATES
 S APCLCOMT=Y
 D @APCLCOMT
 G:$D(APCLQUIT) COMM
ZIS ;
DEMO ;
 D DEMOCHK^APCLUTL(.APCLDEMO)
 I APCLDEMO=-1 G COMM
 S XBRP="^APCLCH1P",XBRC="^APCLCH11",XBNS="APCL",XBRX="XIT^APCLCH1"
 D ^XBDBQUE
 Q
 ;
XIT ;
 K APCLQUIT,APCLCOMT,APCLBD,APCLED,APCLDFN,APCLSD,APCLX,APCLY,APCLER,APCL1,APCL2,APCL3,APCL4,APCLAGEP,APCLC,APCLCOMI,APCLLCOM,APCLMDFN,APCLSCOM,APCLSU,APCLSUF,APCLVAL
 D KILL^AUPNPAT
 D ^XBFMK
 Q
O ;one community
 S DIC="^AUTTCOM(",DIC(0)="AEMQ",DIC("A")="Which COMMUNITY: " D ^DIC K DIC
 I Y=-1 S APCLQUIT="" Q
 S ^XTMP("APCLCH1",APCLJOB,APCLBTH,"COMMUNITIES",$P(^AUTTCOM(+Y,0),U))=""
 Q
S ;all communities within APCLSU su
 S X=0 F  S X=$O(^AUTTCOM(X)) Q:X'=+X  I $P(^AUTTCOM(X,0),U,5)=APCLSU S ^XTMP("APCLCH1",APCLJOB,APCLBTH,"COMMUNITIES",$P(^AUTTCOM(X,0),U))=""
 Q
 ;
T ;taxonomy - call qman interface
 K APCLCOMM
 S X="COMMUNITY",DIC="^AMQQ(5,",DIC(0)="FM",DIC("S")="I $P(^(0),U,14)" D ^DIC K DIC,DA I Y=-1 W "OOPS - QMAN NOT CURRENT - QUITTING" G XIT
 D PEP^AMQQGTX0(+Y,"APCLCOMM(")
 I '$D(APCLCOMM) G COMM
 I $D(APCLCOMM("*")) K APCLCOMM,^XTMP("APCLCH1",APCLJOB,APCLBTH,"COMMUNITIES") W !!,$C(7),$C(7),"ALL communities is NOT an option with this report",! G T
 S X="" F  S X=$O(APCLCOMM(X)) Q:X=""  S ^XTMP("APCLCH1",APCLJOB,APCLBTH,"COMMUNITIES",X)=""
 K APCLCOMM
 Q
INFORM ;tell user what is going on
 ;
 W:$D(IOF) @IOF
 W !!?10,"*************  COMMUNITY HEALTH PROFILE  ************"
 W !!,"This report will present a profile of health care for patients who reside in a",!,"community or communities that you select.  You will be asked to enter a date",!,"range and to identify the communities of interest.",!!
 Q
SET ;EP - ENTRY POINT
 S APCLC="" F  S APCLC=$O(^XTMP("APCLCH1",APCLJOB,APCLBTH,"RP",APCLC)) Q:APCLC=""   D
 .S APCL4="REPORT",APCL1="OUTDXC",APCL3="OUTDX" D SET1
 .S APCL4="REPORT",APCL1="INDXC",APCL3="INDX" D SET1
 .S APCL4="REPORT",APCL1="INJC",APCL3="INJ" D SET1
 .S APCL4="REPORT",APCL1="DENTALC",APCL3="DENT" D SET1
 .S APCL4="REPORT",APCL1="SURG PROCC",APCL3="SURG PROC" D SET1
 D SETSU
 Q
SET1 ;
 S APCL2="^XTMP(""APCLCH1"",APCLJOB,APCLBTH,""RP"","""_APCLC_""","""_APCL4_""","""_APCL3_""",X)"
 S X="" F  S X=$O(@APCL2) Q:X=""  S %=^(X) S ^XTMP("APCLCH1",APCLJOB,APCLBTH,"RP",APCLC,APCL4,APCL1,9999999-%,X)=%
 Q
SETSU ;
 S APCL4="SU",APCL1="OUTDXC",APCL3="OUTDX" D SETSU1
 S APCL4="SU",APCL1="INDXC",APCL3="INDX" D SETSU1
 S APCL4="SU",APCL1="INJC",APCL3="INJ" D SETSU1
 S APCL4="SU",APCL1="DENTALC",APCL3="DENT" D SETSU1
 S APCL4="SU",APCL1="SURG PROCC",APCL3="SURG PROC" D SETSU1
 Q
SETSU1 ;
 S APCL2="^XTMP(""APCLCH1"",APCLJOB,APCLBTH,"""_APCL4_""","""_APCL3_""",X)"
 S X="" F  S X=$O(@APCL2) Q:X=""  S %=^(X) S ^XTMP("APCLCH1",APCLJOB,APCLBTH,APCL4,APCL1,9999999-%,X)=%
 Q
