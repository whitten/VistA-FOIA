APCDFPPV ; IHS/CMI/LAB - PRINT UNCODED DX ;
 ;;2.0;IHS PCC SUITE;**6**;MAY 14, 2009;Build 11
 ;
LOC ;
 K APCDLOCT S APCDLOCT=""
 S DIR(0)="S^A:ALL Locations/Facilities;S:One SERVICE UNIT'S Locations/Facilities;O:ONE Location/Facility",DIR("A")="Include Visits to Which Location/Facilities",DIR("B")="A"
 S DIR("A")="Enter a code indicating what LOCATIONS/FACILITIES are of interest",DIR("B")="O" K DA D ^DIR K DIR,DA
 G:$D(DIRUT) XIT
 S APCDLOCT=Y
 I APCDLOCT="A" G DATE
 D @APCDLOCT
 G:$D(APCDQUIT) LOC
DATE ;
 S APCDFILE=9000010.07
 W !!,"The search for Uncoded "_$P(^DIC(APCDFILE,0),U),"'s can begin at any date",!,"that you specify.  To get all of the uncoded entries enter a really early "
 W !,"date like 01/01/1930.  If you want to only review data for visits ",!,"in the past week, enter T-7.",!
 S APCDFUDT=""
 S DIR(0)="D^::EP",DIR("A")="Enter the Beginning Date to Search for Uncoded "_$P(^DIC(APCDFILE,0),U)_"'s" KILL DA D ^DIR KILL DIR
 I $D(DIRUT) G LOC
 S APCDFUDT=Y
 S DIR(0)="D^::EP",DIR("A")="Enter the Ending Date to Search for Uncoded "_$P(^DIC(APCDFILE,0),U)_"'s" KILL DA D ^DIR KILL DIR
 I $D(DIRUT) G LOC
 S APCDFUET=Y
 I APCDFUET<APCDFUDT W !,"Ending date cannot be before beginning date." G DATE
PROV ;
 K APCDPRVT S APCDPRVT=""
 S DIR(0)="S^A:ALL Providers (PRIMARY);O:ONE Provider (PRIMARY)",DIR("A")="Include Visits to Which Provider",DIR("B")="A"
 S DIR("A")="Enter a code indicating what LOCATIONS/FACILITIES are of interest",DIR("B")="O" K DA D ^DIR K DIR,DA
 G:$D(DIRUT) XIT
 S APCDPRVT=Y
 I APCDPRVT="A" G ZIS
 K DIC S DIC("A")="Which Provider: ",DIC="^VA(200,",DIC(0)="AEMQ" D ^DIC K DIC,DA G:X="^" PROV K DIC,DA
 G:Y=-1 PROV
 S APCDPRVT("ONE")=+Y
ZIS ;
 K IO("Q")
 W !!,"Enter the Device for printing"
 S %ZIS="PQ" D ^%ZIS
 I POP K IO("Q") G XIT
 I $D(IO("Q")) G TSKMN
 ;
EN ; Entry point if for taskman.
 S %DT="",X="T" D ^%DT X ^DD("DD") S APCDDT=Y
 U IO
 S APCDFILE="",APCDPG=0
 D HEAD
GETCODE ;
 I $P($G(^APCCCTRL(DUZ(2),0)),U,5)="" S APCDCODE=$P($$ICDDX^ICDCODE(".9999"),U,1) I 1
 E  S APCDCODE=$P(^APCCCTRL(DUZ(2),0),U,5)
 I APCDCODE="" W !!,"ERROR -- .9999 NOT IN ICD DIAGNOSIS FILE, NOTIFY YOUR SUPERVISOR" G XIT
 F APCDFILE=9000010.07,9000011,9000014,9000013 D PROC Q:$D(APCDQUIT)
 G:$D(APCDQUIT) XIT
 I $P($G(^APCCCTRL(DUZ(2),0)),U,6)="" S APCDCODE=$P($$ICDOP^ICDCODE(".9999"),U,1) I 1
 E  S APCDCODE=$P(^APCCCTRL(DUZ(2),0),U,6)
 I APCDCODE="" W !!,"ERROR -- .9999 NOT IN ICD OPERATION FILE, NOTIFY YOUR SUPERVISOR" G XIT
 F APCDFILE=9000010.08 D PROC Q:$D(APCDQUIT)
 S:$D(ZTQUEUED) ZTREQ="@"
 ;
XIT K APCDDOB,APCDDFN,APCDFILE,IO("Q"),APCDVDG,APCDCODE,APCDG,APCDVIGR,APCDHRN,APCDF,APCDDT,APCDL,APCDPG,APCDQUIT,ZTSK,APCDVCTR
 K AUPNSEX,AUPNPAT,AUPNDOB,AUPNDOD,AUPNDAYS,APCDVSIT,APCDLOCT,APCDOK,APCDFUET,APCDFPPV,APCDFUDT,APCDPRVT
 K A,DX,Y,X,S,DA,D0,DIC,DIE,DIQ,DK,DL,DR,POP,D1,D2
 D ^%ZISC
 Q
CHKLOC ;
 S APCDVSIT=""
 I $L(APCDFILE)=7 S APCDOK=1 Q
 S APCDOK=0
 S APCDG=APCDVDG_"APCDDFN,0)" S Y=$P(@APCDG,U,2),APCDVSIT=$P(@APCDG,U,3) I Y=""!(APCDVSIT="") W !,"ERROR IN GLOBAL -- NOTIFY PROGRAMMER - PATIENT OR VISIT DFN MISSING" Q
 I APCDLOCT="O",$P(^AUPNVSIT(APCDVSIT,0),U,6)'=APCDLOCT("ONE") Q
 I APCDLOCT="S",$$VALI^XBDIQ1(9999999.06,$P(^AUPNVSIT(APCDVSIT,0),U,6),.05)'=APCDLOCT("SU") Q
 S APCDOK=1
 Q
CHKPRV ;
 S APCDVSIT="" I $L(APCDFILE)=7 S APCDOK=1 Q
 S APCDOK=0
 S APCDG=APCDVDG_"APCDDFN,0)" S Y=$P(@APCDG,U,2),APCDVSIT=$P(@APCDG,U,3) I Y=""!(APCDVSIT="") W !,"ERROR IN GLOBAL -- NOTIFY PROGRAMMER - PATIENT OR VISIT DFN MISSING" Q
 I APCDPRVT="O",$$PRIMPROV^APCLV(APCDVSIT,"I")'=APCDPRVT("ONE") Q
 S APCDOK=1
 Q
O ;one community
 S DIC="^AUTTLOC(",DIC(0)="AEMQ",DIC("A")="Which LOCATION: " D ^DIC K DIC
 I Y=-1 S APCDQUIT="" Q
 S APCDLOCT("ONE")=+Y
 Q
S ;all communities within APCDSU su
 S DIC="^AUTTSU(",DIC("B")=$$VAL^XBDIQ1(9999999.06,DUZ(2),.05),DIC(0)="AEMQ",DIC("A")="Which SERVICE UNIT: " D ^DIC K DIC
 I Y=-1 S APCDQUIT="" Q
 S APCDLOCT("SU")=+Y
 Q
 ;
PROC ;
 K APCDQUIT
 S APCDDFN=""
 S APCDVDG=^DIC(APCDFILE,0,"GL")
 S APCDG=APCDVDG_"""B"",APCDCODE)"
 D:$Y>(IOSL-8) HEAD Q:$D(APCDQUIT)  D SUBHEAD
 I '$D(@APCDG) D:$Y>(IOSL-4) HEAD Q:$D(APCDQUIT)  D NONE Q
 S APCDVIGR=APCDVDG_"""B"",APCDCODE,APCDDFN)"
 S APCDDFN=0 F  S APCDDFN=$O(@APCDVIGR) Q:APCDDFN'=+APCDDFN!($D(APCDQUIT))  S APCDOK=0 D CHKLOC I APCDOK D CHKPRV I APCDOK D PRT
 Q
PRT ;
 D CHKDATE I 'APCDOK Q
 S APCDG=APCDVDG_"APCDDFN,0)" S Y=$P(@APCDG,U,2) I Y="" W !,"ERROR IN GLOBAL -- NOTIFY PROGRAMMER - PATIENT DFN MISSING" G XIT
 D ^AUPNPAT
 S Y=AUPNDOB X ^DD("DD") S APCDDOB=Y
 S APCDHRN="" I $D(^AUPNPAT(AUPNPAT,41,DUZ(2),0)) S APCDHRN=$P(^AUPNPAT(AUPNPAT,41,DUZ(2),0),U,2)
 I $Y>(IOSL-8) D HEAD Q:$D(APCDQUIT)
 W !!,"HRN: ",APCDHRN,"     DOB: ",APCDDOB,"    SEX: ",AUPNSEX
 S DA=APCDDFN,DIC=APCDVDG,DR=0 D EN^DIQ K DIC,DA,DR
 I APCDVSIT,$L(APCDFILE)>7 S APCDVCTR=$$OPER(APCDVSIT) W ?2,"OPERATOR FROM FORMS TRACKING OR CREATED BY: " I APCDVCTR W $P(^VA(200,APCDVCTR,0),U) W !
 I APCDVSIT,$L(APCDFILE)>7 W ?2,"LOCATION OF ENCOUNTER: ",$$LOCENC^APCLV(APCDVSIT,"E")
 I APCDVSIT,$L(APCDFILE)>7 W !?2,"PROVIDER: ",$$PRIMPROV^APCLV(APCDVSIT,"N")
 I APCDFILE=9000011,$P($G(^AUPNPROB(APCDDFN,1)),U,4)]"" W ?2,"RECORDING PROVIDER: ",$$VAL^XBDIQ1(9000011,APCDDFN,1.04),!
 I APCDFILE=9000011,$P($G(^AUPNPROB(APCDDFN,1)),U,3)]"" W ?2,"ENTERED BY: ",$$VAL^XBDIQ1(9000011,APCDDFN,1.03),!
 I APCDFILE=9000011,$P($G(^AUPNPROB(APCDDFN,1)),U,5)]"" W !?2,"RESPONSIBLE PROVIDER: ",$$VAL^XBDIQ1(9000011,APCDDFN,1.05)
 Q
 ;
OPER(V) ;
 I $G(V)="" Q ""
 ;find operator in forms tracking first, if none return .23 of visit (user who created)
 NEW Y,D,M S Y=""
 S D=$O(^APCDFORM("AB",V,"")) I D="" Q $P(^AUPNVSIT(V,0),U,23)
 S M=$O(^APCDFORM("AB",V,D,"")) I M="" Q $P(^AUPNVSIT(V,0),U,23)
 S Y=$P(^APCDFORM(D,11,M,0),U,2)
 Q $S(Y:Y,1:$P(^AUPNVSIT(V,0),U,23))
NONE ;
 W !!,"There are no .9999 Uncoded diagnoses or procedures in the ",$P(^DIC(APCDFILE,0),U)," file."
 Q
CHKDATE ;
 S APCDOK=0
 S APCDG=APCDVDG_"APCDDFN,0)" S Y=$P(@APCDG,U,2),APCDVSIT=$P(@APCDG,U,3) I Y=""!(APCDVSIT="") W !,"ERROR IN GLOBAL -- NOTIFY PROGRAMMER - PATIENT OR VISIT DFN MISSING" Q
 I $L(APCDFILE)>7 Q:'$D(^AUPNVSIT(APCDVSIT))  I $P($P(^AUPNVSIT(APCDVSIT,0),U),".")<APCDFUDT!($P($P(^AUPNVSIT(APCDVSIT,0),U),".")>APCDFUET) Q  ;before date wanted
 I $L(APCDFILE)=7,$P(@APCDG,U,3)<APCDFUDT Q  ;quit if problem modified before date
 S APCDOK=1
 Q
HEAD ;
 I 'APCDPG G HEAD1
 I $E(IOST)="C",IO=IO(0) W ! S DIR(0)="EO" D ^DIR I Y=0!($D(DTOUT)) K DIR S APCDQUIT="" Q
HEAD1 ;
 ;
 W:$D(IOF) @IOF S APCDPG=APCDPG+1
 W !,APCDDT,?70,"Page: ",APCDPG
 W !?29,"PCC Data Entry Module"
 W !?16,"***********************************************"
 W !?16,"* LISTING OF UNCODED DIAGNOSES AND PROCEDURES *"
 W !?16,"***********************************************"
 Q
SUBHEAD ;
 W !!,$P(^DIC(APCDFILE,0),U)," entries that need coded:"
 Q
TSKMN ;
 K ZTSAVE
 S ZTSAVE("APCD*")=""
 S ZTSAVE("DUZ(2)")="",ZTIO=ION,ZTCPU=$G(IOCPU),ZTRTN="EN^APCDFPPV",ZTDTH="",ZTDESC="VISIT ERROR REPORT - DATA ENTRY" D ^%ZTLOAD
 D XIT
 Q
DOC ;
 ; need to change to go thru PT node of ICD9 and 
 ; fix all files in the 9000001-9000099 range.
 ;
