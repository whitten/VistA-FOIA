APCHHMRQ ; IHS/CMI/LAB - DISPLAY HEALTH MAINTENANCE REMINDER ;
 ;;2.0;IHS PCC SUITE;**2**;MAY 14, 2009
 ;; ;
EP ;EP - called from option to select and display a hmr
 W !!,"This option will list the Health Maintenance Reminders available for display",!,"on a health summary.",!!
ACT ;
 S APCHACT=""
 K DIR
 S DIR(0)="S^A:ACTIVE Reminders (those marked as Active/'On');I:INACTIVE Reminders (those marked as Inactive/'Off');B:Both ACTIVE and INACTIVE Reminders"
 S DIR("A")="List which set of Reminders",DIR("B")="B" KILL DA D ^DIR KILL DIR
 I $D(DIRUT) D XIT Q
 S APCHACT=Y
SORT ;
 S APCHSORT=""
 S DIR(0)="S^C:By Category;N:By Name;S:Status",DIR("A")="How would you like the list sorted",DIR("B")="C" KILL DA D ^DIR KILL DIR
 I $D(DIRUT) G ACT
 S APCHSORT=Y
ZIS ;
 S DIR(0)="S^P:PRINT Output;B:BROWSE Output on Screen",DIR("A")="Do you wish to ",DIR("B")="P" K DA D ^DIR K DIR
 I $D(DIRUT) G XIT
 I $G(Y)="B" D BROWSE,XIT Q
 S XBRC="PROCESS^APCHHMRQ",XBRP="PRINT^APCHHMRQ",XBRX="XIT^APCHHMRQ",XBNS="APCH"
 D ^XBDBQUE
 D XIT
 Q
XIT ;
 K J,K,X,Z,Y
 D EN^XBVK("APCH")
 Q
BROWSE ;
 S XBRP="VIEWR^XBLM(""PRINT^APCHHMRQ"")"
 S XBNS="APCH",XBRC="PROCESS^APCHHMRQ",XBRX="XIT^APCHHMRQ",XBIOP=0 D ^XBDBQUE
 Q
 ;
PROCESS ; -- init variables and list array
 S APCHJ=$J,APCHH=$H
 S ^XTMP("APCHHMRQ",0)=$$FMADD^XLFDT(DT,14)_"^"_DT_"^"_"PCC DATA ENTRY LAB REPORT"
 K ^XTMP("APCHHMRQ",APCHJ,APCHH)
 S APCHN="",APCHHMR=0 F  S APCHN=$O(^APCHSURV("B",APCHN)) Q:APCHN=""  S APCHHMR=0 F  S APCHHMR=$O(^APCHSURV("B",APCHN,APCHHMR)) Q:APCHHMR'=+APCHHMR  D PROCESS1
 Q
PROCESS1 ;
 Q:$P(^APCHSURV(APCHHMR,0),U,3)="D"
 Q:$P(^APCHSURV(APCHHMR,0),U,7)'="R"
 S S=$P(^APCHSURV(APCHHMR,0),U,3)
 I APCHACT="A",S'=1 Q
 I APCHACT="I",S'=0 Q
 ;gather up reminder for display
 I S="" S S=0
 S C=$$VAL^XBDIQ1(9001018,APCHHMR,.05)
 I C="" S C="?"
 S N=$P(^APCHSURV(APCHHMR,0),U,1)
 I APCHSORT="C" S ^XTMP("APCHHMRQ",APCHJ,APCHH,C,N,$$EXTSET^XBFUNC(9001018,.03,S),APCHHMR)=""
 I APCHSORT="N" S ^XTMP("APCHHMRQ",APCHJ,APCHH,N,C,$$EXTSET^XBFUNC(9001018,.03,S),APCHHMR)=""
 I APCHSORT="S" S ^XTMP("APCHHMRQ",APCHJ,APCHH,$$EXTSET^XBFUNC(9001018,.03,S),C,N,APCHHMR)=""
 Q
 ;
 ;
PRINT ;EP - called from xbdbque
 S APCHPG=0,APCHQ=0 D HEAD
 S APCHS="" F  S APCHS=$O(^XTMP("APCHHMRQ",APCHJ,APCHH,APCHS)) Q:APCHS=""!(APCHQ)  D
 .S APCHC="" F  S APCHC=$O(^XTMP("APCHHMRQ",APCHJ,APCHH,APCHS,APCHC)) Q:APCHC=""!(APCHQ)  D
 ..S APCHN="" F  S APCHN=$O(^XTMP("APCHHMRQ",APCHJ,APCHH,APCHS,APCHC,APCHN)) Q:APCHN=""!(APCHQ)  D
 ...S APCHHMR=0 F  S APCHHMR=$O(^XTMP("APCHHMRQ",APCHJ,APCHH,APCHS,APCHC,APCHN,APCHHMR)) Q:APCHHMR=""!(APCHQ)  D
 ....I $Y>(IOSL-3) D HEAD Q:APCHQ
 ....W !,$E($$VAL^XBDIQ1(9001018,APCHHMR,.01),1,25),?27,$E($$VAL^XBDIQ1(9001018,APCHHMR,.05),1,15),?44,$$VAL^XBDIQ1(9001018,APCHHMR,.03)
 ....S APCHSC=0,APCHZ=0
 ....F  S APCHZ=$O(^APCHSCTL(APCHZ)) Q:APCHZ'=+APCHZ  D
 .....S APCHK=0 F  S APCHK=$O(^APCHSCTL(APCHZ,5,APCHK)) Q:APCHK'=+APCHK!(APCHQ)  I $P(^APCHSCTL(APCHZ,5,APCHK,0),U,2)=APCHHMR W:APCHSC>0 ! W ?59,$E($P(^APCHSCTL(APCHZ,0),U),1,20) S APCHSC=APCHSC+1
 .Q
 ;K ^XTMP("APCHHMRQ",APCHJ,APCHH),APCHJ,APCHH
 Q
S(Y,F,C,T) ;set up array
 I '$G(F) S F=0
 I '$G(T) S T=0
 ;blank lines
 F F=1:1:F S X="" D S1
 S X=Y
 I $G(C) S L=$L(Y),T=(80-L)/2 D  D S1 Q
 .F %=1:1:(T-1) S X=" "_X
 F %=1:1:T S X=" "_Y
 D S1
 Q
S1 ;
 S %=$P(^XTMP("APCHHMRQ",APCHJ,APCHH,0),U)+1,$P(^XTMP("APCHHMRQ",APCHJ,APCHH,0),U)=%
 S ^XTMP("APCHHMRQ",APCHJ,APCHH,%)=X
 Q
HEAD I 'APCHPG G HEAD1
 I $E(IOST)="C",IO=IO(0) W ! S DIR(0)="EO" D ^DIR K DIR I Y=0!(Y="^")!($D(DTOUT)) S APCHQ=1 Q
HEAD1 ;
 W:$D(IOF) @IOF S APCHPG=APCHPG+1
 W !,$P(^VA(200,DUZ,0),U,2),?72,"Page ",APCHPG,!
 W ?(80-$L($P(^DIC(4,DUZ(2),0),U))/2),$P(^DIC(4,DUZ(2),0),U),!
 W $$CTR("HEALTH SUMMARY HEALTH MAINTENANCE REMINDERS",80),!
 W $$CTR($S(APCHACT="A":"ACTIVE",APCHACT="I":"INACTIVE",1:"BOTH ACTIVE AND INACTIVE REMINDERS"),80),!
 W $TR($J("",80)," ","-"),!
 W !,"REMINDER",?27,"CATEGORY",?44,"STATUS",?59,"HEALTH SUMMARY TYPES",!
 W $TR($J("",80)," ","-")
 Q
 ;
CTR(X,Y) ;EP - Center X in a field Y wide.
 Q $J("",$S($D(Y):Y,1:IOM)-$L(X)\2)_X
 ;----------
EOP ;EP - End of page.
 Q:$E(IOST)'="C"
 Q:$D(ZTQUEUED)!'(IOT="TRM")!$D(IO("S"))
 NEW DIR
 K DIRUT,DFOUT,DLOUT,DTOUT,DUOUT
 S DIR(0)="E" D ^DIR
 Q
 ;----------
USR() ;EP - Return name of current user from ^VA(200.
 Q $S($G(DUZ):$S($D(^VA(200,DUZ,0)):$P(^(0),U),1:"UNKNOWN"),1:"DUZ UNDEFINED OR 0")
 ;----------
LOC() ;EP - Return location name from file 4 based on DUZ(2).
 Q $S($G(DUZ(2)):$S($D(^DIC(4,DUZ(2),0)):$P(^(0),U),1:"UNKNOWN"),1:"DUZ(2) UNDEFINED OR 0")
 ;----------
