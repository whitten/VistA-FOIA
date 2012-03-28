BGP0DICR ; IHS/CMI/LAB - ICARE LIST ;
 ;;10.0;IHS CLINICAL REPORTING;;JUN 18, 2010
 ;
 ;
 ;
START ;EP
 W !,$$CTR("2010 ICARE DATA ITEMS",80)
 W !,$$CTR($$RPTVER^BGP0BAN,80)
INTRO ;
 W !!,"This report will list all individual measures that contain iCare data"
 W !,"data elements.",!
 K DIR S DIR(0)="Y",DIR("A")="Do you wish to continue",DIR("B")="Y" KILL DA D ^DIR KILL DIR
 I $D(DIRUT) D XIT Q
 I 'Y D XIT Q
ZIS ;call to XBDBQUE
 K ZTSK
 K IOP,%ZIS S %ZIS="PQM" D ^%ZIS I POP S IO=IO(0) Q
 G:$D(IO("Q")) QUE
NOQUE ;
 U IO
 D PRINT
 D ^%ZISC
 D XIT
 Q
QUE ;
 K ZTSAVE S ZTSAVE("BGP*")=""
 S ZTRTN="PRINT^BGP0DICR",ZTDESC="BGP 10 ICARE TEXT REPORT",ZTIO=ION,ZTDTH=""
 D ^%ZTLOAD
 D HOME^%ZIS
 D XIT
 Q
XIT ;
 D EN^XBVK("BGP") I $D(ZTQUEUED) S ZTREQ="@"
 D ^XBFMK
 Q
 ;
PRINT ;
 S (BGPPAGE,BGPQUIT)=0
 S BGPIOSL=$S($G(BGPGUI):55,1:$G(IOSL))
 D HEADER
 S BGPO="" F  S BGPO=$O(^BGPINDTC("AB",BGPO)) Q:BGPO=""!(BGPQUIT)  D
 .S BGPON="" F  S BGPON=$O(^BGPINDTC("AB",BGPO,BGPON)) Q:BGPON=""!(BGPQUIT)  D
 ..S BGPX=0 F  S BGPX=$O(^BGPINDTC("AB",BGPO,BGPON,BGPX)) Q:BGPX'=+BGPX!(BGPQUIT)  D
 ...Q:$P($G(^BGPINDTC(BGPX,17)),U,1)=""  ;not an icare item
 ...I $Y>(IOSL-3) D HEADER Q:BGPQUIT
 ...W !,BGPON,?10,$P(^BGPINDTC(BGPX,0),U,4),?21,$$VAL^XBDIQ1(90377.02,BGPX,1701),?42,$$VAL^XBDIQ1(90377.02,BGPX,1702)
 ...W ?63,$$VAL^XBDIQ1(90377.02,BGPX,1704)
 ...W ?70,$$VAL^XBDIQ1(90377.02,BGPX,1705),!
 ...I $Y>(IOSL-3) D HEADER Q:BGPQUIT
 ...W "iCare Report Code: ",$$VALI^XBDIQ1(90377.02,BGPX,1706),"  ",$$VAL^XBDIQ1(90377.02,BGPX,1706),!
 ...I $Y>(IOSL-3) D HEADER Q:BGPQUIT
 ...W "iCare Name: ",$$VAL^XBDIQ1(90377.02,BGPX,1703),!
 ...I $Y>(IOSL-3) D HEADER Q:BGPQUIT
 ...W "iCare Tooltip: ",!
 ...S BGPY=0 F  S BGPY=$O(^BGPINDTC(BGPX,18,BGPY)) Q:BGPY'=+BGPY!(BGPQUIT)  D
 ....I $Y>(IOSL-3) D HEADER Q:BGPQUIT
 ....W ^BGPINDTC(BGPX,18,BGPY,0),!
 W !
 D EOP
 Q
HEADER ;EP
 G:'BGPPAGE HEADER1
 K DIR I $E(IOST)="C",IO=IO(0),'$D(ZTQUEUED),'$D(IO("S")) W ! S DIR(0)="EO" D ^DIR K DIR I Y=0!(Y="^")!($D(DTOUT)) S BGPQUIT=1 Q
HEADER1 ;
 S BGPPAGE=BGPPAGE+1
 I BGPPAGE'=1 W:$D(IOF) @IOF
 W $P(^VA(200,DUZ,0),U,2),?70,"Page ",BGPPAGE,!
 W $$CTR("*** ICARE TEXT Report ***",80),!
 S X="Date Report Run: "_$$FMTE^XLFDT(DT) W $$CTR(X,80),!
 S X="Site where Run: "_$P(^DIC(4,DUZ(2),0),U) W $$CTR(X,80),!
 S X="Report Generated by: "_$P(^VA(200,DUZ,0),U) W $$CTR(X,80),!
 W $$CTR($$RPTVER^BGP0BAN,80),!
 W "ORDER",?10,"MEAS ID",?21,"ICARE GROUP",?42,"ICARE CATEGORY",?63,"EXCEPT",?70,"PER DIR",!
 S X=$$REPEAT^XLFSTR("-",80) W !,X
 W !
 Q
CTR(X,Y) ;EP - Center X in a field Y wide.
 Q $J("",$S($D(Y):Y,1:IOM)-$L(X)\2)_X
 ;----------
EOP ;EP - End of page.
 Q:$E(IOST)'="C"
 Q:$D(ZTQUEUED)!'(IOT["TRM")!$D(IO("S"))
 NEW DIR
 K DIR,DIRUT,DFOUT,DLOUT,DTOUT,DUOUT
 W ! S DIR("A")="End of report.  Press ENTER to continue",DIR(0)="E" D ^DIR KILL DIR
 Q
 ;----------
USR() ;EP - Return name of current user from ^VA(200.
 Q $S($G(DUZ):$S($D(^VA(200,DUZ,0)):$P(^(0),U),1:"UNKNOWN"),1:"DUZ UNDEFINED OR 0")
 ;----------
LOC() ;EP - Return location name from file 4 based on DUZ(2).
 Q $S($G(DUZ(2)):$S($D(^DIC(4,DUZ(2),0)):$P(^(0),U),1:"UNKNOWN"),1:"DUZ(2) UNDEFINED OR 0")
 ;----------
 ;
