BMCOUTRP ; IHS/PHXAO/TMJ - OUTSIDE PROVIDER REFERRALS ;    
 ;;4.0;REFERRED CARE INFO SYSTEM;;JAN 09, 2006
 ;ITSC/IHS/FCJ REMOVED KILL AND RESET BMCOLOC
 ;ITSC/IHS/FCJ ADDED SEC REF PRINT
 ;
 S BMCPG=0 D @("HEAD"_(2-($E(IOST,1,2)="C-"))) I '$D(^XTMP("BMCOUTRP",BMCJOB,BMCBT)) W !,"No referrals to report",! G XIT
 S BMCPTN=0,BMCQUIT=0
 S BMCDATE="" F  S BMCDATE=$O(^XTMP("BMCOUTRP",BMCJOB,BMCBT,"DATA HITS",BMCDATE)) Q:BMCDATE=""!(BMCQUIT)  D P
XIT ;
 K ^XTMP("BMCOUTRP",BMCJOB,BMCBT)
 D DONE^BMCRLP2
 D KILL^AUPNPAT
 K BMCDATE,BMCOMDT,BMCRIEN,BMCREVN,BMCREVP,BMCRNUMB
 S BMCOLOC=$P(^BMCPARM(DUZ(2),0),U,11)
 Q
P ;
 S BMCPTN="" F  S BMCPTN=$O(^XTMP("BMCOUTRP",BMCJOB,BMCBT,"DATA HITS",BMCDATE,BMCPTN)) Q:BMCPTN=""!(BMCQUIT)  D PRINT
 Q
PRINT ;print one referral
 I $Y>(IOSL-10) D HEAD Q:BMCQUIT
 S BMCRIEN=0 F  S BMCRIEN=$O(^XTMP("BMCOUTRP",BMCJOB,BMCBT,"DATA HITS",BMCDATE,BMCPTN,BMCRIEN)) Q:BMCRIEN'=+BMCRIEN!(BMCQUIT)  S BMCRREC=^BMCREF(BMCRIEN,0),DFN=$P(BMCRREC,U,3) D PRINT1
 Q
PRINT1 ;
 I $Y>(IOSL-3) D HEAD Q:BMCQUIT
 S BMCRNUMB=$P($G(^BMCREF(BMCRIEN,0)),U,2)
 S BMCHRN="????" I $D(^AUPNPAT(DFN,41,DUZ(2))) S BMCHRN=$P(^AUTTLOC(DUZ(2),0),U,7)_$P(^AUPNPAT(DFN,41,DUZ(2),0),U,2)
 W !,$E($P(^DPT(DFN,0),U),1,30),?32,BMCHRN,?43,"DOB: ",$$DOB^AUPNPAT(DFN,"E"),"  ",$$AGE^AUPNPAT(DFN,DT,"R")," ",$$SSN^AUPNPAT(DFN)
 W !,"Referral #: ",BMCRNUMB,$P($G(^BMCREF(BMCRIEN,1)),U)
 W ?29,"Date Init: ",$$REFDTI^BMCRLU(BMCRIEN,"S")
 W ?50,"Tribe: ",$E($$TRIBE^AUPNPAT(DFN,"E"),1,20)
 ;
SECREF ;
 I $P($G(^BMCREF(BMCRIEN,1)),U)="" D SECREF2^BMCRUTL
 ;
 W !,"Priority: ",$$VAL^XBDIQ1(90001,BMCRIEN,.32),"  CHS Auth Dec: ",$$VAL^XBDIQ1(90001,BMCRIEN,1112),"  MCC Action: ",$$VAL^XBDIQ1(90001,BMCRIEN,1123)
 ;W !,"Utilization Review by MD: ",$$VAL^XBDIQ1(90001,BMCRIEN,1125)
LOCAT ;Print Local Categories 
 I $D(^BMCREF(BMCRIEN,21,0)) D
 . S BMCLOCC=0
 .F  S BMCLOCC=$O(^BMCREF(BMCRIEN,21,"B",BMCLOCC)) Q:BMCLOCC'=+BMCLOCC  D
 ..S BMCLOCI=0
 ..F  S BMCLOCI=$O(^BMCREF(BMCRIEN,21,"B",BMCLOCC,BMCLOCI)) Q:BMCLOCI'=+BMCLOCI  D
 ... S BMCLOCP=$P(^BMCREF(BMCRIEN,21,BMCLOCI,0),U)
 ... Q:BMCLOCP=""
 ... S BMCLOCPP=$P(^BMCLCAT(BMCLOCP,0),U)
 ... W !,"Local Category:   "_BMCLOCPP
 ;
 ;
ALT ;Alternate Resource Letter Date
 I $Y>(IOSL-3) D HEAD Q:BMCQUIT
 W !,"Alternate Resource Letter Date: ",$$VAL^XBDIQ1(90001,BMCRIEN,1401)
 ;
 I '$D(^BMCREF(BMCRIEN,2)) G NEXT
 W !,"Business Office Comments:"
 S BMCNODE=1,BMCIOM=70,BMCFILE=90001.04,BMCDA=BMCRIEN D WP^BMCFDR K BMCIOM
 S Y=0 F  S Y=$O(BMCWP(Y)) Q:Y'=+Y!(BMCQUIT)  D
 .I $Y>(IOSL-3) D HEAD Q:BMCQUIT
 .W !?5,BMCWP(Y)
 Q:BMCQUIT
NEXT ;
 W !,"--------------------",!
 Q
HEAD ;ENTRY POINT
 NEW X,Y,Z,C
 I $E(IOST)="C",IO=IO(0) W ! S DIR(0)="EO" D ^DIR K DIR I Y=0!(Y="^")!($D(DTOUT)) S BMCQUIT=1 Q
HEAD1 ;
 W:$D(IOF) @IOF
HEAD2 ;
 S BMCPG=BMCPG+1
 W !?13,"********** CONFIDENTIAL PATIENT INFORMATION **********"
 W !?(80-$L($P(^DIC(4,DUZ(2),0),U))/2),$P(^DIC(4,DUZ(2),0),U),?72,"Page ",BMCPG,!
 S Y=DT D DD^%DT W ?(80-$L(Y)/2),Y,!
 W ?15,"**REFERRALS INITIATED AT OUTSIDE FACILITY (CALL'ins)**"
 S Y=BMCBD D DD^%DT W !,?28,"BEG DATE: "_Y
 S Y=BMCED D DD^%DT W !,?28,"END DATE: "_Y,!
 W !,$TR($J(" ",80)," ","-")
 Q
