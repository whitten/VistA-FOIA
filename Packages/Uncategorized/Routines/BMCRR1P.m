BMCRR1P ; IHS/PHXAO/TMJ - PRNT BILL VSTS ;    [ 09/27/2006  2:05 PM ]
 ;;4.0;REFERRED CARE INFO SYSTEM;**1**;JAN 09, 2006
 ;4.0*1 3.24.06 IHS/OIT/FCJ ADDED RUN DATE TO REPORT
START ;
 S BMC80E="==============================================================================="
 S BMC80D="-------------------------------------------------------------------------------"
 S BMCPG=0 D @("HEAD"_(2-($E(IOST,1,2)="C-"))) I '$D(^TMP("BMCRR1",BMCJOB,BMCBT)) W !,"No referrals to report",! G DONE
 S BMCSORT=0 K BMCQUIT
 F  S BMCSORT=$O(^TMP("BMCRR1",BMCJOB,BMCBT,"DATA HITS",BMCSORT)) Q:BMCSORT=""!($D(BMCQUIT))  D PRINT
 G:$D(BMCQUIT) DONE
 I $Y>(IOSL-6) D HEAD G:$D(BMCQUIT) DONE
DONE ;
 K ^TMP("BMCRR1",BMCJOB,BMCBT)
 D DONE^BMCRLP2
 Q
PRINT ;print one referral
 I $G(BMCSPAGE),BMCPG'=1 D HEAD Q:$D(BMCQUIT)
 I $Y>(IOSL-10) D HEAD Q:$D(BMCQUIT)
 W !! I BMCSTYPE="F" W "FACILITY REFERRED TO:  ",BMCSORT,!
 I BMCSTYPE="T" W "TIME SINCE END OF SERVICE:  ",$S(BMCSORT=4:"0-1 Months",BMCSORT=3:"2-3 Months",BMCSORT=2:"4-6 Months",BMCSORT=1:">6 Months",1:"???"),!
 S BMCREF=0 F  S BMCREF=$O(^TMP("BMCRR1",BMCJOB,BMCBT,"DATA HITS",BMCSORT,BMCREF)) Q:BMCREF'=+BMCREF!($D(BMCQUIT))  S BMCRREC=^BMCREF(BMCREF,0),DFN=$P(BMCRREC,U,3) D PRINT1
 Q
PRINT1 ;
 I $Y>(IOSL-9) D HEAD Q:$D(BMCQUIT)
 W !,$$FMTE^XLFDT($P(BMCRREC,U),"5D")
 W ?12,$E($P(^DPT(DFN,0),U),1,18)
 S BMCHRN="????" I $D(^AUPNPAT(DFN,41,DUZ(2))) S BMCHRN=$P(^AUTTLOC(DUZ(2),0),U,7)_$P(^AUPNPAT(DFN,41,DUZ(2),0),U,2)
 W ?32,BMCHRN
 W ?43,$S($P(BMCRREC,U,6):$$PROVINI^XBFUNC1($P(BMCRREC,U,6)),1:"--")
 S BMCFAC=$$FACREF^BMCRLU(BMCREF)
 I BMCFAC="" S BMCFAC="????"
 W ?49,$E(BMCFAC,1,16)
 W ?66,$S($P($G(^BMCREF(BMCREF,11)),U,6)]"":$$FMTE^XLFDT($P($G(^BMCREF(BMCREF,11)),U,6),"5D")_" (A)",$P($G(^BMCREF(BMCREF,11)),U,5):$$FMTE^XLFDT($P($G(^BMCREF(BMCREF,11)),U,5),"5D")_" (E)",1:"")
 S %=$$FMDIFF^XLFDT(DT,$$AVEOS^BMCRLU(BMCREF,"I"))
 S BMCEND=$$AVEOS^BMCRLU(BMCREF)
 W !?5,"Ending Date of Service: "_$S(BMCEND="":"UNKNOWN",1:BMCEND)
 I BMCEND="" S %="UNKNOWN"
 E  S %1=%\365.25,%=$S(%1>2:%1_" YRS",%<31:%1_" DYS",1:%\30_" MOS")
 W ?50,"Time Lapsed: ",%
 W !?5,"Case Manager: ",$S($P(BMCRREC,U,19):$P(^VA(200,$P(BMCRREC,U,19),0),U),1:"")
 W !?5,"ICD Diagnosis Category:  ",$S($P(BMCRREC,U,12):$P(^BMCTDXC($P(BMCRREC,U,12),0),U),1:"")
 W !?5,"CPT Service Category:  ",$S($P(BMCRREC,U,13):$P(^BMCTSVC($P(BMCRREC,U,13),0),U),1:""),!
 Q
HEAD ;ENTRY POINT
 I $E(IOST)="C",IO=IO(0) W ! S DIR(0)="EO" D ^DIR K DIR I Y=0!(Y="^")!($D(DTOUT)) S BMCQUIT="" Q
HEAD1 ;
 W:$D(IOF) @IOF
HEAD2 ;
 S BMCPG=BMCPG+1
 W !?13,"********** CONFIDENTIAL PATIENT INFORMATION **********"
 W !?(80-$L($P(^DIC(4,DUZ(2),0),U))/2),$P(^DIC(4,DUZ(2),0),U),?72,"Page ",BMCPG,!
 W ?10,"REFERRALS FOR WHICH MEDICAL/COST DATA HAS NOT BEEN RECEIVED",!
 W !,"Report Run Date: ",$$FMTE^XLFDT($$HTFM^XLFDT($H),"1P") ;4.0*1 3.24.06 IHS/OIT/FCJ ADDED RUN DATE TO REPORT
 W !,"REF DATE",?11,"PATIENT NAME",?32," HRN",?43,"PROV",?49,"FACILITY REF TO",?67,"BEG DOS."
 W !,BMC80D
 Q
