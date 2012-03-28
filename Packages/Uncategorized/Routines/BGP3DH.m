BGP3DH ; IHS/CMI/LAB - cover page for gpra ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;
 ;
 W:$D(IOF) @IOF
 W !!,$$CTR("Cover Page",80)
 W:BGPRTYPE=4 !!,$$CTR("*** IHS FY03 Local Clinical Performance Indicator Report ***",80)
 W:BGPRTYPE=1 !!,$$CTR("*** IHS FY03 GPRA Clinical Performance Indicators ***",80)
 W:BGPRTYPE=2 !!,$$CTR("*** IHS FY03 Annual Area Clinical Performance Indicators ***",80)
 W !,$$CTR("Date Report Run: "_$$FMTE^XLFDT(DT),80)
 W !,$$CTR("Site where Run: "_$P(^DIC(4,DUZ(2),0),U),80)
 W !,$$CTR("Report Generated by: "_$$USR,80)
 S X="Reporting Period: "_$$FMTE^XLFDT(BGPBD)_" to "_$$FMTE^XLFDT(BGPED) W !,$$CTR(X,80)
 S X="Previous Year Period:  "_$$FMTE^XLFDT(BGPPBD)_" to "_$$FMTE^XLFDT(BGPPED) W !,$$CTR(X,80)
 S X="Baseline Period:  "_$$FMTE^XLFDT(BGPBBD)_" to "_$$FMTE^XLFDT(BGPBED) W !,$$CTR(X,80)
 W !!,"Indicators: ",$S(BGPINDT="G":"GPRA Indicators (All)",BGPINDT="A":"Area Performance Indicators (All)",BGPINDT="D":"Diabetes Indicators (All)",BGPINDT="C":"Cardiovascular Indicators (All)",BGPINDT="S":"Selected Indicators",1:"")
 D ENDTIME
 I BGPRTYPE=4 D DENOMHDR
 I BGPRTYPE=1 D GPRAHDR
 I BGPRTYPE=2 D AREAHDR
 I $G(BGPFTA) W !!,"A file will be created called BG",$P(^AUTTLOC(DUZ(2),0),U,10)_"."_BGPRPT,".",!,"It will reside in the public/export directory.",!,"This file should be sent to your Area Office.",!
 I BGPRTYPE=4,BGPROT'="P" W !!,"A delimited output file called ",BGPDELF,!,"has been placed in the public directory for your use in Excel or some",!,"other software package.",!,"See your site manager to access this file.",!
 W !!?10,"The following communities are included in this report:",!
 NEW BGPX
 S BGPX="" F  S BGPX=$O(BGPTAX(BGPX)) Q:BGPX=""  D
 .I $Y>(IOSL-4) D EOP W:$D(IOF) @IOF
 .W !?15,BGPX
 .Q
 K BGPX,BGPQUIT
 Q
DENOMHDR ;
 W !
 S BGPX=$O(^BGPCTRL("B",2003,0))
 S BGPY=0 F  S BGPY=$O(^BGPCTRL(BGPX,13,BGPY)) Q:BGPY'=+BGPY  D
 .I $Y>(IOSL-2) D EOP W:$D(IOF) @IOF
 .W !,^BGPCTRL(BGPX,13,BGPY,0)
 .Q
 Q
AREAHDR ;
 W !
 S BGPX=$O(^BGPCTRL("B",2003,0))
 S BGPY=0 F  S BGPY=$O(^BGPCTRL(BGPX,15,BGPY)) Q:BGPY'=+BGPY  D
 .I $Y>(IOSL-2) D EOP W:$D(IOF) @IOF
 .W !,^BGPCTRL(BGPX,15,BGPY,0)
 .Q
 Q
GPRAHDR ;
 W !
 S BGPX=$O(^BGPCTRL("B",2003,0))
 S BGPY=0 F  S BGPY=$O(^BGPCTRL(BGPX,14,BGPY)) Q:BGPY'=+BGPY  D
 .I $Y>(IOSL-2) D EOP W:$D(IOF) @IOF
 .W !,^BGPCTRL(BGPX,14,BGPY,0)
 .Q
 Q
ENDTIME ;
 I $D(BGPET) S BGPTS=(86400*($P(BGPET,",")-$P(BGPBT,",")))+($P(BGPET,",",2)-$P(BGPBT,",",2)),BGPHR=$P(BGPTS/3600,".") S:BGPHR="" BGPHR=0 D
 .S BGPTS=BGPTS-(BGPHR*3600),BGPM=$P(BGPTS/60,".") S:BGPM="" BGPM=0 S BGPTS=BGPTS-(BGPM*60),BGPS=BGPTS W !!,"RUN TIME (H.M.S): ",BGPHR,".",BGPM,".",BGPS
 Q
AREACP ;EP - area cover page
 ;
 S BGPGPG=0 D HEADER^BGP3DPH
 W !!?1,"Report includes the following facility data:"
 NEW BGPX
 S BGPX="" F  S BGPX=$O(BGPSUL(BGPX)) Q:BGPX=""  D
 .I $Y>(IOSL-5) D EOP W:$D(IOF) @IOF
 .S X=$P(^BGPGPDC(BGPX,0),U,9),X=$O(^AUTTLOC("C",X,0)) S X=$S(X:$P(^DIC(4,X,0),U),1:"?????")
 .W !?3,X
 .W !?5,"Communities: " S X=0,N=0,Y="" F  S X=$O(^BGPGPDC(BGPX,9999,X)) Q:X'=+X  S N=N+1,Y=Y_$S(N=1:"",1:";")_$P(^BGPGPDC(BGPX,9999,X,0),U)
 .S X=0,C=0 F X=1:3:N W !?10,$E($P(Y,";",X),1,20),?30,$E($P(Y,";",(X+1)),1,20),?60,$E($P(Y,";",(X+2)),1,20)
 .Q
 K BGPX,BGPQUIT
 Q
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
 ;;
