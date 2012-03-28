SDNOS1 ;ALB/LDB - NO-SHOW REPORT ; [ 01/02/2002  1:57 PM ]
 ;;5.3;Scheduling;**194**;Aug 13, 1993
 ;IHS/ANMC/LJF 11/03/2000 changed SSN to HRCN
 ;                        used IHS call for rebook date
 ;                        added total # appts to % statement
 ;                        removed page header for list template
 ;                        removed table of contents from list template
 ;                        chckd 4 temp HRCN in a loop;VA assumes numeric
 ;             12/14/2000 added confidential message to heading
 ;             12/22/2000 added patient died message
 ;              3/23/2001 changed X ^DD(FUNC",2,1) to $$TIME^BDGF
 ;              1/02/2002 removed automatic form feed before totals
 ;
 D:'SDV1 CL I SDV1 F SDDIV=0:0 S SDDIV=$O(^UTILITY($J,"SDNO",SDDIV)) Q:'SDDIV!(SDDIV="")  Q:SDIO=IO(0)&(SDEND)  D CL Q:SDEND  D:SDIO=IO(0) SCR Q:SDEND
 ;I 'SDABB D:SDIO'=IO TP^DGUTL           ;IHS/ANMC/LJF 11/03/2000
 I '$G(VALM),'SDABB D:SDIO'=IO TP^DGUTL  ;IHS/ANMC/LJF 11/03/2000
 D END^SDNOS Q
 ;
CL S SDC="***TOT***",Q=SDCL(1) I SDABB D ABB Q
 ;
 ;IHS/ANMC/LJF 1/02/2002
 ;F C=0:1 S SDC=$O(^UTILITY($J,"SDNO",SDDIV,SDC)) Q:(SDC?1"***".E)!(SDC="")!SDEND  I (^(SDC,"***TOT***")>0&'Q)!Q D HDR,HDR1 S SDHD=1 D WR S SDTOT=1 Q:SDEND  D:SDIO=IO(0) SCR Q:SDEND  D HDR,HDR2,TOT S SDTOT=0 W !! D:SDIO=IO(0) SCR Q:SDEND
 F C=0:1 S SDC=$O(^UTILITY($J,"SDNO",SDDIV,SDC)) Q:(SDC?1"***".E)!(SDC="")!SDEND  I (^(SDC,"***TOT***")>0&'Q)!Q D HDR,HDR1 S SDHD=1 D WR S SDTOT=1 Q:SDEND  D:SDIO=IO(0) SCR Q:SDEND  D TOT S SDTOT=0 W !! D:SDIO=IO(0) SCR Q:SDEND
 ;
 Q:SDEND  S SDTOT1=1 D SDTOT^SDNOS2 S SDTOT1=0
 Q
 ;
HDR ;D NOW^%DTC S Y=% X ^DD("DD") W @IOF,Y,?70,"PAGE " S P1=P1+1 W P1  ;IHS/ANMC/LJF 11/03/2000
 ;W !,?30,"NO SHOW REPORT",$S(SDTOT!SDTOT1:" TOTALS",1:""),! D LINE^SDNOS1A W !!,?2,"FOR PERIOD COVERING: " S Y=SDBD D D^DIQ S SDBG=Y W ?30,SDBG  ;IHS/ANMC/LJF 11/03/2000
 I '$G(VALM) D NOW^%DTC S Y=% X ^DD("DD") W @IOF,Y,?70,"PAGE " S P1=P1+1 W P1  ;IHS/ANMC/LJF 11/03/2000
 I '$G(VALM) W !,?30,"NO SHOW REPORT",$S(SDTOT!SDTOT1:" TOTALS",1:""),! D LINE^SDNOS1A   ;IHS/ANMC/LJF 11/03/2000
 W !?15,$$CONF^BSDU    ;IHS/ANMC/LJF 12/14/2000
 W !!,?2,"FOR PERIOD COVERING: " S Y=SDBD D D^DIQ S SDBG=Y W ?30,SDBG  ;IHS/ANMC/LJF 11/03/2000
 I $D(SDED) S Y=SDED D D^DIQ W " TO ",Y
 D DIV^SDNOS1A D:SDABB HDR3 Q
 ;
HDR1 ;Q:SDIO=IO&(SDEND)  W !,"DATE",?23,"TIME",?32,"PATIENT",?63,"SSN",!,"----",?23,"----",?32,"-------",?63,"---"  ;IHS/ANMC/LJF 11/03/2000
 Q:SDIO=IO&(SDEND)  W !,"DATE",?23,"TIME",?32,"PATIENT",?63,"HRCN",!,"----",?23,"----",?32,"-------",?63,"----"  ;IHS/ANMC/LJF 11/03/2000
 Q
 ;
WR S (SDNO,X1,Y3)=0 S C1=0 F C6=1:1 S Y3=C1,C1=$O(^UTILITY($J,"SDNO",SDDIV,SDC,C1)) Q:SDEND  D:(C6=1)&(C1?1"***".E) NONE Q:C1?1"***".E!(C1="")!(SDEND)  S:C6=1 Y3=C1 S X1=0 Q:SDIO=IO(0)&(SDEND)  D WR1
 Q
 ;
NONE W !!!,"*** NO NO-SHOWS OCCURRED IN THIS CLINIC DURING THIS TIME FRAME ***" Q
 ;
 ;IHS/ANMC/LJF 11/03/2000 C3 could be temporary chart #
WR1 ;N SDX S SDPT=0 F C2=0:0 S SDPT=$O(^UTILITY($J,"SDNO",SDDIV,SDC,C1,SDPT)) Q:SDPT?1"***".E!(SDPT="")!(SDEND)  S C3=0 F C4=0:0 S C3=$O(^UTILITY($J,"SDNO",SDDIV,SDC,C1,SDPT,C3)) Q:C3<1!(SDEND)  S SDX=^(C3) D WR2
 N SDX S SDPT=0 F C2=0:0 S SDPT=$O(^UTILITY($J,"SDNO",SDDIV,SDC,C1,SDPT)) Q:SDPT?1"***".E!(SDPT="")!(SDEND)  S C3=0 F C4=0:0 S C3=$O(^UTILITY($J,"SDNO",SDDIV,SDC,C1,SDPT,C3)) Q:(C3="")!(SDEND)  S SDX=^(C3) D WR2
 ;
 Q
 ;
WR2 ;S X=C1 X ^DD("FUNC",2,1) S Y2=X   ;IHS/ANMC/LJF 3/23/2001
 S X=C1 S Y2=$$TIME^BDGF(X)         ;IHS/ANMC/LJF 3/23/2001
 S X=C1 D DW^%DTC S SDOW=X,Y=C1 X ^DD("DD") S Y1=$P(Y,"@")
 Q:SDEND
 I $Y+6>IOSL D:SDIO=IO(0) SCR Q:SDEND  D HDR,HDR1 S SDHD=1 Q:SDEND
 ;I SDHD=1 S X=C1 X ^DD("FUNC",2,1) S Y2=X W !!,SDOW,?10,Y1 W:$L(Y2)>7 ?22 W:$L(Y2)<8 ?23 W Y2,?32,SDPT,?63,C3  ;IHS/ANMC/LJF 3/23/2001
 I SDHD=1 S X=C1 S Y2=$$TIME^BDGF(X) W !!,SDOW,?10,Y1 W:$L(Y2)>7 ?22 W:$L(Y2)<8 ?23 W Y2,?32,SDPT,?63,C3  ;IHS/ANMC/LJF 3/23/2001
 I $P(Y3,".",2)']""&('SDHD) W !!,SDOW,?10,Y1 W:$L(Y2)>7 ?22 W:$L(Y2)<8 ?23 W Y2,?32,SDPT,?63,C3
 I $P(Y3,".",2)]""&('SDHD) W !! W:$L(Y2)>7 ?22 W:$L(Y2)<8 ?23 W Y2,?32,SDPT,?63,C3
 I $$DEAD^BDGF2(+$P(SDX,U,4)) W !?32,"** Patient Died on "_$$GET1^DIQ(2,+$P(SDX,U,4),.351)   ;IHS/ANMC/LJF 12/22/2000
 W !,?32,"CLERK: ",$S($P(SDX,U,3):$P($G(^VA(200,$P(SDX,U,3),0)),U),$P(SDX,U)["NT":"NONE - NO ACTION TAKEN",1:"UNKNOWN")
 S SDHD=0,Y3=C1
WR3 ;I $P(SDX,U)["A" W !,?32,"REBOOKED ON " S SDRB=$P(SDX,U,2),Y=SDRB X ^DD("DD") W Y,!  ;IHS/ANMC/LJF 11/03/2000
 I $P(SDX,U)["A" W !,?32,"REBOOKED TO " S SDRB=$P(SDX,U,2),SDRB=$$RBKDT^BSDN1(SDRB,+$P(SDX,U,4)),Y=SDRB X ^DD("DD") W Y,!  ;IHS/ANMC/LJF 11/03/2000
 Q
 ;
TOT I 'SDABB F C1=0:0 S C1=$O(^UTILITY($J,"SDNO",SDDIV,SDC,C1)) Q:(C1?1"***".E)!(C1="")!SDEND  D TOTAL
 S SDT4=$G(^UTILITY($J,"SDNO",SDDIV,SDC,"***N***","***TOT***"))+$G(^UTILITY($J,"SDNO",SDDIV,SDC,"***NT***","***TOT***"))
 S SDT5=+$G(^UTILITY($J,"SDNO",SDDIV,SDC,"***NA***","***TOT***"))
 S SDT6=+^UTILITY($J,"SDNO",SDDIV,SDC,"***TOT***")
 Q:SDEND
 D:$Y+6>IOSL&(SDIO=IO(0)) SCR Q:SDEND
 D:$Y+6>IOSL HDR D:'SDABB HDR2
 I 'SDABB W !,?27,"___",?45,"___",?75,"___",!!,?27,SDT4,?47,SDT5,?75,SDT6
 I 'SDABB D:$Y+6>IOSL&(SDIO=IO(0)) SCR Q:SDEND  D:$Y+6>IOSL HDR
 S SDPR=$S(^UTILITY($J,"SDNO",SDDIV,SDC,"***TOT***"):$J(((^("***TOT***")/(^UTILITY($J,"SDNO",SDDIV,SDC,"***SDNMS***")+^UTILITY($J,"SDNO",SDDIV,SDC,"***TOT***")))*100),2,0),1:0)_"%"
 ;I 'SDABB W !!!,SDPR," of appointments for ",SDC," were NO-SHOWS for this period" Q  ;IHS/ANMC/LJF 11/03/2000
 NEW X S X=^UTILITY($J,"SDNO",SDDIV,SDC,"***SDNMS***")+^UTILITY($J,"SDNO",SDDIV,SDC,"***TOT***")  ;IHS/ANMC/LJF 11/03/2000 total appts
 I 'SDABB W !!!,SDPR," of "_X_" appointments for ",SDC," were NO-SHOWS for this period" Q  ;IHS/ANMC/LJF 11/03/2000
 I SDABB W !,SDC,?40,$J(SDT4,5),?50,$J(SDT5,5),?60,$J(SDT6,5),?70,$J(SDPR,5)
 Q
 ;
TOTAL S SDT1=$G(^UTILITY($J,"SDNO",SDDIV,SDC,C1,"***N***","***TOT***"))+$G(^UTILITY($J,"SDNO",SDDIV,SDC,C1,"***NT***","***TOT***"))
 S:SDT1 SDOK=1
 S SDT2=+$G(^UTILITY($J,"SDNO",SDDIV,SDC,C1,"***NA***","***TOT***"))
 I SDT1!(SDT2) D WTOT
 Q
 ;
WTOT D:$Y+5>IOSL&(SDIO=IO(0)) SCR Q:SDEND  D:$Y+6>IOSL HDR,HDR2 S X=C1 D DW^%DTC W !,X S Y=C1 X ^DD("DD") W ?10,Y,?27,SDT1,?47,SDT2 S SDT3=SDT1+SDT2 W ?75,SDT3,!
 Q
 ;
HDR2 W !!,?23,"TOTAL NO-SHOWS W/NO",?45,"TOTAL NO-SHOWS W/",?65,"TOTAL NO-SHOWS"
 W:'SDTOT1 !,"DATE" W:SDTOT1 ! W ?23,"REBOOKED APPTS.",?45,"REBOOKED APPTS." D LINE^SDNOS1A
 Q
 ;
SCR I $E(IOST,1,2)="C-" D OUT^SDUTL Q
 Q
 ;
ABB ;Print abbreviated no-show report (clinic totals only)
 S (SDTOT,SDTOT1)=1 D HDR
 F C=0:1 S SDC=$O(^UTILITY($J,"SDNO",SDDIV,SDC)) Q:(SDC?1"***".E)!(SDC="")!SDEND  D
 .I (^UTILITY($J,"SDNO",SDDIV,SDC,"***TOT***")>0&'Q)!Q D:$Y>(IOSL-2) ABBHD Q:SDEND  D TOT
 .Q
 Q:SDEND  D:$E(IOST,1,2)="C-" SCR Q:SDEND  D SDTOT^SDNOS2
 Q
 ;
ABBHD I $E(IOST,1,2)="C-" D OUT^SDUTL Q
 D HDR,HDR3 Q
 ;
HDR3 N SDLINE,SDI
 S SDLINE="",$P(SDLINE,"-",31)=""
 W ?40,"Without",?50,"With",!?40,"Rebooked",?50,"Rebooked",?60,"Total"
 W ?70,"Percent",!,"Clinic",?40,"Appts.",?50,"Appts.",?60,"No-Shows"
 W ?70,"No-Shows",!,SDLINE F SDI=1:1:4 W ?(30+(10*SDI)),$E(SDLINE,1,8)
 Q
