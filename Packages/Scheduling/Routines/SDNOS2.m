SDNOS2 ;ALB/LDB - DIVISION TOTAL FOR NO-SHOW REPORT ; [ 09/13/2001  2:36 PM ]
 ;;5.3;Scheduling;**194**;Aug 13, 1993
 ;IHS/ANMC/LJF 11/03/2000 added total # appts to % statement
 ;                        removed page # from list template
 ;
SDTOT Q:SDEND  D HDR W !,?30,"TOTALS FOR   ",SDDIV2 D HDR2
 N SDRT S SDRT="^UTILITY($J,""SDNO"",SDDIV)"
 W !!,?27,$G(@SDRT@("***N***","***TOT***"))+$G(@SDRT@("***NT***","***TOT***")),?47,@SDRT@("***NA***","***TOT***"),?75,@SDRT@("***TOT***"),!!!
 S SDPR1(SDDIV)=$S(@SDRT@("***TOT***"):$J(((^("***TOT***")/(@SDRT@("***SDNMS***")+@SDRT@("***TOT***")))*100),2,0),1:0)_"%" ; NAKED REFERENCE - ^UTILITY($J,"SDNO",DIV,"***TOT***")
 D:(SDIO=IO(0))&($E(IOST,1,2)="C-") OUT^SDUTL Q:SDEND  D:$Y+6>IOSL HDR
 ;W !!,SDPR1(SDDIV)," of appointments in the clinic(s) selected for this report",!,"for this period were NO-SHOWS ",! Q  ;IHS/ANMC/LJF 11/03/2000
 NEW X S X=$G(^UTILITY($J,"SDNO",SDDIV,"***SDNMS***"))+$G(^UTILITY($J,"SDNO",SDDIV,"***TOT***"))  ;IHS/ANMC/LJF 11/03/2000 total appts
 W !!,SDPR1(SDDIV)," of "_X_" appointments in the all clinics selected for this report",!,"for this period were NO-SHOWS ",! Q  ;IHS/ANMC/LJF 11/03/2000
 ;
HDR ;Q:SDIO=IO&(SDEND)  D NOW^%DTC S Y=% X ^DD("DD") W @IOF,Y,?70,"PAGE " S P1=P1+1 W P1  ;IHS/ANMC/LJF 11/03/2000
 Q:SDIO=IO&(SDEND)  I '$G(VALM) D NOW^%DTC S Y=% X ^DD("DD") W @IOF,Y,?70,"PAGE " S P1=P1+1 W P1  ;IHS/ANMC/LJF 11/03/2000
 W !,?30,"NO SHOW REPORT",$S(SDTOT!SDTOT1:" TOTALS",1:""),! D LINE W !!,?2,"FOR PERIOD COVERING: " S Y=SDBD D D^DIQ S SDBG=Y W ?30,SDBG
 I $D(SDED) S Y=SDED D D^DIQ W " TO ",Y
DIV I $D(^DG(40.8,SDDIV,0)) S SDDIV2=$P(^(0),U,1)
 I $D(^DG(43,1,"GL")),$P(^("GL"),U,2),$D(^DG(40.8,SDDIV,0)) W !,?9,"FOR DIVISION: ",?30,SDDIV2
 I $L(SDC),'$D(^UTILITY($J,"DGTC",SDC))&('SDTOT)&('SDTOT1) S ^UTILITY($J,"DGTC",SDC,P1)=""
 I $L(SDC),'$D(^UTILITY($J,"DGTC",SDC_" TOTALS"))&(SDTOT) S ^UTILITY($J,"DGTC",SDC_" TOTALS",P1)=""
 I '$D(^UTILITY($J,"DGTC",SDDIV2))&('SDV1) S ^UTILITY($J,"DGTC",SDDIV2,P1)=""
 I '$D(^UTILITY($J,"DGTC",SDDIV2_" TOTALS"))&(SDTOT1) S ^UTILITY($J,"DGTC",SDDIV2_" TOTALS",P1)=""
 W:'SDTOT1!(SDTOT1&(SDC>0)) !,?11,"FOR CLINIC:",?30,SDC D LINE W ! Q
 ;
LINE S X="",$P(X,"=",IOM)="" W !,X Q
 ;
HDR2 Q:SDEND  W !!,?22,"TOTAL NO-SHOWS W/NO",?45,"TOTAL NO-SHOWS W/",?65,"TOTAL NO-SHOWS"
 W:'SDTOT1 !,"DATE" W:SDTOT1 ! W ?22,"REBOOKED APPTS.",?45,"REBOOKED APPTS." D LINE Q
