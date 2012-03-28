DGMTO1 ;ALB/CAW,AEG - AGREED TO PAY DEDUCTIBLE PRINT (CON'T) ; 8/12/92
 ;;5.3;Registration;**33,182,358**;Aug 13, 1993
 ;
START ;
 ; loop through cat Cs for active ones
 S (DGPAGE,DGSTOP)=0
 F DGCAT=2,6 F DFN=0:0 S DFN=$O(^DPT("ACS",DGCAT,DFN)) Q:DFN'>0  D CATCLST
 D CATCOUT
 K ^TMP("DGMTO",$J,"CNULL"),DFN
 D CLOSE^DGMTUTL Q
CATCLST N DGWHEN,DGDT,IEN,NODE0
 S NODE0=$G(^DPT(DFN,0)) Q:(+$G(^(.35)))!($P(NODE0,U,14)'=DGCAT)
 F DGDT=0:0 S DGDT=$O(^DGMT(408.31,"AD",1,DFN,DGDT)) Q:'DGDT  S IEN=$$MTIEN^DGMTU3(1,DFN,-DGDT) I IEN,(DGDT'<DGYRAGO)&(DGDT'>DGTODAY) D
 .Q:DGCAT'[$P($G(^DGMT(408.31,+IEN,0)),U,3)
 .Q:$P($G(^DGMT(408.31,+IEN,0)),U,11)=1
 .S DGWHEN=""
 .I $$ACTIVE(DGYRAGO,DGTODAY) S $P(DGWHEN,U,1)="X" ;PAST YR
 .I +$G(^DPT(DFN,.105)) S $P(DGWHEN,U,2)="X" ;INHOUSE
 .I $$ACTIVE(DGTODAY,9999999) S $P(DGWHEN,U,3)="X" ;FUTURE
 .S:DGWHEN]"" ^TMP("DGMTO",$J,"CNULL",$P(NODE0,U,1),DFN)=DGWHEN_";;"_$P(NODE0,U,1)_";;"_DGCAT_";;"_$$SR^DGMTAUD1($G(^DGMT(408.31,+IEN,0)))
QTC Q
ACTIVE(FROM,TO) ;
 ;Y=0 IF NOT ACTIVE
 ;1:DISPOSITION
 ;2:CLINIC APPT
 ;3:SCHEDULED ADMISSION
 ;4:PATIENT MOVEMENT
 ;
 N A,X,Y
 S Y=0
 S X=$O(^DPT(DFN,"DIS",(9999999-TO))) S:X&(X<(9999999-FROM)) Y=1
 I 'Y S X=$O(^DPT(DFN,"S",FROM)) S:(+X)&(+X<TO) Y=2
 I 'Y F A=0:0 S A=$O(^DGS(41.1,"B",DFN,A)) Q:A'>0  S X=$P($G(^DGS(41.1,+A,0)),U,2) S:(X'<FROM)&(X'>TO) Y=3
 I 'Y S X=$O(^DGPM("APRD",DFN,FROM)) S:(+X)&(+X<TO) Y=4
 Q Y
CATCOUT ;
 U IO D HDR
 I $D(^TMP("DGMTO",$J,"CNULL")) D PRINT,LEGEND Q
 W:'$D(^TMP("DGMTO",$J,"CNULL")) !,?5,"NO ACTIVE PATIENTS WHO HAVE NOT AGREED TO PAY DEDUCTIBLE",!?5,"   ------",!
 Q
PRINT ;
 S DGNAME=""
 F  S DGNAME=$O(^TMP("DGMTO",$J,"CNULL",DGNAME)) Q:DGNAME']""  D  Q:DGSTOP
 .F DFN=0:0 S DFN=$O(^TMP("DGMTO",$J,"CNULL",DGNAME,DFN)) Q:DFN'>0  S DGX=^(DFN) D  Q:DGSTOP
 ..D PID^VADPT6
 ..W !,$P(DGX,";;",2),?25,$S($P(DGX,";;",3)=2:"Pend Adj",1:"Cat. C"),?35,VA("PID"),?50,$P(DGX,";;",4),?59,$P($P(DGX,";;",1),U,1),?67,$P($P(DGX,";;",1),U,2),?75,$P($P(DGX,";;",1),U,3)
 ..D CHK
 K VA,VAPTYP,DGNAME
 Q
 ;
HDR ;
 S DGPAGE=DGPAGE+1
 W:$E(IOST,1,2)["C-" @IOF W "Active Patients Who Have Not Agreed To Pay Deductible",?70,"Page: "_DGPAGE
 W !,"Date Range: "_$$FDATE^DGMTUTL(DGYRAGO)_" to "_$$FDATE^DGMTUTL(DGTODAY) D NOW^%DTC W ?51,"Run Date: "_$E($$FTIME^DGMTUTL(%),1,18)
 W !,""
 W !,?37,"PATIENT",?47,"MEANS TEST"
 W !,"PATIENT NAME",?25,"STATUS",?40,"ID",?49,"SOURCE",?58,"PAST",?64,"INHOUSE",?73,"FUTURE"
 S DGLINE="",$P(DGLINE,"=",IOM)=""
 W !,DGLINE
 Q
CHK ;Check to pause on screen
 I ($Y+5)>IOSL,$E(IOST,1,2)="C-" D PAUSE S DGP=Y D:DGP HDR I 'DGP S DGSTOP=1 Q
 I $E(IOST,1,2)="P-",($Y+5)>IOSL,$O(^TMP("DGMTO",$J,DGNAME,DFN)) D HDR Q
 Q
PAUSE ;
 W ! S DIR(0)="E" D ^DIR K DIR W !
 Q
LEGEND ;Legend at end of report
 W !!,"ACTIVE= Sched. Admissions, Dispositions, Pt. Movements, or Clinic Appts."
 W !!,?10,"INHOUSE = Current Inpatient"
 W !,?10,"PAST    = ",$$FDATE^DGMTUTL(DGYRAGO)," to ",$$FDATE^DGMTUTL(DGTODAY)
 W !,?10,"FUTURE  = After ",$$FDATE^DGMTUTL(DGTODAY)
 Q
