SROQ2 ;B'HAM ISC/ADM - SUMMARY REPORT ; [ 07/31/00  11:44 AM ]
 ;;3.0; Surgery ;**38,62,70,50,95**;24 Jun 93
 ;
 ;** NOTICE: This routine is part of an implementation of a nationally
 ;**         controlled procedure.  Local modifications to this routine
 ;**         are prohibited.
 ;
 S SRSOUT=0 W @IOF,!,"SUMMARY REPORT FOR SURGICAL SERVICE"
 W !!,"Enter date range for data to be included on report."
SDATE ; enter starting date
 W !,"Start with date:  " R X:DTIME I '$T!(X["^")!(X="") S SRSOUT=1 G END
 I X["?" W !,"Enter the EARLIEST date for data to be included in this report." S X="?",%DT="EX" D ^%DT G SDATE
 S %DT="EXP" D ^%DT G SDATE:Y<1 S SRSTART=Y
 I SRSTART>DT W !!,"Cannot report on operations for future dates !",! G SDATE
 S SRAC=$O(^SRF("AC",0)) I SRSTART<SRAC S Y=SRAC D DD^%DT S SRAC1=$E(Y,1,12) W !!,"NOTE: ",$S(SRAC:"No surgical case data exists before "_SRAC1_".",1:"There are no surgical cases on record !")
EDATE ; enter ending date
 W !!,"End with date:  " R X:DTIME I '$T!(X["^")!(X="") S SRSOUT=1 G END
 I X["?" W !,"Enter the LATEST date for data to be included in this report." S X="?",%DT="EX" D ^%DT G EDATE
 S %DT="EXP" D ^%DT G EDATE:Y<1 S SREND=Y
 I SRSTART>SREND W !!,"The ENDING date must be later than the BEGINNING date.  Please try again.",! G SDATE
 I SREND>DT W !!,"Cannot report on operations for future dates !",! G EDATE
 S SRFLG=0
 N SRINSTP S SRINST=$$INST^SROUTL0() G:SRINST="^" END S SRINSTP=$P(SRINST,U),SRINST=$S(SRINST["ALL DIVISIONS":SRINST,1:$P(SRINST,U,2))
IO W ! K %ZIS,IO("Q"),POP S %ZIS("A")="Print report on which Device: ",%ZIS="Q" D ^%ZIS I POP S SRSOUT=1 G END
 I $D(IO("Q")) K IO("Q") S ZTDESC="Summary Report - Surgical Service",(ZTSAVE("SRSTART"),ZTSAVE("SRINSTP"),ZTSAVE("SREND"),ZTSAVE("SRFLG"))="",ZTRTN="EN^SROQ2" D ^%ZTLOAD S SRSOUT=1 G END
EN ; entry point when queued
 D SET,^SROQ1,END
 Q
SET ; collect data
 S SRSD=SRSTART-.0001,SRED=SREND+.9999 D ZERO
 N SRXX S SRXX=$$SITE^SROVAR
 I SRFLG=1 D
 .I SRMULT D  Q
 ..S:'SRIEN SRINST=$P(SRXX,"^",2),SRSTATN=$P(SRXX,"^",3)
 ..S:'$D(SRINSTP) SRINSTP="ALL DIVISIONS",SRINST=SRINST_" - ALL DIVISIONS"
 .S SRINSTP=$P(SRXX,"^"),SRINST=$P(SRXX,"^",2),SRSTATN=+$P(SRXX,"^",3)
 I 'SRFLG D
 .I SRINSTP["ALL DIV" S SRINST=$P(SRXX,"^",2)_" - ALL DIVISIONS",SRSTATN=$P(SRXX,"^",3) Q
 .S SRINST=$$GET1^DIQ(4,SRINSTP,.01),SRSTATN=$$GET1^DIQ(4,SRINSTP,99)
 F  S SRSD=$O(^SRF("AC",SRSD)) Q:SRSD>SRED!('SRSD)  S SRTN=0 F  S SRTN=$O(^SRF("AC",SRSD,SRTN)) Q:'SRTN  I $D(^SRF(SRTN,0)),$$MANDIV^SROUTL0(SRINSTP,SRTN) D CASE
 D MORT^SROQ1A,DEATH S Y=SRSTART D DD^%DT S SRSD=$E(Y,1,12) S Y=SREND D DD^%DT S SRED=$E(Y,1,12),SRYR=$E(Y,9,12) I SRFLG,$E(SRSTART,4,5)=10 S SRYR=SRYR+1
 Q
END W ! I 'SRSOUT,$E(IOST)="C" W !!,"Press <RET> to continue  " R X:DTIME
 D KTMP W:$E(IOST)="P" @IOF I $D(ZTQUEUED) Q:$G(ZTSTOP)  S ZTREQ="@" Q
 D ^SRSKILL K SR14,SRADMT,SRHOSP,SRIDP,SRINSTP,SRINV,SRIOSTAT,SRTN D ^%ZISC W @IOF
 Q
KTMP F I="SRDEATH","SRDPT","SRDREL","SRDTH","SREXP","SRINOUT","SRIOD","SRP","SRPROC","SRREL","SRSP","SRSS","SRTN" K ^TMP(I,$J)
 Q
CASE ;  examine case
 Q:$P($G(^SRF(SRTN,30)),"^")!'$P($G(^SRF(SRTN,.2)),"^",12)!($P($G(^SRF(SRTN,"NON")),"^")="Y")
 S SRCASES=SRCASES+1 D ^SROQ0
 Q
ZERO ; set counters to 0
 D KTMP S (SR60,SRADMT,SRCASES,SRCOMP,SRDPT,SREMERG,SRIN,SRINPAT,SRIX,SRMAJOR,SRMORT,SROPD,SRSOUT,SRWC)=0 F I=1:1:7 S SRASA(I)=0
 S SRSS=0 F I=50:1:62,500,501,502 S SRSPEC(I)=I,^TMP("SRSS",$J,I)="0^0^0^0",SREXP(I)=0
 S ^TMP("SRSS",$J,"ZZ")="0^0^0^0",SREXP("ZZ")=0
 F I=0:1:4 S (SRATT(I),SRATT("J",I),SRATT("N",I))=0
 F SRPROC=1:1:12 S ^TMP("SRPROC",$J,SRPROC)="0^0",SRDEATH(SRPROC)=0
 S (SRINV("I"),SRINV("O"))=0 F I=1:1:32 S SRC(I)=0
 Q
DEATH ; tabulate deaths
 S SRED=SREND+.9999,SRSD=SRSTART-.0001,DFN=0 F  S DFN=$O(^TMP("SREXP",$J,DFN)) Q:'DFN  D SPEC
 S SRSS=0 F  S SRSS=$O(SREXP(SRSS)) Q:SRSS=""  S ^TMP("SRSS",$J,SRSS)=^TMP("SRSS",$J,SRSS)_"^"_SREXP(SRSS)
 S DFN=0 F  S DFN=$O(^TMP("SRDEATH",$J,DFN)) Q:'DFN  D IP
 F J=1:1:12 S ^TMP("SRPROC",$J,J)=^TMP("SRPROC",$J,J)_"^"_SRDEATH(J)
 S DFN=0 F  S DFN=$O(^TMP("SRIOD",$J,DFN)) Q:'DFN  D INOUT
 Q
SPEC ; determine related specialty
 I $O(^TMP("SRSP",$J,DFN,0))="" S Y=^TMP("SREXP",$J,DFN),SRTN=$P(Y,"^"),SRSS=$P(Y,"^",2),SRDT=$P(^SRF(SRTN,0),"^",9) Q:SRDT>SRED!(SRDT<SRSD)  S SREXP(SRSS)=SREXP(SRSS)+1,SRMORT=SRMORT+1 Q
 S SRDT=$O(^TMP("SRSP",$J,DFN,0)) I (9999999-SRDT)'>SRED&((9999999-SRDT)'<SRSD) S SRSS=^TMP("SRSP",$J,DFN,SRDT),SREXP(SRSS)=SREXP(SRSS)+1,SRMORT=SRMORT+1
 Q
IP ; determine related index procedure (if any)
 I $O(^TMP("SRP",$J,DFN,0))="" S Y=^TMP("SRDEATH",$J,DFN),SRTN=$P(Y,"^"),SRPROC=$P(Y,"^",2),SRDT=$P(^SRF(SRTN,0),"^",9) Q:SRDT>SRED!(SRDT<SRSD)  S SRDEATH(SRPROC)=SRDEATH(SRPROC)+1 Q
 S SRDT=$O(^TMP("SRP",$J,DFN,0)) I (9999999-SRDT)'>SRED&((9999999-SRDT)'<SRSD) S SRPROC=^TMP("SRP",$J,DFN,SRDT),SRDEATH(SRPROC)=SRDEATH(SRPROC)+1
 Q
INOUT ; decide if death is in or out-pat surgery death
 S SRIOSTAT="" I $O(^TMP("SRINOUT",$J,DFN,0))="" S Y=^TMP("SRIOD",$J,DFN),SRTN=$P(Y,"^"),SRIOSTAT=$P(Y,"^",2),SRDT=$P(^SRF(SRTN,0),"^",9) Q:SRDT>SRED!(SRDT<SRSD)!(SRIOSTAT'="O")  S SROPD=SROPD+1 Q
 S SRDT=$O(^TMP("SRINOUT",$J,DFN,0)) S SRIOSTAT=^TMP("SRINOUT",$J,DFN,SRDT) I (9999999-SRDT)'>SRED&((9999999-SRDT)'<SRSD)&(SRIOSTAT="O") S SROPD=SROPD+1
 Q
