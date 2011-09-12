NURARWL9 ;HIRMFO/MD-FACILITY TOTAL ROUTINE FOR WORKLOAD STATISTICS REPORTS ;1/22/99  13:30
 ;;4.0;NURSING SERVICE;**20,22**;Apr 25, 1997
FACTOT ; FACILITY TOTALS
 F Y=1,2,3 D
 .S $P(NFVAR,U,Y)=$J($P(NFFTEE,U,Y),1,1)-$J($P(NFREQ,U,Y),1,1) I $J($P(NFFTEE,U,Y),1,1),$J($P(NFREQ,U,Y),1,1),NURSZAP'>6 S $P(NFPROD,U,Y)=($J($P(NFREQ,U,Y),1,1)/$J($P(NFFTEE,U,Y),1,1))*100
 .Q
 I '$G(NURSUMSW) W !,?18,$$REPEAT^XLFSTR("-",114)
 I +NFREQ!(+$P(NFREQ,U,2))!(+$P(NFREQ,U,3)) D
 .W !,$E(NPFAC,1,10)," SUMMARY",?22,$J($P(NFREQ,U),4,1),?28,$J($P(NFFTEE,U),4,1),?34,$J($P(NFVAR,U),5,1) W:NURSZAP'>6 ?41,$J($P(NFPROD,U),3,0) W ?48,$J($P(NFREQ,U,2),4,1),?54,$J($P(NFFTEE,U,2),4,1)
 .W ?60,$J($P(NFVAR,U,2),5,1) W:NURSZAP'>6 ?67,$J($P(NFPROD,U,2),3,0) W ?74,$J($P(NFREQ,U,3),4,1),?80,$J($P(NFFTEE,U,3),4,1),?86,$J($P(NFVAR,U,3),5,1) W:NURSZAP'>6 ?93,$J($P(NFPROD,U,3),3,0)
 .W ?97,$J($P(NFPC,U),4,1),?103,$J($P(NFPC,U,2),4,1),?109,$J($P(NFPC,U,3),4,1),?115,$J($P(NFPC,U,4),4,1),?122,$J($P(NFPC,U,5),3,1),?126,$J(NFPCC,5,1)
 F Z="DOM","REC","HEM" I +MFHRS(Z)!($P(MFHRS(Z),U,2))!(+$P(MFHRS(Z),U,3))!(+FCOUNT(Z)) D
 .W !,$E(NPFAC,1,10)," ",Z," COUNT",?28,$J(+MFHRS(Z),4,1),?54,$J($P(MFHRS(Z),U,2),4,1),?80,$J($P(MFHRS(Z),U,3),4,1)
 .W ?97,$J(+FCOUNT(Z),4,1),?103,$J($P(FCOUNT(Z),U,2),4,1),?109,$J($P(FCOUNT(Z),U,3),4,1),?115,$J($P(FCOUNT(Z),U,4),4,1),?122,$J($P(FCOUNT(Z),U,5),3,1),?126,$J(+FCOUNT(Z),5,1)
 .S (MFHRS(Z),FCOUNT(Z))=0
 .Q
 F X=1:1:3 S $P(NFREQ,U,X)=0,$P(NFFTEE,U,X)=0,$P(NFPROD,U,X)=0
 F X=1:1:5 S $P(NFPC,U,X)=0
 S NFPCC=0,NBR=2
 W:$G(NURSUMSW) ! Q
VAR ;
 I '$G(NURSUMSW) S (TOT1,TOT2,VAR,NSW1,PCT)=0,UNIT=""
 I '$G(NURSUMSW) S NPFAC="" F  S NPFAC=$O(^TMP("NURVAR",$J,NPFAC)) Q:NPFAC=""  D HEADER Q:NURQUIT  S UNIT="" F  S UNIT=$O(^TMP("NURVAR",$J,NPFAC,UNIT)) Q:UNIT=""  S UNIT=$P($G(^(UNIT)),U) I $G(^(UNIT))'="" D GOOD
 I $G(NURSUMSW) D HEADER S NPFAC="" F  S NPFAC=$O(^TMP("NURVAR",$J,NPFAC)) Q:NPFAC=""  D GOOD
 Q
GOOD ;
 I '$G(NURSUMSW),$G(UNIT)'="" D
 . I '$G(NHOS),'(NPWARD=UNIT) Q
 . S (PC2,PC1,PRT)=0
 . S TOTB=$P($G(^TMP("NURVAR",$J,NPFAC,UNIT)),U,2)+$P($G(^(UNIT)),U,3)+$P($G(^(UNIT)),U,4),TOTA=$P($G(^(UNIT)),U,5)+$P($G(^(UNIT)),U,6)+$P($G(^(UNIT)),U,7)
 . S VAR=TOTA-TOTB
 . I TOTA=0 S PC2=$J($G(PC1),3,0) Q
 . I TOTB=0 S PC2=$J($G(PC1),3,0) Q
 . S PCT=TOTA/TOTB,PC1=PCT*100,PC2=$J(PC1,3,0)
 . Q
 I $G(NURSUMSW) D
 . S PC2(NPFAC)=0,PC1(NPFAC)=0,TOTA(NPFAC)=0,TOTB(NPFAC)=0,VAR(NPFAC)=0,PCT(NPFAC)=0,NSW1=0
 . S TOTB(NPFAC)=$P($G(^TMP("NURVAR",$J,NPFAC)),U)+$P($G(^(NPFAC)),U,2)+$P($G(^(NPFAC)),U,3),TOTA(NPFAC)=$P($G(^(NPFAC)),U,4)+$P($G(^(NPFAC)),U,5)+$P($G(^(NPFAC)),U,6)
 . S VAR(NPFAC)=TOTA(NPFAC)-TOTB(NPFAC)
 . I TOTA(NPFAC)=0 S PC2(NPFAC)=$J($G(PC1(NPFAC)),3,0) Q
 . I TOTB(NPFAC)=0 S PC2(NPFAC)=$J($G(PC1(NPFAC)),3,0) Q
 . S PCT(NPFAC)=TOTA(NPFAC)/TOTB(NPFAC),PC1(NPFAC)=PCT(NPFAC)*100,PC2(NPFAC)=$J(PC1(NPFAC),3,0)
 . Q
PRT I ($Y>(IOSL-7)!'NSW1),'$G(NURSUMSW) D HEADER Q:NURQUIT
 I '$G(NURSUMSW) W !?15,$E(UNIT,1,10),?30,TOTA,?35,"|",?39,TOTB,?43,"|",?46,VAR,?51,"|",?54,$S($G(PC2)>0:PC2_"%",$G(TOTA)>0:PC2_"%",1:"*NSS*"),?59,"|"
 I $G(NURSUMSW) W !?15,$E(NPFAC,1,13),?30,TOTA(NPFAC),?35,"|",?39,TOTB(NPFAC),?43,"|",?46,VAR(NPFAC),?51,"|",?54,$S($G(PC2(NPFAC))>0:PC2(NPFAC)_"%",$G(TOTA(NPFAC))>0:PC2(NPFAC)_"%",1:"*NSS*"),?59,"|"
 W !?15,"---------------------------------------------"
 Q
HEADER ;
 I 'NUROUT,$E(IOST)="C",NSW1 D ENDPG^NURSUT1 S:NUROUT NURQUIT=+NUROUT Q:NURQUIT
 S NPCT=NPCT+1,NSW1=1
 U IO W:$E(IOST)="C"!(NPCT>1) @IOF
 W ?45,"STAFFING VARIANCE WORKSHEET" I NURMDSW,NHOS,'$G(NURSUMSW) W ?$X," FOR ",$G(NPFAC)
 W !?15,"DATE: " D NOW^%DTC S Y=% D DT^DIQ W ?($X+2),$S(NURSHFT="D":"DAY",NURSHFT="E":"EVENING",NURSHFT="N":"NIGHT",1:"")," SHIFT"
 I $D(NRPTDAT("PWLS")) W " (PROJECTED) FOR "_$$FMTE^XLFDT(NRPTDAT,"2D")
 W !!?15,"INSTRUCTIONS:",!?15,"COLUMN A:  Total FTEE on duty (from computer printout and staffing adjustments)",!?15,"COLUMN B:  Total FTEE required for workload (taken from computer print-out)"
 W !?15,"COLUMN C:  Total Variance by FTEE (Column A - Column B = Column C)",!?15,"COLUMN D:  Percentage of Total Variance (Column A / Column B = Column D)",!?26,"The letters *NSS* in COLUMN D indicate that no staff are scheduled for this unit"
 W !!?15,"               SOD",?38,"REQ",?46,"VAR",?54,"%TV"
 W !?15,$S($G(NURSUMSW):"FACILITY",1:"UNIT")_"            A",?39,"B",?47,"C",?55,"D"
 W !,?15,$$REPEAT^XLFSTR("-",45)
 Q