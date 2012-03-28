ADGPTLP1 ; IHS/ADC/PDW/ENM - PRINT PATIENT ROSTER IN ALPHA ORDER ;  [ 03/25/1999  11:48 AM ]
 ;;5.0;ADMISSION/DISCHARGE/TRANSFER;;MAR 25, 1999
 ;
 S DGPG=0,DGSTOP=""
 S DGFAC=$P(^DIC(4,DUZ(2),0),U),DGDUZ=$P(^VA(200,DUZ,0),U,2)
 S DGX=^AUTTLOC(DUZ(2),0)
 S DGCITY=$P(DGX,U,13)_","_$P(^DIC(5,+$P(DGX,U,14),0),U)
 S (DGLIN,DGLIN1)="",$P(DGLIN,"-",80)="",$P(DGLIN1,"=",80)=""
 D HEAD
 ;
 S DGNM=""
A S DGNM=$O(^TMP("DGZPTL",$J,"A",DGNM)) G END:DGNM="" S DFN=0
A1 S DFN=$O(^TMP("DGZPTL",$J,"A",DGNM,DFN)) G A:DFN="" S DGSTR=^(DFN)
 S DGRM=$P(DGSTR,U),DGAD=$P(DGSTR,U,2),DGSER=$P(DGSTR,U,3)
 S DGPRV=$P(DGSTR,U,4),DGDS=$P(DGSTR,U,6)
 S DGWARD=$P(DGRM,"-",1),DGBED=$P(DGRM,"-",2,3),DGLOS=""
 K ^UTILITY("DIQ1",$J) S DA=DFN,DIC=2,DR=.033 D EN^DIQ1
 S AGE=^UTILITY("DIQ1",$J,2,DFN,.033) K ^UTILITY("DIQ1",$J)
 I DGAD'="" S X=$P(DGAD,".",1) D H^%DTC S DGLOS=(+$H-+%H)+1
 S DGCHART=$P($G(^AUPNPAT(DFN,41,DUZ(2),0)),U,2),DGXX=6-$L(DGCHART)
 F DGII=1:1:DGXX S DGCHART="0"_DGCHART
 S:DGSER?1N.N DGSER=$P(^DIC(45.7,DGSER,0),U)
 S:DGPRV?1N.N DGPRV=$P(^VA(200,DGPRV,0),U)
 ;
 I $Y>(IOSL-5) D NEWPG G END1:DGSTOP=U
 W !?1,DGWARD,?7,DGDS,?10,DGBED,?17,$E(DGNM,1,20)
 W ?38,AGE,?43,$J(DGLOS,2),?48
 W:DGCHART?1N.N $E(DGCHART,1,2)_"-"_$E(DGCHART,3,4)_"-"_$E(DGCHART,5,6)
 W ?57,DGSER,!
 W:DGPRV'="" ?17,"(",$E(DGPRV,1,15),")" W !
 G A1
 ;
END I IOST["C-" K DIR S DIR(0)="E" D ^DIR
END1 G END1^ADGPTLP
 ;
 ;
NEWPG ;***> subrtn for end of page control
 I IOST'?1"C-".E D HEAD S DGSTOP="" Q
 K DIR S DIR(0)="E" D ^DIR S DGSTOP=X
 I DGSTOP'=U D HEAD
 Q
 ;
HEAD ;***> subrtn to print heading
 I (IOST["C-")!(DGPG>0) W @IOF
 W !,DGLIN1 S DGPG=DGPG+1
 W !?11,"*****Confidential Patient Data Covered by Privacy Act*****"
 W !?80-$L(DGFAC)/2,DGFAC,!,DGDUZ
 S DGTY="PATIENT ROSTER " W ?80-$L(DGTY)/2,DGTY,?70,"Page: ",DGPG
 W ! D TIME^ADGUTIL W ?80-$L(DGCITY)/2,DGCITY
 S Y=DT X ^DD("DD") W !?80-$L(Y)/2,Y
 W !,DGLIN1
 W !,"Ward",?10,"Room",?22,"Patient",?37,"Age",?42,"Days"
 W ?49,"Chart",?59,"Service",!?22,"(Provider)",?51,"No."
 W !,"----",?10,"----",?18,"-----------------",?37,"----"
 W ?42,"----",?48,"-------",?57,"----------------"
 W !
 Q
