ADGPTLP0 ; IHS/ADC/PDW/ENM - PRINT PATIENT LIST BY WARD ;  [ 03/25/1999  11:48 AM ]
 ;;5.0;ADMISSION/DISCHARGE/TRANSFER;;MAR 25, 1999
 ;PRINT PATIENTS W/OUT ROOM-BED ASSIGNMENTS
 ;
 S DFN=0,DGRMX=$P(DGRM,"-")_"-"
A1 S DFN=$O(^TMP("DGZPTL",$J,"WD",DGRMX,DFN)) G END:DFN=""
 ;
 S (DGNM,DGAD,DGSER,DGDX,DGPRV,DGCOM,DGCHART,AGE,DGLOS,DGDS)=""
 S DGSTR=^TMP("DGZPTL",$J,"WD",DGRMX,DFN)
 S DFN=$P(DGSTR,U),DGNM=$P(DGSTR,U,2),DGDS=$P(DGSTR,U,7)
 S DGAD=$P(DGSTR,U,3),DGDX=$P(DGSTR,U,4),DGPRV=$P(DGSTR,U,5)
 S DGCOM=$P(DGSTR,U,6) S:DGPRV?1N.N DGPRV=$P(^VA(200,DGPRV,0),U)
 S DGCHART=$P($G(^AUPNPAT(DFN,41,DUZ(2),0)),U,2),DGXX=6-$L(DGCHART)
 F DGII=1:1:DGXX S DGCHART="0"_DGCHART
 K ^UTILITY("DIQ1",$J) S DA=DFN,DIC=2,DR=.033 D EN^DIQ1
 S AGE=^UTILITY("DIQ1",$J,2,DFN,.033) K ^UTILITY("DIQ1",$J)
 I DGAD'="" S X=$P(DGAD,".",1) D H^%DTC S DGLOS=(+$H-+%H)+1
 I DGO=2,DGDX?1N.N S DGSER=$E($P(^DIC(45.7,DGDX,0),U),1,20)
 ;
PRNT I $Y>(IOSL-5) D NEWPG^ADGPTLP Q:DGSTOP=U
 W !,DGDS,?9,$E(DGNM,1,20)
 W:DGCHART ?31,$E(DGCHART,1,2)_"-"_$E(DGCHART,3,4)_"-"_$E(DGCHART,5,6)
 W ?42,AGE I DGO=4 W ! W:DGDX'="" ?20,"(",$E(DGDX,1,25),")" G PRNT1
 W ?47,$J(DGLOS,2)
 W ?53,$S(DGO=1:$E(DGDX,1,20),DGO=2:DGSER,1:"")
 W ! W:DGPRV'="" ?11,"(",$E(DGPRV,1,15),")"
 W:DGCOM'="" ?33,"(",DGCOM,")"
PRNT1 W !,DGLIN G A1
END I IOST["C-" K DIR S DIR(0)="E" D ^DIR S DGSTOP=X Q
