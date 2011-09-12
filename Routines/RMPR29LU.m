RMPR29LU ;HIN/RVD-LAB ISSUE FROM STOCK UTILITY;5/27/1998
 ;;3.0;PROSTHETICS;**33,37,44**;Feb 09, 1996
 ;Per VHA Directive 10-93-142, this routine should not be modified.
LIS K ^UTILITY("DIQ1",$J),HLD,DIC
 S (RMPRDFN,DFN,Y)=$P(^RMPR(664.1,RMPRDA,0),U,2) D DEM^VADPT
 S RMPRNAM=$P(VADM(1),U),RMPRSSN=$P(VADM(2),U),RMPRDOB=$P(VADM(3),U)
 Q:$G(RMPRDA)'>0  S DIC="^RMPR(664.1,",DA=RMPRDA,DR=".02;.11;.04;.09;2;4;13;15;19"
 D EN^DIQ1 K DIQ,DR S PAGE=1,DA=RMPRDA
 F RI=0:0 S RI=$O(^RMPR(664.1,DA,2,RI)) Q:RI'>0  I $D(^(RI,0)) D
 .S RM6(RI)=$G(^RMPR(664.1,DA,2,RI,0))
 .S DIC="^RMPR(664.1,",DR="6"
 .S DR(664.16)=".01;2;3;8;9;10;7;12;13;14;15;16;17;18",DA(664.16)=RI
 .S HLD(RI)=$$ITM1^RMPR31U($P(^RMPR(664.1,DA,2,RI,0),U))
 .D EN^DIQ1 K DIQ,DR
 I '$D(PNK) D HD^RMPR29W
 I $D(PNK) D HDC^RMPR29W
 S RI=0
ITD ;ITEM DISPLAY
 S RI=$O(HLD(RI)) I '$G(RI)!$G(RMEXIT) K ^UTILITY("DIQ1"),^UTILITY($J),PNK,DIR Q
 W !,HLD(RI),?10,$E(^UTILITY("DIQ1",$J,664.16,RI,.01),1,15)
 W ?27,$E(^UTILITY("DIQ1",$J,664.16,RI,12),1,15),?45,^(2),?50,^(3),?55,^(8),?65,^(9)
 W !,?10,"HCPCS: ",^UTILITY("DIQ1",$J,664.16,RI,13)
WP ;WORD PROCESSING FIELD DISPLAY
 ;D:($Y+8>IOSL) ASK Q:$D(RMEXIT)
 ;S RWP=$O(^UTILITY("DIQ1",$J,664.16,RI,7,0))
 ;I RWP'>0 K HLD(RI) K D0 D ADC^RMPR293(RMPRDA,RI) D  G ITD
 S RLOC=^UTILITY("DIQ1",$J,664.16,RI,17)
 S RGIP=^UTILITY("DIQ1",$J,664.16,RI,18)
 S RMINVF=$S(RLOC'="":"PROS INVENTORY",RGIP'="":"GIP",1:"OTHER")
 W:$D(RMINVF) !,?10,"*** ",RMINVF," ***"
 D:($Y+8>IOSL) ASK
 G ITD
 ;
 ;S X=$G(^UTILITY("DIQ1",$J,664.16,RI,7,RWP))
 ;K ^UTILITY($J) S DIWL=1,DIWR=60,DIWF="R" D ^DIWP Q
 ;
POST I RMPRGIP S PRCP("QTY")=$P(R1(0),U,7)*-1,PRCP("TYP")="R" D ^PRCPUSA
 I $D(PRCP("ITEM")) W !!,"Error encountered while posting to GIP. Inventory Issue did not post..." H 10 S RMEXIT=1 G EXIT
 I RMPRG'="" G GGC
 L +^RMPR(669.9,RMPRSITE,0):999 I $T=0 S RMPRG=DT_99 G GGC
 S RMPRG=$P(^RMPR(669.9,RMPRSITE,0),U,7),RMPRG=RMPRG-1
 S $P(^RMPR(669.9,RMPRSITE,0),U,7)=RMPRG L -^RMPR(669.9,RMPRSITE,0)
GGC S $P(RMPRI("AMS"),U,1)=RMPRG
 S $P(RLB("D"),U,6)=RMTIME,$P(RLB("D"),U,7)=$J(RMLACO,0,2)
 S $P(RLB("D"),U,8)=$J(RMPRCOST,0,2),RMTOTC=RMLACO+RMPRCOST
 S $P(RLB("D"),U,9)=$J(RMTOTC,0,2),$P(RLB("D"),U,11)=DT,RMLAB="Y"
 S RMHCPC=$P(R1(1),U,4),$P(R1(0),U,13)=15,$P(R1(0),U,16)=""
 S RMSER=$P(R1(0),U,11),RMQTY=$P(R1(0),U,7) I $D(RMLOC) D ADD^RMPR5NU1
 W:$D(RMLOC) !!,"Posted to inventory module.."
 ;posting for employee lab item count
 I '$D(RMLOC) D
 .K Y,DD,DO S DIC="^RMPR(661.2,",DIC(0)="L",X=DT,DLAYGO=661.2 D FILE^DICN K DLAYGO S (RM6612,DA)=+Y
 .S ^RMPR(661.2,DA,0)=DT_"^"_RMPRDFN_"^"_RMSO_"^"_RMHCPC_"^^^"_DUZ_"^^"_$P(^RMPR(661.1,RMHCPC,0),U,1)_"^^^^^^"_RMPR("STA")_"^^"
 .S:$D(RMLAB) ^RMPR(661.2,DA,1)=RMTIME_"^"_$J(RMLACO,0,2)
 .S DIK="^RMPR(661.2," D IX1^DIK
 S DIK="^RMPR(660,",(RM660,DA)=+Y D IX1^DIK K DIC W !!,"Posted to 2319..",!
 Q
DEL ;delete status 2529-3
 K DIR,Y
 S DIR(0)="Y",DIR("A")="Would you like to Delete this 2529-3 Entry"
 S DIR("B")="NO" D ^DIR Q:$D(DTOUT)!($D(DUOUT))!(Y=0)
 ;delete entry in the 2319 record and mark entry in 664.1 as deleted
 N BO S BO=0
 F  S BO=$O(^RMPR(664.1,RMPRDA,2,BO)) Q:BO'>0  D
 .S DA=$P(^RMPR(664.1,RMPRDA,2,BO,0),U,5) Q:DA=""
 .S DIK="^RMPR(660," D ^DIK
 W !,?5,"Updated 10-2319" K DA,DIK
 S DIE="^RMPR(664.1,",DA=RMPRDA,DR="16///^S X=""D""" D ^DIE W !,?5,$C(7),"Marked As Deleted..."
 S RDEL=1
 Q
RDL ;delete record
 ;the record is only deleted from 664.1 when the user creats a new
 ;W !! S DIR(0)="Y",DIR("A")="Would you like to delete this Request "
 ;S DIR("B")="NO" D ^DIR Q:$D(DTOUT)!($D(DUOUT))!(Y=0)
 S DA=0,BO=0 Q:$G(REDIT)
 F  S BO=$O(^RMPR(664.1,RMPRDA,2,BO)) Q:BO'>0  D
 .S DA=$P(^RMPR(664.1,RMPRDA,2,BO,0),U,5) Q:DA=""
 .S DIK="^RMPR(660," D ^DIK
 K DIK,DA S DA=RMPRDA,DIK="^RMPR(664.1,"
 D ^DIK K DIK W !!,?5,$C(7),"Deleted..."
 Q
 ;
ASK ;
 K DIR S DIR(0)="E"
 S DIR("A")="Enter 'Return' to view more Items or '^' to QUIT item listing"
 D ^DIR I $D(DTOUT)!$D(DUOUT)!(Y=0) S RMEXIT=1 Q
 W @IOF,RMPR("L")
 Q
 ;
EN4 ;CREATE JOB RECORD
 S RMPR("REF")=$P(^RMPR(664.1,RMPRDA,0),U,4),$P(^(0),U,20)="",RN=+$P(^(0),U,24)
 K DA,D0,DD,DO S DIC="^RMPR(664.2,",DIC(0)="LZ",X=$P(^RMPR(664.1,RMPRDA,0),U,13) D FILE^DICN Q:+Y'>0
 S (RM6642,DA)=+Y,RN=RN+1
 K DIC,Y F RT=0:0 S RT=$O(^RMPR(664.1,RMPRDA,2,RT)) Q:RT'>0  I $D(^(RT,0)) S DA660=$P(^(0),U,5) I +DA660 D  S $P(^RMPR(664.1,RMPRDA,0),U,24)=RN
 .S $P(^RMPR(660,DA660,"LB"),U,5)=RM6642,DA=DA660,DIE="^RMPR(660,",DR="83///^S X=$P(^RMPR(664.1,RMPRDA,0),U,1)" D ^DIE
 S $P(^RMPR(664.2,RM6642,0),U,2)=DA660,$P(^(0),U,3)=RMPR("STA"),$P(^(0),U,4)=RN,$P(^(0),U,8)=RMPR("REF") S DA=RM6642,DIK="^RMPR(664.2," D IX1^DIK
 Q
EXIT ;COMMON EXIT POINT
 D:($Y+8>IOSL) ASK Q:$D(RMEXIT)
 ;S RL=$O(^UTILITY($J,"W",DIWL,0)) I +RL W !,?10,^(RL,0) K ^(0) G EXT
 K ^UTILITY($J)
 Q
HCP(RD0,RD1) ;print HCPCS and GIP or Pros Inventory in -3.
 Q:'$D(^RMPR(664.1,RD0,2,RD1,0))
 S R643=$G(^RMPR(664.1,RD0,2,RD1,3))
 S RPSAITEM=$P(R643,U,3),RPSALOC=$P(R643,U,4)
 S RPHCPC=$P($G(^RMPR(664.1,RD0,2,RD1,2)),U,1)
 Q:'$G(RPHCPC)
 Q:'$D(^RMPR(661.1,RPHCPC,0))
 S RPGIP=$P($G(^RMPR(664.1,RD0,2,RD1,0)),U,13)
 W !,?9,"HCPCS: ",$P(^RMPR(661.1,RPHCPC,0),U,1)
 I $G(RPSALOC),RPSAITEM'="",$D(^RMPR(661.3,RPSALOC,0)) D
 .S RHDA=$O(^RMPR(661.3,RPSALOC,1,"B",RPHCPC,0)) Q:'$G(RHDA)
 .S RIDA=$O(^RMPR(661.3,RPSALOC,1,RHDA,1,"B",RPSAITEM,0))
 .S RIDES=$P($G(^RMPR(661.3,RPSALOC,1,RHDA,1,RIDA,0)),U,8)
 .W ?26,RIDES
 I $G(RPSALOC) W !,?9,"*** Pros Inventory ***",?35,"Location: " I $D(^RMPR(661.3,RPSALOC,0)) W $P(^RMPR(661.3,RPSALOC,0),U,1)
 I '$G(RPSALOC),$G(RPGIP) W !,?9,"*** GIP ***"
 I '$G(RPSALOC),'$G(RPGIP) W !,?9,"*** OTHER ***"
 Q
 ;
CHK ;CHECK DISABILITY AND ITEMS
 ;kill record if not all mandatory fields defined
 K RKILL,RMEXIT,RMEDIT
 F RCK=1,2,3,4,11,15 I $P(^RMPR(664.1,RMPRDA,0),U,RCK)="" S RKILL=1 S DA=RMPRDA,DIK="^RMPR(664.1," D ^DIK W !!,?5,$C(7),"ALL MANDATORY FIELDS NOT DEFINED FORM 2529-3 DELETED" Q
 I $D(RKILL) G EXIT^RMPR29LI
 ;disability code missing
 K DKILL
 I '$D(^RMPR(664.1,RMPRDA,1))!('$O(^RMPR(664.1,RMPRDA,1,0))) S DKILL=1
 F RI=0:0 S RI=$O(^RMPR(664.1,RMPRDA,1,RI)) Q:RI'>0  I $P(^(RI,0),U,1)=""!($P(^(0),U,2)="") S DKILL=1
 ;item missing
 K IKILL
 I '$D(^RMPR(664.1,RMPRDA,2))!('$O(^RMPR(664.1,RMPRDA,2,0))) S IKILL=1
 F RI=0:0 S RI=$O(^RMPR(664.1,RMPRDA,2,RI)) Q:RI'>0  I $P(^(RI,0),U,1)=""!($P(^(0),U,2)="")!($P(^(0),U,4)="")!($P(^(0),U,7)="")!($P(^(0),U,8)="") S IKILL=1
ER1 ;error message
 I $D(DKILL) W $C(7),!!,?5,"2529-3 FORM INCOMPLETE.  DISABILITY CODE INFORMATION IS MISSING!!"
 I $D(IKILL) W $C(7),!!,?5,"2529-3 FORM INCOMPLETE.  ITEM INFORMATION IS MISSING!!"
 I $D(IKILL)!($D(DKILL)) S DIR(0)="Y",DIR("B")="YES" D
 .S DIR("A")="Would you like to EDIT this 2529-3 Entry"
 .D ^DIR I $D(DTOUT)!$D(DUOUT)!(Y=0) S RMEXIT=1 Q
 .S RMEDIT=1
 ;K DA,DIC,DIK,DIWF,DIWL,DIWR,PAGE,PNK,RCK,RI,RL,RWP,X
 ;G LAB^RMPR29LI
 Q