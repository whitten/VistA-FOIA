PRCBE0 ;WISC@ALTOONA/CTB-285 DISTRIBUTION ;4/14/93  11:27 AM
V ;;5.1;IFCAP;;Oct 20, 2000
 ;Per VHA Directive 10-93-142, this routine should not be modified.
OUT K %,%X,%Y,D,D0,D1,D2,DD,DO,DQ,DA,DIC,DIE,DLAYGO,DQ,DR,DWLW,I,J,X,Y Q
 ;
EN4 ;DISTRIBUTION OF RD 2-285 DATA
 S PRCF("X")="AB" D ^PRCFSITE G:'% OUT4
EN41 S DIC("A")="Select TDA NUMBER FOR STATION "_PRC("SITE")_", FY "_PRC("FY")_": "
 S DIC("S")="S ZX=^(0) I $P(ZX,U,16)]"""",$P(ZX,""-"",1,2)=PRCF(""SIFY"")",DIC="^PRCF(421,",DIC(0)="AEQXZ",D="C" D IX^DIC K DIC,ZX,D G:+Y<0 OUT4 S DA=+Y,DIE="^PRCF(421,"
 I $P(Y(0),U,4)="" W !,"TRANSACTION NUMBER ",X," DOES NOT CONTAIN A TDA NUMBER, AND THEREFORE",!,"MAY NOT BE DISTRIBUTED FOR THE RD 2 285 REPORT",$C(7),! G EN41
 F I=1:1:4 S PX(I)=$P(Y(0),U,I+6) I PX(I)="" S PX(I)=0
EN42 W !!,"TDA IS DISTRIBUTED AS FOLLOWS: "
 S PX="1ST QTR^2ND QTR^3RD QTR^4TH QTR" W !! F I=1:1:4 W ?$X+10,$P(PX,U,I)
 W !! F I=1:1:4 W ?$X+2,$J(PX(I),15,0)
 W !!,"ENTER THE NON CUMULATIVE DISTRIBUTION FOR THIS TDA"
 W ! S DR="[PRCB RD2-285 ALL]",PY="" D ^DIE I $D(Y)'=0 D OUT4,OUT Q
 S:'$D(^PRCF(421,DA,3)) ^(3)="" S Q=^(3),Q(0)=0 F I=1:1:12 S Q(I)=Q(I-1)+$P(Q,U,I)
 F I=1:1:4 S Q(I+20)=Q(I*3)-Q(I-1*3)
 F I=1:1:4 I +Q(I+20)'=+PX(I) W !,"CUMULATIVE TOTAL FOR ",$P(PX,U,I)," IS NOT CORRECT",!,$C(7) S I(1)=1
 I $D(I(1)) K I W ! S %A="DO YOU WISH TO MAKE CORRECTIONS AT THIS TIME",%B="",%=1 D ^PRCFYN G:%=1 EN42 D OUT4 G OUT
 S Q="" K I F I=1:1:12 S Q=Q_Q(I)_U
 S ^PRCF(421,DA,2)=Q K I,J,Q,P,X,Y W !!,"DISTRIBUTION IS CORRECT BY QUARTER ",!! G EN41
OUT4 K I,J,Q,P,X,Y,PRCF Q