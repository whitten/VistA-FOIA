LRCAPV1A ; IHS/DIR/AAB - SET NEW WKLD CODE INTO ^LAM ;
 ;;5.2;LR;**1002**;JUN 01, 1998
 ;;5.2;LAB SERVICE;**105,127,163**;Sep 27, 1994
SET ;from LRCAPV1
 S LRCDEF0X="" I LRCDEF0(1) S LRCDEF0X=LRCDEF0 S:'$D(^LAM(LRP,0))#2 LRNOCODE=1 Q:LRNOCODE  S LRCDEF0=^(0)
 S LRCODEN=$P(LRCODE,".")_"."_LRCDEF,LRPN=$O(^LAM("C",LRCODEN_" ",0)) G:LRPN<1 DICN S LRP=LRPN
 I LRCDEF0(1) S LRCDEF0=LRCDEF0X
 Q
DICN N DIC,DR,DD,DIE
 Q:'$D(^LAM(LRP,0))#2
 S X=$E($P(^LAM(LRP,0),U)_"~"_$S('LRCDEF0(1):$P(LRCDEF0,U),1:$P(LRCDEF0X,U)),1,60),DIC(0)="L",DLAYGO=64,DIC="^LAM(" D FILE^DICN
 S LRCY=Y,LRSTR=^LAM(LRP,0),$P(LRSTR,U)=$P(LRCY,U,2),$P(LRSTR,U,2)=LRCODEN
 S $P(LRSTR,U,14)=$S($P($G(LRCDEF0),U,14):$P($G(LRCDEF0),U,14),1:1)
 I $P($P(LRSTR,U,2),".")=82410 S $P(LRSTR,U,11)=""
 I $P($P(LRSTR,U,2),".")'=82410 S $P(LRSTR,U,3)=""
 S ^LAM(+LRCY,0)=LRSTR,^LAM("C",LRCODEN_" ",+LRCY)="",^LAM("E",LRCODEN,+LRCY)="",(DA,LRP)=+LRCY
 I $G(LRCAPSET) K DR S DR="4;7;8;9;13;14;15;21;19",DIE=DIC D ^DIE
 K DLAYGO,LRCY,LRSTR
 I LRCDEF0(1) S LRCDEF0=LRCDEF0X
 Q
