LRCAPV11 ; IHS/DIR/FJE - CREAT NEW WKLD CODES ADDED BY THE SITE ;
 ;;5.2;LR;;NOV 01, 1997
 ;
 ;;5.2;LAB SERVICE;;Sep 27, 1994
 W !!?20,"ADDING A NEW WKLD CODE TO FILE ",!!
EN ;
 W !! K DIC S (LRCDEF0X,LRCDEF0(1))="",DIC="^LAM(",DIC(0)="AQEZM",LRCAPSET=1
 D ^DIC G:Y<1 END S LRP=+Y,LRCODE=$P(Y(0),U,2) I Y(0)["~"!($P(LRCODE,".",2)>0) W !!?30,$C(7)," THIS CODE ALREADY HAS A SUFFIX ",!!,$C(7) G EN
 W !!?10,"Now enter your Suffix ",!! S DIC="^LAB(64.2," D ^DIC I Y<1 W $C(7),!!?5,"Nothing Selected " G EN
 S LRCDEF0=Y(0),LRCDEF=$P(Y(0),U,2) S DA=$O(^LAM("C",+LRCODE_LRCDEF_" ",0)) I DA W !!,$C(7),?10," This code ("_+LRCODE_LRCDEF_") Already exist " K DIC S DIC="^LAM(" D EN^DIQ W !! G EN
 S LRCODEN=+LRCODE_LRCDEF D DICN^LRCAPV1A S DA=$O(^LAM("C",LRCODEN_" ",0))
 I DA,$D(^LAM(DA,0))#2 S DIC="^LAM(" W !!,"# ",DA D EN^DIQ W !!,?30," NOW IN FILE ",!! G EN
 I 'DA W !!?5,"Nothing Added to File ",!!,$C(7) G EN
 Q
END ;
 K DA,DIC,LRCDEF,LRCDEF0,LRCDER0X,LRCODE,LRCODEN,LRCAPSET,LRP,Y,X
 Q
