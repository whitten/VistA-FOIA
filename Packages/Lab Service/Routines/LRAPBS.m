LRAPBS ; IHS/DIR/AAB - BLOCK/SLIDE DATA ENTRY 8/15/95 11:14 ;
 ;;5.2;LR;**1002**;JUN 01, 1998
 ;;5.2;LAB SERVICE;**72**;Sep 27, 1994
 D ^LRAP G:'$D(Y) END K DR,Y D A^LRAPWU,S^LRAPST W !! D @LRSS G END
 ;
SP S (LRK(1),LRK)="",J=0 D:LRCAPA SP^LRAPBS1 Q:J  F X=1,2,3 S Y(X)=$P(^DD(63.812,X,0),"^") W !?15,X,". ",Y(X)
 S Z="",B=1 F A=0:0 W !,"Selection (",B,"): " R X:DTIME Q:X=""!(X[U)  D:X<1!(X>3)!(X'=+X) HELP I X>0&(X<4)&(X=+X) W " ",Y(X) S:Z'[X Z=Z_X_";" S B=B+1 Q:B=4
 Q:X[U  I Z="" S Z="1;2;3" W "  All selected."
 S DR=.012,(DR(3,63.8121),DR(3,63.822))=".01;.02//^S X=LRK(1);.04;1//^S X=""H & E STAIN""",DR(3,63.824)=".01;.02//^S X=LRK(1);.04;1//^S X=""FROZEN SECTION H & E"""
 S (DR(4,63.8122),DR(4,63.823),DR(4,63.825))=".01;.02//1;.03;.04//^S X=LRK",DR(2,63.812)=Z D ^LRAPBS1 Q
 ;
CY S (LRK,LRK(1))="" F X=1:1:5 S Y(X)=$P(^DD(63.902,X,0),"^") W !?15,X,". ",Y(X)
 S Z="",B=1 F A=0:0 W !,"Selection (",B,"): " R X:DTIME Q:X=""!(X[U)  D:X<1!(X>5)!(X'=+X) HELP I X>0&(X<6)&(X=+X) W " ",Y(X) S:Z'[X Z=Z_X_";" S B=B+1 Q:B=6
 Q:X[U  I Z="" S Z="1;2;3;4;5" W "  All selected."
 S DR=.012,DR(3,63.9121)=".01;1//^S X=""PAP STAIN""",DR(3,63.922)=".01;1//^S X=""H & E STAIN""",DR(4,63.9122)=".01;.02;.03;.04//^S X=LRK",DR(4,63.923)=".01;.02;.03;.04//^S X=LRK"
 S (DR(63.9024),DR(3,9025))=".01;1//^S X=""PAP STAIN""",DR(3,63.924)=".01;1//^S X=""PAP STAIN,MEMBRANE FILTER""",(DR(4,63.9241),DR(4,63.90241),DR(4,63.90251))=".01;.02;.03;.04//^S X=LRK"
 S DR(2,63.902)=Z D ^LRAPBS1 Q
 ;
EM S J=0,X="EM EMBEDDING" D X^LRUWK S LRW(1)=LRT K LRT Q:J
 S DR=.012,DR(3,63.2021)=".01;.02//^S X=LRK(1);1",DR(4,63.20211)=".01;.02;.04//^S X=LRK" F X=2:1:5 S LRW(X)=""
 S DR(2,63.202)=1 D ^LRAPBS1 Q
AU D AU^LRAPBS1 Q:J  S DR=33,DR(2,63.033)=".01;1",DR(3,63.331)=".01;.02//^S X=LRK(1);1",DR(4,63.3311)=".01;.02//1;.03;.04//^S X=LRK" D ^LRAPBS1 Q
 Q
 ;
HELP W $C(7),!!,"Enter a number from 1 to ",$S(LRSS="SP":3,LRSS="CY":5,1:""),! Q
 ;
EN S LRDICS="SP" D ^LRAP G:'$D(Y) END S LRF=1,J=0,LR(1)=$P(^DD(63.812,.04,0),U,3)
 S X="ROUTINE GROSS SURGICAL" D X^LRUWK S LRW(1)=LRT K LRT
 S X="EXTENSIVE GROSS SURGICAL" D X^LRUWK S LRW(2)=LRT K LRT
 S X="TECHNICAL ASSISTANCE SURGICAL" D X^LRUWK S LRW(3)=LRT K LRT
 Q:J  S DR=.012,DR(2,63.812)=".04;.03//^S X=LRK" D ^LRAPBS1 Q
 ;
END D V^LRU Q
