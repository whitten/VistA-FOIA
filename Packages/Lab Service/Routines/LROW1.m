LROW1 ; IHS/DIR/AAB - TEST & SAMP ;  [ 07/22/2002  1:35 PM ]
 ;;5.2;LR;**1002,1003,1013**;JUL 15, 2002
 ;;5.2;LAB SERVICE;**55,100,121,128**;Sep 27, 1994
 K LRSAME,DIC,LRTEST,LRSAMP,LRXST,LRCCOM,LRGCOM S LRTSTN=0 S:'$D(LRCOM) LRCOM=0
L2 K LRNEDC,LROUT K:'$G(LRNN) LRURG ;LRNN=1 when coming from LRFAST
 S DIC="^LAB(60,",DIC(0)="AEMOQ" S DIC("S")="I $L($P(^(0),U,4)),""BI""[$P(^(0),U,3)" S:LRLWC="LC"!(LRLWC="I") DIC("S")=DIC("S")_",$P(^(0),U,9)"
 D ^DIC S:X="^^" DIROUT=1 S:Y<1&$D(LRADDTST) LRADDTST=LRADDTST_"^OUT" Q:Y<1  S LRTY=+Y
NOASK ;from LROR
 S LRTSTN=LRTSTN+1,LRTEST(LRTSTN)=LRTY,LRY=$S($D(LRURG):+LRURG,1:$P(^LAB(60,LRTY,0),U,18)),H=+$P(^(0),U,16) G L3:LRY
 I '$D(LROUTINE) K DIC S DIC="^LAB(62.05,",DIC(0)="AEQF",DIC("B")="ROUTINE",DIC("S")="I '$P(^(0),U,3),Y'<"_H S:LRLWC="LC" DIC("S")=DIC("S")_" I $P(^(0),U,2)" F I=0:0 D ^DIC S LRY=+Y Q:LRY>0  W "  no '^' allowed"
 I $D(LROUTINE) S LRY=LROUTINE
L3 ;
 S LRY=+LRY,LRTEST(LRTSTN)=LRTEST(LRTSTN)_"^"_$S(LRY'=-1:LRY,1:$P(^LAB(69.9,1,3),U,2))
 I $G(LRORIFN),$$VER^LR7OU1 S $P(LRTEST(LRTSTN),"^",7)=LRORIFN ;OE/RR 2.5
 S LRCSN=0,LRSPEC=$S($D(LRSAME):+LRSAME,1:""),LRSAMP=$S($D(LRSAME):+$P(LRSAME,U,2),1:"")
 K DIC,LRCS,LRCSX,H
 W !,"For "_$P(^LAB(60,+LRTEST(LRTSTN),0),"^")_"  "
 I LRLWC="LC" S LRCSN=1,LRCS(1)=$P(^LAB(60,+LRTEST(LRTSTN),0),U,9),LRCSX(+LRCS(1))=1 G W18
 S J=$O(^LAB(60,+LRTEST(LRTSTN),3,0)) G W12:J<1 S LRCSN=1,LRUNQ=+$P(^LAB(60,+LRTEST(LRTSTN),0),U,8),LRCS(1)=+^(3,J,0),LRCSX(+^(0))=1 S X=$P(^LAB(62,LRCS(LRCSN),0),U),X1=$P(^(0),U,3)
 W:'$D(LRSAME)!LRUNQ X,"  ",X1
 G W18:LRUNQ
 G W18B:$D(LRSAME)
 I LRCSN=1 W !,"Correct sample" S %=1 D YN^DICN G W18:%=1
 S J=0 F  S J=$O(^LAB(60,+LRTEST(LRTSTN),3,J)) Q:J<1  S:'$D(LRCSX(+^LAB(60,+LRTEST(LRTSTN),3,J,0))) LRCSN=LRCSN+1,LRCS(LRCSN)=+^(0),LRCSX(+^(0))=J
 G W12:LRCSN<2
 F I=1:1:LRCSN W !,I," ",$P(^LAB(62,LRCS(I),0),U),"  ",$P(^(0),U,3)
 R !,"Choose one: ",X:DTIME IF X?.N&(X>0)&(X<(LRCSN+1)) S LRCSN=+X W " ",$P(^LAB(62,LRCS(LRCSN),0),U) G W18
W12 K DUOUT,DTOUT S LRNEDC=1,DIC="^LAB(62,",DIC(0)="AEFMOQ" D ^DIC I $D(DUOUT)!$D(DTOUT) S LRTSTN=LRTSTN-1 G L2
 G W12:Y<1 S LRCSN=1,LRCS(1)=+Y
W18 S (LRSPEC,Y)=$P(^LAB(62,+LRCS(LRCSN),0),U,2) I LRUNKNOW=+Y,'$D(LRLABKY) W !,"Unknown is not allowed." G W12
W18A I 'LRSPEC S DIC="^LAB(61,",DIC(0)="EMOQ",D="E" R !,"Select SITE/SPECIMEN: ",X:DTIME D IX^DIC:X="?" G W18A:X="?" D ^DIC K DIC I $D(DUOUT)!$D(DTOUT)!(X="") K DTOUT,DUOUT S LRTSTN=LRTSTN-1 G L2
 I LRUNKNOW=+Y,'$D(LRLABKY) W !,"Unknown is not allowed." G W18
 G W18:Y<1 S LRSPEC=+Y
MAX ;
 S LRMAX1=0,LRMAX2=0 I $O(^LAB(60,LRTY,3,"B",+LRCS(LRCSN),0)) S LRMAX2=+$P(^LAB(60,LRTY,3,$O(^LAB(60,LRTY,3,"B",+LRCS(LRCSN),0)),0),U,5) I LRMAX2 D NEW^LRORD2A I $D(LRDAX),%'["Y" S LRTSTN=LRTSTN-1 G L2
 I 'LRMAX2,$D(TT(LRTY,LRSPEC)),$D(^LAB(60,LRTY,3,"B",+LRCS(LRCSN))) S LRMAX1=+$P(^LAB(60,LRTY,3,$O(^LAB(60,LRTY,3,"B",+LRCS(LRCSN),0)),0),U,7)
 I 'LRMAX2,LRMAX1,$D(TT(LRTY,LRSPEC)) I TT(LRTY,LRSPEC)'<LRMAX1 D EN1^LRORDD K LRMAX1 I %'["Y" K LRTEST(LRTSTN) S LRTSTN=LRTSTN-1 G L2
W18B F I=0:0 S I=$O(T(LRTY,I)) Q:I=""  Q:LRSPEC=T(LRTY,I)
 I '$D(LRSAME) S (LRSAMP(LRTSTN),LRSAMP)=+LRCS(LRCSN),LRCSP=+$O(^LAB(60,+LRTEST(LRTSTN),3,"B",+LRSAMP,0))
 I $D(LRSAME) K LRCS S LRCSN=1,LRSAMP=+$P(LRSAME,U,2),LRSAMP(LRTSTN)=LRSAMP,LRCS(LRCSN)=LRSAMP,LRCSP=+$O(^LAB(60,+LRTEST(LRTSTN),3,"B",+LRSAMP,0))
 I $D(LRADDTST) N I,GOT D
 . N LRODT,LRSN S (GOT,LRODT)=0 F  S LRODT=$O(^LRO(69,"C",+LRADDTST,LRODT)) Q:LRODT<1  D  Q:GOT
 .. S LRSN=0 F  S LRSN=$O(^LRO(69,"C",+LRADDTST,LRODT,LRSN)) Q:LRSN<1  D  Q:GOT
 ... I $P($G(^LRO(69,LRODT,1,LRSN,0)),"^",3)=+LRSAMP,$D(^LRO(69,LRODT,1,LRSN,2,"B",+LRTEST(LRTSTN))) S I=$O(^(+LRTEST(LRTSTN),0)) I I,$D(^LRO(69,LRODT,1,LRSN,2,I,0)),'$P(^(0),"^",11) D
 .... W !!,$C(7),"<<DUPLICATE TEST NOT ALLOWED>>",!?5,$P(^LAB(60,+LRTEST(LRTSTN),0),"^")_" has already been requested on this order.",!! K LRTEST(LRTSTN) S LRTSTN=LRTSTN-1,GOT=1 Q
 I $D(LRADDTST),$G(GOT) G L2
 I '$D(LRLABKY) S DIC="^LAB(60,",DA=+LRTEST(LRTSTN),DR=6 D EN^DIQ S DIC="WARD REMARKS: " S DR=0 F  S DR=$O(^LAB(60,DA,3,LRCSP,1,DR)) Q:DR'>0  W !,"  ",DIC,^(DR,0) S DIC=""
 G ^LROW1A
