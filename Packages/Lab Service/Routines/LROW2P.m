LROW2P ;VA/SLC/TGA - PRINTS WARD COLLECT ORDER IN LAB ; 22 Sep 2010  2:51 PM
 ;;5.2;LAB SERVICE;**1027,1028**;NOV 01, 1997;Build 46
 ;;5.2;LAB SERVICE;**100,121,338**;NOV 01, 1997
 ;S ZTRTN="ENT^LROW2P",ZTIO=ION,ZTDTH=$H,ZTSAVE("LRSN")="",ZTSAVE("LRODT")="",ZTDESC="PRINTS WARD COLLECT ORDER" I ION]"" D ^%ZTLOAD
 S ZTRTN="ENT^LROW2P",ZTIO=ION,ZTDTH=$H,ZTSAVE("LRSN")="",ZTSAVE("LRODT")="",ZTDESC="PRINTS WARD/CLINIC COLLECT ORDER" I ION]"" D ^%ZTLOAD  ;IHS/OIRM TUC/AAB 3/20/97
 K ZTSK,ZTRTN,ZTIO,ZTDTH,ZTSAVE,ZTDESC
 Q
ENT ;
 S U="^" S:$D(ZTQUEUED) ZTREQ="@"
ENT2 ;from LRORDST, LROW2
 Q:'$D(^LRO(69,LRODT,1,LRSN,0))
 N LRSAMP,GOT,I S GOT=0
 S I=0 F  S I=$O(^LRO(69,LRODT,1,LRSN,2,I)) Q:I<1  I $D(^(I,0)),'$P(^(0),"^",11) S GOT=1
 Q:'GOT
 S X=^LRO(69,LRODT,1,LRSN,0),LRCSS=$S($D(^(4,1)):^(1,0),1:0),LRDFN=$P(X,U),(LRSAMP,LRCS)=$P(X,U,3),LRLWC=$P(X,U,4),LRDTO=$P(X,U,5),LRPR=$P(X,U,6),LRLLOC=$P(X,U,7),LRORDTIM=$P($P(X,U,8),".",2),LRDUZ=$P(X,U,2)
 S LRCSS=$S($D(^LAB(61,+LRCSS,0)):$P(^(0),U),1:""),LRCS=$S($D(^LAB(62,+LRCS,0)):^(0),1:"")
 S LRDPF=$P(^LR(LRDFN,0),U,2),DFN=$P(^(0),U,3),X=^DIC(LRDPF,0,"GL")_DFN_",0)",PNM=$S($D(@X):$P(@X,U),1:"UNKNOWN"),SSN=$S($D(@X):$P(@X,U,9),1:"UNKNOWN") S X=^DIC(LRDPF,0,"GL")_DFN_",.101)" S LRBED=$S($D(@X):^(.101),1:"")
 D:SSN SSN^LRU
 W !!,?23,"LABORATORY: ",^DD("SITE")  ;IHS/ANMC/CLS 08/18/96
 ;W !!,?23,$S(LRLWC="SP":"Send Patient",LRLWC="WC":"Ward Collect",LRLWC="I":"Immed Lab Collect ",1:"Lab Collect")," ORDER FOR " S Y=LRODT D DD^LRX W Y
 W !,?23,$S(LRLWC="SP":"Send Patient",LRLWC="WC":"Ward/Clinic Collect",LRLWC="I":"Immed Lab Collect ",1:"Lab Collect")," ORDER FOR " S Y=LRODT D DD^LRX W Y  ;IHS/ANMC/CLS 08/18/96 AAB/3/20/97
 W !,?23,"ORDER: ",$S($D(^LRO(69,LRODT,1,LRSN,.1)):^(.1),1:""),?40,"LOCATION: ",LRLLOC W:$L(LRBED) "    BED: ",LRBED
 D:+$P($G(^BLRSITE($G(DUZ(2)),0)),U,10) ENTRYAUD^BLRUTIL("ENT2^LROW2P 4.0","VADM")
 I $G(DOB)="" D PT^LRX       ; IHS/OIT/MKK - LR*5.2*1028
 ; W !,PNM,?40,SSN,!,"ENTERED BY: " S X=LRDUZ D DUZ^LRX K LRDUZ W LRUSNM,?40 S Y=LRDTO D DD^LRX W Y
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1027 -- Add in HRCN, DOB & SEX
 W !!,$E(PNM,1,33),?35,$G(HRCN)
 W ?46,"DOB:" W:$G(DOB)'="" $$FMTE^XLFDT(DOB,"5DZ")
 W ?65,"SEX:",$G(SEX)
 W !,"ENTERED BY: " S X=LRDUZ D DUZ^LRX K LRDUZ W LRUSNM,?40 S Y=LRDTO D DD^LRX W Y
 ; ----- END IHS/OIT/MKK - LR*5.2*1027
 I $L(LRPR) W !,"PRACTITIONER: " W $S($D(^VA(200,LRPR,0)):$P(^(0),"^"),1:"UNKNOWN")
 I LRORDTIM S Y=LRODT_"."_LRORDTIM D DD^LRX W ?32,$S(LRLWC="I":"REQUESTED ",1:" Est.")_" Collect Time: ",Y
 W !,"Collection sample: ",$P(LRCS,U),"  ",$P(LRCS,U,3) W:$P(LRCS,U)'[LRCSS ?32," Site/Specimen: ",LRCSS
 S T=0 F  S T=$O(^LRO(69,LRODT,1,LRSN,2,T)) Q:T<1  S LRTP=^(T,0) D:'$P(LRTP,"^",11) TEST
 ;W !!!,"TIME OF COLLECTION:__________"
 W !!!,"DATE/TIME OF COLLECTION:__________"  ;IHS/ANMC/CLS 08/18/96
 I $G(LRLWC)="WC" W !!,"COLLECTED BY:_____________________________"  ;IHS/ANMC/CLS 08/18/96
 I $D(^LRO(69,LRODT,1,LRSN,6,0)) W !!,"Order comment: " F I=0:0 S I=$O(^LRO(69,LRODT,1,LRSN,6,I)) Q:I<1  W !?2,^(I,0)
 I $G(LRLWC)="SP" W !!,"** PLEASE BRING THIS WITH YOU TO THE LAB **",!  ;IHS/ANMC/CLS 08/18/96
 W ! W:$E(IOST)="P" @IOF D ^%ZISC Q
TEST W !,"TEST/PROCEDURE: ",$P(^LAB(60,+LRTP,0),U) S LRUR=+$P(LRTP,U,2) W:LRUR ?48,$P(^LAB(62.05,LRUR,0),U)
 I $D(^LAB(60,+LRTP,3,"B",+LRSAMP)) S X=$O(^(+LRSAMP,0)) I X,$D(^LAB(60,+LRTP,3,X,1)) N I S I=0 D
 . W !,"Ward Instructions:"
 . F  S I=$O(^LAB(60,+LRTP,3,X,1,I)) Q:I<1  W !?2,^(I,0)
 I $O(^LRO(69,LRODT,1,LRSN,2,T,1,0)) W !,"Ward Comments:"
 S I=0 F  S I=$O(^LRO(69,LRODT,1,LRSN,2,T,1,I)) Q:I<1  W !?2,^(I,0)
 ;
 ;----- BEGIN IHS/OIT/MKK - LR*5.2*1027 -- PRINT OUT 'SIGN OR SYMPTOM' LAB POV
 S LRTPTR=$P($G(^LRO(69,LRODT,1,LRSN,2,T,0)),U)  ;TEST POINTER
 S:LRTPTR'="" LRTNAME=$P($G(^LAB(60,LRTPTR,0)),U)
 W !,"Sign or Symptom for test "_$G(LRTNAME)_" : ",$G(^LRO(69,LRODT,1,LRSN,2,T,9999999)),!!
 K LRTPTR
 ;----- END IHS/OIT/MKK - LR*5.2*1027
 Q
