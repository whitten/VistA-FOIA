LRAPQAT1 ; IHS/DIR/AAB - QA CODE SEARCH 2/12/98 14:31 ; [ 2/12/98 10:21 AM ]
 ;;5.2T9;LR;**1006,1018**;Nov 17, 2004
 ;;5.2;LAB SERVICE;**201**;Sep 27, 1994
 D EN^LRUA S (LR("W"),LRS(5),LRQ(9),LRQ(3))=1,LRSDT=9999999-LRSDT,LRP=0
 F LRB=0:0 S LRP=$O(^TMP("LRAP",$J,LRP)) Q:LRP=""!(LR("Q"))  F LRDFN=0:0 S LRDFN=$O(^TMP("LRAP",$J,LRP,LRDFN)) Q:'LRDFN!(LR("Q"))  S X=^(LRDFN) D L
 Q
L ;S DFN=$P(X,"^",2),LRQ=0,SEX=$P(X,"^",4),SSN=$P(X,"^"),Y=$P(X,"^",3) S DOB=$$FMTE^XLFDT(Y)
 S DFN=$P(X,"^",2),LRQ=0,SEX=$P(X,"^",4),HRCN=$P(X,"^"),Y=$P(X,"^",3) S DOB=$$FMTE^XLFDT(Y)  ;IHS/ANMC/CLS 11/1/95
 G:'$D(^LR(LRDFN,"SP"))&('$D(^LR(LRDFN,"CY")))&('$D(^LR(LRDFN,"EM"))) AU
 D ^LRAPT1 Q:LR("Q")
AU I $D(^LR(LRDFN,"AU")),+^("AU") D ^LRAPT2
 ;Q:'DFN!(LR("Q"))  D INP^VADPT Q:VAIN(1)']""  D A
 ;Q:'DFN!(LR("Q"))  D INP^BLRDPT Q:VAIN(1)']""  D A  ;IHS/ANMC/CLS 11/1/95
 ;----- BEGIN IHS MODIFICATIONS LR*5.2*1018
 Q:'DFN!(LR("Q"))  D @$S($$ISPIMS^BLRUTIL:"INP^VADPT",1:"INP^BLRDPT") Q:VAIN(1)']""  D A  ;IHS/ANMC/CLS 11/1/95
 ;----- END IHS MODIFICATIONS
 Q
A S LRPTF=VAIN(10)
 S LRADM=$P(VAIN(7),U,2)
 S LRWARD=$P(VAIN(4),U,2)
 S LRTS=$P(VAIN(3),U,2)
 K VAIN
 W !,"Adm: ",$P(LRADM,"@"),?35,LRWARD
 W !,?12,"Specialty: ",$P(LRADM,"@"),?35,LRTS
 Q:'LRPTF
 I $D(^DGPT(LRPTF,70)),$P(^(70),"^",10) S W=^(70) F X=10,11,16:1:24 I $P(W,"^",X) S LRF($P(W,"^",X))=""
 F Y=0:0 S Y=$O(^DGPT(LRPTF,"M",Y)) Q:'Y  S W=^(Y,0) F X=5:1:9,11:1:15 I $P(W,"^",X) S LRF($P(W,"^",X))=""
 I $D(^DGPT(LRPTF,"401P")) S W=^("401P") F X=1:1:5 I $P(W,"^",X) S LRC($P(W,"^",X))=""
 F Y=0:0 S Y=$O(^DGPT(LRPTF,"P",Y)) Q:'Y  S W=^(Y,0) F X=5:1:9 I $P(W,"^",X) S LRC($P(W,"^",X))=""
 F Y=0:0 S Y=$O(^DGPT(LRPTF,"S",Y)) Q:'Y  S W=^(Y,0) F X=8:1:12 I $P(W,"^",X) S LRC($P(W,"^",X))=""
 F C=0:0 S C=$O(LRF(C)) Q:'C  I $D(^ICD9(C)) S W=^(C,0) W !,$P(W,"^"),?10,$P(W,"^",3)
 F C=0:0 S C=$O(LRC(C)) Q:'C  I $D(^ICD0(C)) S W=^(C,0) W !,$P(W,"^"),?10,$P(W,"^",4)
 Q
