LAPFICH ; IHS/DIR/AAB - MICROFICH PATH REPORTS 09:59 ; [ 7/20/90 ]
 ;;5.2;LA;**1003**;SEP 01, 1998
 ;
 ;;5.2;AUTOMATED LAB INSTRUMENTS;**42**;Sep 27, 1994
EN ;
 D:'$D(LRPARAM) ^LRPARAM D END W !,"MICROFICH PATH REPORTS"
 W ! S DIC("A")="Select ANATOMIC PATHOLOGY SECTION: ",DIC=68,DIC(0)="AEQM",DIC("S")="I ""SPCYEM""[$P(^(0),U,2)" D ^DIC K DIC G:Y<1 END
 S X=$P(Y,U,2) D ^LRUTL G:Y<0 END
 S %DT=("A")="Select Accession YEAR: ",%DT="AEPQ" D ^%DT G:Y<1 END S LRY=$E(Y,1,3)
A R !,"Start with accession #: ",X:DTIME G:X[U!(X="") END I X'?1N.N W $C(7),!,"Enter a number." G A
 S LR("A")=X-1
B R !,"Go    to   accession #: ",X:DTIME G:X[U!(X="") END I X'?1N.N W $C(7),!,"Enter a number." G B
 S LR("B")=X
 W !!,"Print SNOMED &/or ICD codes on final report " S %=2 D YN^LRU Q:%<1  I %=1 S (LRS(99),S(99))=1
 ;D EN^LRSPRPT2 ;*** USE THIS LINE FOR VER 5.0  COMMENT OUT THE LINE ABOVE
 S ZTRTN="QUE^LRAPFICH" D BEG^LRUTL G:POP!($D(ZTSK)) END
QUE U IO S S(5)="W",(LR("Q"),LR,LR("A"),LR(1),LR(2),LR(3))=0 D L^LRU,XR^LRU,EN2^LRUA,SET^LRUA S LRQ=0,LRA=1,LRQ(1)=^DD("SITE") I $D(DUZ(2)) S LRQ(1)=$S($D(^DIC(4,+DUZ(2),0)):$P(^(0),U),1:LRQ(1))
 K LR("%1") S $P(LR("%1"),"=",IOM-1)="="
 F LRAN=LR("A"):0 S LRAN=$O(^LR(LRXREF,LRY,LRAN)) Q:'LRAN!(LRAN>LR("B"))!(LR("Q"))  S LRDFN=$O(^LR(LRXREF,LRY,LRAN,0)),LRI=$O(^(LRDFN,0)) D EN^LRSPRPT Q:LR("Q")
 D END^LRUTL,END Q
 ;
END D V^LRU K LRS(99),LR("%1"),S(99),LRPMD,LRRMD Q
