NURSAPE0 ;HIRMFO/RM/JH-POSITION CONTROL/EXPERIENCE UTILITY ;5/1/96
 ;;4.0;NURSING SERVICE;;Apr 25, 1997
EN1 ; ENTRY FROM NURSUT1 AND APE XREF IN 211.82,.03
 Q:'$D(NUR("PE"))  S NUR("PE",0)=$S($D(^NURSF(211.8,DA(1),1,DA,0)):^(0),1:""),NUR("PE",200)=$P(NUR("PE",0),U,2),NUR("PE",210)=$O(^NURSF(210,"B",+NUR("PE",200),"")) G Q1:NUR("PE",210)'>0
 S NUR("PE",211.8)=$S($D(^NURSF(211.8,DA(1),0)):^(0),1:"") G Q1:+NUR("PE",211.8)'>0
 S NUR("PE",211.4)=$O(^NURSF(211.4,"B",+NUR("PE",211.8),"")) G Q1:NUR("PE",211.4)'>0
 S NUR("PE",211.4,1)=$S($D(^NURSF(211.4,NUR("PE",211.4),1)):$P(^(1),U,3),1:"") G Q1:NUR("PE",211.4)="" S NUR("PE",211.5)=$S($D(^NURSF(211.5,+NUR("PE",211.4,1),0)):$P(^(0),U),1:"") G Q1:NUR("PE",211.5)=""
 S NUR("PE",44)=$S($D(^SC(+NUR("PE",211.8),0)):$P($P(^(0),U),"NUR ",$P(^(0),U)?1"NUR ".E+1),1:""),NUR("PE","DA")=DA(1)_";"_DA
 S:'$D(^NURSF(210,NUR("PE",210),20,0)) ^(0)="^210.13IA^^" S NUR("PE",210.13)=$O(^NURSF(210,NUR("PE",210),20,"APE",DA(1)_";"_DA,"")) I NUR("PE",210.13)'>0 D ADEXP G Q1:NUR("PE",210.13)'>0
 S NUR("PE",210.13,0)=$S($D(^NURSF(210,NUR("PE",210),20,NUR("PE",210.13),0)):^(0),1:"")
 D UPSP:+NUR("PE")=.03,UPSD:+NUR("PE")=.01,UPED:+NUR("PE")=3
Q1 K NUR("PE")
 Q
ADEXP ; ADD EXPERIENCE ENTRY
 I $P(NUR("PE",0),U,6)'<DT!'$P(NUR("PE",0),U,6) S NUR("PE",210.13)=0 Q
 N DA,X
 S DA(1)=NUR("PE",210),NUR("PE",210.13,"Z")=$P(^NURSF(210,DA(1),20,0),U,3,4) F DA=$P(NUR("PE",210.13,"Z"),U)+1:1 L +^NURSF(210,DA(1),20,DA):0 Q:$T&'$D(^(DA,0))
 S NUR("PE",210.13,0)=NUR("PE",211.5)_U_$P(NUR("PE",0),U,3)_U_NUR("PE","DA")_U_NUR("PE",44)_U_$P(NUR("PE",0),U)_U_$P(NUR("PE",0),U,6)
 S ^NURSF(210,DA(1),20,DA,0)=NUR("PE",210.13,0),NUR("PE","P")=1
 F NUR("PE","X")=.01,1,4,3,2.1,2.5 S X=$P(NUR("PE",210.13,0),U,NUR("PE","P")),NUR("PE","P")=NUR("PE","P")+1 F NUR("PE","Y")=0:0 S NUR("PE","Y")=$O(^DD(210.13,NUR("PE","X"),1,NUR("PE","Y"))) Q:NUR("PE","Y")'>0  X:$D(^(NUR("PE","Y"),1)) ^(1)
 S $P(^NURSF(210,DA(1),20,0),U,3,4)=$S(DA>$P(NUR("PE",210.13,"Z"),U):DA,1:$P(NUR("PE",210.13,"Z"),U))_U_($P(NUR("PE",210.13,"Z"),U,2)+1),NUR("PE",210.13)=DA
 L -^NURSF(210,DA(1),20,DA) Q
UPSP ; UPDATE SERVICE POSITION IN 210.13
 Q:$P(NUR("PE",210.13,0),U,2)=$P(NUR("PE",0),U,3)&$P(NUR("PE"),U,2)
 N DA,X
 S X=$S($P(NUR("PE"),U,2):$P(NUR("PE",0),U,3),1:""),DA(1)=NUR("PE",210),DA=NUR("PE",210.13),$P(^NURSF(210,DA(1),20,DA,0),U,2)=X
 S NUR("PE","X")=1 D IX1
 Q
UPSD ; UPDATE START DATE IN 210.13
 Q:$P(NUR("PE",210.13,0),U,5)=$P(NUR("PE",0),U)&$P(NUR("PE"),U,2)
 N DA,X
 S DA(1)=NUR("PE",210),DA=NUR("PE",210.13)
 I '$P(NUR("PE"),U,2) D KL Q
 S X=$P(NUR("PE",0),U),$P(^NURSF(210,DA(1),20,DA,0),U,5)=X
 S NUR("PE","X")=2.1 D IX1
 Q
UPED ; UPDATE END DATE IN 210.13
 Q:$P(NUR("PE",210.13,0),U,6)=$P(NUR("PE",0),U,6)&$P(NUR("PE"),U,2)
 N DA,X
 S DA(1)=NUR("PE",210),DA=NUR("PE",210.13),X=$S($P(NUR("PE"),U,2):$P(NUR("PE",0),U,6),1:""),$P(^NURSF(210,DA(1),20,DA,0),U,6)=X
 I X'<DT!(X="") S $P(NUR("PE"),U,2)=0 D KL Q
 S NUR("PE","X")=2.5 D IX1
 Q
IX1 ; XREF FIELD NUR("PE","X") IN 210.13
 F NUR("PE","Y")=0:0 S NUR("PE","Y")=$O(^DD(210.13,NUR("PE","X"),1,NUR("PE","Y"))) Q:NUR("PE","Y")'>0  X:$D(^(NUR("PE","Y"),2-$P(NUR("PE"),U,2))) ^(2-$P(NUR("PE"),U,2))
 Q
KL ; KILL 210.13 NODE OFF
 S NUR("PE",210.13,"Z")=$S($D(^NURSF(210,DA(1),20,0)):$P(^(0),U,3,4),1:"")
 S NUR("PE","P")=1 F NUR("PE","X")=.01,1,4,3,2.1,2.5 S X=$P(NUR("PE",210.13,0),U,NUR("PE","P")),NUR("PE","P")=NUR("PE","P")+1 D IX1
 K ^NURSF(210,DA(1),20,DA,0) S $P(^NURSF(210,DA(1),20,0),U,3,4)=$O(^NURSF(210,DA(1),20,DA),-1)_U_($P(NUR("PE",210.13,"Z"),U,2)-1)
 Q