DGPTSUD1 ;ALB/AS - Look for the same DRG in consecutive RAM movements ; Feb 23 87
 ;;5.3;Registration;;Aug 13, 1993
 ;
 ;called from ONE+2^DGPTSUDO
 F I=0:0 S I=$O(^DGPT(PTF,"M",I)) Q:I'>0  I $D(^DGPT(PTF,"M",I,"P")) S DGNODE=^("P"),DGSUDO(+$P(DGNODE,"^",3))=I_"^"_$P(DGNODE,"^",1)
 G Q:'$D(DGSUDO) S (DGSUNX,DGSUPR)=$O(DGSUDO(0)) F %=0:0 S DGSUNX=$O(DGSUDO(DGSUPR)) Q:DGSUNX'>0  D SAME
Q K DGSUPR,DGSUNX,DGNODE,DGSUDO,DG1,DG2,DGMV1,DGMV2,DGSUB,DGSUR,DGSURDT,%,I,J,X1,X2 Q
SAME I $P(DGSUDO(DGSUPR),"^",2)']""!($P(DGSUDO(DGSUPR),"^",2)'=$P(DGSUDO(DGSUNX),"^",2)) S DGSUPR=DGSUNX Q
 S DG1=+$P(DGSUDO(DGSUPR),"^",1),DG2=+$P(DGSUDO(DGSUNX),"^",1),DGMV1=$S($D(^DGPT(PTF,"M",+DG1,"P")):^("P"),1:""),DGMV2=$S($D(^DGPT(PTF,"M",+DG2,"P")):^("P"),1:"")
 ;Fiscal year 89 discharges are checked for a surgery performed while on Surgery Service if the DRG is a surgical DRG
 G 88:'$P(^ICD($P(DGSUDO(DGSUNX),"^",2),0),"^",6)
 I $D(^DGPT(PTF,70)),$P(^DGPT(PTF,70),"^",1)<2881000 G 88
 I $P(DGMV1,"^",2)'="S"&($P(DGMV2,"^",2)'="S") G 88
 I $P(DGMV1,"^",2)="S" S I=DGADM F J=0:0 S J=$O(DGSUDO(J)) Q:J=DGSUPR!(J'>0)  S I=J
 S X1=$S($P(DGMV1,"^",2)="S":I,1:DGSUPR),X2=$S($P(DGMV1,"^",2)="S":DGSUPR,1:DGSUNX),X1=$P(X1,".",1),X2=$P(X2,".",1)_.99,DGSUR=0
 F I=0:0 S I=$O(^DGPT(PTF,"S",I)) Q:I'>0  S DGSURDT=$S($D(^DGPT(PTF,"S",I,0)):+^(0),1:9999999) I X1<DGSURDT&(DGSURDT<X2) S DGSUR=1 Q
 I DGSUR S DGSUB=$S($P(DGMV1,"^",2)="S":DG1,1:DG2) G KILL
88 S DGSUB=$S($P(DGMV1,"^",4)>$P(DGMV2,"^",4):DG1,1:DG2)
KILL S $P(^DGPT(PTF,"M",DGSUB,"P"),"^",4)=$P(DGMV1,"^",4)+$P(DGMV2,"^",4)
 I DGSUB=DG1 S $P(^DGPT(PTF,"M",DG1,"P"),"^",6)=$P(DGMV2,"^",6) K ^DGPT(PTF,"M",DG2,"P"),DGSUDO(DGSUNX) Q
 K ^DGPT(PTF,"M",DG1,"P"),DGSUDO(DGSUPR) S DGSUPR=DGSUNX Q
