ABPVTX2 ;PT 3 OF PVT INS CLAIM EXPORT PROGRAM [ 08/07/91  3:44 PM ]
 ;;2.0;FACILITY PVT-INS TRACKING;*0*;IHS-OKC/KJR;AUGUST 7, 1991
A0 W !,"NOT AN ENTRY POINT" Q
S4C S ZY=^AUTNINS(X,1) F I=1:1:5 S $P(EX2,"^",I+1)=$P(ZY,"^",I)
 Q
S4START S (R,RR,RRR,RCT)=0 K ^ABPVDATA
 U IO(0) W !!,"Generating Private Insurance Claim Export Records:  ",!
 S R=BRECNO
S4A S R=$O(^ABPVFAC(R)) G S4END:+R=0 S ZX=^ABPVFAC(R,0)
 S $P(EX1,"^",1)="ABP1",$P(EX1,"^",2)=$P(^DPT(+$P(ZX,"^",2),0),"^",1)
 S $P(EX1,"^",3)=$P(^AUTTLOC($P(^ABPVFAC(R,0),"^",4),0),"^",10)
 S $P(EX1,"^",4)=$P(ZX,"^",5),$P(EX1,"^",5)=$P(ZX,"^",3)
 S $P(EX1,"^",6)=$P(ZX,"^",6),$P(EX1,"^",7)=$P(ZX,"^",7)
 S $P(EX1,"^",8)=$P(ZX,"^",9),$P(EX1,"^",9)=$P(ZX,"^",10)
 S $P(EX1,"^",10)=$P(ZX,"^",11),$P(EX1,"^",11)=$P(ZX,"^",16)
 S $P(EX1,"^",12)=$P(ZX,"^",17)
 S X=$P(ZX,"^",8) F I=1:1:6 S $P(EX1,"^",I+12)=$P(^AUTNINS(X,0),"^",I)
 S $P(EX1,"^",19)=$P(^AUTNINS(X,0),"^",9)
S4B S ZY="",EX2="ABP2" I $D(^AUTNINS(X,1))=1 D S4C
 S $P(EX1,"^",20)=$P(ZX,"^",1),$P(EX1,"^",21)=DT,$P(EX1,"^",22)="P"
 S $P(EX1,"^",23)=$P(^DPT(+$P(ZX,"^",2),0),"^",9)
 S $P(^ABPVFAC(R,0),"^",14)="Y",$P(^ABPVFAC(R,0),"^",15)=DT
 K ^ABPVFAC("E","N",R)
 S ^ABPVFAC("E","Y",R)="",^ABPVFAC("F",DT,R)=""
 S RCT=RCT+1,^ABPVDATA(RCT)=EX1 S:RCT=1 (FDATE,EDATE)=$P(ZX,"^",11)
 S RCT=RCT+1,^ABPVDATA(RCT)=EX2
 S DA=$O(^ABMDBILL("B",$P(ZX,"^"),"")) I DA>0 S DIE="^ABMDBILL(",DR=".18////1" D ^DIE I $D(^ABMDBILL(DA,1)),$P(^(1),"^",7)>0 S DA=$P(^(1),"^",7),DIE="^ABMDTXST(",DR=".06////1" D ^DIE
 I RCT#4=0 U IO(0) W $J((RCT/2),8)
 I $P(ZX,"^",11)<FDATE S FDATE=$P(ZX,"^",11)
 I $P(ZX,"^",11)>EDATE S EDATE=$P(ZX,"^",11)
 S LRECNO=R G S4A
S4END D ^ABPVVAR I RCT=0 D  G ZEND
 .U IO(0) W !!,*7,?10,"NO RECORDS AVAILABLE FOR EXPORT  -- "
 .W "JOB CANCELLED"
 S ^ABPVDATA(0)=$P(^AUTTLOC(ABPV("SITE"),0),"^",10)_"^"_$P(^DIC(4,ABPV("SITE"),0),"^",1)_"^"_DT_"^"_FDATE_"^"_EDATE_"^"_LRECNO_"^"_(RCT/2)
 U IO(0) W ! I $D(A("PRINT",10)) U IO W @A("PRINT",10)
 S XX=^ABPVDATA(0)
S5START U IO W @IOF D HEADER W @IOF X ^%ZIS("C") D ^%AUCLS,HEADER
WRITETP S AUGL="ABPVDATA",AUTLE="3P CLAIM EXPORT -"
 D ^AUGSAVE I AUFLG G JOBABEND
 K ABPVMESS S ABPVMESS="NORMAL END OF JOB"
 S ABPVMESS(2)="... Press any key to continue ..." D PAUSE^ABPVZMM
 S DIE="^ABPVTXST(",XN=$P(^ABPVTXST(0),"^",4)+1
 I $D(^ABPVTXST("B",DUZ(2))) G SETZERA
 S $P(^ABPVTXST(0),"^",3)=DUZ(2),^ABPVTXST(DUZ(2),0)=DUZ(2)
 S $P(^ABPVTXST(0),"^",4)=XN,^ABPVTXST("B",DUZ(2),DUZ(2))=""
SETZERA S DA=DUZ(2),DR="1///"_DT,DR(2,9002271.01)=".01///"_DT D ^DIE
 S NI=$P(^ABPVTXST(DUZ(2),1,0),"^",3)
 S ^ABPVTXST(DUZ(2),1,NI,0)=DT_"^"_FDATE_"^"_EDATE_"^"_LRECNO_"^"_(RCT/2)_"^^^^^Y"
 G ZEND
JOBABEND W *7,!!,?25,"ABNORMAL END OF PVT INS EXPORT"
 I $D(AUFLG(1)) W !!?(40-($L(AUFLG(1))/2)),AUFLG(1),!! K AUFLG
 W !,"ENTER <RETURN> TO CONTINUE" R X:DTIME
ZEND I $D(IO(0))=1 I $D(IO)=1 I IO'=IO(0) U IO W @IOF X ^%ZIS("C")
ZENDA K BRECNO,EDATE,EX1,FDATE,I,PGNO,R,RCT,RR,RRR,RSAVE,X,Y,ZCLAMT,ZCLCT
 K ZINSCO,ZINSERR,ZX,ZY,ABPV,LEXDATE,ZNODE,ABPV("HD")
 L
 Q
HEADER F I=1:1:70 W "*"
 W !,"*",?12,"PRIVATE INSURANCE BILLING CLAIM EXPORT REPORT",?69,"*",!
 S X="FOR "_$P(^DIC(4,DUZ(2),0),"^",1) W "*",?70-$L(X)/2,X,?69,"*",!
 S Y=DT X ^DD("DD") W "*",?70-$L(Y)/2,Y,?69,"*",! F I=1:1:70 W "*"
 W !!?10,"FACILITY CODE = ",?40,$P(XX,"^",1)
 W !,?10,"DATE EXPORT CREATED = " S Y=$P(XX,"^",3) X ^DD("DD") W ?40,Y
 W !,?10,"BEGINNING CLAIM DATE = " S Y=$P(XX,"^",4) X ^DD("DD") W ?40,Y
 W !,?10,"ENDING CLAIM DATE = " S Y=$P(XX,"^",5) X ^DD("DD") W ?40,Y
 W !,?10,"NUMBER OF CLAIM RECORDS = ",?40,$P(XX,"^",7),!!
 F I=1:1:70 W "*"
 Q
