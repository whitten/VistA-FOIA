%XUCI ;SFISC/STAFF - SWAP UCIs VAX/DSM ;1/23/96  09:28 [ 04/02/2003   8:29 AM ]
 ;;8.0;KERNEL;**1002,1003,1004,1005,1007**;APR 1, 2003
 ;;8.0;KERNEL;**13**;Jul 10, 1995
 ;FOR VAX-DSM
1 R !,"What UCI: ",%UCI:$S($D(DTIME):DTIME,1:10),"  " Q:%UCI=""!(%UCI["^")  G 2
 ;
2 ;
 I %UCI="PROD"!(%UCI="MGR") S %UCI=^%ZOSF(%UCI)
 S X=%UCI X ^%ZOSF("UCICHECK") G ERR:0[Y
 X ^%ZOSF("PROGMODE") I Y W:'($D(XUSLNT)!$D(ZTQUEUED)) !,*7,"NO SWITCHING UCI'S IN PROGRAMMER MODE!",! S Y=0 Q
V D SWAP
U I '($D(XUSLNT)!$D(ZTQUEUED)) W *7,!,"YOU'RE IN UCI: ",$ZU(0),!
 S $ZT="^%ZTER",%=$D(^%ZOSF("OS"))
K K %,%UCI S Y=1 Q
 ;
SWAP ;
 X ^%ZOSF("PROGMODE") I 'Y S X=$S(X[",":$ZC(%SETUCI,$P(X,","),$P(X,",",2)),1:$ZC(%SETUCI,$P(X,","))),X=$ZC(%PGMSET),X=$ZC(%SECMAP)
 Q
 ;
ENT G 2:$D(%UCI)#2,1
 ;
GO ;
 D 2 Q:0[Y  S X=PGM I PGM'?1"%".E X ^%ZOSF("TEST") I '$T W !?9,"'"_X_"' DOES NOT EXIST IN "_%UCI,! HALT
 S X=$&ZLIB.%SETSYM("DHCP$UCI_CHANGE",1)
 K ^XUTL("XQ",$J),^UTILITY($J) G @(U_PGM)
 ;
DO S %UCI=$P(XQZ,"[",2,9),PGM=$P(XQZ,"[",1),%UCI=$E(%UCI,1,$L(%UCI)-1)
 I %UCI="PROD"!(%UCI="MGR") S %UCI=^%ZOSF(%UCI)
 E  S X=%UCI X ^%ZOSF("UCICHECK") G ERR:0[Y
 X ^%ZOSF("UCI") D SAV,D S %UCI=Y D 2,RES Q
D N Y,%XUCI D 2 Q:0[Y  G @PGM Q
SAV S %XUCI="" F %="DUZ","DUZ(0)","DT","DTIME","IO","IO(0)","IOF","IOM","IOST","IOST(0)" S %XUCI=%XUCI_$S($D(@%)#2:@%,1:"")_"^"
 Q
RES F %=1:1:10 S @($P("DUZ^DUZ(0)^DT^DTIME^IO^IO(0)^IOF^IOM^IOST^IOST(0)","^",%))=$P(%XUCI,"^",%)
 Q
 ;
ERR W !?9,"'"_X_"' IS AN INVALID UCI!",!
