BGP0PDL1 ; IHS/CMI/LAB - print ind 1 01 Jul 2009 8:03 PM ;
 ;;10.0;IHS CLINICAL REPORTING;**1**;JUN 18, 2010
 ;
 ;
I1 ;EP
 D:'$G(BGPSUMON) H1
I1A1 ;001.A, 001.B, 001.C
 I BGPINDT'="E" F BGPPC1="1.1","1.2","1.3" D PI
 I BGPINDT="E" S BGPPC1="1.4" D PI
 D I1AGE^BGP0PDL9
 Q
IREG ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:$P(^BGPINDT(BGPIC,0),U,13) S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IASCRN ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:4 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 D ^BGP0PDLL
 Q
I8 ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:1 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
I9 ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:1 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 D ^BGP0PD19
 Q
I12 ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:7 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
I13 ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:3 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 D I1AGE^BGP0PD19
 Q
I14 ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:3 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
I007 ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:7 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
I008 ;
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:5 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IB ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:6 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IH ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:6 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 D I1AGE^BGP0PDLA
 Q
I91 ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:3 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 D I1AGE^BGP0PDLB
 Q
PHYACT ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:$P(^BGPINDT(BGPIC,0),U,13) S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 D I1AGE^BGP0PDPA
 Q
IG ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:2 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IF ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:3 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IA ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:18 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
ICRSAMM ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,0),U,2) F BGPORDP1=1:1:2 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
I031A ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:13 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
ID ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:9 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
I0302 ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:9 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
I0303 ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:3 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
I0302A ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:4 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IHEDBBH ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:6 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IHEDPBH ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:6 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IHEDCHM ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:6 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IE2 ;
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:1 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IE1 ;
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:1 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IK ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:6 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IOMW ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:2 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IRAO ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:2 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
II ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:2 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IL ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:2 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IAS ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:8 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IMS ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:2 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IC2 ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:2 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IAST1 ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:12 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IRAA ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:2 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IRAR ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:2 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IHEDCWP ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:2 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
IHEDURI ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:2 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
I28 ;EP
 D:'$G(BGPSUMON) H1
 S BGPORDP=$P(^BGPINDT(BGPIC,12),U,6) F BGPORDP1=1:1:2 S BGPPC1=BGPORDP_"."_BGPORDP1 D PI
 Q
PI ;EP
 S BGPDENP=0
 S BGPPC2=0 F  S BGPPC2=$O(^BGPINDTC("ABC",BGPPC1,BGPPC2)) Q:BGPPC2=""  S BGPPC=$O(^BGPINDTC("ABC",BGPPC1,BGPPC2,0)) D PI1
 Q
PI1 ;EP
 K BGPEXCT,BGPSDP,BGPSDPN,BGPSDPO,BGP0SDPD
 Q:'$$CHECK^BGP0DP1C(BGPPC)
 I $P(^BGPINDTC(BGPPC,0),U,4)="E-2.B.3" S X=" " D S(X,1,1) D PI1^BGP0PDL2 Q  ;count only
 S BGPDF=$P(^BGPINDTC(BGPPC,0),U,8)
 ;get denominator value of measure
 S BGPNP=$P(^DD(90377.03,BGPDF,0),U,4),N=$P(BGPNP,";"),P=$P(BGPNP,";",2)
 S BGPCYD=$$V^BGP0DP1C(1,BGPRPT,N,P,1) I $G(BGPAREAA) D SETEXA^BGP0DP1C(1,N,P)
 S BGPPRD=$$V^BGP0DP1C(2,BGPRPT,N,P,1) I $G(BGPAREAA) D SETEXA^BGP0DP1C(2,N,P)
 S BGPBLD=$$V^BGP0DP1C(3,BGPRPT,N,P,1) I $G(BGPAREAA) D SETEXA^BGP0DP1C(3,N,P)
 ;write out denominator
 ;write out denom
 I BGPRTYPE=1,$P(^BGPINDTC(BGPPC,0),U,4)="MS.A.9" S BGPDENP=0
 I BGPRTYPE=1,$P(^BGPINDTC(BGPPC,0),U,4)="DM.2.1" S BGPDENP=0
 I BGPRTYPE=1,$P(^BGPINDTC(BGPPC,0),U,4)="E-2.A.1" S BGPDENP=0
 I BGPRTYPE=9,$P(^BGPINDTC(BGPPC,0),U,4)="027.C.36" S BGPDENP=0
 I BGPRTYPE=7,$P(^BGPINDTC(BGPPC,0),U,4)="028.C.4" S BGPDENP=0
 I 'BGPDENP S Y=" " D:$E($P($G(^BGPINDTC(BGPPC,12)),U,4),1,4)'=48.2!($P($G(^BGPINDTC(BGPPC,12)),U,4)="48.2.1")!($P($G(^BGPINDTC(BGPPC,12)),U,4)="43.22.1") S(Y,1,1) D
 .I $P($G(^BGPINDTC(BGPPC,12)),U,14) Q
 .I BGPRTYPE=1,$P($G(^BGPINDTC(BGPPC,20)),U,4)]"" S Y=$P(^BGPINDTC(BGPPC,20),U,4)_" "_$P(^BGPINDTC(BGPPC,20),U,5)_" "_$P(^BGPINDTC(BGPPC,20),U,6) D S(Y,1,1) I 1
 .E  D
 ..I BGPRTYPE=4,BGP0RPTH="P" D S($P(^DIBT(BGPSEAT,0),U)_" Population",1,1)
 ..S Y=$P(^BGPINDTC(BGPPC,0),U,17)_" "_$P(^BGPINDTC(BGPPC,0),U,18)_" "_$P(^BGPINDTC(BGPPC,0),U,21) D S(Y,1,1)
 .S Y=BGPCYD_"^^"_BGPPRD_"^^^"_BGPBLD D S(Y,,2)
 .S BGPDENP=1
 ;get numerator value of measure and calc %
 I $E($P(^BGPINDTC(BGPPC,0),U,4),1,2)="I." D
 .S BGPDF=$P(^BGPINDTC(BGPPC,0),U,8)
 .;get denominator value of measure
 .S BGPNP=$P(^DD(90377.03,BGPDF,0),U,4),N=$P(BGPNP,";"),P=$P(BGPNP,";",2)
 .S BGPCYD=$$V^BGP0DP1C(1,BGPRPT,N,P,1) I $G(BGPAREAA) D SETEXA^BGP0DP1C(1,N,P)
 .S BGPPRD=$$V^BGP0DP1C(2,BGPRPT,N,P,1) I $G(BGPAREAA) D SETEXA^BGP0DP1C(2,N,P)
 .S BGPBLD=$$V^BGP0DP1C(3,BGPRPT,N,P,1) I $G(BGPAREAA) D SETEXA^BGP0DP1C(3,N,P)
 S BGPNF=$P(^BGPINDTC(BGPPC,0),U,9)
 S BGPNP=$P(^DD(90377.03,BGPNF,0),U,4),N=$P(BGPNP,";"),P=$P(BGPNP,";",2)
 D SETN
 I BGPRTYPE=1,$P($G(^BGPINDTC(BGPPC,20)),U)]"" S X=$P(^BGPINDTC(BGPPC,20),U,1)_" "_$P(^BGPINDTC(BGPPC,20),U,2)_" "_$P(^BGPINDTC(BGPPC,20),U,3) D S(X,1,1) I 1
 E  D
 .I $P(^BGPINDTC(BGPPC,0),U,22) D S(" ",1,1)
 .S X=$P(^BGPINDTC(BGPPC,0),U,15)_" "_$P(^BGPINDTC(BGPPC,0),U,16)_" "_$P(^BGPINDTC(BGPPC,0),U,19) D S(X,1,1)
 D H2
 Q
SETN ;EP - set numerator fields
 S BGPIIDEL=1
 D SETN^BGP0DP1C
 Q
C(X,X2,X3) ;
 D COMMA^%DTC
 Q X
S(Y,F,P) ;set up array
 I '$G(F) S F=0
 S %=$P(^TMP($J,"BGPDEL",0),U)+F,$P(^TMP($J,"BGPDEL",0),U)=%
 I '$D(^TMP($J,"BGPDEL",%)) S ^TMP($J,"BGPDEL",%)=""
 S $P(^TMP($J,"BGPDEL",%),U,P)=Y
 Q
CALC(N,O) ;
 NEW Z
 S Z=N-O,Z=$FN(Z,"+,",1)
 Q Z
H3 ;EP
 S X="Age Distribution" D S(X,1,1) S X=" " D S(X,1,1)
 S Y="<15" D S(Y,1,2)
 S Y="15-19" D S(Y,,3)
 S Y="20-24" D S(Y,,4)
 S Y="25-34" D S(Y,,5)
 S Y="35-44" D S(Y,,6)
 S Y="45-54" D S(Y,,7)
 S Y="55-64" D S(Y,,8)
 S Y=">64 yrs" D S(Y,,9)
 Q
SB(X) ;EP - Strip leading and trailing blanks from X.
 NEW %
 X ^DD("FUNC",$O(^DD("FUNC","B","STRIPBLANKS",0)),1)
 Q X
H2 ;EP
 S BGPX="",BGPX=BGPCYN,$P(BGPX,U,2)=$$SB($J(BGPCYP,5,1)),$P(BGPX,U,3)=BGPPRN,$P(BGPX,U,4)=$$SB($J(BGPPRP,5,1)),$P(BGPX,U,5)=$$SB($J($$CALC(BGPCYP,BGPPRP),6)),$P(BGPX,U,6)=BGPBLN,$P(BGPX,U,7)=$$SB($J(BGPBLP,5,1))
 S $P(BGPX,U,8)=$$SB($J($$CALC(BGPCYP,BGPBLP),6))
 D S(BGPX,,2)
 Q
H1 ;EP
 S Y="" D S(Y,1,1)
 S Y="REPORT" D S(Y,1,2)
 S Y="%" D S(Y,,3)
 S Y="PREV YR" D S(Y,,4)
 S Y="%" D S(Y,,5)
 S Y="CHG from" D S(Y,,6)
 S Y="BASE" D S(Y,,7)
 S Y="%" D S(Y,,8)
 S Y="CHG from" D S(Y,,9)
 S Y="PERIOD" D S(Y,1,2)
 S Y="PERIOD" D S(Y,,4)
 S Y="PREV YR %" D S(Y,,6)
 S Y="PERIOD" D S(Y,,7)
 S Y="BASE %" D S(Y,,9)
 Q
