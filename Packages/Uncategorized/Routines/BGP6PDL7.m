BGP6PDL7 ; IHS/CMI/LAB - print ind 1 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;
 ;
I023 ;EP
 D H1^BGP6PDL1
 I BGPRTYPE=4,BGPINDT="C" D  Q
 .S BGPDENP=0 S BGPPC=$O(^BGPINDSC("OR","21.8.1E",0)) D PI1^BGP6PDL1 S BGPPC=$O(^BGPINDSC("OR","21.8.3E",0)) D PI1^BGP6PDL1
 .S BGPDENP=0 S BGPPC=$O(^BGPINDSC("OR","21.9.1E",0)) D PI1^BGP6PDL1 S BGPPC=$O(^BGPINDSC("OR","21.9.3E",0)) D PI1^BGP6PDL1
 .S BGPDENP=0 S BGPPC=$O(^BGPINDSC("OR","21.10.1E",0)) D PI1^BGP6PDL1 S BGPPC=$O(^BGPINDSC("OR","21.10.3E",0)) D PI1^BGP6PDL1
 .S BGPDENP=0 S BGPPC=$O(^BGPINDSC("OR","21.8.1F",0)) D PI1^BGP6PDL1 S BGPPC=$O(^BGPINDSC("OR","21.8.3F",0)) D PI1^BGP6PDL1
 .S BGPDENP=0 S BGPPC=$O(^BGPINDSC("OR","21.9.1F",0)) D PI1^BGP6PDL1 S BGPPC=$O(^BGPINDSC("OR","21.9.3F",0)) D PI1^BGP6PDL1
 .S BGPDENP=0 S BGPPC=$O(^BGPINDSC("OR","21.10.1F",0)) D PI1^BGP6PDL1 S BGPPC=$O(^BGPINDSC("OR","21.10.3F",0)) D PI1^BGP6PDL1
 I BGPRTYPE=4,BGPINDT="E" D  Q
 .S BGPDENP=0 S BGPPC=$O(^BGPINDSC("OR","21.8.1F",0)) D PI1^BGP6PDL1 S BGPPC=$O(^BGPINDSC("OR","21.8.2F",0)) D PI1^BGP6PDL1 S BGPPC=$O(^BGPINDSC("OR","21.8.3F",0)) D PI1^BGP6PDL1
 .S BGPPC=$O(^BGPINDSC("OR","21.8.4F",0)) D PI1^BGP6PDL1 S BGPPC=$O(^BGPINDSC("OR","21.8.5F",0)) D PI1^BGP6PDL1
 .S BGPDENP=0 S BGPPC=$O(^BGPINDSC("OR","21.9.1F",0)) D PI1^BGP6PDL1 S BGPPC=$O(^BGPINDSC("OR","21.9.2F",0)) D PI1^BGP6PDL1 S BGPPC=$O(^BGPINDSC("OR","21.9.3F",0)) D PI1^BGP6PDL1
 .S BGPPC=$O(^BGPINDSC("OR","21.9.4F",0)) D PI1^BGP6PDL1 S BGPPC=$O(^BGPINDSC("OR","21.9.5F",0)) D PI1^BGP6PDL1
 .S BGPDENP=0 S BGPPC=$O(^BGPINDSC("OR","21.10.1F",0)) D PI1^BGP6PDL1 S BGPPC=$O(^BGPINDSC("OR","21.10.2F",0)) D PI1^BGP6PDL1 S BGPPC=$O(^BGPINDSC("OR","21.10.3F",0)) D PI1^BGP6PDL1
 .S BGPPC=$O(^BGPINDSC("OR","21.10.4F",0)) D PI1^BGP6PDL1 S BGPPC=$O(^BGPINDSC("OR","21.10.5F",0)) D PI1^BGP6PDL1
 F BGPPC1="21.1","21.2","21.3","21.4" D PI^BGP6PDL1
 F BGPPC1="21.5","21.6","21.7" D PI^BGP6PDL1
 ;F BGPPC1="21.4" D PI^BGP6PDL1
 D I1AGE
 Q
I1AGE ;EP
 Q:BGPRTYPE'=4
 I BGPINDT="W",BGPRTYPE=4 G FEM
 S BGPHD1="TOTAL ACTIVE CLINICAL POPULATION",BGPHD2="Total Active Clinical"
 S X=^BGPINDS(BGPIC,53,1,0) D S(X,1,1) S X=" " D S(X,1,1) D H3 S X=" " D S(X,1,1)
 K BGPDAC,BGPDAP,BGPDAB S (C,D)=0 F BGPX="BD","BG","BJ","BM","BP","BS" D I1AGE1,I1AGE2
 D I1AGEP
 S BGPHD1="MALE ACTIVE CLINICAL POPULATION",BGPHD2="MALE Active Clinical"
 S X=^BGPINDS(BGPIC,53,1,0) D S(X,1,1) D H3
 K BGPDAC,BGPDAP,BGPDAB S (D,C)=0 F BGPX="BE","BH","BK","BN","BQ","BT" D I1AGE1,I1AGE2
 D I1AGEP
FEM ;
 S BGPHD1="FEMALE ACTIVE CLINICAL POPULATION",BGPHD2="FEMALE Active Clinical"
 S X=^BGPINDS(BGPIC,53,1,0) D S(X,1,1) D H3
 K BGPDAC,BGPDAP,BGPDAB S (C,D)=0 F BGPX="BF","BI","BL","BO","BR","BU" D I1AGE1,I1AGE2
 D I1AGEP
 Q
I1AGE1 ;
 S C=C+1
 S BGPF="032."_BGPX_".1" S BGPPC=$O(^BGPINDSC("C",BGPF,0))
 S BGPDF=$P(^BGPINDSC(BGPPC,0),U,8)
 S BGPNP=$P(^DD(90374.03,BGPDF,0),U,4),N=$P(BGPNP,";"),P=$P(BGPNP,";",2)
 S $P(BGPDAC(C),U)=$$V(1,BGPRPT,N,P)
 S $P(BGPDAP(C),U)=$$V(2,BGPRPT,N,P)
 S $P(BGPDAB(C),U)=$$V(3,BGPRPT,N,P)
 ;set 2nd piece to numerator and 3rd to %
 S BGPNF=$P(^BGPINDSC(BGPPC,0),U,9)
 S BGPNP=$P(^DD(90374.03,BGPNF,0),U,4),N=$P(BGPNP,";"),P=$P(BGPNP,";",2)
 S $P(BGPDAC(C),U,2)=$$V(1,BGPRPT,N,P),$P(BGPDAC(C),U,3)=$S($P(BGPDAC(C),U,1):($P(BGPDAC(C),U,2)/$P(BGPDAC(C),U)*100),1:"")
 S $P(BGPDAP(C),U,2)=$$V(2,BGPRPT,N,P),$P(BGPDAP(C),U,3)=$S($P(BGPDAP(C),U,1):($P(BGPDAP(C),U,2)/$P(BGPDAP(C),U)*100),1:"")
 S $P(BGPDAB(C),U,2)=$$V(3,BGPRPT,N,P),$P(BGPDAB(C),U,3)=$S($P(BGPDAB(C),U,1):($P(BGPDAB(C),U,2)/$P(BGPDAB(C),U)*100),1:"")
 Q
I1AGE2 ;
 S D=D+1
 S BGPF="032."_BGPX_".2" S BGPPC=$O(^BGPINDSC("C",BGPF,0))
 ;set 4th piece to numerator and 5th to %
 S BGPNF=$P(^BGPINDSC(BGPPC,0),U,9)
 S BGPNP=$P(^DD(90374.03,BGPNF,0),U,4),N=$P(BGPNP,";"),P=$P(BGPNP,";",2)
 S $P(BGPDAC(D),U,4)=$$V(1,BGPRPT,N,P),$P(BGPDAC(D),U,5)=$S($P(BGPDAC(D),U,2):($P(BGPDAC(D),U,4)/$P(BGPDAC(D),U,2)*100),1:"")
 S $P(BGPDAP(D),U,4)=$$V(2,BGPRPT,N,P),$P(BGPDAP(D),U,5)=$S($P(BGPDAP(D),U,2):($P(BGPDAP(D),U,4)/$P(BGPDAP(D),U,2)*100),1:"")
 S $P(BGPDAB(D),U,4)=$$V(3,BGPRPT,N,P),$P(BGPDAB(D),U,5)=$S($P(BGPDAB(D),U,2):($P(BGPDAB(D),U,4)/$P(BGPDAB(D),U,2)*100),1:"")
I1AGE3 ;
 S BGPF="032."_BGPX_".3" S BGPPC=$O(^BGPINDSC("C",BGPF,0))
 ;set 6th piece to numerator and 7th to %
 S BGPNF=$P(^BGPINDSC(BGPPC,0),U,9)
 S BGPNP=$P(^DD(90374.03,BGPNF,0),U,4),N=$P(BGPNP,";"),P=$P(BGPNP,";",2)
 S $P(BGPDAC(D),U,6)=$$V(1,BGPRPT,N,P),$P(BGPDAC(D),U,7)=$S($P(BGPDAC(D),U,4):($P(BGPDAC(D),U,6)/$P(BGPDAC(D),U,4)*100),1:"")
 S $P(BGPDAP(D),U,6)=$$V(2,BGPRPT,N,P),$P(BGPDAP(D),U,7)=$S($P(BGPDAP(D),U,4):($P(BGPDAP(D),U,6)/$P(BGPDAP(D),U,4)*100),1:"")
 S $P(BGPDAB(D),U,6)=$$V(3,BGPRPT,N,P),$P(BGPDAB(D),U,7)=$S($P(BGPDAB(D),U,4):($P(BGPDAB(D),U,6)/$P(BGPDAB(D),U,4)*100),1:"")
I1AGE4 ;
 S BGPF="032."_BGPX_".4" S BGPPC=$O(^BGPINDSC("C",BGPF,0))
 ;set 8th piece to numerator and 9th to %
 S BGPNF=$P(^BGPINDSC(BGPPC,0),U,9)
 S BGPNP=$P(^DD(90374.03,BGPNF,0),U,4),N=$P(BGPNP,";"),P=$P(BGPNP,";",2)
 S $P(BGPDAC(D),U,8)=$$V(1,BGPRPT,N,P),$P(BGPDAC(D),U,9)=$S($P(BGPDAC(D),U,4):($P(BGPDAC(D),U,8)/$P(BGPDAC(D),U,4)*100),1:"")
 S $P(BGPDAP(D),U,8)=$$V(2,BGPRPT,N,P),$P(BGPDAP(D),U,9)=$S($P(BGPDAP(D),U,4):($P(BGPDAP(D),U,8)/$P(BGPDAP(D),U,4)*100),1:"")
 S $P(BGPDAB(D),U,8)=$$V(3,BGPRPT,N,P),$P(BGPDAB(D),U,9)=$S($P(BGPDAB(D),U,4):($P(BGPDAB(D),U,8)/$P(BGPDAB(D),U,4)*100),1:"")
I1AGE5 ;
 G I1AGE6
 S BGPF="032."_BGPX_".5" S BGPPC=$O(^BGPINDSC("C",BGPF,0))
 ;set 10th piece to numerator and 11th to %
 S BGPNF=$P(^BGPINDSC(BGPPC,0),U,9)
 S BGPNP=$P(^DD(90374.03,BGPNF,0),U,4),N=$P(BGPNP,";"),P=$P(BGPNP,";",2)
 S $P(BGPDAC(D),U,10)=$$V(1,BGPRPT,N,P),$P(BGPDAC(D),U,11)=$S($P(BGPDAC(D),U,4):($P(BGPDAC(D),U,10)/$P(BGPDAC(D),U,4)*100),1:"")
 S $P(BGPDAP(D),U,10)=$$V(2,BGPRPT,N,P),$P(BGPDAP(D),U,11)=$S($P(BGPDAP(D),U,4):($P(BGPDAP(D),U,10)/$P(BGPDAP(D),U,4)*100),1:"")
 S $P(BGPDAB(D),U,10)=$$V(3,BGPRPT,N,P),$P(BGPDAB(D),U,11)=$S($P(BGPDAB(D),U,4):($P(BGPDAB(D),U,10)/$P(BGPDAB(D),U,4)*100),1:"")
I1AGE6 ;
 S BGPF="032."_BGPX_".6" S BGPPC=$O(^BGPINDSC("C",BGPF,0))
 ;set 12th piece to numerator and 13th to %
 S BGPNF=$P(^BGPINDSC(BGPPC,0),U,9)
 S BGPNP=$P(^DD(90374.03,BGPNF,0),U,4),N=$P(BGPNP,";"),P=$P(BGPNP,";",2)
 S $P(BGPDAC(D),U,12)=$$V(1,BGPRPT,N,P),$P(BGPDAC(D),U,13)=$S($P(BGPDAC(D),U,2):($P(BGPDAC(D),U,12)/$P(BGPDAC(D),U,2)*100),1:"")
 S $P(BGPDAP(D),U,12)=$$V(2,BGPRPT,N,P),$P(BGPDAP(D),U,13)=$S($P(BGPDAP(D),U,2):($P(BGPDAP(D),U,12)/$P(BGPDAP(D),U,2)*100),1:"")
 S $P(BGPDAB(D),U,12)=$$V(3,BGPRPT,N,P),$P(BGPDAB(D),U,13)=$S($P(BGPDAB(D),U,2):($P(BGPDAB(D),U,12)/$P(BGPDAB(D),U,2)*100),1:"")
 Q
I1AGEP ;
 D I1AGEP^BGP6PDL8
 Q
SETN ;set numerator fields
 S BGPCYN=$$V(1,BGPRPT,N,P)
 S BGPPRN=$$V(2,BGPRPT,N,P)
 S BGPBLN=$$V(3,BGPRPT,N,P)
 S BGPCYP=$S(BGPCYD:((BGPCYN/BGPCYD)*100),1:"")
 S BGPPRP=$S(BGPPRD:((BGPPRN/BGPPRD)*100),1:"")
 S BGPBLP=$S(BGPBLD:((BGPBLN/BGPBLD)*100),1:"")
 Q
V(T,R,N,P) ;EP
 I $G(BGPAREAA) G VA
 NEW X
 I T=1 S X=$P($G(^BGPGPDCS(R,N)),U,P) Q $S(X]"":X,1:0)
 I T=2 S X=$P($G(^BGPGPDPS(R,N)),U,P) Q $S(X]"":X,1:0)
 I T=3 S X=$P($G(^BGPGPDBS(R,N)),U,P) Q $S(X]"":X,1:0)
 Q ""
VA ;
 NEW X,V,C S X=0,C="" F  S X=$O(BGPSUL(X)) Q:X'=+X  D
 .I T=1 S C=C+$P($G(^BGPGPDCS(X,N)),U,P)
 .I T=2 S C=C+$P($G(^BGPGPDPS(X,N)),U,P)
 .I T=3 S C=C+$P($G(^BGPGPDBS(X,N)),U,P)
 .Q
 Q $S(C:C,1:0)
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
 ;I O=0!(N=0)!(O="")!(N="") Q "**"
 ;NEW X,X2,X3
 ;S X=N,X2=1,X3=0 D COMMA^%DTC S N=X
 ;S X=O,X2=1,X3=0 D COMMA^%DTC S O=X
 ;I +O=0 Q "**"
 ;S Z=(((N-O)/O)*100),Z=$FN(Z,"+,",1)
 S Z=N-O,Z=$FN(Z,"+,",1)
 Q Z
H3 ;EP
 S X="Age specific Tobacco Use" D S(X,1,1) S Y=" " D S(Y,1,1) S X=BGPHD1 D S(X,1,1) S Y=" " D S(Y,1,1)
 S X="Age Distribution" D S(X,1,1) S X=" " D S(X,1,1)
 S Y="5-13" D S(Y,1,2)
 S Y="14-17" D S(Y,,3)
 S Y="18-24" D S(Y,,4)
 S Y="25-44" D S(Y,,5)
 S Y="45-64" D S(Y,,6)
 S Y="65 AND OLDER" D S(Y,,7)
 Q
SB(X) ;EP - Strip leading and trailing blanks from X.
 X ^DD("FUNC",$O(^DD("FUNC","B","STRIPBLANKS",0)),1)
 Q X
H2 ;
 S BGPX="",BGPX=BGPCYN,$P(BGPX,U,2)=$$SB($J(BGPCYP,5,1)),$P(BGPX,U,3)=BGPPRN,$P(BGPX,U,4)=$$SB($J(BGPPRP,5,1)),$P(BGPX,U,5)=$$SB($J($$CALC(BGPCYP,BGPPRP),6)),$P(BGPX,U,6)=BGPBLN,$P(BGPX,U,7)=$$SB($J(BGPBLP,5,1))
 S $P(BGPX,U,8)=$$SB($J($$CALC(BGPCYP,BGPBLP),6))
 D S(BGPX,,2)
 Q
H1 ;EP
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
