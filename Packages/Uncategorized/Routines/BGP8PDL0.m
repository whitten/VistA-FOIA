BGP8PDL0 ; IHS/CMI/LAB - print ind 1 ; 01 Jul 2008  8:03 PM
 ;;8.0;IHS CLINICAL REPORTING;**2**;MAR 12, 2008
 ;
 ;
IC1 ;EP
 S BGPXXX=$P(^BGPINDE(BGPIC,0),U,6)
 I BGPINDT'="D" D H1^BGP8PDL1
 I BGPINDT'="D" F BGPPC1=BGPXXX_".1",BGPXXX_".2",BGPXXX_".3" D PI^BGP8PDL1
 I BGPINDT'="D" D H1^BGP8PDL1
 I BGPINDT'="D" F BGPPC1=BGPXXX_".4",BGPXXX_".5",BGPXXX_".6" D PI^BGP8PDL1
 D H1^BGP8PDL1
 F BGPPC1=BGPXXX_".7",BGPXXX_".8",BGPXXX_".9" D PI^BGP8PDL1
 D I1AGE
 Q
I1AGE ;EP
 Q:BGPINDT="D"
 Q:BGPRTYPE'=4
 ;S X="Age specific medical nutrition counseling" D S(X,1,1) S Y=" " D S(Y,1,1) S X=BGPHD1 D S(X,1,1) S Y=" " D S(Y,1,1)
 I BGPINDT="W",BGPRTYPE=4 G FEM
 S BGPHD1="TOTAL OBESE ACTIVE CLINICAL POPULATION",BGPHD2="Total Obese Active Clinical"
 S X=^BGPINDE(BGPIC,53,1,0) D S(X,1,1) S X=" " D S(X,1,1) D H3 S X=" " D S(X,1,1)
 K BGPDAC,BGPDAP,BGPDAB S (C,D)=0 F BGPX="ED","EG","EJ","EM","EP" D I1AGE1,I1AGE2
 D I1AGEP
 S BGPHD1="MALE OBESE ACTIVE CLINICAL POPULATION",BGPHD2="MALE Obese Active Clinical"
 S X=^BGPINDE(BGPIC,53,1,0) D S(X,1,1) D H3
 K BGPDAC,BGPDAP,BGPDAB S (D,C)=0 F BGPX="EE","EH","EK","EN","EQ" D I1AGE1,I1AGE2
 D I1AGEP
FEM ;
 S BGPHD1="FEMALE OBESE ACTIVE CLINICAL POPULATION",BGPHD2="FEMALE Obese Active Clinical"
 S X=^BGPINDE(BGPIC,53,1,0) D S(X,1,1) D H3
 K BGPDAC,BGPDAP,BGPDAB S (C,D)=0 F BGPX="EF","EI","EL","EO","ER" D I1AGE1,I1AGE2
 D I1AGEP
 Q
I1AGE1 ;
 S C=C+1
 S BGPF="C-1."_BGPX_".3" S BGPPC=$O(^BGPINDEC("C",BGPF,0))
 S BGPDF=$P(^BGPINDEC(BGPPC,0),U,8)
 S BGPNP=$P(^DD(90533.03,BGPDF,0),U,4),N=$P(BGPNP,";"),P=$P(BGPNP,";",2)
 S $P(BGPDAC(C),U)=$$V^BGP8DP1C(1,BGPRPT,N,P)
 S $P(BGPDAP(C),U)=$$V^BGP8DP1C(2,BGPRPT,N,P)
 S $P(BGPDAB(C),U)=$$V^BGP8DP1C(3,BGPRPT,N,P)
 ;set 2nd piece to numerator and 3rd to %
 S BGPNF=$P(^BGPINDEC(BGPPC,0),U,9)
 S BGPNP=$P(^DD(90533.03,BGPNF,0),U,4),N=$P(BGPNP,";"),P=$P(BGPNP,";",2)
 S $P(BGPDAC(C),U,2)=$$V^BGP8DP1C(1,BGPRPT,N,P),$P(BGPDAC(C),U,3)=$S($P(BGPDAC(C),U,1):($P(BGPDAC(C),U,2)/$P(BGPDAC(C),U)*100),1:"")
 S $P(BGPDAP(C),U,2)=$$V^BGP8DP1C(2,BGPRPT,N,P),$P(BGPDAP(C),U,3)=$S($P(BGPDAP(C),U,1):($P(BGPDAP(C),U,2)/$P(BGPDAP(C),U)*100),1:"")
 S $P(BGPDAB(C),U,2)=$$V^BGP8DP1C(3,BGPRPT,N,P),$P(BGPDAB(C),U,3)=$S($P(BGPDAB(C),U,1):($P(BGPDAB(C),U,2)/$P(BGPDAB(C),U)*100),1:"")
 Q
I1AGE2 ;
 S D=D+1
 S BGPF="C-1."_BGPX_".2" S BGPPC=$O(^BGPINDEC("C",BGPF,0))
 ;set 4th piece to numerator and 5th to %
 S BGPNF=$P(^BGPINDEC(BGPPC,0),U,9)
 S BGPNP=$P(^DD(90533.03,BGPNF,0),U,4),N=$P(BGPNP,";"),P=$P(BGPNP,";",2)
 S $P(BGPDAC(D),U,4)=$$V^BGP8DP1C(1,BGPRPT,N,P),$P(BGPDAC(D),U,5)=$S($P(BGPDAC(D),U,1):($P(BGPDAC(D),U,4)/$P(BGPDAC(D),U,1)*100),1:"")
 S $P(BGPDAP(D),U,4)=$$V^BGP8DP1C(2,BGPRPT,N,P),$P(BGPDAP(D),U,5)=$S($P(BGPDAP(D),U,1):($P(BGPDAP(D),U,4)/$P(BGPDAP(D),U,1)*100),1:"")
 S $P(BGPDAB(D),U,4)=$$V^BGP8DP1C(3,BGPRPT,N,P),$P(BGPDAB(D),U,5)=$S($P(BGPDAB(D),U,1):($P(BGPDAB(D),U,4)/$P(BGPDAB(D),U,1)*100),1:"")
I1AGE3 ;
 S BGPF="C-1."_BGPX_".1" S BGPPC=$O(^BGPINDEC("C",BGPF,0))
 ;set 6th piece to numerator and 7th to %
 S BGPNF=$P(^BGPINDEC(BGPPC,0),U,9)
 S BGPNP=$P(^DD(90533.03,BGPNF,0),U,4),N=$P(BGPNP,";"),P=$P(BGPNP,";",2)
 S $P(BGPDAC(D),U,6)=$$V^BGP8DP1C(1,BGPRPT,N,P),$P(BGPDAC(D),U,7)=$S($P(BGPDAC(D),U,1):($P(BGPDAC(D),U,6)/$P(BGPDAC(D),U,1)*100),1:"")
 S $P(BGPDAP(D),U,6)=$$V^BGP8DP1C(2,BGPRPT,N,P),$P(BGPDAP(D),U,7)=$S($P(BGPDAP(D),U,1):($P(BGPDAP(D),U,6)/$P(BGPDAP(D),U,1)*100),1:"")
 S $P(BGPDAB(D),U,6)=$$V^BGP8DP1C(3,BGPRPT,N,P),$P(BGPDAB(D),U,7)=$S($P(BGPDAB(D),U,1):($P(BGPDAB(D),U,6)/$P(BGPDAB(D),U,1)*100),1:"")
I1AGE4 ;
 S BGPF="C-1."_BGPX_".4" S BGPPC=$O(^BGPINDEC("C",BGPF,0))
 ;set 8th piece to numerator and 9th to %
 S BGPNF=$P(^BGPINDEC(BGPPC,0),U,9)
 S BGPNP=$P(^DD(90533.03,BGPNF,0),U,4),N=$P(BGPNP,";"),P=$P(BGPNP,";",2)
 S $P(BGPDAC(D),U,8)=$$V^BGP8DP1C(1,BGPRPT,N,P),$P(BGPDAC(D),U,9)=$S($P(BGPDAC(D),U,1):($P(BGPDAC(D),U,8)/$P(BGPDAC(D),U,1)*100),1:"")
 S $P(BGPDAP(D),U,8)=$$V^BGP8DP1C(2,BGPRPT,N,P),$P(BGPDAP(D),U,9)=$S($P(BGPDAP(D),U,1):($P(BGPDAP(D),U,8)/$P(BGPDAP(D),U,1)*100),1:"")
 S $P(BGPDAB(D),U,8)=$$V^BGP8DP1C(3,BGPRPT,N,P),$P(BGPDAB(D),U,9)=$S($P(BGPDAB(D),U,1):($P(BGPDAB(D),U,8)/$P(BGPDAB(D),U,1)*100),1:"")
 Q
I1AGEP ;
 D I1AGEP^BGP8PDL5
 Q
SETN ;set numerator fields
 S BGPIIDEL=1
 D SETN^BGP8PDL1
 Q
V(T,R,N,P) ;EP
 I $G(BGPAREAA) G VA
 NEW X
 I T=1 S X=$P($G(^BGPGPDCE(R,N)),U,P) Q $S(X]"":X,1:0)
 I T=2 S X=$P($G(^BGPGPDPE(R,N)),U,P) Q $S(X]"":X,1:0)
 I T=3 S X=$P($G(^BGPGPDBE(R,N)),U,P) Q $S(X]"":X,1:0)
 Q ""
VA ;
 NEW X,V,C S X=0,C="" F  S X=$O(BGPSUL(X)) Q:X'=+X  D
 .I T=1 S C=C+$P($G(^BGPGPDCE(X,N)),U,P)
 .I T=2 S C=C+$P($G(^BGPGPDPE(X,N)),U,P)
 .I T=3 S C=C+$P($G(^BGPGPDBE(X,N)),U,P)
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
 ;;S X="Age specific medical nutrition counseling" D S(X,1,1) S Y=" " D S(Y,1,1) S X=BGPHD1 D S(X,1,1) S Y=" " D S(Y,1,1)
 S X="Age Distribution" D S(X,1,1) S X=" " D S(X,1,1)
 S Y="6-11" D S(Y,1,2)
 S Y="12-19" D S(Y,,3)
 S Y="20-39" D S(Y,,4)
 S Y="40-59" D S(Y,,5)
 S Y="60 and older" D S(Y,,6)
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
