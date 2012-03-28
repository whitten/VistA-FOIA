BGP7D72 ; IHS/CMI/LAB - measure 31 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;
 ;
IHEDBBH ;EP
 S (BGPN1,BGPN2,BGPN3,BGPN4,BGPN5,BGPN6,BGPN7,BGPD1,BGPD2,BGPD3,BGPD4,BGPD5,BGPD6,BGPD7,BGPD8,BGPD9)=0
 S BGPVALUE="" K BGPBETA
 I BGPAGEB<36 S BGPSTOP=1 Q
 I 'BGPACTUP S BGPSTOP=1 Q
 S BGPAMI=$$AMIH(DFN,BGPBDATE,$$FMADD^XLFDT(BGPBDATE,(51*7)))
 I '$P(BGPAMI,U) S BGPSTOP=1 Q  ;no ami
 I $$READM(DFN,$P(BGPAMI,U,4),$P(BGPAMI,U,2)) S BGPSTOP=1 Q
 S BGPV=$P(BGPAMI,U,3)
 S BGPBETAC=$$BETACONT(DFN,"",$S($P(BGPAMI,U,4)]"":$P(BGPAMI,U,4),1:$P(BGPAMI,U,2)))  ;beta contraindications
 I $P(BGPBETAC,U) S BGPSTOP=1 Q  ;beta contraindication
 S BGPBETAL=$$BETAALG1(DFN,$S($P(BGPAMI,U,4)]"":$P(BGPAMI,U,4),1:$P(BGPAMI,U,2)))  ;beta allergy
 I $P(BGPBETAL,U) S BGPSTOP=1 Q
 I BGPACTCL S BGPD1=1
 I BGPACTUP S BGPD2=1
 S BGPBETA=$$BETA7(DFN,$$FMADD^XLFDT($P(BGPAMI,U,2),-60),$$FMADD^XLFDT($P(BGPAMI,U,4),7),$P(BGPAMI,U,4))
 I $P(BGPBETA,U)=1 S BGPN1=1
 S BGPVALUE=$S(BGPRTYPE=3:"",BGPD2:"UP",1:"")_$S(BGPD1:";AC",1:"")_"|||"_$S($P(BGPBETA,U):$P(BGPBETA,U,2)_" "_$P(BGPBETA,U,3),1:"")
 K BGPAMI,BGPBETA,BGPBETAC,BGPBETAL
 K ^TMP($J)
 Q
IHEDPBH ;EP
 S (BGPN1,BGPN2,BGPN3,BGPN4,BGPN5,BGPN6,BGPN7,BGPD1,BGPD2,BGPD3,BGPD4,BGPD5,BGPD6,BGPD7,BGPD8,BGPD9)=0
 K BGPBETA,BGPAMI,BGPV,BGPBETAC,BGPBETAL,BGPVALUE
 I BGPAGEB<36 S BGPSTOP=1 Q
 I 'BGPACTUP S BGPSTOP=1 Q
 S BGPAMI=$$AMI(DFN,$$FMADD^XLFDT(BGPBDATE,-180),$$FMADD^XLFDT(BGPBDATE,180))
 I '$P(BGPAMI,U) S BGPSTOP=1 Q  ;no ami
 S BGPBETAC=$$BETACONT(DFN,"",$S($P(BGPAMI,U,4)]"":$P(BGPAMI,U,4),1:$P(BGPAMI,U,2)))  ;beta contraindications
 I $P(BGPBETAC,U) S BGPSTOP=1 Q  ;beta contraindication
 S BGPBETAL=$$BETAALG1(DFN,$S($P(BGPAMI,U,4)]"":$P(BGPAMI,U,4),1:$P(BGPAMI,U,2)))  ;beta allergy
 I $P(BGPBETAL,U) S BGPSTOP=1 Q
 I BGPACTCL S BGPD1=1
 I BGPACTUP S BGPD2=1
 S BGPBETA=$$BETA(DFN,$P(BGPAMI,U,2),$P(BGPAMI,U,4))
 I $P(BGPBETA,U)=1 S BGPN1=1
 S BGPVALUE=$S(BGPRTYPE=3:"",BGPD2:"UP")_$S(BGPD1:";AC",1:"")_"|||"_$S($P(BGPBETA,U):"YES, beta blocker 135+ "_$P(BGPBETA,U,2),1:"")
 K BGPAMI,BGPBETA,BGPBETAC,BGPBETAL
 K ^TMP($J)
 Q
IHEDCHM ;EP
 S (BGPN1,BGPN2,BGPN3,BGPN4,BGPN5,BGPN6,BGPN7,BGPD1,BGPD2,BGPD3,BGPD4,BGPD5,BGPD6,BGPD7,BGPD8,BGPD9)=0
 I BGPAGEB<18 S BGPSTOP=1 Q
 I BGPAGEB>75 S BGPSTOP=1 Q
 I 'BGPACTUP S BGPSTOP=1 Q
 S BGPAMI=$$AMIO(DFN,$$FMADD^XLFDT(BGPBDATE,-365),$$FMADD^XLFDT(BGPBDATE,-60)) I '$P(BGPAMI,U) S BGPSTOP=1 Q
 I BGPACTUP S BGPD2=1
 I BGPACTCL S BGPD1=1
 I BGPRTYPE=3,'BGPD1 S BGPSTOP=1 Q
 S BGPLDL=$$LDL^BGP7D2(DFN,BGPBDATE,BGPEDATE)
 I $P(BGPLDL,U)=1 S BGPN1=1
 I $P(BGPLDL,U,3)]"" D
 .S V=$P(BGPLDL,U,3)
 .I V]"",+V'>100 S BGPN2=1
 .I +V>100,+V<131 S BGPN3=1
 .I +V>130 S BGPN4=1
 S BGPXPNV=$P(BGPLDL,U,3)
 S V=$S(BGPRTYPE=3:"",1:"UP")_$S(BGPD1:";AC",1:"")_";"_$P(BGPAMI,U,2)_"|||"
 I $P(BGPLDL,U) S V=V_"LDL "_$$DATE^BGP7UTL($P(BGPLDL,U,2))_" "_$P(BGPLDL,U,3)
 S BGPVALUE=V
 K V,BGPAMI,BGPLDL,D
 K ^TMP($J)
 Q
CHOL(P,BDATE,EDATE) ;
 K BGPG
 S (Q,R,S,M,N,O,B,D,E,L,G)=""
 S R=$O(^ATXLAB("B","DM AUDIT CHOLESTEROL TAX",0))
 S N=$O(^ATXAX("B","BGP TOTAL CHOLESTEROL LOINC",0))
 S B=9999999-BDATE,E=9999999-EDATE S D=E-1 F  S D=$O(^AUPNVLAB("AE",P,D)) Q:D'=+D!(D>B)!(G]"")  D
 .S L=0 F  S L=$O(^AUPNVLAB("AE",P,D,L)) Q:L'=+L!(G]"")  D
 ..S X=0 F  S X=$O(^AUPNVLAB("AE",P,D,L,X)) Q:X'=+X!(G]"")  D
 ...Q:'$D(^AUPNVLAB(X,0))
 ...I R,$P(^AUPNVLAB(X,0),U),$D(^ATXLAB(R,21,"B",$P(^AUPNVLAB(X,0),U))) S G=(9999999-D)_"^CHOL"_"^"_$P(^AUPNVLAB(X,0),U,4) Q
 ...S J=$P($G(^AUPNVLAB(X,11)),U,13) Q:J=""
 ...I $$LOINC(J,N) S G=(9999999-D)_"^CHOL LOINC"_"^"_$P(^AUPNVLAB(X,0),U,4) Q
 ...Q
 I G]"" Q G
 S E=+$$CODEN^ICPTCOD(82465),%=$$CPTI^BGP7DU(P,BDATE,EDATE,E) I %]"" Q $P(%,U,2)_"^CPT 82465"
 S E=+$$CODEN^ICPTCOD(82465),%=$$TRANI^BGP7DU(P,BDATE,EDATE,E) I %]"" Q $P(%,U,2)_"^TRAN 82465"
 Q ""
LOINC(A,B) ;
 NEW %
 S %=$P($G(^LAB(95.3,A,9999999)),U,2)
 I %]"",$D(^ATXAX(B,21,"B",%)) Q 1
 S %=$P($G(^LAB(95.3,A,0)),U)_"-"_$P($G(^LAB(95.3,A,0)),U,15)
 I $D(^ATXAX(B,21,"B",%)) Q 1
 Q ""
AMIO(P,BDATE,EDATE) ;
 ;did pat have ami, cabg or ptca in this time period - if yes 1^date of dx
 K BGPG
 S Y="BGPG("
 S X=P_"^LAST DX [BGP AMI DXS (HEDIS);DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE) S E=$$START1^APCLDF(X,Y)
 I $D(BGPG(1)) Q 1_U_"AMI DX"  ;has a dx
 ;check for procedure in BGP CABG PROCS
 S BGPG=$$LASTPRC^BGP7UTL1(P,"BGP CABG PROCS",BDATE,EDATE)
 I $P(BGPG,U) Q 1_U_"CABG PROC"
 ;now check cpts
 S BGPG=$$CPT^BGP7DU(P,BDATE,EDATE,$O(^ATXAX("B","BGP CABG CPTS",0)),6)
 I $P(BGPG,U) Q 1_U_"CABG CPT"
 S BGPG=$$TRAN^BGP7DU(P,BDATE,EDATE,$O(^ATXAX("B","BGP CABG CPTS",0)),6)
 I $P(BGPG,U) Q 1_U_"CABG TRAN"
 ;now check for PTCA
 ;check for procedure in BGP PTCA PROCS
 S BGPG=$$LASTPRC^BGP7UTL1(P,"BGP PTCA PROCS",BDATE,EDATE)
 I $P(BGPG,U) Q 1_U_"PTCA PROC"
 ;now check cpts
 S BGPG=$$CPT^BGP7DU(P,BDATE,EDATE,$O(^ATXAX("B","BGP PTCA CPTS",0)),6)
 I $P(BGPG,U) Q 1_U_"PTCA CPT"
 S BGPG=$$TRAN^BGP7DU(P,BDATE,EDATE,$O(^ATXAX("B","BGP PTCA CPTS",0)),6)
 I $P(BGPG,U) Q 1_U_"PTCA TRAN"
 ;now check IVD dxs
 S BGPG=$$LASTDX^BGP7UTL1(P,"BGP IVD DXS",BDATE,EDATE)
 I $P(BGPG,U) Q 1_U_"IVD DX"
 Q ""
AMI(P,BDATE,EDATE) ;
 ;look for any H with AMI discharge dx
 K ^TMP($J,"A"),G
 S A="^TMP($J,""A"",",B=P_"^ALL VISITS;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(B,A)
 I '$D(^TMP($J,"A",1)) Q 0  ;no HOSP
 S T=$O(^ATXAX("B","BGP AMI DXS (HEDIS)",0))
 S (BGPX,G,M,D,E)=0 F  S BGPX=$O(^TMP($J,"A",BGPX)) Q:BGPX'=+BGPX  S V=$P(^TMP($J,"A",BGPX),U,5) D
 .Q:'$D(^AUPNVSIT(V,0))
 .Q:'$P(^AUPNVSIT(V,0),U,9)
 .Q:$P(^AUPNVSIT(V,0),U,11)
 .Q:"AOSH"'[$P(^AUPNVSIT(V,0),U,7)
 .;Q:"CV"[$P(^AUPNVSIT(V,0),U,3)
 .S H=0
 .I $P(^AUPNVSIT(V,0),U,7)="H" S H=$O(^AUPNVINP("AD",V,0)) D  Q:'B
 ..S B=0
 ..I 'H Q
 ..Q:$$AMA(H)  ;ama
 ..Q:$$TRANS(H)  ;transferred
 ..Q:$$EXPIRED(H)  ;died
 ..S B=1
 .S (D,Y)=0 F  S Y=$O(^AUPNVPOV("AD",V,Y)) Q:Y'=+Y!(D)  I $D(^AUPNVPOV(Y,0)) S %=$P(^AUPNVPOV(Y,0),U) I $$ICD^ATXCHK(%,T,9) S D=1
 .I D S G=G+1,G($P($P(^AUPNVSIT(V,0),U),"."))=V ;got one visit
 I 'G Q G
 S D=$O(G(0)),V=G(D),H=$O(^AUPNVINP("AD",V,0))
 Q 1_U_$O(G(0))_U_V_U_$S(H:$P($P(^AUPNVINP(H,0),U),"."),1:"")_U_H
 ;
AMIH(P,BDATE,EDATE) ;
 ;look for any H with AMI discharge dx
 K ^TMP($J,"A"),G
 S A="^TMP($J,""A"",",B=P_"^ALL VISITS;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(B,A)
 I '$D(^TMP($J,"A",1)) Q 0  ;no HOSP
 S T=$O(^ATXAX("B","BGP AMI IND 30",0))
 S (BGPX,G,M,D,E)=0 F  S BGPX=$O(^TMP($J,"A",BGPX)) Q:BGPX'=+BGPX  S V=$P(^TMP($J,"A",BGPX),U,5) D
 .Q:'$D(^AUPNVSIT(V,0))
 .Q:'$P(^AUPNVSIT(V,0),U,9)
 .Q:$P(^AUPNVSIT(V,0),U,11)
 .Q:$P(^AUPNVSIT(V,0),U,7)'="H"
 .;Q:"CV"[$P(^AUPNVSIT(V,0),U,3)
 .S H=0
 .S H=$O(^AUPNVINP("AD",V,0)) D  Q:'B
 ..S B=0
 ..I 'H Q
 ..Q:$P($P(^AUPNVINP(H,0),U),".")>EDATE
 ..Q:$$AMA(H)  ;ama
 ..Q:$$TRANS(H)  ;transferred
 ..Q:$$EXPIRED(H)  ;died
 ..S B=1
 .S (D,Y)=0 F  S Y=$O(^AUPNVPOV("AD",V,Y)) Q:Y'=+Y!(D)  I $D(^AUPNVPOV(Y,0)) S %=$P(^AUPNVPOV(Y,0),U) I $$ICD^ATXCHK(%,T,9) S D=1
 .I D S G=G+1,G($P($P(^AUPNVSIT(V,0),U),"."))=V ;got one visit
 I 'G Q G
 S D=$O(G(0)),V=G(D),H=$O(^AUPNVINP("AD",V,0))
 Q 1_U_$O(G(0))_U_V_U_$S(H:$P($P(^AUPNVINP(H,0),U),"."),1:"")_U_H
READM(P,D,PV) ;EP
 S ED=$$FMADD^XLFDT(D,7),G=0
 S X=0,V=0 F  S X=$O(^AUPNVSIT("AAH",P,X)) Q:X'=+X  D
 .S V=0 F  S V=$O(^AUPNVSIT("AAH",P,X,V)) Q:V'=+V  D
 ..Q:PV=V
 ..S E=$P($P($G(^AUPNVSIT(V,0)),U),".")
 ..Q:E<D
 ..Q:E>ED
 ..S G=1
 Q G
BETACONT(P,BDATE,EDATE) ;EP BETA BLOCKER CONTRAINDICATION
 I $G(BDATE)="" S BDATE=$$DOB^AUPNPAT(P)
 K BGPG,BGPD
 S Y="BGPG("
 S X=P_"^ALL DX [BGP ASTHMA DXS;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE) S E=$$START1^APCLDF(X,Y)
 S (X,G)=0 F  S X=$O(BGPG(X)) Q:X'=+X  S BGPD($P(BGPG(X),U))=""
 S (X,G)=0 F  S X=$O(BGPD(X)) Q:X'=+X  S G=G+1
 I G>1 Q 1_U_"2 dx asthma contra"  ;has 2 dx of asthma so contraindication
 S BGPG=$$LASTDX^BGP7UTL1(P,"BGP HYPOTENSION DXS",$$DOB^AUPNPAT(P),EDATE)
 I $P(BGPG,U)=1 Q 1_U_"hypotension dx contra"  ;has hypotension dx
 S BGPG=$$LASTDX^BGP7UTL1(P,"BGP CMS 2/3 HEART BLOCK DXS",$$DOB^AUPNPAT(P),EDATE)
 I $P(BGPG,U)=1 Q 1_U_"heart blk contra"  ;has heart block dx
 K BGPG,BGPD
 S Y="BGPG("
 S X=P_"^ALL DX [BGP COPD DXS BB CONT;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE) S E=$$START1^APCLDF(X,Y)
 S (X,G)=0 F  S X=$O(BGPG(X)) Q:X'=+X  S BGPD($P(BGPG(X),U))=""
 S (X,G)=0 F  S X=$O(BGPD(X)) Q:X'=+X  S G=G+1
 I G>1 Q 1_U_"copd dx contra"
 Q 0
AMA(H) ;EP
 S X=$P(^AUPNVINP(H,0),U,6)
 I X="" Q 0
 S X=$P($G(^DG(405.1,X,"IHS")),U,1)
 I X=3 Q 1
 Q 0
EXPIRED(H) ;EP
 S X=$P(^AUPNVINP(H,0),U,6)
 I X="" Q 0
 S X=$P($G(^DG(405.1,X,"IHS")),U,1)
 I X=4!(X=5)!(X=6)!(X=7) Q 1
 Q 0
DSCH(H) ;
 Q $P($P(^AUPNVINP(H,0),U),".")
TRANS(H) ;EP
 S X=$P(^AUPNVINP(H,0),U,6)
 I X="" Q 0
 S X=$P($G(^DG(405.1,X,"IHS")),U,1)
 I X=2 Q 1
 Q 0
BETA(P,BGPADMD,BGPDD) ;EP
 ;get all beta blockers 
 I BGPADMD="" Q ""
 K ^TMP($J,"MEDS")
 S BGPC1=0 K BGPZ
 I $G(BGPDD)="" S BGPDD=BGPADMD
 S (G,N,Y,X,T,T1,T2,M,K,S,C,K,R)=""
 S Y="^TMP($J,""MEDS"",",X=P_"^ALL MED;DURING "_$$FMTE^XLFDT(BGPADMD)_"-"_$$FMTE^XLFDT($$FMADD^XLFDT(BGPDD,180)) S E=$$START1^APCLDF(X,Y)
 S T=$O(^ATXAX("B","BGP HEDIS BETA BLOCKER MEDS",0))
 S T1=$O(^ATXAX("B","BGP HEDIS BETA BLOCKER NDC",0))
 ;S T2=$O(^ATXAX("B","BGP HEDIS BETA BLOCKER CLASS",0))
 S X=0 F  S X=$O(^TMP($J,"MEDS",X)) Q:X'=+X  S Y=+$P(^TMP($J,"MEDS",X),U,4) D
 .Q:'$D(^AUPNVMED(Y,0))
 .S G=0
 .S D=$P(^AUPNVMED(Y,0),U)
 .I T,$D(^ATXAX(T,21,"B",D)) S G=1 G BETA1
 .S C=$P($G(^PSDRUG(D,0)),U,2)
 .;I C]"",T2,$D(^ATXAX(T2,21,"B",C)) S G=1 G BETA1
 .S N=$P($G(^PSDRUG(D,2)),U,4)
 .I N]"",T1,$D(^ATXAX(T1,21,"B",N)) S G=1
 .Q:'G
BETA1 .;
 .S J=$P(^AUPNVMED(Y,0),U,8)
 .S V=$P(^AUPNVMED(Y,0),U,3)
 .Q:'V
 .Q:'$D(^AUPNVSIT(V,0))
 .;S IS DAYS SUPPLY, J IS DATE DISCONTINUED
 .I J]"" S S=$$FMDIFF^XLFDT(J,$P($P(^AUPNVSIT(V,0),U),"."))
 .I J="" S S=$P(^AUPNVMED(Y,0),U,7)
 .S K=S+K  ;TOTAL DAYS SUPPLY
 .I R]"" S R=R_";"
 .S R=R_$$DATE^BGP7UTL($P($P(^AUPNVSIT(V,0),U),"."))_"("_S_")"
 I K>134 Q 1_U_" total days beta blocker: "_K
BETAADM ;now add in any before admission
 K ^TMP($J,"MEDS")
 S Y="^TMP($J,""MEDS"",",X=P_"^LAST 30 MED;DURING "_$$FMTE^XLFDT($$DOB^AUPNPAT(P))_"-"_$$FMTE^XLFDT($$FMADD^XLFDT(BGPADMD,-1)) S E=$$START1^APCLDF(X,Y)
  S X=0 F  S X=$O(^TMP($J,"MEDS",X)) Q:X'=+X  S Y=+$P(^TMP($J,"MEDS",X),U,4) D
 .Q:'$D(^AUPNVMED(Y,0))
 .S G=0
 .S D=$P(^AUPNVMED(Y,0),U)
 .I T,$D(^ATXAX(T,21,"B",D)) S G=1 G BETA2
 .S C=$P($G(^PSDRUG(D,0)),U,2)
 .I C]"",T2,$D(^ATXAX(T2,21,"B",C)) S G=1 G BETA2
 .S N=$P($G(^PSDRUG(D,2)),U,4)
 .I N]"",T1,$D(^ATXAX(T1,21,"B",N)) S G=1
 .Q:'G
BETA2 .;
 .S J=$P(^AUPNVMED(Y,0),U,8)
 .S V=$P(^AUPNVMED(Y,0),U,3)
 .Q:'V
 .Q:'$D(^AUPNVSIT(V,0))
 .;S IS DAYS SUPPLY, J IS DATE DISCONTINUED
 .Q:J]""  ;don't use if discontinued
 .S D=$$FMDIFF^XLFDT(BGPDD,$P($P(^AUPNVSIT(V,0),U),"."))  ;difference between dsch date and date prescribed
 .S S=$P(^AUPNVMED(Y,0),U,7)
 .S S=S-D  ;subtract the number of days used
 .S:S<0 S=0
 .S K=S+K  ;TOTAL DAYS SUPPLY
 .I R]"" S R=R_";"
 .S R=R_$$DATE^BGP7UTL($P($P(^AUPNVSIT(V,0),U),"."))_"("_S_")"
 I K>134 Q 1_U_" total days beta blocker: "_K
 Q 0_U_R_" total days beta blocker: "_K
BETA7(P,BDATE,EDATE,BGPDD) ;
 ;see if there ACTIVE PRESCRIPTION of beta blockers in time window
 K ^TMP($J,"MEDS")
 S BGPG=0
 S Y="^TMP($J,""MEDS"",",X=P_"^ALL MED;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE) S E=$$START1^APCLDF(X,Y)
 S T=$O(^ATXAX("B","BGP HEDIS BETA BLOCKER MEDS",0))
 S T1=$O(^ATXAX("B","BGP HEDIS BETA BLOCKER NDC",0))
 ;S T2=$O(^ATXAX("B","BGP HEDIS BETA BLOCKER CLASS",0))
 S X=0 F  S X=$O(^TMP($J,"MEDS",X)) Q:X'=+X!(BGPG)  S Y=+$P(^TMP($J,"MEDS",X),U,4) D
 .Q:'$D(^AUPNVMED(Y,0))
 .S G=0
 .S D=$P(^AUPNVMED(Y,0),U)
 .I T,$D(^ATXAX(T,21,"B",D)) S G=1 G BETA8
 .S C=$P($G(^PSDRUG(D,0)),U,2)
 .;I C]"",T2,$D(^ATXAX(T2,21,"B",C)) S G=1 G BETA8
 .S N=$P($G(^PSDRUG(D,2)),U,4)
 .I N]"",T1,$D(^ATXAX(T1,21,"B",N)) S G=1
 .Q:'G  ;NOT A BETA BLOCKER
BETA8 .;
 .S J=$P(^AUPNVMED(Y,0),U,8)
 .S V=$P(^AUPNVMED(Y,0),U,3)
 .Q:'V
 .Q:'$D(^AUPNVSIT(V,0))
 .;S IS DAYS SUPPLY, J IS DATE DISCONTINUED
 .I J]"" Q:J<EDATE  ;discontinued W/IN 7 days of discharge date
 .S S=$P(^AUPNVMED(Y,0),U,7)
 .S Z=$$FMDIFF^XLFDT(EDATE,$P($P(^AUPNVSIT(V,0),U),"."))
 .I S>Z S BGPG=1_U_$$DATE^BGP7UTL($P($P(^AUPNVSIT(V,0),U),"."))_U_$P(^PSDRUG(D,0),U)
 .Q
 Q BGPG
BETAALG1(P,BGPD) ;EP - does patient have an Beta Blocker allergy
 ;get all povs with 995.0-995.3 with ecode of e935.3 up to discharge date
 ;BGPD is discharge date
 S BGPC=0
BETAPOV ;
 K BGPG,BGPY S Y="BGPG(",X=P_"^ALL DX [BGP ASA ALLERGY 995.0-995.3;DURING "_$$FMTE^XLFDT($$DOB^AUPNPAT(P))_"-"_$$FMTE^XLFDT(BGPD) S E=$$START1^APCLDF(X,Y)
 S X=0 F  S X=$O(BGPG(X)) Q:X'=+X  S Y=+$P(BGPG(X),U,4) D
 .S N=$$VAL^XBDIQ1(9000010.07,Y,.04) S N=$$UP^XLFSTR(N)
 .I N["BETA BLOCK"!(N["BBLOCK")!(N["B BLOCK") S BGPC=BGPC+1,BGPY(BGPC)="POV:  "_$$DATE^BGP7UTL($P(BGPG(X),U))_"  ["_$P(BGPG(X),U,2)_"]  "_N
 .S Z=$P(^AUPNVPOV(Y,0),U,9) I Z]"",$P($$ICDDX^ICDCODE(Z),U,2)="E942.0" S BGPC=BGPC+1,BGPY(BGPC)="POV:  "_$$DATE^BGP7UTL($P(BGPG(X),U))_"  ["_$P(BGPG(X),U,2)_" + E942.0]  "_N
 .Q
 I BGPC>0 Q 1
 K BGPG S BGPC=0 S Y="BGPG(",X=P_"^ALL DX V14.8;DURING "_$$FMTE^XLFDT($$DOB^AUPNPAT(DFN))_"-"_$$FMTE^XLFDT(BGPD) S E=$$START1^APCLDF(X,Y)
 S X=0 F  S X=$O(BGPG(X)) Q:X'=+X  S Y=+$P(BGPG(X),U,4) D
 .S N=$$VAL^XBDIQ1(9000010.07,Y,.04),N=$$UP^XLFSTR(N)
 .I N["BETA BLOCK"!(N["BBLOCK")!(N["B BLOCK") S BGPC=BGPC+1,BGPY(BGPC)="POV:  "_$$DATE^BGP7UTL($P(BGPG(X),U))_"  ["_$P(BGPG(X),U,2)_"]  "_N
 I BGPC>0 Q 1
 ;now check problem list for these codes
 S BGPC=0
 S T="",T=$O(^ATXAX("B","BGP ASA ALLERGY 995.0-995.3",0))
 S X=0 F  S X=$O(^AUPNPROB("AC",P,X)) Q:X'=+X  D
 .S I=$P($G(^AUPNPROB(X,0)),U),Y=$P($$ICDDX^ICDCODE(I),U,2)
 .S N=$$VAL^XBDIQ1(9000011,X,.05),N=$$UP^XLFSTR(N)
 .Q:$P(^AUPNPROB(X,0),U,8)>BGPD  ;added after discharge date
 .I Y="V14.8"!($$ICD^ATXCHK(I,T,9)),N["BETA BLOCK"!(N["BBLOCK")!(N["B BLOCK") S BGPC=BGPC+1,BGPY(BGPC)="PROBLEM LIST:  "_$$DATE^BGP7UTL($P(^AUPNPROB(X,0),U,8))_"  ["_Y_"]  "_N
 .Q
 I BGPC>0 Q 1
 ;now check allergy tracking
 S BGPC=0
 S X=0 F  S X=$O(^GMR(120.8,"B",P,X)) Q:X'=+X  D
 .Q:$P($G(^GMR(120.8,X,0)),U,26)>BGPD  ;entered after end date
 .S N=$P($G(^GMR(120.8,X,0)),U,2),N=$$UP^XLFSTR(N)
 .I N["BETA BLOCK" S BGPC=BGPC+1,BGPY(BGPC)="ALLERGY TRACKING:  "_$$DATE^BGP7UTL($P(^GMR(120.8,X,0),U,4))_"  "_N
 I BGPC>0 Q 1
 Q 0
