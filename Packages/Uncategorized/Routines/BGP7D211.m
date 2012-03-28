BGP7D211 ; IHS/CMI/LAB - measure 6 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;
DNKA(V) ;EP
 NEW D,N
 S D=$$PRIMPOV^APCLV(V,"C")
 I D=".0860" Q 1
 S N=$$PRIMPOV^APCLV(V,"N")
 I $E(D)="V",N["DNKA" Q 1
 I $E(D)="V",N["DID NOT KEEP APPOINTMENT" Q 1
 I $E(D)="V",N["DID NOT KEEP APPT" Q 1
 Q 0
GFR(P,BDATE,EDATE) ;EP
 S BGPC=""
 S T=$O(^LAB(60,"B","ESTIMATED GFR",0))
 S T1=$O(^ATXLAB("B","BGP GPRA ESTIMATED GFR TAX",0))
 S T2=$O(^ATXAX("B","BGP ESTIMATED GFR LOINC",0))
 S B=9999999-BDATE,E=9999999-EDATE S D=E-1 F  S D=$O(^AUPNVLAB("AE",P,D)) Q:D'=+D!(D>B)!(BGPC]"")  D
 .S L=0 F  S L=$O(^AUPNVLAB("AE",P,D,L)) Q:L'=+L!(BGPC]"")  D
 ..S X=0 F  S X=$O(^AUPNVLAB("AE",P,D,L,X)) Q:X'=+X!(BGPC]"")  D
 ...Q:'$D(^AUPNVLAB(X,0))
 ...Q:$P(^AUPNVLAB(X,0),U,4)=""
 ...I T,$P(^AUPNVLAB(X,0),U)=T S BGPC=1_U_(9999999-D) Q
 ...I T1,$D(^ATXLAB(T1,21,"B",$P(^AUPNVLAB(X,0),U))) S BGPC=1_U_(9999999-D) Q
 ...Q:'T2
 ...S J=$P($G(^AUPNVLAB(X,11)),U,13) Q:J=""
 ...Q:'$$LOINC(J,T2)
 ...S BGPC=1_U_(9999999-D)
 ...Q
 Q BGPC
QUANTUP(P,BDATE,EDATE) ;EP
 K BGPC,BGPX,BGP1
 S BGPC=""
 S %="",E=+$$CODEN^ICPTCOD(82043),%=$$CPTI^BGP7DU(P,BDATE,EDATE,E)
 I %]"" Q "1^QUANT UP-CPT^"_$P(%,U,2)
 S %="",E=+$$CODEN^ICPTCOD(82042),%=$$CPTI^BGP7DU(P,BDATE,EDATE,E)
 I %]"" Q "1^QUANT UP-CPT^"_$P(%,U,2)
 S %="",E=+$$CODEN^ICPTCOD(84156),%=$$CPTI^BGP7DU(P,BDATE,EDATE,E)
 I %]"" Q "1^QUANT UP-CPT^"_$P(%,U,2)
 S %="",E=+$$CODEN^ICPTCOD(82043),%=$$TRANI^BGP7DU(P,BDATE,EDATE,E)
 I %]"" Q "1^QUANT UP-TRAN^"_$P(%,U,2)
 S %="",E=+$$CODEN^ICPTCOD(82042),%=$$TRANI^BGP7DU(P,BDATE,EDATE,E)
 I %]"" Q "1^QUANT UP-TRAN^"_$P(%,U,2)
 S %="",E=+$$CODEN^ICPTCOD(84156),%=$$TRANI^BGP7DU(P,BDATE,EDATE,E)
 I %]"" Q "1^QUANT UP-TRAN^"_$P(%,U,2)
 K BGPG S %=P_"^LAST LAB [BGP QUANT URINE PROTEIN;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(%,"BGPG(")
 I $D(BGPG(1)) Q 1_"^"_$P(BGPG(1),U,3)_"^"_$P(BGPG(1),U)
 S T=$O(^ATXAX("B","BGP QUANT URINE PROT LOINC",0))
 S BGPC="",B=9999999-BDATE,E=9999999-EDATE S D=E-1 F  S D=$O(^AUPNVLAB("AE",P,D)) Q:D'=+D!(D>B)!(BGPC]"")  D
 .S L=0 F  S L=$O(^AUPNVLAB("AE",P,D,L)) Q:L'=+L!(BGPC]"")  D
 ..S X=0 F  S X=$O(^AUPNVLAB("AE",P,D,L,X)) Q:X'=+X!(BGPC]"")  D
 ...Q:'T
 ...S J=$P($G(^AUPNVLAB(X,11)),U,13) Q:J=""
 ...Q:'$$LOINC(J,T)
 ...S %=$P(^AUPNVLAB(X,0),U,4)
 ...S BGPC=1_U_%_U_(9999999-D)
 I BGPC]"" Q BGPC
 Q ""
LOINC(A,B) ;EP
 NEW %
 S %=$P($G(^LAB(95.3,A,9999999)),U,2)
 I %]"",$D(^ATXAX(B,21,"B",%)) Q 1
 S %=$P($G(^LAB(95.3,A,0)),U)_"-"_$P($G(^LAB(95.3,A,0)),U,15)
 I $D(^ATXAX(B,21,"B",%)) Q 1
 Q ""
ESRD(P,BDATE,EDATE) ;EP
 K BGPG S %=P_"^LAST DX 585.5;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(%,"BGPG(")
 I $D(BGPG(1)) Q "1^ESRD 585.5^"_$P(BGPG(1),U)
 K BGPG S %=P_"^LAST DX 585.6;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(%,"BGPG(")
 I $D(BGPG(1)) Q "1^ESRD 585.6^"_$P(BGPG(1),U)
 K BGPG S %=P_"^LAST DX V45.1;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(%,"BGPG(")
 I $D(BGPG(1)) Q "1^ESRD V45.1^"_$P(BGPG(1),U)
 S T=$O(^ATXAX("B","BGP ESRD CPTS",0))
 I T D  I X]"" Q 1_U_"ESRD "_$P(X,U,2)_U_$P(X,U,1)
 .S X=$$CPT^BGP7DU(P,$$DOB^AUPNPAT(P),EDATE,T,5) I X]"" Q
 .S X=$$TRAN^BGP7DU(P,$$DOB^AUPNPAT(P),EDATE,T,5)
 Q 0
