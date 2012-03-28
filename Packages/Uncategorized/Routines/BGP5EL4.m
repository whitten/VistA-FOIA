BGP5EL4 ; IHS/CMI/LAB - indicator 1,2,3,4 05 Apr 2005 1:44 PM ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;
FUNCTION(P,BDATE,EDATE) ;EP
 S BD=(9999999-BDATE)
 S ED=(9999999-EDATE)-1
 K BGPG
 S (BGP1,BGP2)=0
 F  S ED=$O(^AUPNVELD("AA",P,ED)) Q:ED=""!(ED>BD)  D
 .S BGPX=0 F  S BGPX=$O(^AUPNVELD("AA",P,ED,BGPX)) Q:BGPX'=+BGPX  D
 ..S Y=^AUPNVELD(BGPX,0)
 ..I $P(Y,U,4)]"" S:'$O(BGPG("TLT",0)) BGPG("TLT",ED)="",BGP1=1
 ..I $P(Y,U,5)]"" S:'$O(BGPG("BATH",0)) BGPG("BATH",ED)="",BGP1=1
 ..I $P(Y,U,6)]"" S:'$O(BGPG("DRES",0)) BGPG("DRES",ED)="",BGP1=1
 ..I $P(Y,U,7)]"" S:'$O(BGPG("XFER",0)) BGPG("XFER",ED)="",BGP1=1
 ..I $P(Y,U,8)]"" S:'$O(BGPG("FEED",0)) BGPG("FEED",ED)="",BGP1=1
 ..I $P(Y,U,9)]"" S:'$O(BGPG("CONT",0)) BGPG("CONT",ED)="",BGP1=1
 ..I $P(Y,U,11)]"" S:'$O(BGPG("FIN",0)) BGPG("FIN",ED)="",BGP2=1
 ..I $P(Y,U,12)]"" S:'$O(BGPG("COOK",0)) BGPG("COOK",ED)="",BGP2=1
 ..I $P(Y,U,13)]"" S:'$O(BGPG("SHOP",0)) BGPG("SHOP",ED)="",BGP2=1
 ..I $P(Y,U,14)]"" S:'$O(BGPG("HSWK",0)) BGPG("HSWK",ED)="",BGP2=1
 ..I $P(Y,U,15)]"" S:'$O(BGPG("MEDS",0)) BGPG("MEDS",ED)="",BGP2=1
 ..I $P(Y,U,16)]"" S:'$O(BGPG("TRNS",0)) BGPG("TRNS",ED)="",BGP2=1
 K BGPV
 S X="" F  S X=$O(BGPG(X)) Q:X=""  S ED=$O(BGPG(X,0)) S:$D(BGPV(ED)) BGPV(ED)=BGPV(ED)_";" S BGPV(ED)=$G(BGPV(ED))_X
 S BGPQ=""
 S BGPQ=$S((BGP1+BGP2)=2:1,1:0)_U
 S Y=0 F  S Y=$O(BGPV(Y)) Q:Y'=+Y  D
 .S $P(BGPQ,U,2)=$P(BGPQ,U,2)_$$DATE^BGP5UTL((9999999-Y))_": "_BGPV(Y)_" "
 K BGPV,BGPG
 Q BGPQ
 ;
PHNV(P,BDATE,EDATE,HOMELOC) ;EP
 S HOMELOC=$G(HOMELOC)
 K ^TMP($J,"A") S A="^TMP($J,""A"","
 S B=P_"^ALL VISITS;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(B,A)
 I '$D(^TMP($J,"A",1)) Q "0^0^0^0^0^0^0^0^0^0^0^0"
 S (X,Y)=0,C="0^0^0^0^0^0^0^0^0^0^0^0" F  S X=$O(^TMP($J,"A",X)) Q:X'=+X  S V=$P(^TMP($J,"A",X),U,5) D
 .;S Y=0 I $$CLINIC^APCLV(V,"C")=45 S Y=1 Q
 .S (D,Y,Z)=0
 .F  S D=$O(^AUPNVPRV("AD",V,D)) Q:D'=+D  S Q=$P(^AUPNVPRV(D,0),U) D
 ..Q:Q=""
 ..S %=$$VALI^XBDIQ1($S($P(^DD(9000010.06,.01,0),U,2)["200":200,1:6),Q,$S($P(^DD(9000010.06,.01,0),U,2)["200":53.5,1:2))
 ..I % S %=$P($G(^DIC(7,+%,9999999)),U)
 ..I %'=13,%'=91 Q  ;not a phn or driver
 ..S $P(C,U,1)=$P(C,U,1)+1
 ..I %=91 S $P(C,U,6)=$P(C,U,6)+1
 ..D HOME
 ..D AGE
 Q C
 ;
HOME ;
 S HV=0
 I $$CLINIC^APCLV(V,"C")=11 S $P(C,U,7)=$P(C,U,7)+1,HV=1 S:%=91 $P(C,U,12)=$P(C,U,12)+1 Q
 Q:HOMELOC=""
 I HOMELOC=$P(^AUPNVSIT(V,0),U,6) S $P(C,U,7)=$P(C,U,7)+1,HV=1 S:%=91 $P(C,U,12)=$P(C,U,12)+1 Q
 Q
AGE ;
 I BGPD2 S $P(C,U,2)=$P(C,U,2)+1 S:HV=1 $P(C,U,8)=$P(C,U,8)+1 Q
 I BGPD3 S $P(C,U,3)=$P(C,U,3)+1 S:HV=1 $P(C,U,9)=$P(C,U,9)+1 Q
 I BGPD4 S $P(C,U,4)=$P(C,U,4)+1 S:HV=1 $P(C,U,10)=$P(C,U,10)+1 Q
 I BGPD5 S $P(C,U,5)=$P(C,U,5)+1 S:HV=1 $P(C,U,11)=$P(C,U,11)+1 Q
 W BGPBOMB
 Q
TXBMD(P,BDATE,EDATE,HOSP) ;EP
 ;first see if there are any procedures in this date range
 S HOSP=$G(HOSP)
 K BGPG
 S BGPG=$$LASTPRC^BGP5UTL1(P,"BGP BMD PROCEDURES",BDATE,EDATE)
 I $P(BGPG,U)=1 Q 1_U_"bmd proc "_$P(BGPG,U,2)_" "_$$DATE^BGP5UTL($P(BGPG,U,3))
 ;now check cpts
 S T=$O(^ATXAX("B","BGP BMD CPTS",0))
 S BGPG=$$CPT^BGPDU(P,BDATE,EDATE,T,5)
 I BGPG]"" Q 1_U_BGPG  ;had a cpt
 ;now check RAD
 S T=$O(^ATXAX("B","BGP BMD CPTS",0))
 S BGPG=$$RAD^BGPDU(P,BDATE,EDATE,T,5)
 I BGPG]"" Q 1_U_BGPG  ;had a cpt
 I HOSP Q ""
 ;now check all meds
 K ^TMP($J,"MEDS")
 S G=0 K BGPZ
 S Y="^TMP($J,""MEDS"",",X=P_"^ALL MED;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE) S E=$$START1^APCLDF(X,Y)
 S T=$O(^ATXAX("B","BGP HEDIS OSTEOPOROSIS DRUGS",0))
 S T1=$O(^ATXAX("B","BGP HEDIS OSTEOPOROSIS NDC",0))
 ;S T2="" I TAX3]"" S T2=$O(^ATXAX("B",TAX3,0))
 S X=0 F  S X=$O(^TMP($J,"MEDS",X)) Q:X'=+X!(G)  S Y=+$P(^TMP($J,"MEDS",X),U,4) D
 .Q:'$D(^AUPNVMED(Y,0))
 .S G=0
 .S D=$P(^AUPNVMED(Y,0),U)
 .S C=$P($G(^PSDRUG(D,0)),U,2)
 .;I C]"",T2,$D(^ATXAX(T2,21,"B",C)) S G=X
 .S C=$P($G(^PSDRUG(D,2)),U,4)
 .I C]"",T1,$D(^ATXAX(T1,21,"B",C)) S G=$P(^TMP($J,"MEDS",X),U)
 .I T,$D(^ATXAX(T,21,"B",D)) S G=$P(^TMP($J,"MEDS",X),U)
 .Q
 K ^TMP($J,"MEDS")
 I G Q 1_U_"osteo med: "_$$DATE^BGP5UTL(G)
 Q ""
