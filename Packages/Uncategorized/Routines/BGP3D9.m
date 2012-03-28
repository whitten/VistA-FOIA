BGP3D9 ; IHS/CMI/LAB - indicator J ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;
J1 ;EP
 S (BGPN1,BGPN2,BGPN3,BGPN4,BGPN5,BGPN6,BGPD1,BGPD2,BGPD3,BGPD4,BGPD5,BGPD6,BGPD7,BGPD8,BGPD9,BGPHOSP)=0
 I BGPIHD S BGPD1=1
 I 'BGPD1 S BGPSTOP=1 Q  ;not ihd patient
 S (BGPVALUE,BGPLP,BGPLDL,BGPHDL,BGPTRI)=""
 S BGPLP=$$LIPID(DFN,$$FMADD^XLFDT(BGPEDATE,-(5*365)),BGPEDATE)
 I BGPLP]"" S BGPN1=1
 S BGPLDL=$$LDL(DFN,$$FMADD^XLFDT(BGPEDATE,-(5*365)),BGPEDATE) ;date^value
 I BGPN1'=1 D
 .S BGPTRI=$$TRIG(DFN,$$FMADD^XLFDT(BGPEDATE,-(5*365)),BGPEDATE)
 .S BGPHDL=$$HDL(DFN,$$FMADD^XLFDT(BGPEDATE,-(5*365)),BGPEDATE)
 .I BGPTRI,BGPHDL,BGPLDL S BGPN1=1
 I $P(BGPLDL,U,2)]"" D
 .S V=$P(BGPLDL,U,2)
 .I V]"",+V'>100 S BGPN2=1
 .I +V>100.99999,+V<131 S BGPN3=1
 .I +V>130.99999,+V<161 S BGPN4=1
 .I +V>160.9999 S BGPN5=1
 S BGPDV=$S(BGPD1:"1,",1:"")_$S(BGPD2:"2,",1:"")_$S(BGPD3:"3",1:"")_"; "
 S BGPVALUE=BGPDV_$S(BGPN1:"LP; ",1:"")
 S %=$S($P(BGPLDL,U,3):$$DATE^BGP3UTL($P(BGPLDL,U,3))_" "_$S($P(BGPLDL,U,2)]"":$P(BGPLDL,U,2),1:"no result"),1:"")
 S BGPVALUE=BGPVALUE_%
 K X,Y,Z,%,A,B,C,D,E,H,BDATE,EDATE,P,V,S,F,T
 Q
J2 ;EP
 S (BGPN1,BGPN2,BGPN3,BGPN4,BGPN5,BGPN6,BGPD1,BGPD2,BGPD3,BGPD4,BGPD5,BGPD6,BGPD7,BGPD8,BGPD9,BGPHOSP)=0
 I BGPIHD S BGPD1=1
 I 'BGPD1 S BGPSTOP=1 Q  ;not ihd patient
 S BGPVALUE=$$MEANBP(DFN,BGP365,BGPEDATE)
 S BGPN1=$S($P(BGPVALUE,U,2)=1:1,1:0)
 S BGPN2=$S($P(BGPVALUE,U,2)=2:1,1:0)
 S BGPN3=$S($P(BGPVALUE,U,2)=3:1,1:0)
 S BGPN4=$S($P(BGPVALUE,U,2)=4:1,1:0)
 S BGPN5=$S($P(BGPVALUE,U,2)=5:1,1:0)
 S BGPVALUE=$S(BGPD1:"1,",1:"")_$S(BGPD2:"2,",1:"")_$S(BGPD3:"3",1:"")_"; "_$P(BGPVALUE,U)
 Q
J3 ;EP
 S (BGPN1,BGPN2,BGPN3,BGPN4,BGPN5,BGPN6,BGPD1,BGPD2,BGPD3,BGPD4,BGPD5,BGPD6,BGPD7,BGPD8,BGPD9,BGPHOSP)=0
 I BGPIHD S BGPD1=1
 I 'BGPD1 S BGPSTOP=1 Q  ;not ihd patient
 S BGPVALUE=$$TOBACCO(DFN,BGP365,BGPEDATE),BGPN1=$S(BGPVALUE]"":1,1:0)
 S BGPSDX=$$DX(DFN,BGP365,BGPEDATE)
 I BGPSDX]"" S BGPN1=1
 S F=$P(BGPVALUE,U)
 I F["CURRENT"!(BGPSDX]"") S BGPN2=1
 I $$PED^BGP3D6(DFN,BGP365,BGPEDATE) S BGPN3=1
 I $$CL94(DFN,BGP365,BGPEDATE) S BGPN4=1
 I F["CESSATION"!(F["PREVIOUS") S BGPN5=1
 S V=""
 S BGPVALUE=$P(BGPVALUE,U,2)_" "_$P(BGPVALUE,U,1)_" "_$S(BGPSDX]"":" ;"_BGPSDX,1:"")
 K X,Y,Z,%,A,B,C,D,E,H,BDATE,EDATE,P,V,S,F
 Q
DX(P,BDATE,EDATE) ;
 K BGPG
 S X=P_"^LAST DX [BGP GPRA SMOKING DXS;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE) S E=$$START1^APCLDF(X,"BGPG(")
 I $D(BGPG(1)) Q $P(BGPG(1),U,2)
 S T=$O(^ATXAX("B","BGP GPRA SMOKING DXS",0))
 S X=0,G="" F  S X=$O(^AUPNPROB("AC",P,X)) Q:X'=+X!(G]"")  D
 .Q:$P(^AUPNPROB(X,0),U,8)>EDATE
 .S Y=$P(^AUPNPROB(X,0),U)
 .Q:'$$ICD^ATXCHK(Y,T,9)
 .;S G=$P(^ICD9(Y,0),U)
 .S G=$P($$ICDDX^ICDCODE(Y),U,2)
 .Q
 Q G
PED(P,BDATE,EDATE) ;EP
 K BGPG
 S Y="BGPG("
 S X=P_"^FIRST EDUC TO-Q;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE) S E=$$START1^APCLDF(X,Y)
 I $D(BGPG(1)) Q 1
 S X=P_"^FIRST EDUC TO-LA;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE) S E=$$START1^APCLDF(X,Y)
 I $D(BGPG(1)) Q 1
 K ^TMP($J,"A")
 S A="^TMP($J,""A"",",B=P_"^ALL VISITS;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(B,A)
 I '$D(^TMP($J,"A",1)) Q ""
 S (X,G)=0 F  S X=$O(^TMP($J,"A",X)) Q:X'=+X!(G)  S V=$P(^TMP($J,"A",X),U,5) D
 .Q:'$D(^AUPNVSIT(V,0))
 .Q:'$P(^AUPNVSIT(V,0),U,9)
 .Q:$P(^AUPNVSIT(V,0),U,11)
 .S Z=0 F  S Z=$O(^AUPNVDEN("AD",V,Z)) Q:Z'=+Z!(G)  S B=$P($G(^AUPNVDEN(Z,0)),U) I B S B=$P($G(^AUTTADA(B,0)),U) I B=1320 S G=G+1
 .Q
 Q $S(G:1,1:"")
CL94(P,BDATE,EDATE) ;
 K ^TMP($J,"A")
 S A="^TMP($J,""A"",",B=P_"^ALL VISITS;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(B,A)
 I '$D(^TMP($J,"A",1)) Q ""
 S (X,G)=0 F  S X=$O(^TMP($J,"A",X)) Q:X'=+X!(G)  S V=$P(^TMP($J,"A",X),U,5) D
 .Q:'$D(^AUPNVSIT(V,0))
 .Q:'$P(^AUPNVSIT(V,0),U,9)
 .Q:$P(^AUPNVSIT(V,0),U,11)
 .S B=$$CLINIC^APCLV(V,"C")
 .I B=94 S G=G+1 Q
 .Q
 Q $S(G:1,1:"")
TOBACCO(P,BDATE,EDATE) ;EP
 K BGPTOB,BGP
 D TOBACCO1
 I BGPTOB]"" Q BGPTOB
 D TOBACCO0
 I $D(BGPTOB) Q BGPTOB
 Q ""
TOBACCO1 ;check for tobacco documented in health factors
 K BGPTOB S BGPTOB=$$LASTHF(P,"TOBACCO",BDATE,EDATE) K O,D,H
 Q
TOBACCO0 ;lookup in health status
 S (X,Y)=0 F  S X=$O(^AUPNHF("AA",P,X)) Q:X'=+X!(Y)  I $$VAL^XBDIQ1(9999999.64,X,.03)="TOBACCO" S Y=X
 Q:'Y
 S E=$O(^AUPNHF("AA",P,Y,0)) Q:'E
 I (9999999-E)>EDATE Q  ;documented after time frame
 I (9999999-E)<BDATE Q  ;documented before year
 S Y=$P(^AUTTHF(Y,0),U)
 S BGPTOB=Y_"^"_$$DATE^BGP3UTL(9999999-E)
 K Y,E,X
 Q
 ;
LASTHF(P,C,BDATE,EDATE) ;EP - get last factor in category C for patient P
 S C=$O(^AUTTHF("B",C,0)) ;ien of category passed
 I '$G(C) Q ""
 S (H,D)=0 K O
 F  S H=$O(^AUTTHF("AC",C,H))  Q:'+H  D
 .Q:'$D(^AUPNVHF("AA",P,H))
 .S D="" F  S D=$O(^AUPNVHF("AA",P,H,D)) Q:D'=+D  D
 ..Q:(9999999-D)>EDATE  ;after time frame
 ..Q:(9999999-D)<BDATE  ;before time frame
 ..S O(D)=$O(^AUPNVHF("AA",P,H,D,""))
 .Q
 S D=$O(O(0))
 I D="" Q D
 Q $$VAL^XBDIQ1(9000010.23,O(D),.01)_"^"_$$DATE^BGP3UTL(9999999-D)
 ;
MEANBP(P,BDATE,EDATE) ;
 S X=$$BPS(P,BDATE,EDATE,"I")
 S S=$$SYSMEAN(X) I S="" Q "u^5"
 S DS=$$DIAMEAN(X) I DS="" Q "u^5"
 I S>159!(DS>100) Q S_"/"_DS_" SUNC"_U_4
 I S>139&(S<160)!(DS>90&(DS<101)) Q S_"/"_DS_" UNC"_U_3
 I S>130&(S<140)!(DS>80&(DS<91)) Q S_"/"_DS_" CON"_U_2
 I S<131&(DS<81) Q S_"/"_DS_" OPT"_U_1
 Q ""
 ;
SYSMEAN(X) ;EP
 I X="" Q ""
 S C=0 F Y=1:1:2 I $P(X,";",Y)]"" S C=C+1
 I C'=2 Q ""
 S C=0 F Y=1:1:2 S C=$P($P(X,";",Y),"/")+C
 Q C\2
 ;
DIAMEAN(X) ;EP
 I X="" Q ""
 S C=0 F Y=1:1:2 I $P(X,";",Y)]"" S C=C+1
 I C'=2 Q ""
 S C=0 F Y=1:1:2 S C=$P($P(X,";",Y),"/",2)+C
 Q C\2
 ;
BPS(P,BDATE,EDATE,F) ;EP ;
 I $G(F)="" S F="E"
 S BGPGLL=0,BGPGV=""
 K BGPG
 S X=P_"^LAST 30 MEAS BP;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE) S E=$$START1^APCLDF(X,"BGPG(")
 S BGPGL=0 F  S BGPGL=$O(BGPG(BGPGL)) Q:BGPGL'=+BGPGL!(BGPGLL=2)  S BGPGBP=$P($G(BGPG(BGPGL)),U,2) D
 .Q:$$CLINIC^APCLV($P(BGPG(BGPGL),U,5),"C")=30
 .S BGPGLL=BGPGLL+1
 .I F="E" S $P(BGPGV,";",BGPGLL)=BGPGBP_"  "_$$FMTE^XLFDT($P(BGPG(BGPGL),U))
 .I F="I" S $P(BGPGV,";",BGPGLL)=$P(BGPGBP," ")
 Q BGPGV
FIRSTIHD(P,EDATE) ;EP
 I $G(P)="" Q ""
 K BGPG
 S Y="BGPG("
 S X=P_"^FIRST DX [BGP ISCHEMIC HEART DXS" S E=$$START1^APCLDF(X,Y)
 I '$D(BGPG(1)) Q ""
 S X=$$FMDIFF^XLFDT(EDATE,$P(BGPG(1),U))
 Q $S(X>365:1,1:"")
 ;
V2IHD(P,BDATE,EDATE) ;EP
 I '$G(P) Q ""
 I '$D(^AUPNVSIT("AC",P)) Q ""
 K ^TMP($J,"A")
 S A="^TMP($J,""A"",",B=P_"^ALL VISITS;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(B,A)
 I '$D(^TMP($J,"A",1)) Q ""
 S T=$O(^ATXAX("B","BGP ISCHEMIC HEART DXS",0))
 I 'T Q ""
 S (X,G)=0 F  S X=$O(^TMP($J,"A",X)) Q:X'=+X!(G>2)  S V=$P(^TMP($J,"A",X),U,5) D
 .Q:'$D(^AUPNVSIT(V,0))
 .Q:'$P(^AUPNVSIT(V,0),U,9)
 .Q:$P(^AUPNVSIT(V,0),U,11)
 .Q:"SAHO"'[$P(^AUPNVSIT(V,0),U,7)
 .S (D,Y)=0 F  S Y=$O(^AUPNVPOV("AD",V,Y)) Q:Y'=+Y!(D)  I $D(^AUPNVPOV(Y,0)) S %=$P(^AUPNVPOV(Y,0),U) I $$ICD^ATXCHK(%,T,9) S D=1
 .Q:'D
 .S G=G+1
 .Q
 Q $S(G<2:"",1:1)
LIPID(P,BDATE,EDATE) ;
 K BGPG
 S %=P_"^LAST LAB [DM AUDIT LIPID PROFILE TAX;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(%,"BGPG(")
 I $D(BGPG(1)) Q 1
 S %="",E=+$$CODEN^ICPTCOD(80061),%=$$CPTI^BGPDU(P,BDATE,EDATE,E)
 Q %
 ;
TRIG(P,BDATE,EDATE) ;
 K BGPG
 S %=P_"^LAST LAB [DM AUDIT TRIGLYCERIDE TAX;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(%,"BGPG(")
 I $D(BGPG(1)) Q 1
 S E=+$$CODEN^ICPTCOD(84478),%=$$CPTI^BGPDU(P,BDATE,EDATE,E)
 Q %
HDL(P,BDATE,EDATE) ;
 K BGPG
 S %=P_"^LAST LAB [DM AUDIT HDL TAX;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(%,"BGPG(")
 I $D(BGPG(1)) Q 1
 S %="",E=+$$CODEN^ICPTCOD(83718),%=$$CPTI^BGPDU(P,BDATE,EDATE,E)
 Q %
 ;
LDL(P,BDATE,EDATE) ;
 K BGPZ
 K BGPG
 S %=P_"^ALL LAB [DM AUDIT LDL CHOLESTEROL TAX;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(%,"BGPG(")
 I '$D(BGPG(1)) G LDLCPT
 ;reorder by date
 NEW X S X=0 F  S X=$O(BGPG(X)) Q:X'=+X  S BGPZ(9999999-$P(BGPG(X),U),X)=BGPG(X)
 S %="" D
 .S (D,X,G)=0 F  S D=$O(BGPZ(D)) Q:D'=+D!(G)  S X=0 F  S X=$O(BGPZ(D,X)) Q:X'=+X!(G)  D
 ..S E=$P(^AUPNVLAB(+$P(BGPZ(D,X),U,4),0),U,4)
 ..I +E S %="1^"_E_"^"_$P(BGPZ(D,X),U) S G=1 Q
 I %]"" Q %
 S D=$O(BGPZ("")),X=$O(BGPZ(D,0)) Q 1_"^^"_$P(BGPZ(D,X),U)
LDLCPT ;
 S E=+$$CODEN^ICPTCOD(83718),%=$$CPTI^BGPDU(P,BDATE,EDATE,E)
 Q $P(%,U)_"^^"_$P(%,U,2)
