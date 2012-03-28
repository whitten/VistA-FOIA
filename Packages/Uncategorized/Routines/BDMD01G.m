BDMD01G ; IHS/CMI/LAB - calc indicators 16 Jan 2007 8:13 PM ;
 ;;2.0;DIABETES MANAGEMENT SYSTEM;**3**;JUN 14, 2007
 ;
ACTDMPT(DFN,BDMBDAT,BDMEDAT,BDMCOMT,BDMBEN,BDM3YE) ;EP
 K BDM365,BDMACUP,BDMACCL,BDMAGEB,BDMAGEE,BDMSEX,BDMDM1,BDMDM2,BDMFDMD,BDM2V,BDM2DMV,BDM1DMV,BDMDMD1,BDMDMD2,BDMDMD3,BDMDMD5
 S BDM365=BDMBDAT,BDMDMD2=0
 S BDMACUP=$$ACTUP(DFN,BDM3YE,BDMEDAT,BDMCOMT,BDMBEN) ;user pop
 I 'BDMACUP Q 0
 S BDMACCL=$$ACTCL(DFN,BDM3YE,BDMEDAT) ;active clinical
 I 'BDMACCL Q 0 ;not active clinical
 S BDMAGEB=$$AGE^AUPNPAT(DFN,BDMBDAT)
 S BDMAGEE=$$AGE^AUPNPAT(DFN,BDMEDAT)
 S BDMSEX=$P(^DPT(DFN,0),U,2)
 S BDMDM1=$$DM(DFN,,BDMEDAT) ;dm diagnosis ever?
 S BDMDM2=$$DM(DFN,BDM365,BDMEDAT) ;dm diagnosis in past year
 I BDMDM1 D  ;set up 4 dm demoninators
 .S BDMFDMD=$$FIRSTDM(DFN,BDMEDAT) ;1 OR 0 FIRST DX BEFORE BEG
 .S BDM2V=$$V2(DFN,BDM365,BDMEDAT)
 .S BDM2DMV=$$V2DM(DFN,$$DOB^AUPNPAT(DFN),BDMEDAT)
 .S BDM1DMV=$$V1DM(DFN,BDM365,BDMEDAT)
 .S BDMDMD2=0 I BDMFDMD,BDM2V,BDM2DMV,BDMACCL S BDMDMD2=1
 K ^TMP($J,"A")
 K BDM365,BDMACUP,BDMACCL,BDMAGEB,BDMAGEE,BDMSEX,BDMDM1,BDMDM2,BDMFDMD,BDM2V,BDM2DMV,BDM1DMV,BDMDMD1
 Q BDMDMD2
V1DM(P,D,EDATE) ;
 I '$G(P) Q ""
 I '$D(^AUPNVSIT("AC",P)) Q ""
 S PC=$O(^ATXAX("B","BGP PRIMARY CARE CLINICS",0))
 I 'PC Q ""
 ;I 'PP Q ""
 K ^TMP($J,"A")
 S A="^TMP($J,""A"",",B=P_"^ALL VISITS;DURING "_$$FMTE^XLFDT(D)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(B,A)
 I '$D(^TMP($J,"A",1)) Q ""
 S T=$O(^ATXAX("B","SURVEILLANCE DIABETES",0))
 I 'T Q ""
 S (X,G)=0 F  S X=$O(^TMP($J,"A",X)) Q:X'=+X!(G>2)  S V=$P(^TMP($J,"A",X),U,5) D
 .Q:'$D(^AUPNVSIT(V,0))
 .Q:'$P(^AUPNVSIT(V,0),U,9)
 .Q:$P(^AUPNVSIT(V,0),U,11)
 .Q:"SAHO"'[$P(^AUPNVSIT(V,0),U,7)
 .Q:'$D(^AUPNVPRV("AD",V))
 .;Q:$P(^AUPNVSIT(V,0),U,6)'=DUZ(2) ;PER TERRY
 .Q:'$D(^AUPNVPOV("AD",V))
 .S (D,Y)=0 F  S Y=$O(^AUPNVPOV("AD",V,Y)) Q:Y'=+Y!(D)  I $D(^AUPNVPOV(Y,0)) S %=$P(^AUPNVPOV(Y,0),U) I $$ICD^ATXCHK(%,T,9) S D=1
 .Q:'D
 .Q:"V"[$P(^AUPNVSIT(V,0),U,3)
 .S Y=$$PRIMPROV^APCLV(V,"F")
 .Q:'Y
 .;Q:'$D(^ATXAX(PP,21,"B",Y))
 .Q:$P($G(^DIC(7,Y,9999999)),U,3)'="Y"
 .S Y=$$CLINIC^APCLV(V,"I")
 .Q:'Y
 .Q:'$D(^ATXAX(PC,21,"B",Y))
 .S G=G+1
 .Q
 Q $S(G<1:"",1:1)
 ;
V2(P,BDATE,EDATE) ;
 I '$D(^AUPNVSIT("AC",P)) Q ""
 K ^TMP($J,"A")
 S A="^TMP($J,""A"",",B=P_"^ALL VISITS;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(B,A)
 I '$D(^TMP($J,"A",1)) Q ""
 S (X,G)=0 F  S X=$O(^TMP($J,"A",X)) Q:X'=+X!(G>2)  S V=$P(^TMP($J,"A",X),U,5) D
 .Q:'$D(^AUPNVSIT(V,0))
 .Q:'$P(^AUPNVSIT(V,0),U,9)
 .Q:$P(^AUPNVSIT(V,0),U,11)
 .Q:"SAHO"'[$P(^AUPNVSIT(V,0),U,7)
 .Q:"V"[$P(^AUPNVSIT(V,0),U,3)
 .S G=G+1
 .Q
 Q $S(G<2:"",1:1)
 ;
V2DM(P,BDATE,EDATE) ;
 I '$G(P) Q ""
 I '$D(^AUPNVSIT("AC",P)) Q ""
 K ^TMP($J,"A")
 S A="^TMP($J,""A"",",B=P_"^ALL VISITS;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(B,A)
 I '$D(^TMP($J,"A",1)) Q ""
 S T=$O(^ATXAX("B","SURVEILLANCE DIABETES",0))
 I 'T Q ""
 S (X,G)=0 F  S X=$O(^TMP($J,"A",X)) Q:X'=+X!(G>2)  S V=$P(^TMP($J,"A",X),U,5) D
 .Q:'$D(^AUPNVSIT(V,0))
 .Q:'$P(^AUPNVSIT(V,0),U,9)
 .Q:$P(^AUPNVSIT(V,0),U,11)
 .Q:"SAHO"'[$P(^AUPNVSIT(V,0),U,7)
 .Q:"V"[$P(^AUPNVSIT(V,0),U,3)
 .S (D,Y)=0 F  S Y=$O(^AUPNVPOV("AD",V,Y)) Q:Y'=+Y!(D)  I $D(^AUPNVPOV(Y,0)) S %=$P(^AUPNVPOV(Y,0),U) I $$ICD^ATXCHK(%,T,9) S D=1
 .Q:'D
 .S G=G+1
 .Q
 Q $S(G<2:"",1:1)
 ;
DM(P,BDATE,EDATE) ;EP is patient diabetic 1 or 0
 I $G(BDATE)="" S BDATE=$$DOB^AUPNPAT(P)
 K BDMG
 S Y="BDMG("
 S X=P_"^LAST DX [SURVEILLANCE DIABETES;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE) S E=$$START1^APCLDF(X,Y)
 I $D(BDMG(1)) Q 1  ;has a dx
 Q 0
 ;
ACTUP(P,BDATE,EDATE,T,B) ;EP - is this patient in user pop?
 I B=1,$$BEN^AUPNPAT(P,"C")'="01" Q 0  ;must be Indian/Alaskan Native
 I B=2,$$BEN^AUPNPAT(P,"C")="01" Q 0  ;must not be I/A
 S DOD=$$DOD^AUPNPAT(P) I DOD]"",DOD<EDATE Q 0
 S X=$P($G(^AUPNPAT(P,11)),U,18) I X="" Q 0
 I '$D(^ATXAX(T,21,"B",($P(^AUPNPAT(P,11),U,18)))),'$D(^ATXAX(T,21,"AA",$P(^AUPNPAT(P,11),U,18),$P(^AUPNPAT(P,11),U,18))) Q 0
 S X=$$LASTVD(P,BDATE,EDATE)
 Q $S(X:1,1:0)
 ;
ACTCL(P,BDATE,EDATE) ;EP - clinical user
 NEW CL S CL=$O(^BDMCNTL("B","DM AUDIT 2005 CLINIC LIST 1",0))
 NEW CL2 S CL2=$O(^BDMCNTL("B","DM AUDIT 2005 CLINIC LIST 2",0))
 S (X,G,F,S)=0 F  S X=$O(^TMP($J,"A",X)) Q:X'=+X!(F)  S V=$P(^TMP($J,"A",X),U,5) D
 .Q:'$D(^AUPNVSIT(V,0))
 .Q:'$P(^AUPNVSIT(V,0),U,9)
 .Q:$P(^AUPNVSIT(V,0),U,11)
 .Q:'$D(^AUPNVPRV("AD",V))
 .Q:"SAHO"'[$P(^AUPNVSIT(V,0),U,7)
 .Q:"V"[$P(^AUPNVSIT(V,0),U,3)
 .S B=$$CLINIC^APCLV(V,"C")
 .Q:B=""
 .I 'G,$D(^BDMCNTL(CL,11,"B",B)) S G=V  ;must be a primary clinic S G=V
 .I V'=G,$D(^BDMCNTL(CL2,11,"B",B)) S S=1
 .I G,S S F=1
 .Q
 Q $S(F:1,1:0)
 ;
FIRSTDM(P,EDATE) ;
 I $G(P)="" Q ""
 K BDMG
 S Y="BDMG("
 S X=P_"^FIRST DX [SURVEILLANCE DIABETES" S E=$$START1^APCLDF(X,Y)
 I '$D(BDMG(1)) Q ""
 S X=$$FMDIFF^XLFDT(EDATE,$P(BDMG(1),U))
 Q $S(X>365:1,1:"")
 ;
LASTVD(P,BDATE,EDATE) ;
 I '$D(^AUPNVSIT("AC",P)) Q ""
 K ^TMP($J,"A")
 S A="^TMP($J,""A"",",B=P_"^ALL VISITS;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(B,A)
 I '$D(^TMP($J,"A",1)) Q ""
 S (X,G)=0 F  S X=$O(^TMP($J,"A",X)) Q:X'=+X!(G)  S V=$P(^TMP($J,"A",X),U,5) D
 .Q:'$D(^AUPNVSIT(V,0))
 .Q:'$P(^AUPNVSIT(V,0),U,9)
 .Q:$P(^AUPNVSIT(V,0),U,11)
 .Q:'$D(^AUPNVPRV("AD",V))
 .Q:"SAHO"'[$P(^AUPNVSIT(V,0),U,7)
 .Q:"V"[$P(^AUPNVSIT(V,0),U,3)
 .S G=1
 .Q
 Q G
 ;
LOINC(A,B) ;
 NEW %
 S %=$P($G(^LAB(95.3,A,9999999)),U,2)
 I %]"",$D(^ATXAX(B,21,"B",%)) Q 1
 S %=$P($G(^LAB(95.3,A,0)),U)_"-"_$P($G(^LAB(95.3,A,0)),U,15)
 I $D(^ATXAX(B,21,"B",%)) Q 1
 Q ""
