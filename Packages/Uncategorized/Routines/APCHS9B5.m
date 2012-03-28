APCHS9B5 ; IHS/CMI/LAB - DIABETIC CARE SUMMARY SUPPLEMENT ; 20 Sep 2010  1:37 PM
 ;;2.0;IHS PCC SUITE;**5**;MAY 14, 2009
 ;
 ;
MAM ;EP
 K APCHSDAT,APCHSTEX
 S APCHSDAT=""
 ;apchsdat=date of last, apchstex is display
 Q:$P(^DPT(APCHSPAT,0),U,2)="M"
 K APCHSEXD,APCHSDF1
 S APCHSTXN=0
 S APCHSDAT=$$LASTMAM^APCLAPI1(APCHSPAT)_"^"_$$MAMREF^APCHS9B4(APCHSPAT,APCHSDAT)
 I $$VERSION^XPDUTL("BW")>2.9 G MAMA
 S APCHSBWR=0 S:$D(X) APCHSAVX=X S X="BWUTL1" X ^%ZOSF("TEST") S:$D(APCHSAVX) X=APCHSAVX K APCHSAVX I $T S APCHSBWR=1
 I APCHSBWR,$D(^BWP(APCHSPAT,0)) S APCHSTXN=APCHSTXN+1,APCHSTEX(APCHSTXN)=$$BNEED^BWUTL1(APCHSPAT) I APCHSTEX(1)="UNKNOWN" K APCHSTEX(1) S APCHSTXN=0
 I $O(APCHSTEX("")) Q
MAMA ;
 Q:$$AGE^AUPNPAT(APCHSPAT,DT,"Y")<50
 Q:$$AGE^AUPNPAT(APCHSPAT,DT,"Y")>69
 K APCHSTXN
 S APCHSINT=365
 I $P(APCHSDAT,U,2)]"" S APCHSTEX(1)=$P(APCHSDAT,U,2),APCHSDAT=$P(APCHSDAT,U) Q
 I APCHSDAT="" S APCHSTEX(1)="MAY BE DUE NOW" Q
 K APCHSBWR
 S X1=APCHSDAT,X2=APCHSINT D C^%DTC S Y=X X APCHSCVD S APCHSTEX(1)="Next Due: "_Y,APCHSWD=Y
 S X2=APCHSDAT,X1=DT D ^%DTC I X>APCHSINT S APCHSTEX(1)=$S('$D(APCHSDD):"MAY BE DUE NOW (WAS DUE "_APCHSWD_")",1:"MAY BE DUE NOW")
 Q
 ;
 ;
PAP ;EP
 K APCHSDAT,APCHSTEX,APCHSTP
 S APCHSDAT=""
 ;apchsdat=date of last, apchstex is display
 Q:$$AGE^AUPNPAT(APCHSPAT,DT,"Y")<18!($P(^DPT(APCHSPAT,0),U,2)="M")
 K APCHSEXD,APCHSDF1
 S APCHSTXN=0
 I $$VERSION^XPDUTL("BW")>2.9 G PAPA
 S APCHSBWR=0 S:$D(X) APCHSAVX=X S X="BWUTL1" X ^%ZOSF("TEST") S:$D(APCHSAVX) X=APCHSAVX K APCHSAVX I $T S APCHSBWR=1
 I APCHSBWR,$D(^BWP(APCHSPAT,0)) S APCHSTXN=APCHSTXN+1,APCHSTEX(APCHSTXN)=$$CNEED^BWUTL1(APCHSPAT) I APCHSTEX(1)="UNKNOWN" K APCHSTEX(1) S APCHSTXN=0
PAPA ;
 S APCHSTP=$$HYSTER^APCHS9B4(APCHSPAT,DT)
 I APCHSTP]"" S APCHSTXN=APCHSTXN+1,APCHSTEX(APCHSTXN)="Pt had hysterectomy.  Pap may be necessary",APCHSTXN=APCHSTXN+1,APCHSTEX(APCHSTXN)="based on individual followup."
 I $O(APCHSTEX("")) S APCHSDAT="" Q
 Q
 ;
 ;
ACE(P,D) ;EP - return date of last ACE iNHIBITOR
 ;IHS/CMI/LAB patch 3 - added this subroutine
 ;go through all v meds until 9999999-D and find all drugs with class CV800 or CV805
 ;if none found check taxonomy
 I '$G(P) Q ""
 I '$G(D) S D=0 ;if don't pass date look at all time
 NEW V,I,%
 S %=""
 S I=0 F  S I=$O(^AUPNVMED("AA",P,I)) Q:I'=+I!(%)!(I>(9999999-D))  D
 .S V=0 F  S V=$O(^AUPNVMED("AA",P,I,V)) Q:V'=+V  I $D(^AUPNVMED(V,0)) S G=$P(^AUPNVMED(V,0),U) I $P($G(^PSDRUG(G,0)),U,2)="CV800"!($P($G(^PSDRUG(G,0)),U,2)="CV805") S %=V
 I %]"" D  Q %
 .I $P(^AUPNVMED(%,0),U,8)="" S %="Yes - "_$$FMTE^XLFDT($P($P(^AUPNVSIT($P(^AUPNVMED(%,0),U,3),0),U),".")) Q
 .I $P(^AUPNVMED(%,0),U,8)]"" S %="Discontinued - "_$$FMTE^XLFDT($P($P(^AUPNVSIT($P(^AUPNVMED(%,0),U,3),0),U),".")) Q
 NEW T S T=$O(^ATXAX("B","DM AUDIT ACE INHIBITORS",0))
 I 'T Q ""
 S I=0 F  S I=$O(^AUPNVMED("AA",P,I)) Q:I'=+I!(%)!(I>(9999999-D))  D
 .S V=0 F  S V=$O(^AUPNVMED("AA",P,I,V)) Q:V'=+V  I $D(^AUPNVMED(V,0)) S G=$P(^AUPNVMED(V,0),U) I $D(^ATXAX(T,21,"B",G)) S %=V
 I %]"" D  Q %
 .I $P(^AUPNVMED(%,0),U,8)="" S %="Yes - "_$$FMTE^XLFDT($P($P(^AUPNVSIT($P(^AUPNVMED(%,0),U,3),0),U),".")) Q
 .I $P(^AUPNVMED(%,0),U,8)]"" S %="Discontinued - "_$$FMTE^XLFDT($P($P(^AUPNVSIT($P(^AUPNVMED(%,0),U,3),0),U),".")) Q
 Q "No"
 ;
ASPREF(P) ;EP - CHECK FOR ASPIRIN NMI OR REFUSAL
 I '$G(P) Q ""
 NEW X,N,Z,D,IEN,DATE,DRUG
 K X
 S T=$O(^ATXAX("B","DM AUDIT ASPIRIN DRUGS",0))
 I 'T Q ""
 S (D,G)=0 F  S D=$O(^AUPNPREF("AA",P,50,D)) Q:D'=+D!(G)  D
 .Q:'$D(^ATXAX(T,21,"B",D))
 .S X=$O(^AUPNPREF("AA",P,50,D,0))
 .S N=$O(^AUPNPREF("AA",P,50,D,X,0))
 .S G=1,DATE=9999999-X,DRUG=D,IEN=N
 I 'G Q ""
 Q $$VAL^XBDIQ1(50,DRUG,.01)_" "_$$TYPEREF^APCHSMU(IEN)_" on "_$$FMTE^XLFDT(DATE)
PNEU(P) ;EP
 NEW APCHY,PNEU,X S %=P_"^LAST 2 IMMUNIZATION "_$S($$BI:33,1:19),E=$$START1^APCLDF(%,"APCHY(") ;IHS/CMI/LAB patch 3 - changed line to support new imm package
 I $D(APCHY(1)) S PNEU(9999999-$P(APCHY(1),U))=""
 I $D(APCHY(2)) S PNEU(9999999-$P(APCHY(2),U))=""
 K APCHY S %=P_"^LAST 2 IMMUNIZATION 100",E=$$START1^APCLDF(%,"APCHY(")
 I $D(APCHY(1)) S PNEU(9999999-$P(APCHY(1),U))=""
 I $D(APCHY(2)) S PNEU(9999999-$P(APCHY(2),U))=""
 K APCHY S %=P_"^LAST 2 IMMUNIZATION 109",E=$$START1^APCLDF(%,"APCHY(")
 I $D(APCHY(1)) S PNEU(9999999-$P(APCHY(1),U))=""
 I $D(APCHY(2)) S PNEU(9999999-$P(APCHY(2),U))=""
 K APCHY S X=0,C=0 F  S X=$O(PNEU(X)) Q:X'=+X!(C>2)  S C=C+1,APCHY(C)=9999999-X
 I $D(APCHY(1)) Q "Yes  "_$$FMTE^XLFDT($P(APCHY(1),U))_"   "_$$FMTE^XLFDT($P($G(APCHY(2)),U))
 S G=$$REFDF^APCHS9B3(APCHSPAT,9999999.14,$O(^AUTTIMM("C",$S($$BI:33,1:19),0)),$P($G(APCHY(1)),U))
 I G]"" Q G
 S G=$$REFDF^APCHS9B3(APCHSPAT,9999999.14,$O(^AUTTIMM("C",$S($$BI:109,1:19),0)),$P($G(APCHY(1)),U))
 I G]"" Q G
 S G=$$REFDF^APCHS9B3(APCHSPAT,9999999.14,$O(^AUTTIMM("C",$S($$BI:100,1:19),0)),$P($G(APCHY(1)),U))
 I G]"" Q G
 Q "No"
PPD(P) ;EP
 NEW APCHY,Y,X,%,E S %=P_"^LAST SKIN PPD",E=$$START1^APCLDF(%,"APCHY(")
 I $D(APCHY(1)) Q $P(^AUPNVSK(+$P(APCHY(1),U,4),0),U,5)_"     "_$$FMTE^XLFDT($P(APCHY(1),U))
 K APCHY S X=P_"^LAST DX V74.1" S E=$$START1^APCLDF(X,"APCHY(")
 I $D(APCHY(1)) Q $$FMTE^XLFDT($P(APCHY(1),U))_"  (by Diagnosis)"
 S G=$$REFDF^APCHS9B3(APCHSPAT,9999999.28,$O(^AUTTSK("B","PPD",0)))
 I G]"" Q G
 Q ""
PPDS(P) ;EP
 ;check for tb health factor, problem list, povs if and
 ;indication of pos ppd then return "Known Positive PPD"
 NEW APCHS,E,X
 K APCHS
 S X=P_"^LAST HEALTH [DM AUDIT TB HEALTH FACTORS" S E=$$START1^APCLDF(X,"APCHS(")
 I $D(APCHS) Q "Known Positive PPD or Hx of TB (Health Factor recorded)"
 N T S T=$O(^ATXAX("B","DM AUDIT TB HEALTH FACTORS",0))
 I 'T G PPDSPL
 N G S G=0,X=0 F  S X=$O(^AUPNHF("AA",P,X)) Q:X'=+X!(G)  I $D(^ATXAX(T,21,"B",X)) S G=1
 I G Q "Known Positive PPD or Hx of TB (Health Factor recorded)"
PPDSPL ;CHECK PL
 N T S T=$O(^ATXAX("B","SURVEILLANCE TUBERCULOSIS",0))
 I 'T Q ""
 N X,Y,I S (X,Y,I)=0 F  S X=$O(^AUPNPROB("AC",P,X)) Q:X'=+X!(I)  I $D(^AUPNPROB(X,0)),$P(^AUPNPROB(X,0),U,12)'="D" S Y=$P(^AUPNPROB(X,0),U) I $$ICD^ATXCHK(Y,T,9) S I=1
 I I Q "Known Positive PPD or Hx of TB (Problem List DX)"
 ;check povs
 K APCHS S X=P_"^FIRST DX [SURVEILLANCE TUBERCULOSIS" S E=$$START1^APCLDF(X,"APCHS(")
 I $D(APCHS(1)) Q "Known Positive PPD or Hx of TB (POV/DX "_$$FMTE^XLFDT($P(APCHS(1),U))_")"
 Q ""
BI() ;EP- check to see if using new imm package or not 1/5/1999 IHS/CMI/LAB
 Q $S($O(^AUTTIMM(0))<100:0,1:1)
 ;end new subrotuine CMI/TUCSON/LAB
