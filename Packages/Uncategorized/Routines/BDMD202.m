BDMD202 ; IHS/CMI/LAB - 2000 DIABETES AUDIT ;
 ;;2.0;DIABETES MANAGEMENT SYSTEM;**2**;JUN 14, 2007
 ;
SETN ;set N = to v lab to use
 S N="" NEW A,G S (A,G)=0 F  S A=$O(BDM(A)) Q:A'=+A!(G)  I $P(^AUPNVLAB(+$P(BDM(A),U,4),0),U,4)]"" S G=A
 S N=$S(G:G,1:1)
 Q
TBTX(P) ;EP
 I '$G(P) Q ""
 NEW BDM,E,X
 K BDM
 S X=P_"^LAST HEALTH [DM AUDIT TB HEALTH FACTORS" S E=$$START1^APCLDF(X,"BDM(")
 I E Q ""
 I $D(BDM(1)) Q $P(BDM(1),U,3)_U_$S($P(BDM(1),U,3)["TX COMPLETE":"1 Yes",$P(BDM(1),U,3)["TX INCOMPLETE"!($P(BDM(1),U,3)["TX UNTREATED"):"2 No",1:"4 Unknown")
 N T,Y S T=$O(^ATXAX("B","DM AUDIT TB HEALTH FACTORS",0))
 I 'T Q ""
 N G S G="",X=0 F  S X=$O(^AUPNHF("AA",P,X)) Q:X'=+X!(G]"")  I $D(^ATXAX(T,21,"B",X)) S G=$P(^AUTTHF(X,0),U)
 I G]"" Q G_U_$S(G["TX COMPLETE":"1 Yes",G["TX INCOMPLETE"!(G["TX UNTREATED"):"2 No",1:"4 Unknown")
 Q ""
PAP(P,BDATE,EDATE) ; EP
 NEW X,%DT,ED,%
 S %DT="P",X=EDATE D ^%DT S ED=Y
 I $$SEX^AUPNPAT(P)'="F" Q "N/A - male"
 I $$AGE^AUPNPAT(P,ED)<18 Q "N/A - under 18"
 NEW BDM S %=P_"^LAST LAB PAP SMEAR;DURING "_BDATE_"-"_EDATE,E=$$START1^APCLDF(%,"BDM(")
 I $D(BDM(1)) Q "Yes  "_$$FMTE^XLFDT($P(BDM(1),U))
 K BDM S %=P_"^LAST DX V76.2;DURING "_BDATE_"-"_EDATE,E=$$START1^APCLDF(%,"BDM(")
 I $D(BDM(1)) Q "Yes  "_$$FMTE^XLFDT($P(BDM(1),U))
 K BDM S %=P_"^LAST DX V72.3;DURING "_BDATE_"-"_EDATE,E=$$START1^APCLDF(%,"BDM(")
 I $D(BDM(1)) Q "Yes  "_$$FMTE^XLFDT($P(BDM(1),U))
 K BDM S %=P_"^LAST PROCEDURE 91.46;DURING "_BDATE_"-"_EDATE,E=$$START1^APCLDF(%,"BDM(")
 I $D(BDM(1)) Q "Yes  "_$$FMTE^XLFDT($P(BDM(1),U))
 ;check CPT codes in year prior to date range
 S T=$O(^ATXAX("B","DM AUDIT PAP CPTS",0))
 K BDM I T S BDM(1)=$$CPT^BDMD202(P,,ED,T,4) I $G(BDM(1))]"" Q "Yes  "_BDM(1)
 NEW G S G=0
 NEW T S T=$O(^LAB(60,"B","PAP SMEAR",0))
 I 'T Q ""
 S X=0 F  S X=$O(^ATXLAB(T,21,X)) Q:X'=+X!(G)  I $$REFUSAL^BDMD207(P,60,$P(^ATXLAB(T,21,X,0),U),BDATE,EDATE) S G=1
 Q $S(G:"Refused",1:"No")
 Q "No"
CPT(P,BDATE,EDATE,T,F) ;EP return ien of CPT entry if patient had this CPT
 I '$G(P) Q ""
 I '$G(T) Q ""
 I '$G(F) S F=1
 I $G(EDATE)="" Q ""
 I $G(BDATE)="" S BDATE=$$FMADD^XLFDT(EDATE,-365)
 ;go through visits in a date range for this patient, check cpts
 NEW D,BD,ED,X,Y,D,G,V
 S ED=9999999-EDATE,BD=9999999-BDATE,G=0
 F  S ED=$O(^AUPNVSIT("AA",P,ED)) Q:ED=""!($P(ED,".")>BD)!(G)  D
 .S V=0 F  S V=$O(^AUPNVSIT("AA",P,ED,V)) Q:V'=+V!(G)  D
 ..Q:'$D(^AUPNVSIT(V,0))
 ..Q:'$D(^AUPNVCPT("AD",V))
 ..S X=0 F  S X=$O(^AUPNVCPT("AD",V,X)) Q:X'=+X!(G)  D
 ...I $$ICD^ATXCHK($P(^AUPNVCPT(X,0),U),T,1) S G=X
 ...Q
 ..Q
 .Q
 I 'G Q ""
 I F=1 Q $S(G:1,1:"")
 I F=2 Q G
 I F=3 S V=$P(^AUPNVCPT(G,0),U,3) I V Q $P($P($G(^AUPNVSIT(V,0)),U),".")
 I F=4 S V=$P(^AUPNVCPT(G,0),U,3) I V Q $$FMTE^XLFDT($P($P($G(^AUPNVSIT(V,0)),U),"."))
 Q ""
EKG(P,EDATE,F) ;EP
 I $G(F)="" S F="E"
 NEW BDM,X,%,E,LEKG S LEKG="",%=P_"^LAST DIAGNOSTIC ECG SUMMARY;DURING "_$$DOB^AUPNPAT(P,"E")_"-"_EDATE,E=$$START1^APCLDF(%,"BDM(")
 I $D(BDM) S LEKG=$P(BDM(1),U)
 K BDM S %=P_"^LAST PROCEDURE 89.51",E=$$START1^APCLDF(%,"BDM(")
 I $D(BDM(1)) D
 .Q:LEKG>$P(BDM(1),U)
 .S LEKG=$P(BDM(1),U)
 K BDM S %=P_"^LAST PROCEDURE 89.52",E=$$START1^APCLDF(%,"BDM(")
 I $D(BDM(1)) D
 .Q:LEKG>$P(BDM(1),U)
 .S LEKG=$P(BDM(1),U)
 K BDM S %=P_"^LAST PROCEDURE 89.53",E=$$START1^APCLDF(%,"BDM(")
 I $D(BDM(1)) D
 .Q:LEKG>$P(BDM(1),U)
 .S LEKG=$P(BDM(1),U)
 Q $S(F="E":$$FMTE^XLFDT(LEKG),1:LEKG)
 ;
ALT(P,BDATE,EDATE) ;EP
 NEW BDM,X,%,E,R,V
 K BDM
 S %=P_"^LAST LAB [DM AUDIT ALT TAX;DURING "_BDATE_"-"_EDATE,E=$$START1^APCLDF(%,"BDM(")
 I '$D(BDM(1)) Q ""
 S D=$P(BDM(1),U),D=$$FMTE^XLFDT(D) K BDM S %=P_"^ALL LAB [DM AUDIT ALT TAX;DURING "_D_"-"_D,E=$$START1^APCLDF(%,"BDM(")
 NEW N D SETN
 Q $P(^AUPNVLAB(+$P(BDM(N),U,4),0),U,4)_"  "_$$FMTE^XLFDT($P(BDM(N),U),5)
AST(P,BDATE,EDATE) ;EP
 NEW BDM,X,%,E,R,V
 K BDM
 S %=P_"^LAST LAB [DM AUDIT AST TAX;DURING "_BDATE_"-"_EDATE,E=$$START1^APCLDF(%,"BDM(")
 I '$D(BDM(1)) Q ""
 S D=$P(BDM(1),U),D=$$FMTE^XLFDT(D) K BDM S %=P_"^ALL LAB [DM AUDIT AST TAX;DURING "_D_"-"_D,E=$$START1^APCLDF(%,"BDM(")
 NEW N D SETN
 Q $P(^AUPNVLAB(+$P(BDM(N),U,4),0),U,4)_"  "_$$FMTE^XLFDT($P(BDM(N),U),5)
INSULIN(P,BDATE,EDATE) ;EP
 NEW X,BDM,E
 S X=P_"^LAST MEDS [DM AUDIT INSULIN DRUGS"_";DURING "_BDATE_"-"_EDATE S E=$$START1^APCLDF(X,"BDM(")
 I $D(BDM(1)) Q "X"
 Q ""
 ;
SULF(P,BDATE,EDATE) ;EP
 NEW X,BDM,E
 S X=P_"^LAST MEDS [DM AUDIT SULFONYLUREA DRUGS"_";DURING "_BDATE_"-"_EDATE S E=$$START1^APCLDF(X,"BDM(")
 I $D(BDM(1)) Q "X"
 Q ""
MET(P,BDATE,EDATE) ;EP
 NEW X,BDM,E
 S X=P_"^LAST MEDS [DM AUDIT METFORMIN DRUGS"_";DURING "_BDATE_"-"_EDATE S E=$$START1^APCLDF(X,"BDM(")
 I $D(BDM(1)) Q "X"
 Q ""
 ;
ACAR(P,BDATE,EDATE) ;EP
 NEW X,BDM,E
 S X=P_"^LAST MEDS [DM AUDIT ACARBOSE DRUGS"_";DURING "_BDATE_"-"_EDATE S E=$$START1^APCLDF(X,"BDM(")
 I $D(BDM(1)) Q "X"
 Q ""
 ;
TROG(P,BDATE,EDATE) ;EP
 NEW X,BDM,E
 S X=P_"^LAST MEDS [DM AUDIT GLITAZONE DRUGS"_";DURING "_BDATE_"-"_EDATE S E=$$START1^APCLDF(X,"BDM(")
 I $D(BDM(1)) Q "X"
 Q ""
 ;
 ;
MAMMOG(P,BDATE,EDATE) ;  EP
 NEW X,%DT,ED
 S %DT="P",X=EDATE D ^%DT S ED=Y
 I $$SEX^AUPNPAT(P)'="F" Q "N/A - male"
 I $$AGE^AUPNPAT(P,ED)<40 Q "N/A - under 40"
 I '$G(P) Q ""
 NEW LMAM S LMAM=""
 I $G(^AUTTSITE(1,0)),$P(^AUTTLOC($P(^AUTTSITE(1,0),U),0),U,10)="353101" S LMAM=$$MAMMOG1(P,BDATE,EDATE)
 NEW BDM S %=P_"^LAST RAD 76091;DURING "_BDATE_"-"_EDATE,E=$$START1^APCLDF(%,"BDM(")
 I $D(BDM(1)) D
 .Q:LMAM>$P(BDM(1),U)
 .S LMAM=$P(BDM(1),U)
 K BDM S %=P_"^LAST RAD 76092;DURING "_BDATE_"-"_EDATE,E=$$START1^APCLDF(%,"BDM(")
 I $D(BDM(1)) D
 .Q:LMAM>$P(BDM(1),U)
 .S LMAM=$P(BDM(1),U)
 K BDM S %=P_"^LAST RAD 76090;DURING "_BDATE_"-"_EDATE,E=$$START1^APCLDF(%,"BDM(")
 I $D(BDM(1)) D
 .Q:LMAM>$P(BDM(1),U)
 .S LMAM=$P(BDM(1),U)
 K BDM S %=P_"^LAST DX V76.11;DURING "_BDATE_"-"_EDATE,E=$$START1^APCLDF(%,"BDM(")
 I $D(BDM(1)) D
 .Q:LMAM>$P(BDM(1),U)
 .S LMAM=$P(BDM(1),U)
 K BDM S %=P_"^LAST DX V76.12;DURING "_BDATE_"-"_EDATE,E=$$START1^APCLDF(%,"BDM(")
 I $D(BDM(1)) D
 .Q:LMAM>$P(BDM(1),U)
 .S LMAM=$P(BDM(1),U)
 K BDM S %=P_"^LAST PROCEDURE 87.37;DURING "_BDATE_"-"_EDATE,E=$$START1^APCLDF(%,"BDM(")
 I $D(BDM(1)) D
 .Q:LMAM>$P(BDM(1),U)
 .S LMAM=$P(BDM(1),U)
 K BDM S %=P_"^LAST PROCEDURE 87.36;DURING "_BDATE_"-"_EDATE,E=$$START1^APCLDF(%,"BDM(")
 I $D(BDM(1)) D
 .Q:LMAM>$P(BDM(1),U)
 .S LMAM=$P(BDM(1),U)
 ;check CPT codes in year prior to date range
 S T=$O(^ATXAX("B","DM AUDIT MAMMOGRAM CPTS",0))
 K BDM I T S BDM(1)=$$CPT^BDMD202(P,,ED,T,3) D
 .I BDM(1)="" K BDM Q
 .Q:LMAM>$P(BDM(1),U)
 .S LMAM=$P(BDM(1),U)
 Q $S(LMAM]"":"Yes  "_$$FMTE^XLFDT(LMAM),1:"No")
 ;
MAMMOG1(P,BDATE,EDATE) ;for radiology 4.5+ or until qman can handle taxonomies for radiology procedures
 ;
 ;IHS/ANMC/LJF 8/26/99 new code to look for all mammograms no matter
 ;    how they are spelled in file 71 - for Rad version 4.5+
 NEW BDMMAM,CODE,COUNT,IEN,X
 S CODE=$O(^DIC(40.7,"C",72,0)) I 'CODE Q "No    <never recorded>"
 S IEN=0 F  S IEN=$O(^RAMIS(71,IEN)) Q:'IEN  D
 . Q:$G(^RAMIS(71,IEN,"I"))  ;inactive
 . Q:'$D(^RAMIS(71,IEN,"STOP","B",CODE))  ;no mamm stop code
 . S COUNT=$G(COUNT)+1,BDMMAM(COUNT)=$P(^RAMIS(71,IEN,0),U)
 ;
 ; -- use data fetcher to find mammogram dates
 NEW BDMY,BDMSAV,BDMX,BDMNAM
 S (BDMSAV,BDMX)=0 F  S BDMX=$O(BDMMAM(BDMX)) Q:'BDMX  D
 . S %=P_"^LAST RAD "_BDMMAM(BDMX)_";DURING "_BDATE_"-"_EDATE,E=$$START1^APCLDF(%,"BDMY(")
 . ; save latest date and procedure name
 . I $G(BDMY(1)),$P(BDMY(1),U)>BDMSAV S BDMSAV=$P(BDMY(1),U),BDMNAM=BDMMAM(BDMX)
 ;
 ; -- return results
 I BDMSAV'=0 Q BDMSAV
 ;IHS/ANMC/LJF 8/26/99 end of new code
 ;
 Q ""
TXNAME(V) ;EP
 I $G(V)="" Q ""
 S V=$$TXNAMES(V)
 Q $E(V,1,16)
TXNAMES(Y) ;
 I Y=1 Q "DIET"
 I Y=2 Q "INSULIN"
 I Y=3 Q "SULFONYLUREA"
 I Y=4 Q "METFORMIN (GLUCOPHAGE)"
 I Y=5 Q "ACARBOSE OR MIGLITOL"
 I Y=6 Q "GLITAZONE"
 I Y=9 Q "UNKNOWN/REFUSED"
 I Y=23 Q "INSULIN+S'UREA"
 I Y=24 Q "INSULIN+MET"
 I Y=25 Q "INSULIN+ACAR"
 I Y=26 Q "INSULIN+GLITAZONE"
 I Y=34 Q "S'UREA+MET"
 I Y=35 Q "S'UREA+ACAR"
 I Y=36 Q "S'UREA+GLITAZONE"
 I Y=45 Q "MET+ACAR"
 I Y=46 Q "MET+GLITAZONE"
 I Y=56 Q "ACAR+GLITAZONE"
 I Y=234 Q "INS+S'UREA+MET"
 I Y=235 Q "INS+S'UREA+ACAR"
 I Y=236 Q "INS+S'UREA+GLIT"
 I Y=245 Q "INS+MET+ACAR"
 I Y=246 Q "INS+MET+GLITAZONE"
 I Y=256 Q "INS+ACAR+GLITAZONE"
 I Y=345 Q "S'UREA+MET+ACAR"
 I Y=346 Q "S'UREA+MET+GLIT"
 I Y=356 Q "S'UREA+ACAR+GLIT"
 I Y=456 Q "MET+ACAR+GLIT"
 Q ""
 ;
