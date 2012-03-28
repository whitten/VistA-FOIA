APCHPWH6 ; IHS/CMI/LAB - PCC HEALTH SUMMARY - MAIN DRIVER PART 2 ;   
 ;;2.0;IHS PCC SUITE;**2,6**;MAY 14, 2009;Build 11
 ;
 ;EO MEASURES IN PWH
CANCER ;EP - cancer CCI measure
 NEW APCHVAL,Y,APCHHD
 D SUBHEAD^APCHPWHU
 S APCHHD=0  ;D S^APCHPWH1("CANCER SCREENING",1)
 D MAMMOGM
 D PAP
 D COLORECT
 Q
 ;
MAMMOGM ;
 Q:$P(^DPT(APCHSDFN,0),U,2)'="F"
 Q:$$AGE^AUPNPAT(APCHSDFN)<50
 Q:$$AGE^AUPNPAT(APCHSDFN)>69
 S APCHVAL=$$LASTMAM^APCLAPI1(APCHSDFN)
 ;I 'APCHHD D S^APCHPWH1("CANCER SCREENING",1) S APCHHD=1
 D S^APCHPWH1("MAMMOGRAM",1)
 I APCHVAL="" D  Q
 .D S^APCHPWH1("No mammogram on file.  It is recommended that you receive a mammogram")
 .D S^APCHPWH1("every year.  Ask your health care provider to order a mammogram for you.")
 I $$FMDIFF^XLFDT(DT,APCHVAL)<365 D  Q
 .D S^APCHPWH1("Your last mammogram was on "_$$FMTE^XLFDT(APCHVAL)_".  Your next mammogram will")
 .D S^APCHPWH1("be due on "_$$FMTE^XLFDT($$FMADD^XLFDT(APCHVAL,365))_".")
 D S^APCHPWH1("Your last mammogram was on "_$$FMTE^XLFDT(APCHVAL)_".  We recommend that you")
 D S^APCHPWH1("receive a mammogram every year.  Ask your health care provider to order a ")
 D S^APCHPWH1("mammogram for you.")
 Q
 ;
PAP ;
 Q:$P(^DPT(APCHSDFN,0),U,2)'="F"
 Q:$$AGE^AUPNPAT(APCHSDFN)<19
 I $$HYSTER^APCHSM04(APCHSDFN) Q
 D S^APCHPWH1("PAP SMEAR",1)
 S APCHVAL=$$LASTPAP^APCLAPI1(APCHSDFN)
 I APCHVAL="" D  Q
 .D S^APCHPWH1("No Pap Smear on file.  We recommend that you receive a Pap Smear")
 .D S^APCHPWH1("every 3 years.  Ask your health care provider to order a Pap Smear")
 .D S^APCHPWH1("for you.")
 I $$FMDIFF^XLFDT(DT,APCHVAL)<(3*365) D  Q
 .D S^APCHPWH1("Your last Pap Smear was on "_$$FMTE^XLFDT(APCHVAL)_".  Your next Pap Smear will")
 .D S^APCHPWH1("be due on "_$$FMTE^XLFDT($$FMADD^XLFDT(APCHVAL,(3*365)))_".")
 D S^APCHPWH1("Your last Pap smear was on "_$$FMTE^XLFDT(APCHVAL)_".  We recommend that you")
 D S^APCHPWH1("receive a Pap smear every 3 years.  Ask your health care provider to order a ")
 D S^APCHPWH1("Pap smear for you.")
 Q
 ;
COLORECT ;
 Q:$$AGE^AUPNPAT(APCHSDFN)<51
 Q:$$CRC(APCHSDFN)
 ;I 'APCHHD D S^APCHPWH1("CANCER SCREENING",1) S APCHHD=1
 D S^APCHPWH1("COLON HEALTH SCREENING",1)
 NEW D,Y
 S D="",Y=""
 S APCHVAL=$$LASTCOLO^APCLAPI(APCHSDFN,$$FMADD^XLFDT(DT,-(10*365)),DT,"D")
 I APCHVAL S D=$$FMADD^XLFDT(APCHVAL,(10*365))
 S APCHVAL=$$LASTFSIG^APCLAPI(APCHSDFN,$$FMADD^XLFDT(DT,-(5*365)),DT,"D")
 I APCHVAL S Y=$$FMADD^XLFDT(APCHVAL,(5*365)) I Y>D S D=Y
 S APCHVAL=$$LASTBE^APCLAPI4(APCHSDFN,$$FMADD^XLFDT(DT,-(10*365)),DT,"D")
 I APCHVAL S Y=$$FMADD^XLFDT(APCHVAL,(10*365)) I Y>D S D=Y
 S APCHVAL=$$LASTFOBT^APCLAPI3(APCHSDFN,$$FMADD^XLFDT(DT,-365),DT,"D")
 I APCHVAL S Y=$$FMADD^XLFDT(APCHVAL,365) I Y>D S D=Y
 I D D COLODISP Q
 D S^APCHPWH1("It is recommended that all people who are 51 years and older be screened")
 D S^APCHPWH1("for colon cancer.  Ask your health care provider to order a colon cancer")
 D S^APCHPWH1("screening for you.")
 Q
COLODISP ;
 D S^APCHPWH1("You are up to date for colon cancer screening.  Your next colon cancer")
 D S^APCHPWH1("screening will be due on "_$$FMTE^XLFDT(D)_".")
 Q
 ;
CRC(P) ;EP
 NEW APCHX,Y,X,T
 K APCHX
 S Y="APCHX("
 S X=P_"^LAST DX [BGP COLORECTAL CANCER DXS;DURING "_$$FMTE^XLFDT($$DOB^AUPNPAT(P))_"-"_$$FMTE^XLFDT(DT) S E=$$START1^APCLDF(X,Y)
 I $D(APCHX(1)) Q 1  ;has a dx
 S X=$$LASTCPTT^APCLAPIU(P,,DT,"BGP COLORECTAL CANCER CPTS","D")
 I X]"" Q 1
 S X=$$LASTPRCT^APCLAPIU(P,,DT,"BGP TOTAL CHOLECTOMY PROCS","D")
 I X Q 1
 S X=$$LASTCPTT^APCLAPIU(P,,DT,"BGP TOTAL CHOLECTOMY CPTS","D")
 I X Q 1
 I $$PLTAX^APCHSMU(P,"BGP COLORECTAL CANCER DXS") Q 1
 Q 0
 ;
CKD(P) ;EP - Does patient have chronic kidney disease (CKD)?
 NEW T,APCHLLAB
 ;get last serum creatinine value
 S APCHLLAB=$$LAB(P,$O(^ATXLAB("B","DM AUDIT CREATININE TAX",0)),$O(^ATXAX("B","BGP CREATININE LOINC")))
 I $$SEX^AUPNPAT(APCHSDFN)="F",APCHLLAB>1.3 Q 1
 I $$SEX^AUPNPAT(APCHSDFN)="M",APCHLLAB>1.5 Q 1
 ;get last urine protein value
 S APCHLLAB=$$LAB(P,$O(^ATXLAB("B","DM AUDIT URINE PROTEIN TAX",0)),$O(^ATXAX("B","DM AUDIT URINE PROTEIN LOINC")))
 I +APCHLLAB>200 Q 1
 ;get last A/C ratio value
 S APCHLLAB=$$LAB(P,$O(^ATXLAB("B","DM AUDIT A/C RATIO TAX",0)),$O(^ATXAX("B","DM AUDIT A/C RATIO LOINC")))
 I +APCHLLAB>200 Q 1
 ;get estimated GFR
 S APCHLLAB=$$LAB(P,$O(^ATXLAB("B","BGP GPRA ESTIMATED GFR TAX",0)),$O(^ATXAX("B","BGP ESTIMATED GFR LOINC")),"ESTIMATED GFR")
 I +APCHLLAB,APCHLLAB<60 Q 1
 Q ""
 ;
LAB(P,T,LT,LN) ;EP
 I '$G(LT) S LT=""
 S LN=$G(LN)
 NEW D,V,G,X,J S (D,G)=0 F  S D=$O(^AUPNVLAB("AE",P,D)) Q:D'=+D!(G)  D
 .S X=0 F  S X=$O(^AUPNVLAB("AE",P,D,X)) Q:X'=+X!(G)  D
 ..S Y=0 F  S Y=$O(^AUPNVLAB("AE",P,D,X,Y)) Q:Y'=+Y!(G)  D
 ...I $D(^ATXLAB(T,21,"B",X)),$P(^AUPNVLAB(Y,0),U,4)]"" S G=Y Q
 ...I LN]"",$$VAL^XBDIQ1(9000010.09,Y,.01)=LN S G=Y Q
 ...Q:'LT
 ...S J=$P($G(^AUPNVLAB(Y,11)),U,13) Q:J=""
 ...Q:'$$LOINC(J,LT)
 ...S G=Y
 ...Q
 ..Q
 .Q
 I 'G Q ""
 Q $P(^AUPNVLAB(G,0),U,4)
 ;
LOINC(A,B) ;
 NEW %
 S %=$P($G(^LAB(95.3,A,9999999)),U,2)
 I %]"",$D(^ATXAX(B,21,"B",%)) Q 1
 S %=$P($G(^LAB(95.3,A,0)),U)_"-"_$P($G(^LAB(95.3,A,0)),U,15)
 I $D(^ATXAX(B,21,"B",%)) Q 1
 Q ""
 ;
FOOTEX(P,BDATE,EDATE) ;EP
 NEW APCHY,APCHV,%,LDFE,PROV,D,V,G
 S LDFE="",%=P_"^LAST EXAM DIABETIC FOOT EXAM;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(%,"APCHY(")
 I $D(APCHY(1)) S LDFE=$P(APCHY(1),U)
 ;now check any clinic 65 or prov 33/25
 K APCHY,APCHV
 S %=P_"^ALL VISITS;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(%,"APCHY(")
 ;reorder by date
 S %=0 F  S %=$O(APCHY(%)) Q:%'=+%  S APCHV(9999999-$P(APCHY(%),U),$P(APCHY(%),U,5))=""
 S (D,V)=0,G="" F  S D=$O(APCHV(D)) Q:D'=+D!(G)  S V=0 F  S V=$O(APCHV(D,V)) Q:V'=+V!(G)  D
 .S PROV=$$PRIMPROV^APCLV(V,"D") I (PROV=33!(PROV=25)),'$$DNKA^APCHS9B4(V) S G=9999999-D Q
 .S PROV=$$CLINIC^APCLV(V,"C") I PROV=65!(PROV="B7"),'$$DNKA^APCHS9B4(V) S G=9999999-D
 I G,G>LDFE S LDFE=G
 Q LDFE
LAB30 ;EP - all labs in past 100 days
 NEW APCHLAB,APCHX,X,L,N,R,APCHL
 S APCHLAB="APCHLAB"
 D ALLLAB^APCLAPIU(DFN,$$FMADD^XLFDT(DT,-100),DT,,,,.APCHLAB)
 Q:'$D(APCHLAB)
 D SUBHEAD^APCHPWHU
 D S^APCHPWH1("RECENT LAB RESULTS")
 D WRITET^APCHPWHU("RECENT LAB RESULTS")
 S X="LABORATORY TEST",$E(X,32)="RESULT",$E(X,52)="REFERENCE RANGE",$E(X,68)="DATE" D S^APCHPWH1(X)
 S APCHX=0 F  S APCHX=$O(APCHLAB(APCHX)) Q:APCHX'=+APCHX  D
 .S APCHL=$P(APCHLAB(APCHX),U,4)
 .Q:$P(^AUPNVLAB(APCHL,0),U,4)=""  ;no result - this is pending
 .Q:$P(^AUPNVLAB(APCHL,0),U,4)="canc"  ;CANCELLED TEST
 .S N=$P($G(^AUPNVLAB(APCHL,11)),U,1)  ;UNITS
 .I $$UP^XLFSTR($P(^AUPNVLAB(APCHL,0),U,4))["REJECTED TEST" Q
 .S R=""
 .I $P($G(^AUPNVLAB(APCHL,11)),U,4)]"",$P($G(^AUPNVLAB(APCHL,11)),U,5)]"" S R=$P(^AUPNVLAB(APCHL,11),U,4)_"-"_$P(^AUPNVLAB(APCHL,11),U,5)
 .S X=$E($P(APCHLAB(APCHX),U,2),1,30),$E(X,32)=$P(APCHLAB(APCHX),U,3)_"  "_N,$E(X,52)=R,$E(X,68)=$$FMTE^XLFDT($P(APCHLAB(APCHX),U,1))
 .D S^APCHPWH1(X)
 .;DISPLAY COMMENTS?  YES/NO
 .Q:'$P(^APCHPWHT(APCHPWHT,1,APCHSORD,0),U,4)
 .;display comments (21 multiple using ^DIWP with a length of 65, indented to column 8
 .K ^UTILITY($J,"W")
 .S APCMZ=0
 .S DIWL=1,DIWR=65 F  S APCMZ=$O(^AUPNVLAB(APCHL,21,APCMZ)) Q:APCMZ'=+APCMZ  D
 ..S X=^AUPNVLAB(APCHL,21,APCMZ,0) I X]"" D ^DIWP
 ..Q
 .S Z=0 F  S Z=$O(^UTILITY($J,"W",DIWL,Z)) Q:Z'=+Z  S X="",$E(X,8)=^UTILITY($J,"W",DIWL,Z,0) D S^APCHPWH1(X)
 .K ^UTILITY($J,"W")
 Q
