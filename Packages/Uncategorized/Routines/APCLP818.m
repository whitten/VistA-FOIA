APCLP818 ; IHS/CMI/LAB - 2003 DIABETES AUDIT ;
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
 ;
 ;cmi/anch/maw 9/10/2007 code set versioning in WT
 ;
HT(P,BDATE,EDATE) ;PEP - CALLED TO GET LAST HEIGHT VALUE IN DATE RANGE BDATE TO EDATE
 I 'P Q ""
 NEW %,APCLARRY,H,E
 S %=P_"^LAST MEAS HT;DURING "_BDATE_"-"_EDATE NEW X S E=$$START1^APCLDF(%,"APCLARRY(") S H=$P($G(APCLARRY(1)),U,2)
 I H="" Q H
 I H["?" Q ""
 S H=$J(H,4,1)
 Q H
WT(P,BDATE,EDATE) ;EP
 I 'P Q ""
 NEW %,E,APCLW,X,APCLN,APCL,APCLD,APCLZ,APCLX,ICD
 K APCL S APCLW="" S APCLX=P_"^LAST 24 MEAS WT;DURING "_BDATE_"-"_EDATE S E=$$START1^APCLDF(APCLX,"APCL(")
 S APCLN=0 F  S APCLN=$O(APCL(APCLN)) Q:APCLN'=+APCLN!(APCLW]"")  D
 .S APCLZ=$P(APCL(APCLN),U,5)
 .I '$D(^AUPNVPOV("AD",APCLZ)) S APCLW=$P(APCL(APCLN),U,2) Q
 . S APCLD=0 F  S APCLD=$O(^AUPNVPOV("AD",APCLZ,APCLD)) Q:'APCLD!(APCLW]"")  D
 .. ;S ICD=$P(^ICD9($P(^AUPNVPOV(APCLD,0),U),0),U) D  ;cmi/anch/maw 9/12/2007 orig line
 .. S ICD=$P($$ICDDX^ICDCODE($P(^AUPNVPOV(APCLD,0),U)),U,2) D  ;cmi/anch/maw 9/12/2007 csv
 ...I $E(ICD,1,3)="V22" Q
 ...I $E(ICD,1,3)="V23" Q
 ...I $E(ICD,1,3)="V27" Q
 ...I $E(ICD,1,3)="V28" Q
 ...I ICD>629.9999&(ICD<676.95) Q
 ...I ICD>61.49&(ICD<61.71) Q
 ...S APCLW=$P(APCL(APCLN),U,2)
 ..Q
 Q APCLW
BMI(P,BDATE,EDATE) ;EP
 I 'P Q -1
 NEW %,W,H,B,D,%DT
 S %DT="P",X=EDATE D ^%DT S D=Y
 S %=""
 I $$AGE^AUPNPAT(P,D)>19 D  Q %
 .S W=$$WT(P,BDATE,EDATE) I W="" Q
 .S HDATE=$$FMTE^XLFDT($$FMADD^XLFDT($P(^DPT(P,0),U,3),(19*365)))
 .S H=$$HT(P,HDATE,EDATE) I H="" Q
 .S W=W*.45359,H=(H*.0254),H=(H*H),%=(W/H),%=$J(%,4,1)
 S X=$$HTWTSD(P,BDATE,EDATE)
 I '$P(X,"^") Q %
 I '$P(X,"^",2) Q %
 S W=$P(X,"^"),H=$P(X,"^",2)
 S W=W*.45359,H=(H*.0254),H=(H*H),%=(W/H),%=$J(%,4,1)
 Q %
HTWTSD(P,BDATE,EDATE) ;get last ht / wt on same day
 I '$G(P) Q ""
 NEW APCLWTS,APCLHTS,%,X,APCLWTS1,APCLHTS1
 ;get all hts during time frame
 S %=P_"^ALL MEAS HT;DURING "_BDATE_"-"_EDATE NEW X S E=$$START1^APCLDF(%,"APCLHTS(")
 ;set the array up by date
 K APCLHTS1 S X=0 F  S X=$O(APCLHTS(X)) Q:X'=+X  S APCLHTS1($P(APCLHTS(X),U))=X
 ;get all wts during time frame
 S %=P_"^ALL MEAS WT;DURING "_BDATE_"-"_EDATE NEW X S E=$$START1^APCLDF(%,"APCLWTS(")
 ;set the array up by date
 K APCLWTS1 S X=0 F  S X=$O(APCLWTS(X)) Q:X'=+X  S APCLWTS1($P(APCLWTS(X),U))=X
 NEW APCLCHT S APCLCHT="",X=9999999 F  S X=$O(APCLWTS1(X),-1) Q:X=""!(APCLCHT]"")  I $D(APCLHTS1(X)) S APCLCHT=$P(APCLWTS(APCLWTS1(X)),U,2)_U_$P(APCLHTS(APCLHTS1(X)),U,2)
 Q APCLCHT
