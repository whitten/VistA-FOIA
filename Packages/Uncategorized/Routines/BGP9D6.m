BGP9D6 ; IHS/CMI/LAB - measure 31 ;
 ;;9.0;IHS CLINICAL REPORTING;;JUL 1, 2009
 ;
I031 ;EP
 S (BGPN1,BGPN2,BGPN3,BGPN4,BGPN5,BGPD1,BGPD2,BGPD3,BGPD4,BGPD5,BGPD6,BGPD7,BGPD8,BGPD9)=0
 I BGPAGEB<2 S BGPSTOP=1 Q
 I BGPAGEB>74 S BGPSTOP=1 Q
 I BGPAGEB>1 S BGPD1=1
 I BGPAGEB>1,BGPAGEB<6 S BGPD2=1
 I BGPAGEB>5,BGPAGEB<12 S BGPD3=1
 I BGPAGEB>11,BGPAGEB<20 S BGPD4=1
 I BGPAGEB>19,BGPAGEB<25 S BGPD5=1
 I BGPAGEB>24,BGPAGEB<35 S BGPD6=1
 I BGPAGEB>34,BGPAGEB<45 S BGPD7=1
 I BGPAGEB>44,BGPAGEB<55 S BGPD8=1
 I BGPAGEB>54,BGPAGEB<75 S BGPD9=1
 I '(BGPD1+BGPD2+BGPD3+BGPD4+BGPD5+BGPD6+BGPD7+BGPD8+BGPD9) S BGPSTOP=1 Q
 S BGPBMI=$$BMI(DFN,BGPEDATE,BGPAGEE),BGPN1=$S(BGPBMI]"":1,1:0)
 S BGPXPNV=$$SB^BGP9PDL1($J($P(BGPBMI,U),6,2)) S:BGPXPNV="0.00" BGPXPNV=""
 S BGPN2=$$OW(DFN,BGPBMI,BGPAGEE)
 S BGPN3=$$OB(DFN,BGPBMI,BGPAGEE)
 I BGPN2!(BGPN3) S BGPN4=1
 I 'BGPN1 S BGPREF=$$REF(DFN,BGP365,BGPEDATE,BGPAGEB) I $P(BGPREF,U)=1 S BGPN5=1
 I BGPN5 S BGPN1=1
 S BGPVALUE=$S(BGPD1:"UP",1:"") I BGPD1,BGPACTCL S BGPVALUE=BGPVALUE_";AC"
 S BGPVALUE=BGPVALUE_"|||"_$S(BGPBMI]"":$$SB^BGP9PDL1($J($P(BGPBMI,U),6,2)),1:"")_$S(BGPN2:" OW",1:"")_$S(BGPN3:" OB",1:"")
 I BGPN5 S BGPVALUE=BGPVALUE_"ref "_$P(BGPREF,U,2)_" "_$$DATE^BGP9UTL($P(BGPREF,U,3))_";"_$P(BGPREF,U,5)_" "_$$DATE^BGP9UTL($P(BGPREF,U,6))
 K BGPL,BGPLWTS,BGPLHTS,%,X,BGPLWTS1,BGPLHTS1,Y
 Q
I031A ;
 S (BGPN1,BGPN2,BGPN3,BGPN4,BGPN5,BGPD1,BGPD2,BGPD3,BGPD4,BGPD5,BGPD6,BGPD7,BGPD8,BGPD9)=0
 I BGPAGEB<2 S BGPSTOP=1 Q
 I BGPAGEB>5 S BGPSTOP=1 Q
 I BGPAGEE>5 S BGPSTOP=1 Q
 I 'BGPACTCL S BGPSTOP=1 Q
 S BGPD1=1
 S BGPBMI=$$BMIOR(DFN,BGPBDATE,BGPEDATE,BGPAGEE)
 I $P(BGPBMI,U)="" S BGPSTOP=1 Q  ;no bmi
 S BGPOW=$$OW(DFN,$P(BGPBMI,U),$$AGE^AUPNPAT(DFN,$P(BGPBMI,U,2))) I BGPOW S BGPN1=1
 S BGPOB=$$OB(DFN,$P(BGPBMI,U),$$AGE^AUPNPAT(DFN,$P(BGPBMI,U,2))) I BGPOB S BGPN2=1
 I BGPN1!(BGPN2) S BGPN3=1
 S A=$$AGE^AUPNPAT(DFN,$P(BGPBMI,U,2))
 I A=2 S BGPD2=1
 I A=3 S BGPD3=1
 I A=4 S BGPD4=1
 I A=5 S BGPD5=1
 I '(BGPD2+BGPD3+BGPD4+BGPD5) W !,BGPBOMB
 S BGPVALUE=$S(BGPD1:"AC",1:"")
 I $P(BGPBMI,U)]"" S BGPVALUE=BGPVALUE_"|||"_$P(BGPBMI,U)_":"_$$DATE^BGP9UTL($P(BGPBMI,U,2))_" Age at BMI: "_A
 I $P(BGPOW,U,2)]""!($P(BGPOB,U,2)]"") S BGPVALUE=BGPVALUE_" Outside Data Check Limits"
 I BGPN1 S BGPVALUE=BGPVALUE_" At Risk 85-94% "
 I BGPN2 S BGPVALUE=BGPVALUE_" OW 95% "
 K BGPBMIH,BGPBMI,BGPOW,BGPOB
 Q
BMIOR(P,BDATE,EDATE,AGE) ;EP
 KILL %,W,H,B,D,%DT
 S BGPBMIH=""
 D  Q BGPBMIH
 .S BDATE=$$FMTE^XLFDT(BDATE),EDATE=$$FMTE^XLFDT(EDATE)
 .S X=$$HTWTSDOR(P,BDATE,EDATE)
 .I '$P(X,"^") Q
 .S W=$P(X,"^",2),H=$P(X,"^",3)
 .S W=W*.45359,H=(H*.0254),H=(H*H),BGPBMIH=$$SB^BGP9PDL1($J((W/H),6,2))_U_$P(X,U,1)
 .;S W=$P(X,"^",5),H=$P(X,"^",6)
 .;S W=W*.45359,H=(H*.0254),H=(H*H),$P(BGPBMIH,U,3)=$$SB^BGP9PDL1($J((W/H),6,2))_U_$P(X,U,4)
 .Q
 Q ""
HTWTSDOR(P,BDATE,EDATE) ;get last ht / wt on same day
 I '$G(P) Q ""
 KILL BGPLWTS,BGPLHTS,%,X,BGPLWTS1,BGPLHTS1,Y
 ;get all hts during time frame
 S %=P_"^ALL MEAS HT;DURING "_BDATE_"-"_EDATE S E=$$START1^APCLDF(%,"BGPLHTS(")
 S Y=0 F  S Y=$O(BGPLHTS(Y)) Q:Y'=+Y  I $P(BGPLHTS(Y),U,2)="?"!($P(BGPLHTS(Y),U,2)="") K BGPLHTS(Y)
 ;set the array up by date
 K BGPLHTS1 S X=0 F  S X=$O(BGPLHTS(X)) Q:X'=+X  S BGPLHTS1($P(BGPLHTS(X),U))=X
 ;get all wts during time frame
 S %=P_"^ALL MEAS WT;DURING "_BDATE_"-"_EDATE S E=$$START1^APCLDF(%,"BGPLWTS(")
 S Y=0 F  S Y=$O(BGPLWTS(Y)) Q:Y'=+Y  I $P(BGPLWTS(Y),U,2)="?"!($P(BGPLWTS(Y),U,2)="") K BGPLWTS(Y)
 ;set the array up by date
 K BGPLWTS1 S X=0 F  S X=$O(BGPLWTS(X)) Q:X'=+X  S BGPLWTS1($P(BGPLWTS(X),U))=X
 S BGPLCHT="",X=9999999 F  S X=$O(BGPLWTS1(X),-1) Q:X=""!(BGPLCHT]"")  I $D(BGPLHTS1(X)) S BGPLCHT=$P(BGPLWTS(BGPLWTS1(X)),U)_U_$P(BGPLWTS(BGPLWTS1(X)),U,2)_U_$P(BGPLHTS(BGPLHTS1(X)),U,2)
 Q BGPLCHT
 ;KEEP IN CASE
 K BGPLWTS,BGPLHTS,%,X,BGPLWTS1,BGPLHTS1,Y
 S EDATE=$$FMTE^XLFDT($$FMADD^XLFDT($P(BGPLCHT,U),-91))
 S %=P_"^ALL MEAS HT;DURING "_BDATE_"-"_EDATE S E=$$START1^APCLDF(%,"BGPLHTS(")
 S Y=0 F  S Y=$O(BGPLHTS(Y)) Q:Y'=+Y  I $P(BGPLHTS(Y),U,2)="?"!($P(BGPLHTS(Y),U,2)="") K BGPLHTS(Y)
 ;set the array up by date
 K BGPLHTS1 S X=0 F  S X=$O(BGPLHTS(X)) Q:X'=+X  S BGPLHTS1($P(BGPLHTS(X),U))=X
 ;get all wts during time frame
 S %=P_"^ALL MEAS WT;DURING "_BDATE_"-"_EDATE S E=$$START1^APCLDF(%,"BGPLWTS(")
 S Y=0 F  S Y=$O(BGPLWTS(Y)) Q:Y'=+Y  I $P(BGPLWTS(Y),U,2)="?"!($P(BGPLWTS(Y),U,2)="") K BGPLWTS(Y)
 ;set the array up by date
 K BGPLWTS1 S X=0 F  S X=$O(BGPLWTS(X)) Q:X'=+X  S BGPLWTS1($P(BGPLWTS(X),U))=X
 S X=0 F  S X=$O(BGPLWTS1(X)) Q:X=""!($P(BGPLCHT,U,4)]"")  I $D(BGPLHTS1(X)) S $P(BGPLCHT,U,4)=$P(BGPLWTS(BGPLWTS1(X)),U)_U_$P(BGPLWTS(BGPLWTS1(X)),U,2)_U_$P(BGPLHTS(BGPLHTS1(X)),U,2)
 Q BGPLCHT
OB(P,BMI,A) ;EP obese
 I $G(BMI)="" Q ""
 S S=$P(^DPT(P,0),U,2)
 I S="" Q ""
 S R=0,R=$O(^APCLBMI("H",S,A,R))
 I 'R S R=$O(^APCLBMI("H",S,A)) I R S R=$O(^APCLBMI("H",S,R,""))
 I 'R Q ""
 I BMI>$P(^APCLBMI(R,0),U,7)!(BMI<$P(^APCLBMI(R,0),U,6)) Q "0^Outside Data Check Limits"
 I BMI'<$P(^APCLBMI(R,0),U,5) Q 1
 Q ""
OW(P,BMI,A) ;EP overweight
 I $G(BMI)="" Q ""
 S S=$P(^DPT(P,0),U,2)
 I S="" Q ""
 S R=0,R=$O(^APCLBMI("H",S,A,R))
 I 'R S R=$O(^APCLBMI("H",S,A)) I R S R=$O(^APCLBMI("H",S,R,""))
 I 'R Q ""
 I BMI>$P(^APCLBMI(R,0),U,7)!(BMI<$P(^APCLBMI(R,0),U,6)) Q "0^Outside Data Check Limits"
 I BMI'<$P(^APCLBMI(R,0),U,4),BMI<$P(^APCLBMI(R,0),U,5) Q 1
 Q ""
HT(P,BDATE,EDATE) ;EP
 I 'P Q ""
 KILL %,BGPARRY,H,E
 S %=P_"^LAST MEAS HT;DURING "_BDATE_"-"_EDATE S E=$$START1^APCLDF(%,"BGPARRY(") S H=$P($G(BGPARRY(1)),U,2)
 I H="" Q H
 I H["?" Q ""
 S H=$J(H,2,0)
 Q H
WT(P,BDATE,EDATE) ;EP
 I 'P Q ""
 KILL %,E,BGPLW,X,BGPLN,BGPL,BGPLD,BGPLZ,BGPLX,ICD
 K BGPL S BGPLW="" S BGPLX=P_"^LAST 24 MEAS WT;DURING "_BDATE_"-"_EDATE S E=$$START1^APCLDF(BGPLX,"BGPL(")
 S BGPLN=0 F  S BGPLN=$O(BGPL(BGPLN)) Q:BGPLN'=+BGPLN!(BGPLW]"")  D
 .S BGPLZ=$P(BGPL(BGPLN),U,5)
 .I '$D(^AUPNVPOV("AD",BGPLZ)) S BGPLW=$P(BGPL(BGPLN),U,2) Q
 . S BGPLD=0 F  S BGPLD=$O(^AUPNVPOV("AD",BGPLZ,BGPLD)) Q:'BGPLD!(BGPLW]"")  D
 .. S D=$P(BGPL(BGPLN),U)
 .. S ICD=$P($$ICDDX^ICDCODE($P(^AUPNVPOV(BGPLD,0),U),D),U,2) D
 ...I $E(ICD,1,3)="V22" Q
 ...I $E(ICD,1,3)="V23" Q
 ...I $E(ICD,1,3)="V27" Q
 ...I $E(ICD,1,3)="V28" Q
 ...I ICD>629.9999&(ICD<676.95) Q
 ...I ICD>61.49&(ICD<61.71) Q
 ...S BGPLW=$P(BGPL(BGPLN),U,2)
 ..Q
 Q BGPLW
 ;
BMI(P,EDATE,AGE) ;EP
 KILL %,W,H,B,D,%DT,BDATE
 ;S BDATE=$$FMADD^XLFDT(EDATE,-365),BDATE=$$FMTE^XLFDT(BDATE),EDATE=$$FMTE^XLFDT(EDATE)
 S BGPBMIH=""
 I AGE>18,AGE<51 D  Q BGPBMIH
 .S BDATE=$$FMADD^XLFDT(EDATE,-(5*365)),BDATE=$$FMTE^XLFDT(BDATE),EDATE=$$FMTE^XLFDT(EDATE)
 .S W=$$WT(P,BDATE,EDATE) I W=""!(W="?") Q
 .;S HDATE=$$FMTE^XLFDT($$FMADD^XLFDT($P(^DPT(P,0),U,3),(19*365)))
 .S HDATE=BDATE
 .S H=$$HT(P,HDATE,EDATE) I H="" Q
 .S W=W*.45359,H=(H*.0254),H=(H*H),BGPBMIH=(W/H)
 I AGE>50 D  Q BGPBMIH
 .S BDATE=$$FMADD^XLFDT(EDATE,-(2*365)),BDATE=$$FMTE^XLFDT(BDATE),EDATE=$$FMTE^XLFDT(EDATE)
 .S W=$$WT(P,BDATE,EDATE) I W=""!(W="?") Q
 .;S HDATE=$$FMTE^XLFDT($$FMADD^XLFDT($P(^DPT(P,0),U,3),(19*365)))
 .S HDATE=BDATE
 .S H=$$HT(P,HDATE,EDATE) I H="" Q
 .S W=W*.45359,H=(H*.0254),H=(H*H),BGPBMIH=(W/H)
 I AGE<19 D  Q BGPBMIH
 .S BDATE=$$FMADD^XLFDT(EDATE,-365),BDATE=$$FMTE^XLFDT(BDATE),EDATE=$$FMTE^XLFDT(EDATE)
 .S X=$$HTWTSD(P,BDATE,EDATE)
 .I '$P(X,"^") Q
 .I '$P(X,"^",2) Q
 .S W=$P(X,"^"),H=$P(X,"^",2)
 .S W=W*.45359,H=(H*.0254),H=(H*H),BGPBMIH=(W/H)
 .Q
 Q
HTWTSD(P,BDATE,EDATE) ;get last ht / wt on same day
 I '$G(P) Q ""
 KILL BGPLWTS,BGPLHTS,%,X,BGPLWTS1,BGPLHTS1,Y
 ;get all hts during time frame
 S %=P_"^ALL MEAS HT;DURING "_BDATE_"-"_EDATE S E=$$START1^APCLDF(%,"BGPLHTS(")
 S Y=0 F  S Y=$O(BGPLHTS(Y)) Q:Y'=+Y  I $P(BGPLHTS(Y),U,2)="?"!($P(BGPLHTS(Y),U,2)="") K BGPLHTS(Y)
 ;set the array up by date
 K BGPLHTS1 S X=0 F  S X=$O(BGPLHTS(X)) Q:X'=+X  S BGPLHTS1($P(BGPLHTS(X),U))=X
 ;get all wts during time frame
 S %=P_"^ALL MEAS WT;DURING "_BDATE_"-"_EDATE S E=$$START1^APCLDF(%,"BGPLWTS(")
 S Y=0 F  S Y=$O(BGPLWTS(Y)) Q:Y'=+Y  I $P(BGPLWTS(Y),U,2)="?"!($P(BGPLWTS(Y),U,2)="") K BGPLWTS(Y)
 ;set the array up by date
 K BGPLWTS1 S X=0 F  S X=$O(BGPLWTS(X)) Q:X'=+X  S BGPLWTS1($P(BGPLWTS(X),U))=X
 S BGPLCHT="",X=9999999 F  S X=$O(BGPLWTS1(X),-1) Q:X=""!(BGPLCHT]"")  I $D(BGPLHTS1(X)) S BGPLCHT=$P(BGPLWTS(BGPLWTS1(X)),U,2)_U_$P(BGPLHTS(BGPLHTS1(X)),U,2)
 Q BGPLCHT
 ;
REF(P,BDATE,EDATE,AGE) ;EP - get ht/wt refusal in past year, same date for 18 and under
 S R=""
 I AGE<19 G AGE18R
 S X=$$REFUSAL^BGP9UTL1(P,9999999.07,$O(^AUTTMSR("B","HT",0)),BDATE,EDATE) I X S R=1_U_"HT"_U_$P(X,U,2)
 S Y=$$REFUSAL^BGP9UTL1(P,9999999.07,$O(^AUTTMSR("B","WT",0)),BDATE,EDATE) I Y S $P(R,U,4)=1,$P(R,U,5)="WT",$P(R,U,6)=$P(Y,U,2)
 I X="",Y="" Q ""
 Q R
AGE18R ;
 K H,W
 S H=$O(^AUTTMSR("B","HT",0))
 S Z=0 F  S Z=$O(^AUPNPREF("AA",P,9999999.07,H,Z)) Q:Z'=+Z  D
 .S V=0 F  S V=$O(^AUPNPREF("AA",P,9999999.07,H,Z,V)) Q:V'=+V  D
 ..Q:'$D(^AUPNPREF(V,0))
 ..S D=$P(^AUPNPREF(V,0),U,3)
 ..Q:D>EDATE
 ..Q:D<BDATE
 ..S H(D)=""
 ..Q
 .Q
 S W=$O(^AUTTMSR("B","WT",0))
 S Z=0 F  S Z=$O(^AUPNPREF("AA",P,9999999.07,W,Z)) Q:Z'=+Z  D
 .S V=0 F  S V=$O(^AUPNPREF("AA",P,9999999.07,W,Z,V)) Q:V'=+V  D
 ..Q:'$D(^AUPNPREF(V,0))
 ..S D=$P(^AUPNPREF(V,0),U,3)
 ..Q:D>EDATE
 ..Q:D<BDATE
 ..Q:"NRU"'[$P(^AUPNPREF(V,0),U,7)
 ..S W(D)=""
 ..Q
 .Q
 ;is there an H and w on same day?
 S X=0 F  S X=$O(H(X)) Q:X'=+X!(R]"")  I $D(W(X)) S R=1_U_"HT/WT"_U_X
 Q R
