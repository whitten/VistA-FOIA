APCHPWH1 ; IHS/CMI/LAB - Patient Wellness Handout ; 15 Dec 2009  8:20 AM
 ;;2.0;IHS PCC SUITE;**2**;MAY 14, 2009
 ;
S(Y,F,C,T) ;EP - set up array
 I '$G(F) S F=0
 I '$G(T) S T=0
 NEW %,X
 ;blank lines
 F F=1:1:F S X="" D S1
 S X=Y
 I $G(C) S L=$L(Y),T=(80-L)/2 D  D S1 Q
 .F %=1:1:(T-1) S X=" "_X
 F %=1:1:T S X=" "_Y
 D S1
 Q
S1 ;
 S %=$P(^TMP($J,"APCHPWH",0),U)+1,$P(^TMP($J,"APCHPWH",0),U)=%
 S ^TMP($J,"APCHPWH",%)=X
 Q
 ;
EP(APCHSDFN,APCHPWHT,APCHPRTH) ;PEP - PASS DFN get back array of patient wellness handout
 ;handout returned in ^TMP("APCHPHS",$J,"APCHPWH"
 ;APCHPWHT - ien of the PWH type
 ;APCHPRTH - 1 if you don't want the header line printed
 K ^TMP($J,"APCHPWH")
 S ^TMP($J,"APCHPWH",0)=0
 I '$G(APCHPWHT) S APCHPWHT=$O(^APCHPWHT("B","ADULT REGULAR",0))
 I '$G(APCHPWHT) Q
 D SETARRAY
 Q
SETARRAY ;set up array containing pwh
 ;all handouts get this demographic section
 NEW X,APCHPRV,APCHSO,APCHSCMP,APCHSCMI
 I '$G(APCHPRTH) S X="My Wellness Handout",$E(X,40)="Report Date:  "_$$FMTE^XLFDT(DT) D S(X)
 S X="********** CONFIDENTIAL PATIENT INFORMATION ["_$P(^VA(200,DUZ,0),U,2)_"]  "_$$FMTE^XLFDT(DT)_" **********" D S(X)
 ;S X=$P($P(^DPT(APCHSDFN,0),U),",",2)_" "_$P($P(^DPT(APCHSDFN,0),U),",")_"   HRN:  "_$$HRN^AUPNPAT(APCHSDFN,DUZ(2)),$E(X,50)=$S($P(^APCCCTRL(DUZ(2),0),U,13)]"":$P(^APCCCTRL(DUZ(2),0),U,13),1:$P(^DIC(4,DUZ(2),0),U)) D S(X,1)
 S X=$P(^DPT(APCHSDFN,0),U)_"   HRN:  "_$$HRN^AUPNPAT(APCHSDFN,DUZ(2)),$E(X,50)=$S($P(^APCCCTRL(DUZ(2),0),U,13)]"":$P(^APCCCTRL(DUZ(2),0),U,13),1:$P(^DIC(4,DUZ(2),0),U)) D S(X,1)
 S X=$$VAL^XBDIQ1(2,APCHSDFN,.111)
 I $P($G(^APCCCTRL(DUZ(2),0)),U,18)]"" D  I 1
 .S $E(X,50)=$P(^APCCCTRL(DUZ(2),0),U,18)_$S($P(^APCCCTRL(DUZ(2),0),U,18)]"":", ",1:" ")_$S($P($G(^APCCCTRL(DUZ(2),0)),U,19):$P(^DIC(5,$P(^APCCCTRL(DUZ(2),0),U,19),0),U,2),1:"")_"  "_$P(^APCCCTRL(DUZ(2),0),U,21) D S(X)
 E  S $E(X,50)=$$VAL^XBDIQ1(9999999.06,DUZ(2),.15)_$S($$VAL^XBDIQ1(9999999.06,DUZ(2),.15)]"":", ",1:" ")_$S($P($G(^AUTTLOC(DUZ(2),0)),U,16):$P(^DIC(5,$$VALI^XBDIQ1(9999999.06,DUZ(2),.16),0),U,2),1:"") D
 .S X=X_"  "_$$VAL^XBDIQ1(9999999.06,DUZ(2),.17) D S(X)
 S X=$$VAL^XBDIQ1(2,APCHSDFN,.114)_$S($$VAL^XBDIQ1(2,APCHSDFN,.114)]"":",  ",1:" ")_$$VAL^XBDIQ1(2,APCHSDFN,.115)_"   "_$$VAL^XBDIQ1(2,APCHSDFN,.116)
 S APCHPRV=$$DPCP(APCHSDFN)
 I APCHPRV D
 .S $E(X,50)=$P(^VA(200,APCHPRV,0),U) D S(X)
 I 'APCHPRV D S(X)
 S X=$$VAL^XBDIQ1(2,APCHSDFN,.131),$E(X,50)=$P(^AUTTLOC(DUZ(2),0),U,11) D S(X)  ;put provider phone at 50
 ;I $G(APCDVSIT)]"",$D(^AUPNVSIT("AC",APCHSDFN,APCDVSIT)) S APCHPROV=$$PRIMPROV^APCLV(APCDVSIT)
 ;S X="Hello "_$S($$SEX^AUPNPAT(APCHSDFN)="M":"Mr. ",1:"Ms. ")_$E($P($P(^DPT(APCHSDFN,0),U),","))_$$LOW^XLFSTR($E($P($P(^DPT(APCHSDFN,0),U),","),2,99))_"," D S(X,1)
 S X="Thank you for choosing "_$S($P(^APCCCTRL(DUZ(2),0),U,13)]"":$P(^APCCCTRL(DUZ(2),0),U,13),1:$P(^DIC(4,DUZ(2),0),U))_"." D S(X,1)
 S X="This handout is a new way for you and your doctor to look at your health." D S(X)
 ;now process each component assigned to this type
 ;
COMPS ;
 ;I $$AGE^AUPNPAT(APCHSDFN)<18 D S("This handout is designed for patients 18 years of age and older.",2) Q
 S APCHSORD=0 F  S APCHSORD=$O(^APCHPWHT(APCHPWHT,1,APCHSORD)) Q:APCHSORD'=+APCHSORD  D
 .S APCHSCMP=$P(^APCHPWHT(APCHPWHT,1,APCHSORD,0),U,2)
 .Q:'APCHSCMP
 .Q:'$D(^APCHPWHC(APCHSCMP,0))
 .S APCHSCMI=$P(^APCHPWHC(APCHSCMP,0),U,2)
 .D @($P(APCHSCMI,";",1)_U_$P(APCHSCMI,";",2))
 S X="******** END CONFIDENTIAL PATIENT INFORMATION ["_$P(^VA(200,DUZ,0),U,2)_"]  "_$$FMTE^XLFDT(DT)_" ********" D S(X,2)
 Q
 ;
DPCP(P) ;EP
 NEW R
 D ALLDP^BDPAPI(P,"DESIGNATED PRIMARY PROVIDER",.R)
 I $D(R("DESIGNATED PRIMARY PROVIDER")) Q $P(R("DESIGNATED PRIMARY PROVIDER"),U,2)
 S R=$P(^AUPNPAT(P,0),U,14) I R Q R
 S R=""
 Q R
 ;
HELP1 ;EP - called from help prompt of structure multiple
 D EN^DDIOL("This field contains a number which specifies the relative order in which")
 D EN^DDIOL("the related component will appear on the Patient Wellness Handout.")
 D EN^DDIOL("The values for this field (i.e., for separte entries in the STRUCTURE")
 D EN^DDIOL("multiple) need not be sequential, and need not be entered in sequence.")
 D EN^DDIOL("For example, if entered in the order 5 10 7 15, the related components")
 D EN^DDIOL("will appear in the order 5 7 10 15.")
 Q
HELP2 ;EP - called from help prompt of measure multiple
 D EN^DDIOL("This field contains a number which specifies the relative order in which")
 D EN^DDIOL("the related MEASURE will appear within the QUALITY OF CARE TRANSPARENCY")
 D EN^DDIOL("REPORT CARD component.  The values for this field (i.e., for separate")
 D EN^DDIOL("entries in the SEQUENCE multiple) need not be sequential, and need not")
 D EN^DDIOL("be entered in sequence.  For example, if entered in the order 5 10 7 15,")
 D EN^DDIOL("the related components will appear in the order 5 7 10 15.")
 Q
