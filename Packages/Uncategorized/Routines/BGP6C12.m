BGP6C12 ; IHS/CMI/LAB - calc CMS measures 26 Sep 2004 11:28 AM ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;
EN ;EP
 K BGPDATA
ALLALG ;
 I BGPIND=3 G ALLRX
 S ^XTMP("BGP6C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,21,"ALL Allergies from Problem List:")=""
 K BGPDATA
 D ALLALG1(DFN,$$DSCH(BGPVINP),.BGPDATA)
 S X=0 F  S X=$O(BGPDATA(X)) Q:X'=+X  D
 .S ^XTMP("BGP6C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,21,"ALL Allergies from Problem List:",X)=BGPDATA(X)
ALLALGA ;
 S ^XTMP("BGP6C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,22,"ALL Allergies from Allergy Tracking:")=""
 K BGPDATA
 D ALLALGA1(DFN,$$DSCH(BGPVINP),.BGPDATA)
 S X=0 F  S X=$O(BGPDATA(X)) Q:X'=+X  D
 .S ^XTMP("BGP6C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,22,"ALL Allergies from Allergy Tracking:",X)=BGPDATA(X)
ALLRX ;
 ;S V=$P($P(BGPVSIT0,U),"."),Z="Last of each drug dispensed "_$$DATE^BGP6UTL($$FMADD^XLFDT(V,-365))_"-"_$$DATE^BGP6UTL($$FMADD^XLFDT($$DSCH(BGPVINP),30))_":"
 ;S ^XTMP("BGP6C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,23,Z)=""
 ;K BGPDATA
 ;D ALLRX1(DFN,$P($P(BGPVSIT0,U),"."),$$DSCH(BGPVINP),.BGPDATA)
 ;S X=0 F  S X=$O(BGPDATA(X)) Q:X'=+X  D
 ;.S W=0 F  S W=$O(BGPDATA(X,W)) Q:W'=+W  D
 ;..S V=$P($P(BGPVSIT0,U),"."),Z="Last of each drug dispensed "_$$DATE^BGP6UTL($$FMADD^XLFDT(V,-365))_"-"_$$DATE^BGP6UTL($$FMADD^XLFDT($$DSCH(BGPVINP),30))_":"
 ;..S ^XTMP("BGP6C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,23,Z,W)=BGPDATA(X,W)
ALLINPM ;
 K BGPY,BGPDATA
 S V=$P($P(BGPVSIT0,U),"."),Z="ALL Unit Dose/IV Meds during Hospital Stay: "_$$DATE^BGP6UTL($P($P(BGPVSIT0,U),"."))_"-"_$$DATE^BGP6UTL($$DSCH(BGPVINP))_":"
 S ^XTMP("BGP6C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,24,Z)=""
 K BGPDATA
 D IVUD(DFN,$P($P(BGPVSIT0,U),"."),$$DSCH(BGPVINP),,.BGPDATA)
 S X=0 F  S X=$O(BGPDATA(X)) Q:X'=+X  D
 .S W=0 F  S W=$O(BGPDATA(X,W)) Q:W'=+W  D
 ..S V=$P($P(BGPVSIT0,U),"."),Z="ALL Unit Dose/IV Meds during Hospital Stay: "_$$DATE^BGP6UTL($P($P(BGPVSIT0,U),"."))_"-"_$$DATE^BGP6UTL($$DSCH(BGPVINP))_":"
 ..S ^XTMP("BGP6C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,24,Z,W)=BGPDATA(X,W)
 Q
DSCH(H) ;
 Q $P($P(^AUPNVINP(H,0),U),".")
ALLALG1(P,BGPD,BGPY) ;
 ;get all ALLERGIES FROM PROBLEM LIST UP THROUGH DATE OF DISCHARGE ADDED
 NEW ED,BD,BGPG,BGPC,X,Y,Z,N
 ;BGPD is discharge date
 S BGPC=0
 S X=0 F  S X=$O(^AUPNPROB("AC",P,X)) Q:'X  D
 .;S I=$P(^ICD9(+^AUPNPROB(X,0),0),U,1)
 .S I=$P($$ICDDX^ICDCODE(+^AUPNPROB(X,0)),U,2)
 .S Z=$$PROBACHK(I,X)
 .Q:Z=0
 .S D=$P(^AUPNPROB(X,0),U,8)
 .Q:D>BGPD
 .I Z=2 D  Q
 ..S BGPC=BGPC+1,BGPY(BGPC)="NO ALLERGY NOTED ON "_$$DATE^BGP6UTL(D)
 .S N=$P(^AUTNPOV(+$P(^AUPNPROB(X,0),U,5),0),U,1)
 .I N="" S N="???"
 .S BGPC=BGPC+1,BGPY(BGPC)="["_I_"]  "_N_"  "_$$DATE^BGP6UTL(D)
 .Q
 Q
ALLALGA1(P,BGPD,BGPY) ;all allergies from the allergy tracking system
 ;
 ;now check allergy tracking
 S BGPC=0
 S X=0 F  S X=$O(^GMR(120.8,"B",P,X)) Q:X'=+X  D
 .Q:$P($G(^GMR(120.8,X,0)),U,26)>BGPD  ;entered after discharge date
 .S N=$P($G(^GMR(120.8,X,0)),U,2),N=$$UP^XLFSTR(N)
 .S BGPC=BGPC+1,BGPY(BGPC)=N_"  "_$$DATE^BGP6UTL($P(^GMR(120.8,X,0),U,4))
 Q
PROBACHK(I,X) ;checking for allergy codes
 I I="692.3" Q 1
 I I="693.0" Q 1
 I I="995.0" Q 1
 I I=995.2 Q 1
 I (+I'<999.4),(+I'>999.8) Q 1
 I I?1"V14."1E Q 1
 I I="692.5" Q 1
 I I="693.1" Q 1
 I I["V15.0" Q 1
 I $E(I,1,3)=692,I'="692.9" Q 1
 I I="693.8" Q 1
 I I="693.9" Q 1
 I I="989.5" Q 1
 I I="989.82" Q 1
 I I="995.3" Q 1
 I $P(^AUPNPROB(X,0),U,5)="" Q 0
 S N=$P(^AUTNPOV($P(^AUPNPROB(X,0),U,5),0),U)
 I I="799.9"!(I="V82.9"),N["NO KNOWN ALLERG"!(N["NKA")!(N["NKDA")!(N["NO KNOWN DRUG ALLERG") Q 2
 Q 0
ALLRX1(P,BGPA,BGPD,BGPY) ;
 NEW BGPG,BGPC,X,Y,Z,E,BD,ED
 S BGPC=0
 K ^TMP($J,"A")
 S ED=$$FMADD^XLFDT(BGPD,30),ED=9999999-ED,ED=ED-1
 S BD=$$FMADD^XLFDT(BGPD,-365),BD=9999999-BD
 S D=ED F  S D=$O(^AUPNVMED("AA",P,D)) Q:D'=+D!(D>BD)  D
 .S N=0 F  S N=$O(^AUPNVMED("AA",P,D,N)) Q:N'=+N  D
 ..S C=$P($G(^AUPNVMED(N,0)),U)
 ..Q:C=""
 ..Q:'$D(^PSDRUG(C,0))
 ..S C=$P(^PSDRUG(C,0),U)
 ..S ^TMP($J,"A",C,D)=N
 ..Q
 S D="" F  S D=$O(^TMP($J,"A",D)) Q:D=""  D
 .S A=$O(^TMP($J,"A",D,0))
 .S B=9999999-A
 .S Y=^TMP($J,"A",D,A)
 .S BGPC=BGPC+1,BGPY(A,BGPC)=D_"   "_$P(^AUPNVMED(Y,0),U,5)_"  qty: "_$P(^AUPNVMED(Y,0),U,6)_" days: "_$P(^AUPNVMED(Y,0),U,7)_" "_$$DATE^BGP6UTL(B)
 .Q
 K ^TMP($J,"A")
 K BGPG
 Q
 ;
IVUD(P,BD,ED,TAX,BGPY) ;EP
 ;p - patient
 ;bd - beg date
 ;ed - ending date
 ;BGPY - return array
 ;tax - taxonomy ien
 NEW C,X,E,D
 S TAX=$G(TAX)
 S C=0
 S X=0 F  S X=$O(^PS(55,P,5,X)) Q:X'=+X  D
 .S D=$P($G(^PS(55,P,5,X,.1)),U)
 .Q:D=""
 .I TAX Q:'$D(^ATXAX(TAX,21,"B",D))
 .S E=$P($P($G(^PS(55,P,5,X,2)),U,2),".",1)
 .Q:E>ED
 .Q:E<BD
 .S C=C+1,BGPY(C)="Unit Dose:  "_$P(^PS(50.3,D,0),U)_"  Date: "_$$DATE^BGP6UTL(E)
 .Q
 S X=0 F  S X=$O(^PS(55,P,"IV",X)) Q:X'=+X  D
 .S E=$P(^PS(55,P,"IV",X,0),U,2),E=$P(E,".")
 .Q:E>ED
 .Q:E<BD
 .S D=$P($G(^PS(55,P,"IV",X,6)),U)
 .Q:D=""
 .I TAX Q:'$D(^ATXAX(TAX,21,"B",D))
 .S C=C+1,BGPY((9999999-E),C)="IV:  "_$P(^PS(50.3,D,0),U)_"  Date: "_$$DATE^BGP6UTL(E)
 .Q
 Q
SMOKER ;EP
 S ^XTMP("BGP6C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,8,"Smoker?")=""
 K BGPASAAL
 D SMOKER1(DFN,$P($P(BGPVSIT0,U),"."),$$DSCH(BGPVINP),.BGPASAAL)
 S X=0 F  S X=$O(BGPASAAL(X)) Q:X'=+X  D
 .S ^XTMP("BGP6C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,8,"Smoker?",X)=BGPASAAL(X)
CESS ;
 S ^XTMP("BGP6C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,9,"Smoking Cessation Advice/Counseling Status?")=""
 K BGPASAAL
 K BGPY D CESS1(DFN,$P($P(BGPVSIT0,U),"."),$$DSCH(BGPVINP),.BGPASAAL)
 S X=0 F  S X=$O(BGPASAAL(X)) Q:X'=+X  D
 .S ^XTMP("BGP6C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,9,"Smoking Cessation Advice/Counseling Status?",X)=BGPASAAL(X)
ST ;
 S ^XTMP("BGP6C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,17,"ST Elevation?")=""
 K BGPASAAL
 D ST1(DFN,$P($P(BGPVSIT0,U),"."),$$DSCH(BGPVINP),.BGPASAAL)
 S X=0 F  S X=$O(BGPASAAL(X)) Q:X'=+X  D
 .S ^XTMP("BGP6C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,17,"ST Elevation?",X)=BGPASAAL(X)
LBBB ;
 S ^XTMP("BGP6C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,18,"LBBB on ECG?")=""
 K BGPASAAL
 D LBBB1(DFN,$P($P(BGPVSIT0,U),"."),$$DSCH(BGPVINP),.BGPASAAL)
 S X=0 F  S X=$O(BGPASAAL(X)) Q:X'=+X  D
 .S ^XTMP("BGP6C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,18,"LBBB on ECG?",X)=BGPASAAL(X)
TA ;
 S ^XTMP("BGP6C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,19,"Thrombolytic Agent Rx Status?")=""
 K BGPASAAL
 D TA1(DFN,$P($P(BGPVSIT0,U),"."),$$DSCH(BGPVINP),.BGPASAAL)
 S X=0 F  S X=$O(BGPASAAL(X)) Q:X'=+X  D
 .S ^XTMP("BGP6C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,19,"Thrombolytic Agent Rx Status?",X)=BGPASAAL(X)
PCI ;
 S ^XTMP("BGP6C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,20,"PCI Status?")=""
 K BGPASAAL
 D PCI1(DFN,$P($P(BGPVSIT0,U),"."),$$DSCH(BGPVINP),.BGPASAAL)
 S X=0 F  S X=$O(BGPASAAL(X)) Q:X'=+X  D
 .S ^XTMP("BGP6C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,20,"PCI Status?",X)=BGPASAAL(X)
 Q
SMOKER1(P,BGPA,BGPD,BGPY) ;EP
 K BGPY S BGPC=0
 NEW BGPTOB,BGPDX,BGPDENT
 S BGPTOB=$$TOBACCO^BGP6D7(P,$$FMADD^XLFDT(BGPA,-365),BGPA)
 I BGPTOB["CURRENT SMOKER"!(BGPTOB["CESSATION-SMOKER") S BGPC=BGPC+1,BGPY(BGPC)="Yes, Health Factor: "_$P(BGPTOB,U)_" on "_$P(BGPTOB,U,2)
 S BGPDX=$$DX^BGP6D7(P,$$FMADD^XLFDT(BGPA,-365),BGPA)
 I BGPDX]"",$P(BGPDX,U)'="305.13" S BGPC=BGPC+1,BGPY(BGPC)="Yes, Diagnosis: "_$P(BGPDX,U)_" on "_$$DATE^BGP6UTL($P(BGPDX,U,2))
 S BGPDENT=$$DENT^BGP6D7(P,$$FMADD^XLFDT(BGPA,-365),BGPA)
 I BGPDENT]"" S BGPC=BGPC+1,BGPY(BGPC)="Yes, "_$P(BGPDENT,U)_" on "_$$DATE^BGP6UTL($P(BGPDENT,U,2))
 Q
CESS1(P,BDATE,EDATE,BGPY) ;EP
 K BGPALLED K BGPY S BGPC=0
 S Y="BGPALLED("
 S X=P_"^ALL EDUC;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE) S E=$$START1^APCLDF(X,Y)
 I $D(BGPALLED(1)) S %="" D
 .S (X,D)=0,%="",T="" F  S X=$O(BGPALLED(X)) Q:X'=+X  D
 ..S T=$P(^AUPNVPED(+$P(BGPALLED(X),U,4),0),U)
 ..Q:'T
 ..Q:'$D(^AUTTEDT(T,0))
 ..S T=$P(^AUTTEDT(T,0),U,2)
 ..I $P(T,"-")="TO" S BGPC=BGPC+1,BGPY(BGPC)=$$DATE^BGP6UTL($P(BGPALLED(X),U))_" Topic: "_T Q
 ..I $P(T,"-",2)="TO" S BGPC=BGPC+1,BGPY(BGPC)=$$DATE^BGP6UTL($P(BGPALLED(X),U))_" Topic: "_T Q
 ..I $P(T,"-",2)="SHS" S BGPC=BGPC+1,BGPY(BGPC)=$$DATE^BGP6UTL($P(BGPALLED(X),U))_" Topic: "_T Q
 K ^TMP($J,"A")
 S A="^TMP($J,""A"",",B=P_"^ALL VISITS;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(B,A)
 I '$D(^TMP($J,"A",1)) Q ""
 S X=0,G="" F  S X=$O(^TMP($J,"A",X)) Q:X'=+X  S V=$P(^TMP($J,"A",X),U,5) D
 .Q:'$D(^AUPNVSIT(V,0))
 .Q:'$P(^AUPNVSIT(V,0),U,9)
 .Q:$P(^AUPNVSIT(V,0),U,11)
 .S B=$$CLINIC^APCLV(V,"C")
 .I B=94 S BGPC=BGPC+1,BGPY(BGPC)=$$DATE^BGP6UTL($P($P(^AUPNVSIT(V,0),U),"."))_" Clinic 94 visit" Q
 .S Z=0 F  S Z=$O(^AUPNVDEN("AD",V,Z)) Q:Z'=+Z  S B=$P($G(^AUPNVDEN(Z,0)),U) I B S B=$P($G(^AUTTADA(B,0)),U) I B=1320 S BGPC=BGPC+1,BGPY(BGPC)=$$DATE^BGP6UTL($P($P(^AUPNVSIT(V,0),U),"."))_" ADA 1320"
 .Q
 I $D(BGPY) Q
 ;now check all refusals of these education topics
 S G="",X=0 F  S X=$O(^AUPNPREF("AA",P,9999999.09,X)) Q:X=""  D
 .S D=0 F  S D=$O(^AUPNPREF("AA",P,9999999.09,X,D)) Q:D=""!(G]"")  D
 ..S I=0 F  S I=$O(^AUPNPREF("AA",P,9999999.09,X,D,I)) Q:I'=+I!(G]"")  D
 ...S Z=$P($G(^AUPNPREF(I,0)),U,3)
 ...Q:Z=""
 ...I Z<BDATE Q
 ...I Z>EDATE Q
 ...S Y=$P($G(^AUTTEDT(X,0)),U,2)
 ...I $P(Y,"-")="TO"!($P(Y,"-",2)="TO")!($P(Y,"-",2)="SHS") S BGPC=BGPC+1,BGPY(BGPC)=$$DATE^BGP6UTL(Z)_" refusal of Topic: "_Y
 Q
PCI1(P,BDATE,EDATE,BGPY) ;
 K BGPY S BGPC=0
 K BGPG
 S Y="BGPG("
 S X=P_"^ALL PROC 00.66;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE) S E=$$START1^APCLDF(X,Y)
 S X=0 F  S X=$O(BGPG(X)) Q:X'=+X  S BGPC=BGPC+1,BGPY(BGPC)=$$DATE^BGP6UTL($P(BGPG(X),U))_" "_$P(BGPG(X),U,2)_" "_$$VAL^XBDIQ1(9000010.08,+$P(BGPG(X),U,4),.04)
 K BGPG
 Q
ST1(P,BDATE,EDATE,BGPY) ;
 K BGPY S BGPC=0
 K BGPG
 S Y="BGPG("
 S X=P_"^ALL DX [BGP ST ELEVATION DX;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE) S E=$$START1^APCLDF(X,Y)
 S X=0 F  S X=$O(BGPG(X)) Q:X'=+X  S BGPC=BGPC+1,BGPY(BGPC)=$$DATE^BGP6UTL($P(BGPG(X),U))_" "_$P(BGPG(X),U,2)_" "_$$VAL^XBDIQ1(9000010.07,+$P(BGPG(X),U,4),.04)
 K BGPG
 Q
LBBB1(P,BDATE,EDATE,BGPY) ;
 K BGPY S BGPC=0
 K BGPG
 S Y="BGPG("
 S X=P_"^ALL DX 426.3;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE) S E=$$START1^APCLDF(X,Y)
 I '$D(BGPG) Q
 S BGPC=BGPC+1,BGPY(BGPC)=$$DATE^BGP6UTL($P(BGPG(1),U))_" "_$P(BGPG(1),U,2)_" "_$$VAL^XBDIQ1(9000010.07,+$P(BGPG(1),U,4),.04)
 K BGPG
 S Y="BGPG("
 S X=$$LASTPRC^BGP6UTL1(P,"BGP LBBB ON ECG PROC",BDATE,EDATE)
 I X]"" S BGPC=BGPC+1,BGPY(BGPC)="Procedure: "_$$DATE^BGP6UTL($P(X,U,3))_" "_$P(X,U,2)_" "_$$VAL^XBDIQ1(9000010.08,$P(X,U,4),.04)
 K BGPG
 ;now check for CPT codes
 S X=$$CPT^BGP6DU(P,BDATE,EDATE,$O(^ATXAX("B","BGP LBBB ON ECG CPT",0)),6)
 I X]"" S BGPC=BGPC+1,BGPY(BGPC)="CPT: "_$$DATE^BGP6UTL($P(X,U,2))_" "_$P(X,U,3)
 S X=$$TRAN^BGP6DU(P,BDATE,EDATE,$O(^ATXAX("B","BGP LBBB ON ECG CPT",0)),6)
 I X]"" S BGPC=BGPC+1,BGPY(BGPC)="TRAN CODE CPT: "_$$DATE^BGP6UTL($P(X,U,2))_" "_$P(X,U,3)
 Q
TA1(P,BGPA,BGPD,BGPY) ;
 ;get last TA rx before date of adm
 NEW BGPG,BGPC,X,Y,Z,E,BD,ED,G,D
 S BGPC=0 K BGPY
 S ED=$$FMADD^XLFDT(BGPA,-1)
 S BD=$$FMADD^XLFDT(BGPA,-365)
 D GETMEDS^BGP6C1(P,BD,ED,"BGP CMS THROMBOLYTIC MEDS","","BGP THROMBOLYTIC AGENT CLASS")
 S BD=BGPA
 S ED=$$FMADD^XLFDT(BGPD,30)
 D GETMEDS^BGP6C1(P,BD,ED,"BGP CMS THROMBOLYTIC MEDS","","BGP THROMBOLYTIC AGENT CLASS")
 K BGPG
 Q
