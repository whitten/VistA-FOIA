BGP7C3 ; IHS/CMI/LAB - calc CMS measures 26 Sep 2004 11:28 AM ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;
PNEU ;
 ;
 I '$$PNEUDX(BGPVSIT) Q
 ;
 S BGPX=$P(^DPT(DFN,0),U)_U_$$HRN^AUPNPAT(DFN,DUZ(2))_U_$P(^DPT(DFN,0),U,2)_U_$$AGE^AUPNPAT(DFN,$P($P(BGPVSIT0,U),"."))
 S $P(BGPX,U,5)=$$DATE^BGP7UTL($P($P(BGPVSIT0,U),"."))_"-"_$$DATE^BGP7UTL($$DSCH(BGPVINP))
 S $P(BGPX,U,6)=$$PRIMPOV^APCLV(BGPVSIT,"C")_"  "_$$PRIMPOV^APCLV(BGPVSIT,"N")
 S BGPSKIP=0 K BGPZ
 I $$AGE^AUPNPAT(DFN,$P($P(BGPVSIT0,U),"."))<18 S BGPX="*"_BGPX,BGPSKIP=1,BGPZ(1)="under 18 yrs of age"
 S Z=$$VAL^XBDIQ1(9000010.02,BGPVINP,.06)
 S $P(BGPX,U,7)=Z
 ;I $$TRANSIN(BGPVINP) S:'BGPSKIP BGPX="*"_BGPX S BGPSKIP=1,BGPZ(2)="transferred in from another hospital"
 S Z=$$VAL^XBDIQ1(9000010.02,BGPVINP,.07)
 S $P(BGPX,U,8)=Z
 Q:$D(^XTMP("BGP7C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 1",$P(^DPT(DFN,0),U),DFN,BGPVSIT))
 S ^XTMP("BGP7C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 1",$P(^DPT(DFN,0),U),DFN,BGPVSIT)=BGPX  ;a hit on list 1
 K BGPZ1 I $D(BGPZ) S X=0 F  S X=$O(BGPZ(X)) Q:X'=+X  S:$G(BGPZ1)]"" BGPZ1=BGPZ1_", " S BGPZ1=$G(BGPZ1)_BGPZ(X)
 I $D(BGPZ1) S BGPZ1="Exclusions: "_BGPZ1 S $P(^XTMP("BGP7C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 1",$P(^DPT(DFN,0),U),DFN,BGPVSIT),U,12)=BGPZ1
 S BGPCOUNT("L1",BGPIND)=$G(BGPCOUNT("L1",BGPIND))+1
 ;set up second list after applying exclusions
 Q:BGPSKIP
 S BGPX=$P(^DPT(DFN,0),U)_U_$$HRN^AUPNPAT(DFN,DUZ(2))_U_$P(^DPT(DFN,0),U,2)_U_$$AGE^AUPNPAT(DFN,$P($P(BGPVSIT0,U),"."))
 S $P(BGPX,U,5)=$$DATE^BGP7UTL($P($P(BGPVSIT0,U),"."))_"-"_$$DATE^BGP7UTL($$DSCH(BGPVINP))
 S $P(BGPX,U,6)=$$PRIMPOV^APCLV(BGPVSIT,"C")_"  "_$$PRIMPOV^APCLV(BGPVSIT,"N")
 S $P(BGPX,U,7)=$$VAL^XBDIQ1(9000010.02,BGPVINP,.06)
 S $P(BGPX,U,8)=$$VAL^XBDIQ1(9000010.02,BGPVINP,.07)
 S ^XTMP("BGP7C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT)=BGPX
 S BGPCOUNT("L2",BGPIND)=$G(BGPCOUNT("L2",BGPIND))+1
 ;get other povs
 S ^XTMP("BGP7C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,1,"Other Discharge POVs:")=""
 S (X,C)=0 F  S X=$O(^AUPNVPOV("AD",BGPVSIT,X)) Q:X'=+X  D
 .Q:'$D(^AUPNVPOV(X,0))
 .Q:$P(^AUPNVPOV(X,0),U,12)="P"
 .S I=$P(^AUPNVPOV(X,0),U),I=$P($$ICDDX^ICDCODE(I),U,2)
 .S N=$$VAL^XBDIQ1(9000010.07,X,.04),N=$$UP^XLFSTR(N)
 .S C=C+1
 .S ^XTMP("BGP7C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,1,"Other Discharge POVs:",C)=I,$E(^(C),9)=N
 .Q
ABIRX ;
 S ^XTMP("BGP7C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,2,"Recent Antibiotic Rx Status?")=""
 K BGPDATA
 D ABRX1(DFN,$P($P(BGPVSIT0,U),"."),$$DSCH(BGPVINP),.BGPDATA)
 S X=0 F  S X=$O(BGPDATA(X)) Q:X'=+X  D
 .S ^XTMP("BGP7C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,2,"Recent Antibiotic Rx Status?",X)=BGPDATA(X)
PNEUVAX ;
 S ^XTMP("BGP7C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,3,"Pneumovax Status?")=""
 K BGPDATA
 D PNEUVAX1(DFN,.BGPDATA)
 S X=0 F  S X=$O(BGPDATA(X)) Q:X'=+X  D
 .S ^XTMP("BGP7C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,3,"Pneumovax Status?",X)=BGPDATA(X)
ABGPO ;
 S ^XTMP("BGP7C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,4,"ABG/PO Status?")=""
 K BGPDATA
 D ABGPO1^BGP7C31(DFN,$P($P(BGPVSIT0,U),"."),$$DSCH(BGPVINP),.BGPDATA)
 S X=0 F  S X=$O(BGPDATA(X)) Q:X'=+X  D
 .S ^XTMP("BGP7C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,4,"ABG/PO Status?",X)=BGPDATA(X)
ERBC ;
 S ^XTMP("BGP7C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,5,"ER Visit with Blood Culture Status?")=""
 K BGPDATA
 D ERBC1(DFN,$$FMADD^XLFDT($P($P(BGPVSIT0,U),"."),-1),$P($P(BGPVSIT0,U),"."),.BGPDATA)
 S X=0 F  S X=$O(BGPDATA(X)) Q:X'=+X  D
 .S ^XTMP("BGP7C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,5,"ER Visit with Blood Culture Status?",X)=BGPDATA(X)
SMOKER ;EP
 S ^XTMP("BGP7C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,6,"Smoker?")=""
 K BGPASAAL
 D SMOKER1^BGP7C12(DFN,$P($P(BGPVSIT0,U),"."),$$DSCH(BGPVINP),.BGPASAAL)
 S X=0 F  S X=$O(BGPASAAL(X)) Q:X'=+X  D
 .S ^XTMP("BGP7C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,6,"Smoker?",X)=BGPASAAL(X)
CESS ;
 S ^XTMP("BGP7C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,7,"Smoking Cessation Advice/Counseling Status?")=""
 K BGPASAAL
 D CESS1^BGP7C12(DFN,$P($P(BGPVSIT0,U),"."),$$DSCH(BGPVINP),.BGPASAAL)
 S X=0 F  S X=$O(BGPASAAL(X)) Q:X'=+X  D
 .S ^XTMP("BGP7C1",BGPJ,BGPH,BGPORDER,BGPIND,"LIST 2",$P(^DPT(DFN,0),U),DFN,BGPVSIT,7,"Smoking Cessation Advice/Counseling Status?",X)=BGPASAAL(X)
 D EN^BGP7C12
 Q
ABRX1(P,BGPA,BGPD,BGPY) ;EP
 NEW BGPG,BGPC,X,Y,Z,E,BD,ED
 S BGPC=0
 S ED=$$FMADD^XLFDT(BGPA,-1)
 S BD=$$FMADD^XLFDT(BGPA,-365)
 D GETMEDS^BGP7C1(P,BD,ED,"BGP CMS ANTIBIOTIC MEDS","BGP CMS ANTIBIOTIC MEDS NDC","BGP CMS ANTIBIOTICS MEDS CLASS")
 S X=$$CPTI^BGP7DU(P,BD,ED,+$$CODEN^ICPTCOD("G8012"))
 I X S BGPC=BGPC+1,BGPY(BGPC)="CPT code G8012: "_$$DATE^BGP7UTL($P(X,U,2))
 S X=$$TRANI^BGP7DU(P,BD,ED,+$$CODEN^ICPTCOD("G8012"))
 I X S BGPC=BGPC+1,BGPY(BGPC)="TRAN CODE CPT code G8012: "_$$DATE^BGP7UTL($P(X,U,2))
 ;now see if any procedures
 S X=0 F  S X=$O(^AUPNVPRC("AC",P,X)) Q:X'=+X  D
 .Q:'$D(^AUPNVPRC(X,0))
 .S I=$P($G(^AUPNVPRC(X,0)),U) Q:'I
 .S Y=$P($$ICDOP^ICDCODE(I),U,2)
 .I Y=99.21 D
 ..S V=$P(^AUPNVPRC(X,0),U,3)
 ..S V=$P($P($G(^AUPNVSIT(V,0)),U),".")
 ..I V>ED Q
 ..I V<BD Q
 ..S BGPC=BGPC+1,BGPY(BGPC)="ANTIBIOTIC PROCEDURE:  "_$$DATE^BGP7UTL(V)_"  ["_Y_"]  "_$$VAL^XBDIQ1(9000010.08,X,.04)
 S BD=BGPA
 S ED=$$FMADD^XLFDT(BGPD,30)
 D GETMEDS^BGP7C1(P,BD,ED,"BGP CMS ANTIBIOTIC MEDS","BGP CMS ANTIBIOTIC MEDS NDC","BGP CMS ANTIBIOTICS MEDS CLASS")
 S X=$$CPTI^BGP7DU(P,BD,ED,+$$CODEN^ICPTCOD("G8012"))
 I X S BGPC=BGPC+1,BGPY(BGPC)="CPT code G8012: "_$$DATE^BGP7UTL($P(X,U,2))
 S X=$$TRANI^BGP7DU(P,BD,ED,+$$CODEN^ICPTCOD("G8012"))
 I X S BGPC=BGPC+1,BGPY(BGPC)="TRAN CODE CPT code G8012: "_$$DATE^BGP7UTL($P(X,U,2))
 ;now see if any procedures
 S X=0 F  S X=$O(^AUPNVPRC("AC",P,X)) Q:X'=+X  D
 .Q:'$D(^AUPNVPRC(X,0))
 .S I=$P($G(^AUPNVPRC(X,0)),U) Q:'I
 .S Y=$P($$ICDOP^ICDCODE(I),U,2)
 .I Y=99.21 D
 ..S V=$P(^AUPNVPRC(X,0),U,3)
 ..S V=$P($P($G(^AUPNVSIT(V,0)),U),".")
 ..I V>ED Q
 ..I V<BD Q
 ..S BGPC=BGPC+1,BGPY(BGPC)="ANTIBIOTIC PROCEDURE:  "_$$DATE^BGP7UTL(V)_"  ["_Y_"]  "_$$VAL^XBDIQ1(9000010.08,X,.04)
 Q
SET ;
 S BGPX(D)=C1_" "_C_"  "_$$DATE^BGP7UTL(D)
 Q
PNEUVAX1(P,BGPY) ;
 K BGPG,BGPX
 S BGPC=0
 S X=P_"^ALL IMM 33" S E=$$START1^APCLDF(X,"BGPG(")
 S X=0 F  S X=$O(BGPG(X)) Q:X'=+X  S Y=+$P(BGPG(X),U,4) D
 .Q:'$D(^AUPNVIMM(Y,0))
 .S Y=$P(^AUPNVIMM(Y,0),U,1)
 .Q:'Y
 .S C=$P($G(^AUTTIMM(Y,0)),U)
 .Q:C=""
 .S D=$P(BGPG(X),U),C1=$P(^AUTTIMM(Y,0),U,3)
 .D SET
 .Q
 K BGPG S X=P_"^ALL IMM 100" S E=$$START1^APCLDF(X,"BGPG(")
 S X=0 F  S X=$O(BGPG(X)) Q:X'=+X  S Y=+$P(BGPG(X),U,4) D
 .Q:'$D(^AUPNVIMM(Y,0))
 .S Y=$P(^AUPNVIMM(Y,0),U,1)
 .Q:'Y
 .S C=$P($G(^AUTTIMM(Y,0)),U)
 .Q:C=""
 .S D=$P(BGPG(X),U),C1=$P(^AUTTIMM(Y,0),U,3)
 .D SET
 .Q
 K BGPG S X=P_"^ALL IMM 109" S E=$$START1^APCLDF(X,"BGPG(")
 S X=0 F  S X=$O(BGPG(X)) Q:X'=+X  S Y=+$P(BGPG(X),U,4) D
 .Q:'$D(^AUPNVIMM(Y,0))
 .S Y=$P(^AUPNVIMM(Y,0),U,1)
 .Q:'Y
 .S C=$P($G(^AUTTIMM(Y,0)),U)
 .Q:C=""
 .S D=$P(BGPG(X),U),C1=$P(^AUTTIMM(Y,0),U,3)
 .D SET
 .Q
 K BGPG S %=P_"^ALL PROCEDURE 99.55",E=$$START1^APCLDF(%,"BGPG(")
 S X=0 F  S X=$O(BGPG(X)) Q:X'=+X  S Y=+$P(BGPG(X),U,4) D
 .Q:'$D(^AUPNVPRC(Y,0))
 .S Y=$P(^AUPNVPRC(Y,0),U,1)
 .Q:'Y
 .S D=$P(BGPG(X),U)
 .S C=$P($$ICDOP^ICDCODE(Y,D),U,4)
 .Q:C=""
 .S C1=$P($$ICDOP^ICDCODE(Y,D),U,2)
 .D SET
 .Q
 K BGPG S %=P_"^ALL DX V03.82",E=$$START1^APCLDF(%,"BGPG(")
 S X=0 F  S X=$O(BGPG(X)) Q:X'=+X  S Y=+$P(BGPG(X),U,4) D
 .Q:'$D(^AUPNVPOV(Y,0))
 .S Y=$P(^AUPNVPOV(Y,0),U,1)
 .Q:'Y
 .S D=$P(BGPG(X),U)
 .S C=$P($$ICDDX^ICDCODE(Y,D),U,4)
 .Q:C=""
 .S C1=$P($$ICDDX^ICDCODE(Y,D),U,2)
 .D SET
 .Q
 K BGPG S %=P_"^ALL DX V03.89",E=$$START1^APCLDF(%,"BGPG(")
 S X=0 F  S X=$O(BGPG(X)) Q:X'=+X  S Y=+$P(BGPG(X),U,4) D
 .Q:'$D(^AUPNVPOV(Y,0))
 .S Y=$P(^AUPNVPOV(Y,0),U,1)
 .Q:'Y
 .S D=$P(BGPG(X),U)
 .S C=$P($$ICDDX^ICDCODE(Y,D),U,4)
 .Q:C=""
 .S C1=$P($$ICDDX^ICDCODE(Y,D),U,2)
 .D SET
 .Q
 K BGPG S %=P_"^ALL DX V06.6",E=$$START1^APCLDF(%,"BGPG(")
 S X=0 F  S X=$O(BGPG(X)) Q:X'=+X  S Y=+$P(BGPG(X),U,4) D
 .Q:'$D(^AUPNVPOV(Y,0))
 .S Y=$P(^AUPNVPOV(Y,0),U,1)
 .Q:'Y
 .S D=$P(BGPG(X),U)
 .S C=$P($$ICDDX^ICDCODE(Y,D),U,4)
 .Q:C=""
 .S C1=$P($$ICDDX^ICDCODE(Y,D),U,2)
 .D SET
 .Q
 ;now check for cpts
 S X=0 F  S X=$O(^AUPNVCPT("AC",P,X)) Q:X'=+X  D
 .Q:'$D(^AUPNVCPT(X,0))
 .S C1=$$VAL^XBDIQ1(9000010.18,X,.01)
 .I C1'=90732,C1'=90669 Q
 .S C=$$VAL^XBDIQ1(9000010.18,X,.019)
 .S D=$P(^AUPNVCPT(X,0),U,3)
 .S D=$P($P($G(^AUPNVSIT(D,0)),U),".")
 .D SET
 .Q
 ;tran codes
 S X=0 F  S X=$O(^AUPNVTC("AC",P,X)) Q:X'=+X  D
 .Q:'$D(^AUPNVTC(X,0))
 .S C1=$$VAL^XBDIQ1(9000010.33,X,.07)
 .I C1'=90732,C1'=90669 Q
 .S C=$$VALI^XBDIQ1(9000010.33,X,.07)
 .I C="" Q
 .S C=$P($$CPT^ICPTCOD(C),U,3)
 .S D=$P(^AUPNVTC(X,0),U,3)
 .S D=$P($P($G(^AUPNVSIT(D,0)),U),".")
 .D SET
 .Q
 ;refusals?
 K BGPI F X=33,100,109 S Y=$O(^AUTTIMM("C",X,0)) I Y S BGPI(Y)=""
 S X=0 F  S X=$O(^AUPNPREF("AA",P,9999999.14,X)) Q:X'=+X  D
 .Q:'$D(BGPI(X))  ;not an ACEI
 .S D=0 F  S D=$O(^AUPNPREF("AA",P,50,X,D)) Q:D'=+D  D
 ..S Y=9999999-D
 ..Q:$D(BGPX(Y))  S BGPX(Y)="REFUSAL: "_$$VAL^XBDIQ1(9000022,N,.04)_"   "_$$DATE^BGP7UTL($P(^AUPNPREF(N,0),U,3))_"  "_$$VAL^XBDIQ1(9000022,X,1101)
 ..Q
 .Q
 S (X,G)=0,Y=$O(^AUTTIMM("C",33,0)) F  S X=$O(^BIPC("AC",P,Y,X)) Q:X'=+X  D
 .S R=$P(^BIPC(X,0),U,3)
 .Q:R=""
 .Q:'$D(^BICONT(R,0))
 .Q:$P(^BICONT(R,0),U,1)'["Refusal"
 .S D=$P(^BIPC(X,0),U,4)
 .Q:D=""
 .Q:$D(BGPX(D))  S BGPX(D)="REFUSAL: Immunization Package  "_$$DATE^BGP7UTL(D)
 S (X,G)=0,Y=$O(^AUTTIMM("C",100,0)) I Y F  S X=$O(^BIPC("AC",P,Y,X)) Q:X'=+X  D
 .S R=$P(^BIPC(X,0),U,3)
 .Q:R=""
 .Q:'$D(^BICONT(R,0))
 .Q:$P(^BICONT(R,0),U,1)'["Refusal"
 .S D=$P(^BIPC(X,0),U,4)
 .Q:D=""
 .Q:$D(BGPX(D))  S BGPX(D)="REFUSAL: Immunization Package  "_$$DATE^BGP7UTL(D)
 S (X,G)=0,Y=$O(^AUTTIMM("C",109,0)) I Y F  S X=$O(^BIPC("AC",P,Y,X)) Q:X'=+X  D
 .S R=$P(^BIPC(X,0),U,3)
 .Q:R=""
 .Q:'$D(^BICONT(R,0))
 .Q:$P(^BICONT(R,0),U,1)'["Refusal"
 .S D=$P(^BIPC(X,0),U,4)
 .Q:D=""
 .Q:$D(BGPX(D))  S BGPX(D)="REFUSAL: Immunization Package  "_$$DATE^BGP7UTL(D)
 S D=0 F  S D=$O(BGPX(D)) Q:D'=+D  S BGPC=BGPC+1,BGPY(BGPC)=BGPX(D)
 K BGPX,BGPC,BGPI,X,Y,D
 Q
ERBC1(P,BD,ED,BGPY) ;
 K BGPG,BGPY
 S BPGC=0
 S A="BGPG(",B=P_"^ALL VISITS;DURING "_$$FMTE^XLFDT(BD)_"-"_$$FMTE^XLFDT(ED),E=$$START1^APCLDF(B,A)
 I '$D(BGPG(1)) Q
 S (X,G)=0 F  S X=$O(BGPG(X)) Q:X'=+X  S V=$P(BGPG(X),U,5) D
 .Q:'$D(^AUPNVSIT(V,0))
 .Q:'$P(^AUPNVSIT(V,0),U,9)
 .Q:$P(^AUPNVSIT(V,0),U,11)
 .Q:$$CLINIC^APCLV(V,"C")'=30
 .S BGPC=BGPC+1,BGPY(BGPC)="ER Visit: "_$$DATE^BGP7UTL($P($P(^AUPNVSIT(V,0),U),"."))
 .S BGPC=BGPC+1,BGPY(BGPC)="ER Diagnoses: "
 .S A=0 F  S A=$O(^AUPNVPOV("AD",V,A)) Q:A'=+A  D
 ..S BGPC=BGPC+1,BGPY(BGPC)="  "_$$VAL^XBDIQ1(9000010.07,A,.01)_"  "_$$VAL^XBDIQ1(9000010.07,A,.04)
 .S BGPC=BGPC+1,BGPY(BGPC)="Blood Culture: "
 .;now check cpts/tran codes for 87040, 87103
 .S Z=0 F  S Z=$O(^AUPNVCPT("AD",V,Z)) Q:Z'=+Z  D
 ..Q:'$D(^AUPNVCPT(Z,0))
 ..S C1=$$VAL^XBDIQ1(9000010.18,Z,.01)
 ..I C1'=87040,C1'=87103 Q
 ..S BGPC=BGPC+1,BGPY(BGPC)="CPT code: "_C1
 ..Q
 .;tran codes
 .S Z=0 F  S Z=$O(^AUPNVTC("AD",V,Z)) Q:Z'=+Z  D
 ..Q:'$D(^AUPNVTC(Z,0))
 ..S C1=$$VAL^XBDIQ1(9000010.33,Z,.07)
 ..I C1'=87040,C1'=87103 Q
 ..S BGPC=BGPC+1,BGPY(BGPC)="TRAN CODE CPT code: "_C1
 ..Q
 .S T=$O(^ATXAX("B","BGP BLOOD CULTURE LOINC",0))
 .S BGPLT=$O(^ATXLAB("B","BGP CMS BLOOD CULTURE",0))
 .S B=9999999-$P($P(^AUPNVSIT(V,0),U),"."),E=9999999-$P($P(^AUPNVSIT(V,0),U),".") S D=E-1 F  S D=$O(^AUPNVLAB("AE",P,D)) Q:D'=+D!(D>B)  D
 ..S L=0 F  S L=$O(^AUPNVLAB("AE",P,D,L)) Q:L'=+L  D
 ...S A=0 F  S A=$O(^AUPNVLAB("AE",P,D,L,A)) Q:A'=+A  D
 ....Q:'$D(^AUPNVLAB(A,0))
 ....I $$VAL^XBDIQ1(9000010.09,A,.01)="BLOOD CULTURE" D  Q
 .....S BGPC=BGPC+1,BGPY(BGPC)="  LAB:  "_$$VAL^XBDIQ1(9000010.09,A,.01)_"  "_$$DATE^BGP7UTL($P(BGPG(X),U))_"  value: "_$P(^AUPNVLAB(A,0),U,4) Q
 ....I BGPLT,$P(^AUPNVLAB(A,0),U),$D(^ATXLAB(BGPLT,21,"B",$P(^AUPNVLAB(A,0),U))) S BGPC=BGPC+1,BGPY(BGPC)="  LAB:  "_$$VAL^XBDIQ1(9000010.09,A,.01)_"  "_$$DATE^BGP7UTL($P(BGPG(X),U))_"  value: "_$P(^AUPNVLAB(A,0),U,4) Q
 ....Q:'T
 ....S J=$P($G(^AUPNVLAB(A,11)),U,13) Q:J=""
 ....Q:'$$LOINC^BGP7D21(J,T)
 ....S BGPC=BGPC+1,BGPY(BGPC)="  LAB:  "_$$VAL^XBDIQ1(9000010.09,A,.01)_"  "_$$DATE^BGP7UTL($P(BGPG(X),U))_"  value: "_$P(^AUPNVLAB(A,0),U,4)
 ....Q
 .S B=9999999-$P($P(^AUPNVSIT(V,0),U),"."),E=9999999-$P($P(^AUPNVSIT(V,0),U),".") S D=E-1 F  S D=$O(^AUPNVMIC("AE",P,D)) Q:D'=+D!(D>B)  D
 ..S L=0 F  S L=$O(^AUPNVMIC("AE",P,D,L)) Q:L'=+L  D
 ...S A=0 F  S A=$O(^AUPNVMIC("AE",P,D,L,A)) Q:A'=+A  D
 ....Q:'$D(^AUPNVMIC(A,0))
 ....I $$VAL^XBDIQ1(9000010.25,A,.01)="BLOOD CULTURE" D  Q
 .....S BGPC=BGPC+1,BGPY(BGPC)="  MICRO:  "_$$VAL^XBDIQ1(9000010.25,A,.01)_"  "_$$DATE^BGP7UTL($P(BGPG(X),U))_"  value: "_$$VAL^XBDIQ1(9000010.25,A,.04)_" "_$P(^AUPNVMIC(A,0),U,7) Q
 ....I BGPLT,$P(^AUPNVMIC(A,0),U),$D(^ATXLAB(BGPLT,21,"B",$P(^AUPNVMIC(A,0),U))) D
 .....S BGPC=BGPC+1,BGPY(BGPC)="  MICRO:  "_$$VAL^XBDIQ1(9000010.25,A,.01)_"  "_$$DATE^BGP7UTL($P(BGPG(X),U))_"  value: "_$$VAL^XBDIQ1(9000010.25,A,.04)_" "_$P(^AUPNVMIC(A,0),U,7) Q
 ....Q:'T
 ....S J=$P($G(^AUPNVMIC(A,11)),U,13) Q:J=""
 ....Q:'$$LOINC^BGP7D21(J,T)
 ....S BGPC=BGPC+1,BGPY(BGPC)="  MICRO:  "_$$VAL^XBDIQ1(9000010.25,A,.01)_"  "_$$DATE^BGP7UTL($P(BGPG(X),U))_"  value: "_$$VAL^XBDIQ1(9000010.25,A,.04)_" "_$P(^AUPNVMIC(A,0),U,7)
 ....Q
 Q
AMA(H) ;
 S X=$P(^AUPNVINP(H,0),U,6)
 I X="" Q 0
 S X=$P($G(^DG(405.1,X,"IHS")),U,1)
 I X=3 Q 1
 Q 0
PNEUDX(V) ;
 S C=$$PRIMPOV^APCLV(V,"I")
 I C="" Q 0 ;no primary dx
 S T=$O(^ATXAX("B","BGP CMS PNEUMONIA DXS",0))
 I $$ICD^ATXCHK(C,T,9) Q 1  ;primary dx of pneumonia
 ;PRIMARY of resp failure and seconday of pneumonia
 S T=$O(^ATXAX("B","BGP CMS SEPTI/RESP FAIL DXS",0))
 I '$$ICD^ATXCHK(C,T,9) Q 0   ;resp failure not primary pov
 S T=$O(^ATXAX("B","BGP CMS PNEUMONIA DXS",0))
 S (X,G)=0 F  S X=$O(^AUPNVPOV("AD",V,X)) Q:X'=+X!(G)  D
 .Q:'$D(^AUPNVPOV(X,0))
 .Q:$P(^AUPNVPOV(X,0),U,12)="P"
 .S I=$P(^AUPNVPOV(X,0),U)
 .Q:'$$ICD^ATXCHK(I,T,9)
 .S G=1
 .Q
 Q G
EXPIRED(H) ;
 S X=$P(^AUPNVINP(H,0),U,6)
 I X="" Q 0
 S X=$P($G(^DG(405.1,X,"IHS")),U,1)
 I X=4!(X=5)!(X=6)!(X=7) Q 1
 Q 0
DSCH(H) ;
 Q $P($P(^AUPNVINP(H,0),U),".")
TRANSIN(H) ;
 S X=$P(^AUPNVINP(H,0),U,7)
 I X="" Q 0
 S X=$P($G(^DG(405.1,X,"IHS")),U,1)
 I X=2!(X=3) Q 1
 Q 0
TRANS(H) ;
 S X=$P(^AUPNVINP(H,0),U,6)
 I X="" Q 0
 S X=$P($G(^DG(405.1,X,"IHS")),U,1)
 I X=2 Q 1
 Q 0
