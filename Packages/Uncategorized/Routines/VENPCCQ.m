VENPCCQ ; IHS/OIT/GIS - KNOWLEDGEBASE UTILITIES FOR ASQ ; 
 ;;2.6;PCC+;**1,3**;MAR 23, 2011
 ;
 ;
 ;
 ; 
ASQ ; EP - GET ASQ SCORES IN u40-u52 ; WCM UPGRADE
 S IDT=0,STOP=0
 F  S IDT=$O(^AUPNVWC("AA",DFN,IDT)) Q:'IDT  D  I STOP Q
 . S WCIEN=0
 . F  S WCIEN=$O(^AUPNVWC("AA",DFN,IDT,WCIEN)) Q:'WCIEN  D  I STOP Q
 .. S %=$G(^AUPNVWC(WCIEN,2)) I '$L(%) Q
 .. I %?1."^" Q
 .. S VIEN=$P($G(^AUPNVWC(WCIEN,0)),U,3) I 'VIEN Q
 .. S FMDT=+^AUPNVSIT(VIEN,0)\1
 .. S VDT=$$FMTE^XLFDT(FMDT,2) ; DATE OF LAST ASQ
 .. S Z=40,@TMP@("u40")=VDT,STOP=1
 .. F PCE=1:1:5,7 D
 ... S X=$P(%,U,PCE)
 ... S Z=Z+1,MN="u"_Z
 ... K @TMP@(MN)
 ... I $L(X),PCE=7 S X=$P($G(^VEN(7.14,X,0)),U)
 ... I $L(X) S @TMP@(MN)=X
 ... Q
 .. Q
 . Q
ASQNOW ; EP -GET TODAYS ASQ MONTH AND CUTOFF SCORES
 S M=$$ASQAGE(DFN) I 'M Q
 S QIEN=+$$ASQIEN(M) I 'QIEN Q
 S %=$G(^VEN(7.14,QIEN,0)) I '$L(%) Q
 S @TMP@("u47")=$P(%,U,1) ; QUESTIONNAIRE (MONTH)
 S @TMP@("u48")=$P(%,U,4) ; FINE MOTOR CUTOFF
 S @TMP@("u49")=$P(%,U,3) ; GROSS MOTOR CUTOFF
 S @TMP@("u50")=$P(%,U,2) ; COMMUNICATION CUTOFF
 S @TMP@("u51")=$P(%,U,6) ; PERSONAL-SOCIAL CUTOFF
 S @TMP@("u52")=$P(%,U,5) ; PROBLEM SOLVING CUTOFF
 Q
 ;
NAME(X) ; EP - FROM TRIGGER IN VEN EHP KB ASQ FILE
 ; CONVERT # MOS TO ASQ FILE NAME
 I '$G(X) Q ""
 I $L(X)=1 S X="0"_X
 S X=X_" Month Questionnaire.pdf"
 Q X
 ; 
INT(OUT,IN) ; EP - RPC: VEN ASQ INTERVENTION FORM
 ; GIVEN A DFN, RETURNT THE NAME OF THE APPROPRIATE ASQ INTERVENTION FORM
 S OUT=""
 I '$D(^DPT(+$G(IN),0)) Q
 N NAME,X,Y,Z,%,DFN
 S DFN=+IN,%=$$ASQAGE(DFN) I %<4!(%>66) Q
 S X=$S(%<8:4,%<12:8,%<16:12,%<20:16,%<24:20,%<30:24,%<36:30,%<48:36,%<60:48,%<66:60,1:"") I 'X Q
 S Y=$S(X=4:8,X=8:12,X=12:16,X=16:20,X=20:24,X=24:30,X=30:36,X=36:48,X=48:60,X=60:66,1:"") I 'Y Q
 I $L(X)=1 S X="0"_X
 I $L(Y)=1 S Y="0"_Y
 S OUT="Activities "_X_"-"_Y_" months.pdf"
 Q
 ;
ASQAGE(DFN) ; EP - GIVEN A DFN, RETURN THE ASQ AGE IN MONTHS
 N Y1,Y2,M1,M2,D1,D2,YD,MD,M,DOB
 I '$G(DT) Q ""
 S DOB=$$ASQDOB(DFN)
A1 S Y1=+$E(DOB,1,3),M1=+$E(DOB,4,5),D1=+$E(DOB,6,7)
 S Y2=+$E(DT,1,3),M2=+$E(DT,4,5),D2=+$E(DT,6,7)
 I M1>M2 S Y2=Y2-1,M2=M2+12
 S YD=Y2-Y1,MD=M2-M1
 I D2<D1 S MD=MD-1
 S M=YD*12+MD
 Q M
 ; 
DOBAGE(DFN) ; EP - GIVEN A DFN, RETURN THE DOB AGE IN MONTHS
 N Y1,Y2,M1,M2,D1,D2,YD,MD,M,DOB
 S DOB=$P($G(^DPT(+$G(DFN),0)),U,3)
 I '$G(DOB) Q ""
 G A1
 ; 
ASQDOB(DFN) ; EP - GIVEN A DFN, RETURN THE ADJUSTED (ASQ) DOB
 N DOB,GA,%
 S DOB=$P($G(^DPT(+$G(DFN),0)),U,3)
 I '$G(DOB) Q ""
 I $$DOBAGE(DFN)>24 G A2 ; NO CORRECTIONS FOR PREMIES AFTER CHRONLOGICAL AGE OF 24 MOS!
 S GA=$P($G(^AUPNBMSR(DFN,0)),U,6)
 I GA,GA<40 S %=(40-GA)*7 I % S DOB=$$FMADD^XLFDT(DOB,%)
A2 I DT<DOB Q ""
 Q DOB
 ; 
ASQIEN(M) ; EP - GIVEN THE ASQ AGE IN MONTHS, RETURN THE CURRENT ASQ FORM IEN^IEN OF THE NEXT FORM
 N IEN,X,Y,IEN2
 I +$G(M)<3 Q ""
 I M>71 Q ""
 I M>60 S M=60
 S IEN2=""
 S IEN=$O(^VEN(7.14,"B",M,0))
 I IEN S Y=$O(^VEN(7.14,"B",M)) S:Y IEN2=$O(^VEN(7.14,"B",Y,0)) Q IEN_U_IEN2
 S X=M+1,IEN=$O(^VEN(7.14,"B",X,0))
 I IEN S Y=$O(^VEN(7.14,"B",X)) S:Y IEN2=$O(^VEN(7.14,"B",Y,0)) Q IEN_U_IEN2
 S X=M-1,IEN=$O(^VEN(7.14,"B",X,0))
 I IEN S Y=$O(^VEN(7.14,"B",X)) S:Y IEN2=$O(^VEN(7.14,"B",Y,0)) Q IEN_U_IEN2
 S X=$O(^VEN(7.14,"B",M),-1),IEN=$O(^VEN(7.14,"B",X,0))
 I IEN S Y=$O(^VEN(7.14,"B",M)) S:Y IEN2=$O(^VEN(7.14,"B",Y,0)) Q IEN_U_IEN2
 Q ""
 ;
ASQDTR(DFN) ; EP - GIVEN A DFN RETURN THE DATE RANGE FOR THE NEXT ASQ
 N M,ASQM,DATE,DOB,Y,M,D,START,FIN,MS,YS,MF,YF,%,IEN
 S M=$$ASQAGE(+$G(DFN)) I M="" Q ""
 S %=$$ASQIEN(M) I '$L(%) Q ""
 S IEN=$P(%,U,2) I 'IEN Q "" ; IEN OF NEXT ASQ
AD1 S ASQM=+$G(^VEN(7.14,IEN,0)) I 'ASQM Q ""
 I ASQM<M S IEN=IEN+1 G AD1
 S DOB=$$ASQDOB(DFN) I 'DOB Q ""
 S Y=$E(DOB,1,3),M=$E(DOB,4,5),D=$E(DOB,6,7)
 S Y=Y+(ASQM\12),M=M+(ASQM#12)
 I M>12 S Y=Y+1,M=M-12 ; THIS IS THE CENTER MONTH AND YEAR
 S MS=M-1,YS=Y
 I 'MS S MS=12,YS=YS-1
 I $L(MS)=1 S MS="0"_MS
 S MF=M+1,YF=Y
 I MF=13 S MF=1,YF=YF+1
 I $L(MF)=1 S MF="0"_MF
 S START=YS_MS_D,FIN=YF_MF_D
 I START<DT S IEN=IEN+1 G AD1
 Q (START_U_FIN)
 ;
DATA(OUT,IN) ; EP - RPC: VEN ASQ GET DATA
 ; IN = DFN|GA
 S OUT=""
 N STG,DFN,NAME,DOB,CDOB,TODAY,CHART,MOM,REL,PHONE,STREET,CITY,STATE,ZIP,ASQ,NEXT,B,X,Y,Z,%,M,PAGE,GA
INIT S STG=$G(^DPT(+$G(IN),0)) I '$L(STG) Q
 S (CHART,MOM,REL,PHONE,STREET,CITY,STATE,ZIP,NEXT)=""
 S B="|",DFN=+IN,GA=$P(IN,"|",2)
 ; S GA=$P(IN,"|",2) I GA>11,GA<51 D GA(DFN,GA) ; FILE GA
ITEMS S NAME=$P(STG,U) I '$L(NAME) Q
 S DOB=$P(STG,U,3) I 'DOB Q
 S CDOB=$$ASQDOB(DFN)
 I CDOB=DOB S CDOB=""
 S DOB=$$FMTE^XLFDT(DOB,1)
 I CDOB S CDOB=$$FMTE^XLFDT(CDOB,1)
 S TODAY=$$FMTE^XLFDT(DT,1)
 S (ASQ,MONTH,PAGE)=""
 S M=$$ASQAGE(DFN) I 'M G PAGE
 S X=$$ASQIEN(M) I '$L(X) G PAGE
 S ASQ=$G(^VEN(7.14,+X,1)) I '$L(ASQ) Q  ; PDF FILE NAME
 S MONTH=+ASQ
 S PAGE=$P($G(^VEN(7.14,+X,0)),U,7) I 'PAGE Q
PAGE S X=$$ASQDTR(DFN) I $L(X) S NEXT=$$FMTE^XLFDT(+X,1)_" to "_$$FMTE^XLFDT($P(X,U,2),1)
 S X=$P($G(^AUPNPAT(DFN,41,DUZ(2),0)),"^",2) I $L(X) S CHART=X
 S X=$P($G(^DPT(DFN,.24)),U,2) I $L(X) S MOM=X,REL="Mother"
 S X=$P($G(^DPT(DFN,.13)),U,1) I $L(X) S PHONE=X
 S X=$P($G(^DPT(DFN,.11)),U,1) I $L(X) S STREET=X
 S X=$P($G(^DPT(DFN,.11)),U,4) I $L(X) S CITY=X
 S X=$P($G(^DPT(DFN,.11)),U,5) I X S X=$P($G(^DIC(5,X,0)),U) I $L(X) S STATE=X
 S X=$P($G(^DPT(DFN,.11)),U,6) I $L(X) S ZIP=X
 S X=$P($G(^AUPNBMSR(DFN,0)),U,6) S GA=X
 S OUT=NAME_B_DOB_B_CDOB_B_CHART_B_MOM_B_REL_B_PHONE_B_STREET_B_CITY_B_STATE_B_ZIP_B_ASQ_B_MONTH_B_PAGE_B_NEXT_B_GA
 Q
 ;
HOLD(OUT,IN) ; EP - POPULATE THE ASQ HOLDING FILE
 ; IN=PATIENT DFN, OUT="IEN1~IEN2"
 ; DEAD CODE
 S OUT="",DFN=+$G(IN) I '$D(^DPT(DFN,0)) Q
 S DOB=$P(^DPT(DFN,0),U,3),GA=$P($G(^AUPNBMSR(DFN,0)),U,6) ; GA=ACTUAL GA
 S XGA=40 I GA S XGA=GA ; XGA = ACTUAL GA.  IF ACTUAL GA NO AVAILABLE XGA=40
 Q
 ;
GA(DFN,GA) ; EP - FILE THE GA
 N DIC,DA,DR,DIE,X,Y,%
 S DIC="^AUPNBMSR(",DIC(0)="L",DLAYGO=9000024,X="`"_DFN
 D ^DIC I Y=-1 Q
 S DIE=DIC,DA=+Y,DR=".06///^S X=GA"
 L +^AUPNBMSR(DA):1 I  D ^DIE L -^AUPNBMSR(DA)
 D ^XBFMK
 Q
 ; 
PATIENT(OUT,IN) ; EP - RPC: VEN ASQ GET PATIENT ID
 ; GIEN A CHART #, RETURN PATIENT IDENTIFIERS
 I '$L($G(IN)) Q
 N NAME,DOB,MOM,X,Y,%,HRN,AUPNPAT,AUPNSEX,AUPNDOB,AUPNDAYS,AUPNDOD,B,GA
 S OUT="",HRN=IN,B="|"
 S DIC="^AUPNPAT(",X=HRN,DIC(0)="M"
PLK ; EP  - FOR ALT PROCESSING OF HRN
 D ^DIC I Y=-1 Q
 I $G(AUPNDOD) Q  ; PATIENT MUST BE ALIVE
 I '$G(AUPNPAT) Q
 S NAME=$P($G(^DPT(AUPNPAT,0)),U) I '$L(NAME) Q
 S DOB=$$FMTE^XLFDT(AUPNDOB,"2D")
 S MOM=$P($G(^DPT(AUPNPAT,.24)),U,2)
 S GA=$P($G(^AUPNBMSR(AUPNPAT,0)),U,6)
 S OUT=AUPNPAT_B_NAME_B_DOB_B_AUPNSEX_B_MOM_B_GA
 D ^XBFMK
 Q
 ; 
UGA(OUT,IN) ; EP - RPC: VEN ASQ UPDATE GESTATIONAL AGE
 ; GIVEN ASQ DFN AND GA VALUE (DFN|VAL), UPDATE THE TX FILE AND RPMS
 S OUT=""
 I $G(IN)
 E  Q
 N GA,DFN,X,Y,Z,%,REF
 S DFN=+$P(IN,"|",1) I '$D(^DPT(DFN,0)) Q
 S GA=$P(IN,"|",2) I 'GA Q
 D GA(DFN,GA)
 S OUT="OK"
 Q
 ; 
