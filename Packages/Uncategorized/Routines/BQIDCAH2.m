BQIDCAH2 ;VNGT/HS/ALA-Ad Hoc Search continued ; 03 Apr 2009  3:56 PM
 ;;2.1;ICARE MANAGEMENT SYSTEM;;Feb 07, 2011
 ;
PROV(FGLOB,TGLOB,PROV) ;EP - Provider search
 I $G(TGLOB)="" Q
 I $G(PROV)="" Q
 ;
 NEW DFN,IEN,RIEN,VIS
 S IEN=0
 ;
 I $G(FGLOB)'="",$G(FROM)'="" D  Q
 . S DFN=""
 . F  S DFN=$O(@FGLOB@(DFN)) Q:DFN=""  D
 .. I '$D(@VODATA@(DFN,"PRV",PROV)) K @FGLOB@(DFN) Q
 .. ; Check for NUMVIS
 .. I $G(NUMVIS)'="" D  Q
 ... I @(@VODATA@(DFN,"PRV",PROV)_NUMVIS) S @TGLOB@(DFN)="" Q
 ... Q
 .. I $D(MPARMS("NUMVIS")) D  Q
 ... S VCRIT1=$O(MPARMS("NUMVIS","")),VCRIT2=$O(MPARMS("NUMVIS",VCRIT1))
 ... ; If criteria includes a "not" it is inclusive and both must be true
 ... I $E(VCRIT1)="'",@(@VODATA@(DFN,"PRV",PROV)_VCRIT1),@(@VODATA@(DFN,"PRV",PROV)_VCRIT2) S @TGLOB@(DFN)="" Q
 ... ; If criteria does not includes a "not" it is exclusive and one must be true
 ... I $E(VCRIT1)'="'",@(@VODATA@(DFN,"PRV",PROV)_VCRIT1_"!("_(@VODATA@(DFN,"PRV",PROV)_VCRIT2)_")") S @TGLOB@(DFN)="" Q
 ... Q
 .. S @TGLOB@(DFN)=""
 . K @FGLOB
 ;
 I $G(FGLOB)'="" D
 . S RIEN=""
 . F  S RIEN=$O(^AUPNVPRV("B",PROV,RIEN)) Q:'RIEN  D
 .. S DFN=$$GET1^DIQ(9000010.06,RIEN_",",.02,"I") I DFN="" Q
 .. I '$$DECHK^BQIDCAH(DFN) Q
 .. I '$$HRN^BQIUL1(DFN) Q
 .. ; If patient has no visit in last 3 years, quit
 .. ;I '$$VTHR^BQIUL1(DFN) Q
 .. I '$D(@FGLOB@(DFN)) Q
 .. S @TGLOB@(DFN)=""
 ;
 I $G(FGLOB)="" D
 . S RIEN=""
 . F  S RIEN=$O(^AUPNVPRV("B",PROV,RIEN)) Q:'RIEN  D
 .. S DFN=$$GET1^DIQ(9000010.06,RIEN_",",.02,"I") I DFN="" Q
 .. I '$$DECHK^BQIDCAH(DFN) Q
 .. I '$$HRN^BQIUL1(DFN) Q
 .. ; If patient has no visit in last 3 years, quit
 .. ;I '$$VTHR^BQIUL1(DFN) Q
 .. S @TGLOB@(DFN)=""
 Q
 ;
PRVS(TGLOB,PROV,FROM,THRU) ;EP
 ; this cross-reference does not really exist yet, it is a test to determine
 ; a more efficient cross-reference
 S FDT=FROM
 F  S FDT=$O(^AUPNVPRV("AF",PROV,FDT)) Q:FDT=""!(FDT\1>THRU)  D
 . S DFN=""
 . F  S DFN=$O(^AUPNVPRV("AF",PROV,FDT,DFN)) Q:DFN=""  D
 .. I '$$DECHK^BQIDCAH(DFN) Q
 .. S @TGLOB@(DFN)=""
 ;
 Q
 ;
ALGY(FGLOB,TGLOB,BEN,MPARMS) ;EP - Allergy search
 NEW ALGPT
 I $G(TGLOB)="" Q
 I $G(ALLERGY)]"" D ALGY1
 I $D(MPARMS("ALLERGY")) D
 . I ALLOP="!" D  Q
 .. S ALLERGY="" F  S ALLERGY=$O(MPARMS("ALLERGY",ALLERGY)) Q:ALLERGY=""  D ALGY1
 . I ALLOP="&" D
 .. S ALLERGY="",CT=0
 .. F  S ALLERGY=$O(MPARMS("ALLERGY",ALLERGY)) Q:ALLERGY=""  S CT=CT+1
 .. F  S ALLERGY=$O(MPARMS("ALLERGY",ALLERGY)) Q:ALLERGY=""  D ALGY1
 .. S AIEN=""
 .. F  S AIEN=$O(ALGPT(AIEN)) Q:AIEN=""   I ALLOP="&",ALGPT(AIEN)=CT S @TGLOB@(AIEN)=""
 K ALGPT
 Q
 ;
ALGY1 ;EP
 NEW IEN,ALGTX,AN,DFN
 I $G(FGLOB)'="" D  Q
 . S IEN=""
 . F  S IEN=$O(@FGLOB@(IEN)) Q:'IEN  D
 .. I '$$DECHK^BQIDCAH(IEN) Q
 .. S AN=""
 .. F  S AN=$O(^GMR(120.8,"B",IEN,AN)) Q:AN=""  D
 ... I $$GET1^DIQ(120.8,AN_",",22,"I")=1 Q
 ... S ALGTX=$$GET1^DIQ(120.8,AN_",",.02,"E")
 ... I $E(ALGTX,$L(ALGTX),$L(ALGTX))=" " S ALGTX=$$STRIP^BQIUL1(ALGTX," ")
 ... I ALGTX=ALLERGY D
 .... I ALLOP="!" S @TGLOB@(IEN)="" Q
 .... S ALGPT(IEN)=$G(ALGPT(IEN))+1
 .. I ALLOP="&",ALGPT(IEN)'=CT K ALGPT(IEN)
 ;
 I $L(ALLERGY)'>30 D
 . NEW ALGTX,TXT
 . S IEN=""
 . F  S IEN=$O(^GMR(120.8,"C",ALLERGY,IEN)) Q:IEN=""  D ALCK
 . S ALGTX=$O(^GMR(120.8,"C",ALLERGY)),TXT=ALLERGY_" "
 . I ALGTX=TXT D
 .. F  S IEN=$O(^GMR(120.8,"C",ALGTX,IEN)) Q:IEN=""  D ALCK
 ;
 I $L(ALLERGY)>30 D
 . S TALLG=ALLERGY
 . F  S TALLG=$O(^GMR(120.8,"C",TALLG)) Q:TALLG=""!($E(ALLERGY,1,30)'=$E(TALLG,1,30))  D
 .. S IEN=""
 .. F  S IEN=$O(^GMR(120.8,"C",TALLG,IEN)) Q:IEN=""  D ALCK
 Q
 ;
ALCK ;EP
 I $$GET1^DIQ(120.8,IEN_",",22,"I")=1 Q
 S DFN=$$GET1^DIQ(120.8,IEN_",",.01,"I")
 I '$$DECHK^BQIDCAH(DFN) Q
 I '$$HRN^BQIUL1(DFN) Q
 I ALLOP="!" S @TGLOB@(DFN)="" Q
 S ALGPT(DFN)=$G(ALGPT(DFN))+1
 Q
