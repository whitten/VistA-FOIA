BQIDCMSP ;VNGT/HS/ALA-MY PATIENTS-SPECIALTY PROVIDER ; 12 Dec 2008  1:42 PM
 ;;2.1;ICARE MANAGEMENT SYSTEM;;Feb 07, 2011
 ;
 Q
 ;
FND(DATA,PARMS,MPARMS) ;EP - Find records
 ;
 ;Description
 ;  Executable that finds all patients who has a designated case manager
 ;Input
 ;  PARMS = Array of parameters and their values
 ;  MPARMS = Multiple array of a parameter (not currently used for this definition)
 ;Expected to return DATA
 ;
 ;  If the DSPM package is not installed, quit as there is no other case manager
 ;  alternative
 I $$VERSION^XPDUTL("BDP")="" Q
 ;
 NEW UID,NM,PSPEC,SPEC,DFN,PROV
 S UID=$S($G(ZTSK):"Z"_ZTSK,1:$J)
 S DATA=$NA(^TMP("BQIDCMCM",UID))
 K @DATA
 ;
 ;  Set the parameters into variables
 I '$D(PARMS),'$D(MPARMS) Q
 ;
 S NM=""
 F  S NM=$O(PARMS(NM)) Q:NM=""  D
 . S @NM=$P(PARMS(NM),U,1)
 . F I=2:1:$L(PARMS(NM),U) S @($P(PARMS(NM),U,I))
 ;
 ;  Go through the BDP DESG SPECIALTY PROVIDER File to find any patient
 ;  with the specified case manager
 ;
 S IEN=""
 F  S IEN=$O(^BDPRECN("AC",PROV,IEN)) Q:IEN=""  D
 . S PSPEC=$$GET1^DIQ(90360.1,IEN_",",.01,"I")
 . I $G(SPEC)'="",PSPEC'=SPEC Q
 . I $D(MPARMS("SPEC")),'$D(MPARMS("SPEC",PSPEC)) Q
 . S DFN=$$GET1^DIQ(90360.1,IEN_",",.02,"I") I DFN="" Q
 . ; User may now select Living, Deceased or both as a filter so
 . ; if no filters defined assume living patients otherwise let filter decide
 . I $O(^BQICARE(OWNR,1,PLIEN,15,0))="",$P($G(^DPT(DFN,.35)),U,1)'="" Q
 . ; If patient has no active HRNs, quit
 . I '$$HRN^BQIUL1(DFN) Q
 . ; If patient has no visit in last 3 years, quit
 . ;I '$$VTHR^BQIUL1(DFN) Q
 . S @DATA@(DFN)=""
 Q
