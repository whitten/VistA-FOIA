BSDX25 ; IHS/OIT/HMW/MSC/SAT - WINDOWS SCHEDULING RPCS ;
 ;;3.0;IHS WINDOWS SCHEDULING;;DEC 09, 2010
 ;
 ;
CHECKIND(BSDXY,BSDXAPTID,BSDXCDT,BSDXCC,BSDXPRV,BSDXROU,BSDXVCL,BSDXVFM,BSDXOG,BSDXCR) ;EP
 ;Entry point for debugging
 ;
 ;I +$G(^BSDXDBUG("BREAK","CHECKIN")),+$G(^BSDXDBUG("BREAK"))=DUZ D DEBUG^%Serenji("CHECKIN^BSDX25(.BSDXY,BSDXAPTID,BSDXCDT,BSDXCC,BSDXPRV,BSDXROU,BSDXVCL,BSDXVFM,BSDXOG)",$P(^BSDXDBUG("BREAK"),U,2))
 ;E  G ENDBG
 Q
 ;
CHECKIN(BSDXY,BSDXAPTID,BSDXCDT,BSDXCC,BSDXPRV,BSDXROU,BSDXVCL,BSDXVFM,BSDXOG,BSDXCR,BSDXPCC,BSDXWHF) ;EP Check in appointment
 ;
 ; INPUT: BSDXAPTID - Appointment ID
 ;        BSDXCDT   - Check-in date/time. ("@" - indicates delete check-in)
 ;        BSDXCC    -
 ;        BSDXPRV   -
 ;        BSDXROU   - Print Routing Slip flag
 ;        BSDXVCL   -
 ;        BSDXVFM   -
 ;        BSDXOG    -
 ;        BSDXCR    - Generate Chart request upon check-in? (1-Yes, otherwise no)
 ;        BSDXPCC   - ien of PWH Type in HEALTH SUMMARY PWH TYPE file ^APCHPWHT
 ;        BSDXWHF   - Print Patient Wellness Handout flag
 ;
ENDBG ;
 N BSDXNOD,BSDXPATID,BSDXSTART,DIK,DA,BSDXID,BSDXI,BSDXZ,BSDXIENS,BSDXVEN
 N BSDXNOEV,BSDXCAN,EMSG
 S EMSG=""
 S BSDXNOEV=1 ;Don't execute protocol
 S BSDXCAN=0
 ;
 D ^XBKVAR S X="ERROR^BSDX25",@^%ZOSF("TRAP")
 S BSDXI=0
 K ^BSDXTMP($J)
 S BSDXY="^BSDXTMP("_$J_")"
 S ^BSDXTMP($J,0)="T00020ERRORID^T00150MESSAGE"_$C(30)
 I '+BSDXAPTID D ERR("Invalid Appointment ID") Q
 I '$D(^BSDXAPPT(BSDXAPTID,0)) D ERR("Invalid Appointment ID") Q
 ;
 S:BSDXCDT="@" BSDXCAN=1
 S:BSDXCDT["@0000" BSDXCDT=$P(BSDXCDT,"@")
 S %DT="T",X=BSDXCDT D ^%DT S BSDXCDT=Y
 I '$G(BSDXCAN),BSDXCDT=-1 D ERR(70) Q
 I BSDXCDT>$$NOW^XLFDT S BSDXCDT=$$NOW^XLFDT
 S BSDXCDT=$P(BSDXCDT,".",1)_"."_$E($P(BSDXCDT,".",2),1,4)
 S BSDXNOD=^BSDXAPPT(BSDXAPTID,0)
 S DFN=$P(BSDXNOD,U,5)
 S BSDXPATID=$P(BSDXNOD,U,5)
 S BSDXSTART=$P(BSDXNOD,U)
 ;
 S BSDXSC1=$P(BSDXNOD,U,7) ;RESOURCEID
 ;if resourceId is not null AND there is a valid resource record
 I BSDXSC1]"",$D(^BSDXRES(BSDXSC1,0)) D  I +$G(BSDXZ) D ERR($P(BSDXZ,U,2)) Q
 . S BSDXNOD=^BSDXRES(BSDXSC1,0)
 . S BSDXSC1=$P(BSDXNOD,U,4) ;HOSPITAL LOCATION
 . ;Hospital Location is required for CHECKIN
 . I 'BSDXSC1]"",'$D(^SC(+BSDXSC1,0)) D ERR("RPMS Clinic not defined for this Resource: "_$P(BSDXNOD,U,1)_" ("_BSDXSC1_")") Q
 . ;Checkin BSDX APPOINTMENT entry
 . D BSDXCHK(BSDXAPTID,$S(BSDXCAN:"",1:BSDXCDT)) ; sets field .03 (Checkin), in file 9002018.4
 . ;Process cancel checkin
 . I $G(BSDXCAN) D CANCHKIN(BSDXPATID,BSDXSC1,BSDXSTART) Q
 . D APCHK(.BSDXZ,BSDXSC1,BSDXPATID,BSDXCDT,BSDXSTART,BSDXCC,BSDXPRV,BSDXVCL,BSDXVFM,BSDXOG)
 . I $G(BSDXPRV) S DIE="^BSDXAPPT(",DA=BSDXAPTID,DR=".16///"_BSDXPRV D ^DIE
 . ;S DGQUIET=1
 . I $G(BSDXROU)="true" D WISD^BSDX42(DFN,$P(BSDXCDT,"."),"SD",,,.EMSG)    ;print routing slip
 . I $G(BSDXPCC)'="" D WISD^BSDX43(DFN,$P(BSDXCDT,"."),"SD",BSDXPCC,.EMSG) ;print PCC health summary
 . I $G(BSDXWHF)="true" D WISDW^BSDX42(DFN,$P(BSDXCDT,"."),.EMSG)          ;print patient wellness handout
 . I $G(BSDXCR),$$GET1^DIQ(9002018.4,BSDXAPTID,.13)="YES" D
 . . S BSDXDEV=$$GET1^DIQ(9009020.2,$$DIV^BSDU,.05) I BSDXDEV="" D ERR("Chart request failed - No default file room printer found.") Q
 . . S DGQUIET=1 D WISD^BSDROUT(BSDXPATID,$P(BSDXSTART,"."),"",BSDXDEV)
 ;
 S BSDXI=BSDXI+1
 S ^BSDXTMP($J,BSDXI)="0^"_$S(EMSG'="":EMSG,1:"")_$C(30)
 S BSDXI=BSDXI+1
 S ^BSDXTMP($J,BSDXI)=$C(31)
 Q
 ;
BSDXCHK(BSDXAPTID,BSDXCDT) ;
 ;
 S BSDXIENS=BSDXAPTID_","
 S BSDXFDA(9002018.4,BSDXIENS,.03)=BSDXCDT
 D FILE^DIE("","BSDXFDA","BSDXMSG")
 Q
 ;
APCHK(BSDXZ,BSDXSC1,BSDXDFN,BSDXCDT,BSDXSTART,BSDXCC,BSDXPRV,BSDXVCL,BSDXVFM,BSDXOG) ;
 ;Checkin appointment for patient BSDXDFN in clinic BSDXSC1
 ;at time BSDXSD
 N BSDXC,BSDXOUT,IEN
 ;  BSDR("PAT") = ien of patient in file 2
 ;  BSDR("CLN") = ien of clinic in file 44
 ;  BSDR("ADT") = appt date/time
 ;  BSDR("CDT") = checkin date/time
 ;  BSDR("USR") = checkin user
 S BSDXC("PAT")=BSDXDFN
 ;S BSDXC("CLN")=BSDXSC1
 S BSDXC("HOS LOC")=BSDXSC1
 ;S BSDXC("CC")=BSDXCC
 S BSDXC("CLINIC CODE")=$G(BSDXCC)
 ;S BSDXC("PRV")=$G(BSDXPRV)
 S BSDXC("PROVIDER")=$G(BSDXPRV)
 ;S BSDXC("ADT")=BSDXSTART
 S BSDXC("APPT DATE")=BSDXSTART
 S BSDXC("CDT")=BSDXCDT
 S BSDXC("USR")=DUZ
 ;find IEN in ^SC multiple or null
 S APTN=$$FIND^SDAM2(BSDXC("PAT"),BSDXC("APPT DATE"),BSDXC("HOS LOC"))
 ;
 ;Required by NEW API:
 S BSDXC("TIME RANGE")=-1
 S BSDXC("VISIT DATE")=BSDXCDT ;IHS/HMW **1** 20060419
 S BSDXC("SITE")=$G(DUZ(2))
 S BSDXC("VISIT TYPE")=$$GET1^DIQ(9001001.2,BSDXC("SITE"),.11,"I")
 I BSDXC("VISIT TYPE")="" S BSDXC("VISIT TYPE")=$$GET1^DIQ(9001000,BSDXC("SITE"),.04,"I")
 I BSDXC("VISIT TYPE")="" S BSDXC("VISIT TYPE")="I"
 S BSDXC("SRV CAT")=$$GET1^DIQ(9001001.2,BSDXC("SITE"),.12,"I")
 I BSDXC("SRV CAT")="" S BSDXC("SRV CAT")="A"
 ;TELL LINDA ABOUT THESE 2 -- SAME VARIABLES WITH DIFFERENT NAMES
 S BSDXC("CLN")=BSDXC("HOS LOC")
 S BSDXC("ADT")=BSDXC("APPT DATE")
 ;
 ;Set up BSDXVEN array containing VEN EHP CLINIC, VEN EHP FORM, OUTGUIDE FLAG
 ;These values come from input param
 S BSDXVEN("CLINIC")=$G(BSDXVCL)
 S BSDXVEN("FORM")=$G(BSDXVFM)
 S BSDXVEN("OUTGUIDE")=$G(BSDXOG)
 ;
 S BSDXC("APCDAPPT")=$S($P(^DPT(BSDXC("PAT"),"S",BSDXC("APPT DATE"),0),U,7)=3:"A",$P(^DPT(BSDXC("PAT"),"S",BSDXC("APPT DATE"),0),U,7)=4:"W",1:"U")    ;walk-in vs appt
 I "CT"[BSDXC("SRV CAT") K BSDXC("APCDAPPT")  ;not needed for phone calls & chart reviews
 S BSDXC("APCDOPT")=$O(^DIC(19,"B","SD IHS PCC LINK",0))
 S IEN=$$SCIEN^BSDU2(BSDXC("PAT"),BSDXC("HOS LOC"),BSDXC("APPT DATE"))  ;find appt
 ; set checkin
 K DIE,DA,DR
 S DIE="^SC("_BSDXC("HOS LOC")_",""S"","_BSDXC("APPT DATE")_",1,"
 S DA(2)=BSDXC("HOS LOC"),DA(1)=BSDXC("APPT DATE"),DA=IEN
 S DR="309///"_BSDXC("VISIT DATE")_";302///`"_BSDXC("USR")_";305///"_$$NOW^XLFDT
 D ^DIE
 I $$GET1^DIQ(9009017.2,+BSDXC("HOS LOC"),.09)'="YES" Q  ;don't create visit
 S BSDXC("CALLER")="BSD CHECKIN"
 D GETVISIT^BSDAPI4(.BSDXC,.BSDXOUT)  ;call to check for existing visit and checkin appt; checkin appt should create visit if needed
 ;if we came back with at least 1 existing visit, we are done; otherwise we need to create a visit.
 I 'BSDXOUT(0) D
 . S BSDXC("FORCE ADD")=1
 . D GETVISIT^BSDAPI4(.BSDXC,.BSDXOUT)
 ;
 ; add provider to visit
 I $G(BSDXC("PROVIDER")) D
 . N BSDXVSTR,BSDXIEN
 . S BSDXIEN=$O(BSDXOUT(0))
 . S BSDXVSTR=$$VIS2VSTR^BEHOENCX(BSDXC("PAT"),BSDXIEN)
 . D UPDPRV^BEHOENCX(,BSDXC("PAT"),BSDXVSTR,BSDXC("PROVIDER"))
 Q
 ;
CANCHKIN(DFN,SDCL,SDT) ; Logic to cancel a checkin if the checkin date/time is passed in as '@'
 ; input:  DFN := ifn of patient
 ;        SDCL := clinic# 
 ;         SDT := appt d/t
 ;
 N SDDA
 S SDDA=$$FIND(DFN,SDT,SDCL)
 ;I 'SDDA D ERR("BSDX25: Could not locate appointment in database or appointment is cancelled.") Q
 I 'SDDA D ERR("Could not locate appointment in database or appointment is cancelled.") Q
 N SDATA,SDCIHDL,X S SDATA=SDDA_U_DFN_U_SDT_U_SDCL,SDCIHDL=$$HANDLE^SDAMEVT(1)
 D BEFORE^SDAMEVT(.SDATA,DFN,SDT,SDCL,SDDA,SDCIHDL)
 S FDA(44.003,SDDA_","_SDT_","_SDCL_",",309)="" D FILE^DIE(,"FDA","ERR")
 D AFTER^SDAMEVT(.SDATA,DFN,SDT,SDCL,SDDA,SDCIHDL)
 D CHKEVTD(DFN,SDT,SDCL)
 K FDA,ERR
 Q
 ;
FIND(DFN,SDT,SDCL) ; -- return appt ifn for pat
 ;   input:        DFN := ifn of pat.
 ;                 SDT := appt d/t
 ;                SDCL := ifn of clinic
 ;  output: [returned] := ifn if pat has appt on date/time
 ;
 N Y
 S Y=0 F  S Y=$O(^SC(SDCL,"S",SDT,1,Y)) Q:'Y  I $D(^(Y,0)),DFN=+^(0),$D(^DPT(+DFN,"S",SDT,0)),$$VALID(DFN,SDCL,SDT,Y) Q
 Q Y
 ;
VALID(DFN,SDCL,SDT,SDDA) ; -- return valid appt.
 ; **NOTE:  For speed consideration the ^SC and ^DPT nodes must be
 ;          check to see they exist prior to calling this entry point.
 ;   input:        DFN := ifn of pat.
 ;                 SDT := appt d/t
 ;                SDCL := ifn of clinic
 ;                SDDA := ifn of appt
 ;  output: [returned] := 1 for valid appt., 0 for not valid
 Q $S($P(^SC(SDCL,"S",SDT,1,SDDA,0),U,9)'="C":1,$P(^DPT(DFN,"S",SDT,0),U,2)["C":1,1:0)
 ;
CHKEVT(BSDXPAT,BSDXSTART,BSDXSC) ;EP Called by BSDX CHECKIN APPOINTMENT event
 ;when appointments CHECKIN via PIMS interface.
 ;Propagates CHECKIN to BSDXAPPT and raises refresh event to running GUI clients
 ;
 Q:+$G(BSDXNOEV)
 Q:'+$G(BSDXSC)
 N BSDXSTAT,BSDXFOUND,BSDXRES
 S BSDXSTAT=""
 S:$G(SDATA("AFTER","STATUS"))["CHECKED IN" BSDXSTAT=$P(SDATA("AFTER","STATUS"),"^",4)
 S BSDXFOUND=0
 I $D(^BSDXRES("ALOC",BSDXSC)) S BSDXRES=$O(^BSDXRES("ALOC",BSDXSC,0)) S BSDXFOUND=$$CHKEVT1(BSDXRES,BSDXSTART,BSDXPAT,BSDXSTAT)
 I BSDXFOUND D CHKEVT3(BSDXRES) Q
 I $D(^BXDXRES("ASSOC",BSDXSC)) S BSDXRES=$O(^BSDXRES("ASSOC",BSDXSC,0)) S BSDXFOUND=$$CHKEVT1(BSDXRES,BSDXSTART,BSDXPAT,BSDXSTAT)
 I BSDXFOUND D CHKEVT3(BSDXRES)
 Q
 ;
CHKEVT1(BSDXRES,BSDXSTART,BSDXPAT,BSDXSTAT) ;
 ;Get appointment id in BSDXAPT
 ;If found, call BSDXNOS(BSDXAPPT) and return 1
 ;else return 0
 N BSDXFOUND,BSDXAPPT
 S BSDXFOUND=0
 Q:'+$G(BSDXRES) BSDXFOUND
 Q:'$D(^BSDXAPPT("ARSRC",BSDXRES,BSDXSTART)) BSDXFOUND
 S BSDXAPPT=0 F  S BSDXAPPT=$O(^BSDXAPPT("ARSRC",BSDXRES,BSDXSTART,BSDXAPPT)) Q:'+BSDXAPPT  D  Q:BSDXFOUND
 . S BSDXNOD=$G(^BSDXAPPT(BSDXAPPT,0)) Q:BSDXNOD=""
 . I $P(BSDXNOD,U,5)=BSDXPAT,$P(BSDXNOD,U,12)="" S BSDXFOUND=1 Q
 I BSDXFOUND,+$G(BSDXAPPT) D BSDXCHK(BSDXAPPT,BSDXSTAT)
 Q BSDXFOUND
 ;
CHKEVT3(BSDXRES) ;
 ;Call RaiseEvent to notify GUI clients
 ;
 N BSDXRESN
 S BSDXRESN=$G(^BSDXRES(BSDXRES,0))
 Q:BSDXRESN=""
 S BSDXRESN=$P(BSDXRESN,"^")
 ;D EVENT^BSDX23("SCHEDULE-"_BSDXRESN,"","","")
 D EVENT^BMXMEVN("BSDX SCHEDULE",BSDXRESN)
 Q
 ;
CHKEVTD(BSDXPAT,BSDXSTART,BSDXSC) ;EP Called by BSDX CHECKIN APPOINTMENT event
 ;when  an appointment CHECKIN is deleted via.
 ;Deletes CHECKIN to and raises refresh event to running GUI clients
 ;
 ;
 Q:+$G(BSDXNOEV)
 Q:'+$G(BSDXSC)
 N BSDXSTAT,BSDXFOUND,BSDXRES
 S BSDXSTAT=""
 S:$G(SDATA("AFTER","STATUS"))'="CHECKED IN" BSDXSTAT=$P(SDATA("AFTER","STATUS"),"^",4)
 I BSDXSTAT="" S BSDXRES=$O(^BSDXRES("ALOC",BSDXSC,0))
 I BSDXRES D CHKEVT3(BSDXRES) Q
 S BSDXFOUND=0
 ;
 ;I $D(^BSDXRES("ALOC",BSDXSC)) S BSDXRES=$O(^BSDXRES("ALOC",BSDXSC,0)) S BSDXFOUND=$$CHKEVT1(BSDXRES,BSDXSTART,BSDXPAT,BSDXSTAT)
 ;I BSDXFOUND D CHKEVT3(BSDXRES) Q
 ;I $D(^BXDXRES("ASSOC",BSDXSC)) S BSDXRES=$O(^BSDXRES("ASSOC",BSDXSC,0)) S BSDXFOUND=$$CHKEVT1(BSDXRES,BSDXSTART,BSDXPAT,BSDXSTAT)
 ;I BSDXFOUND D CHKEVT3(BSDXRES)
 Q
 ;
 ;CHECK OUT APPOINTMENT - RPC
CHECKOUT(BSDXY,DFN,SDT,SDCODT,BSDXAPTID,VPRV) ;EP Check Out appointment
 ; Returns   BSDXY
 ; Input  -- DFN      Patient file IEN
 ;           SDT      Appointment Date/Time in FM format
 ;           SDCODT   Date/Time of Check Out FM FORMAT [REQUIRED]
 ;           BSDXAPTID - Appointment ID
 ;           VPRV      - V Provider
 ; called by BSDX CHECKOUT APPOINTMENT remote procedure
 ;SETUP ERROR TRACKING
 D ^XBKVAR S X="ERROR^BSDX25",@^%ZOSF("TRAP")
 S BSDXI=0
 K ^BSDXTMP($J)
 S BSDXY="^BSDXTMP("_$J_")"
 S ^BSDXTMP($J,0)="T00020ERRORID"_$C(30)
 I '+BSDXAPTID D ERR("Invalid Appointment ID.") Q
 I '$D(^BSDXAPPT(BSDXAPTID,0)) D ERR("Invalid Appointment ID.") Q
 ;INITIALIZE VARIABLES
 S %DT="T"
 S X=SDT
 D ^%DT   ; GET FM FORMAT FOR APPOINTMENT DATE/TIME
 S SDT=Y
 S X=SDCODT
 D ^%DT   ; GET FM FORMAT FOR CHECKOUT DATE/TIME
 ;ChecOut time cannot be in the future
 S SDCODT=Y
 I SDCODT>$$HTFM^XLFDT($H) D ERR("Check Out time cannot be in the future.") Q
 ;
 ;appointment record
 S BSDXNOD=^BSDXAPPT(BSDXAPTID,0)
 ;make sure CHECKOUT time is after CHECKIN time
 I SDCODT<=$P(BSDXNOD,U,3) D ERR("Check Out time must be at least 1 minute after the Check In time of "_$TR($$FMTE^XLFDT($P(BSDXNOD,U,3)),"@"," ")_".") Q
 ;Hospital Location of RESOURCE
 S BSDXRES=$P(BSDXNOD,U,7) ;RESOURCEID
 S BSDXNOD=^BSDXRES(BSDXRES,0)
 S SDCL=$P(BSDXNOD,U,4) ;HOSPITAL LOCATION
 ; 
 S SDDA=0
 S SDASK=0
 S SDCOALBF=""
 S SDCOACT="CO"
 S SDLNE=""
 S SDQUIET=1
 K APIERR
 S APIERR=0
 D CO^BSDX25A(DFN,SDT,SDCL,SDDA,SDASK,SDCODT,SDCOACT,SDLNE,.SDCOALBF,BSDXAPTID,SDQUIET,VPRV,.APIERR) ;Appt Check Out
 ;ERROR(S) FOUND
 I APIERR>0 D
 . S CNT=""
 . F  S CNT=$O(APIERR(CNT),1,ERR) Q:CNT=""  S BSDXI=BSDXI+1 D ERR(ERR)
 ;NO ERROR
 I APIERR<1 D
 . S BSDXI=BSDXI+1
 . S ^BSDXTMP($J,BSDXI)="0"_$C(30)
 . S BSDXI=BSDXI+1
 . S ^BSDXTMP($J,BSDXI)=$C(31)
 Q
 ;
 ;CHECK OUT APPOINTMENT - RPC
CANCKOUT(BSDXY,BSDXAPTID) ;EP Check Out appointment
 ; Returns   BSDXY
 ; Input  -- BSDXAPTID - Appointment ID
 ; called by BSDX CANCEL CHECKOUT APPT remote procedure
 ;SETUP ERROR TRACKING
 D ^XBKVAR S X="ERROR^BSDX25",@^%ZOSF("TRAP")
 S BSDXI=0
 K ^BSDXTMP($J)
 S BSDXY="^BSDXTMP("_$J_")"
 S ^BSDXTMP($J,0)="T00020ERRORID"_$C(30)
 I '+BSDXAPTID D ERR("Invalid Appointment ID.") Q
 I '$D(^BSDXAPPT(BSDXAPTID,0)) D ERR("Invalid Appointment ID.") Q
 S BSDXNOD=^BSDXAPPT(BSDXAPTID,0)
 S APS=$P(BSDXNOD,U,19)
 S DFN=$P(BSDXNOD,U,5)
 S SDT=$P(BSDXNOD,U)
 S RES=$P(BSDXNOD,U,7)
 S SDCL=$P(^BSDXRES(RES,0),U,4)
 I $P(BSDXNOD,U,14)="" D ERR("Appointment is not Checked Out.") Q
 ; ^BSDXAPPT: update piece 8: Data Entry Clerk; clear piece 14: CHECKOUT;
 S DIE="^BSDXAPPT("
 S DA=BSDXAPTID
 S DR=".14////@;.08///"_DUZ
 D ^DIE
 ; ^SC file 44: clear piece C;3: CHECKED OUT; clear piece C;4: CHECK OUT USER; clear C;6: CHECK OUT ENTERED
 S DIE="^SC("_SDCL_",""S"","_SDT_",1,"
 S DA(2)=SDCL,DA(1)=SDT,(DA,SDN)=$$SCIEN^BSDU2(DFN,SDCL,SDT)
 S DR="303///@;304///@;306///@"
 D ^DIE
 ; ^AUPNVSIT file 9000010: clear piece 18: CHECK OUT DATE&TIME
 S SDOE=$$GETAPT^SDVSIT2(DFN,SDT,SDCL)
 S SDV=$$GET1^DIQ(409.68,SDOE,.05,"I")
 I +SDV D
 . S DIE="^AUPNVSIT(",DA=SDV
 . S DR=".18///@"
 . D ^DIE S AUPNVSIT=SDV D MOD^AUPNVSIT
 ; ^SCE file 409.68: Set piece 12 back to CHECKED IN, pointer to APPOINTMENT STATUS file 409.63; clear piece 7: CHECK OUT PROCESS COMPLETION
 I +APS D
 . S DIE=409.68,DA=SDOE,DR=".07///@;.12///"_APS
 . D ^DIE
 S BSDXI=BSDXI+1
 S ^BSDXTMP($J,BSDXI)="0"_$C(30)
 S BSDXI=BSDXI+1
 S ^BSDXTMP($J,BSDXI)=$C(31)
 Q
 ;
ERROR ;
 D ERR("RPMS Error")
 Q
 ;
ERR(ERRNO) ;Error processing
 I +ERRNO S BSDXERR=ERRNO+134234112 ;vbObjectError
 E  S BSDXERR=ERRNO
 S BSDXI=BSDXI+1
 S ^BSDXTMP($J,BSDXI)=BSDXERR_$C(30)
 S BSDXI=BSDXI+1
 S ^BSDXTMP($J,BSDXI)=$C(31)
 Q
