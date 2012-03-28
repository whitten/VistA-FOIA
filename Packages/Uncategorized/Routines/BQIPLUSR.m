BQIPLUSR ;PRXM/HC/ALA-User Preferences ; 19 Oct 2005  6:29 PM
 ;;2.1;ICARE MANAGEMENT SYSTEM;;Feb 07, 2011
 ;
 Q
 ;
OWNR(USR) ;EP -- Check owner
 ;
 ;Description
 ;  Check if this user who has signed into iCare is already
 ;  in ICARE USER File #90505
 ;Input
 ;  DUZ - User internal entry number signed into iCare
 ;Output
 ;   1 - if user exists or if user added okay
 ;   0 - if there was an error adding user
 ;
 I $G(^BQICARE(USR,0))'="" Q 1
 I $D(^BQICARE(USR)),$G(^BQICARE(USR,0))="" D  Q 1
 . S DIE="^BQICARE(",DA=USR,DR=".01///^S X=USR" D ^DIE
 ;
CR ;  Create new entry
 NEW IENARRY,BQIUSR,ERROR
 S IENARRY(1)=USR
 S BQIUSR(90505,"+1,",.01)=USR
 D UPDATE^DIE("","BQIUSR","IENARRY","ERROR")
 I $D(ERROR) Q 0
 Q 1
 ;
USPF(DATA,FAKE) ;EP -- BQI GET USER PREFS
 ;
 ;Description
 ;  Get the user's preferences
 ;Input
 ;  FAKE - extra 'blank' parameter required by BMXNET async 'feature'
 ;Output
 ;  DATA  - name of global (passed by reference) in which the data
 ;          is stored
 ;Expects
 ;  DUZ - the internal entry number of the person signed on
 ;
 I '$$OWNR(DUZ) S BMXSEC="Unable to access user record" Q
 ;
 NEW UID,II,COD,NAME,PLIEN,PLNM,PLID,PTSRCH,X,PTVIEW,RCVIEW,CMVIEW
 S UID=$S($G(ZTSK):"Z"_ZTSK,1:$J)
 S DATA=$NA(^TMP("BQIPLUSR",UID))
 K @DATA
 ;
 S II=0
 S @DATA@(II)="T00001DEFAULT_VIEW^I00010OWNER^I00099PANEL_IEN^I00010PANEL_ID^T00001PAT_SEARCH^"
 S @DATA@(II)=@DATA@(II)_"I00010PATIENT_VIEW^T00001RECORD_VIEW^T00001CMET_VIEW^T00001EVENT_TIP^"
 S @DATA@(II)=@DATA@(II)_"T00001EVENT_FILTER^T00001TRACKED_TIP^T00001TRACKED_FILTER^"
 S @DATA@(II)=@DATA@(II)_"T00001FOLLOWUP_TIP^T00001FOLLOWUP_FILTER"_$C(30)
 ;
 NEW $ESTACK,$ETRAP S $ETRAP="D ERR^BQIPLUSR D UNWIND^%ZTER" ; SAC 2006 2.2.3.3.2
 ;
 S PLIEN="",PLID=""
 S COD=$$GET1^DIQ(90505,DUZ_",",.02,"I")
 S NAME=$$GET1^DIQ(90505,DUZ_",",.02,"E")
 S PLNM=$$GET1^DIQ(90505,DUZ_",",.03,"E")
 S OWNR=$$GET1^DIQ(90505,DUZ_",",.04,"I")
 S PTSRCH=$$GET1^DIQ(90505,DUZ_",",.05,"I")
 S PTVIEW=$$GET1^DIQ(90505,DUZ_",",.07,"I") S:PTVIEW="" PTVIEW=$O(^BQI(90506.4,"B","Cover Sheet",""))
 S RCVIEW=$$GET1^DIQ(90505,DUZ_",",.08,"I") S:RCVIEW="" RCVIEW="N"
 S CMVIEW=$$GET1^DIQ(90505,DUZ_",",.09,"I")
 S CMTIP=$$GET1^DIQ(90505,DUZ_",",.1,"I") S:CMTIP="" CMTIP="S"
 S CMFIL=$$GET1^DIQ(90505,DUZ_",",.11,"I") S:CMFIL="" CMFIL="S"
 S TRTIP=$$GET1^DIQ(90505,DUZ_",",.12,"I") S:TRTIP="" TRTIP="S"
 S TRFIL=$$GET1^DIQ(90505,DUZ_",",.13,"I") S:TRFIL="" TRFIL="S"
 S FLTIP=$$GET1^DIQ(90505,DUZ_",",.14,"I") S:FLTIP="" FLTIP="S"
 S FLFIL=$$GET1^DIQ(90505,DUZ_",",.15,"I") S:FLFIL="" FLFIL="S"
 I PTSRCH="" S PTSRCH="A"
 I OWNR="" S OWNR=DUZ
 ;
 I PLNM'="" D
 . NEW DIC,DA
 . S DA(1)=OWNR,DIC="^BQICARE("_DA(1)_",1,",X=PLNM,DIC(0)="Z"
 . D ^DIC
 . S PLIEN=+Y
 . S PLID=$$PLID^BQIUG1(OWNR,PLIEN)
 ;
 S II=II+1,@DATA@(II)=COD_U_OWNR_U_PLIEN_U_PLID_U_PTSRCH_U_PTVIEW_U_RCVIEW_U_CMVIEW_U
 S @DATA@(II)=@DATA@(II)_CMTIP_U_CMFIL_U_TRTIP_U_TRFIL_U_FLTIP_U_FLFIL_$C(30)
 ;
DONE ;
 S II=II+1,@DATA@(II)=$C(31)
 Q
 ;
ERR ;
 D ^%ZTER
 NEW Y,ERRDTM
 S Y=$$NOW^XLFDT() X ^DD("DD") S ERRDTM=Y
 S BMXSEC="Recording that an error occurred at "_ERRDTM
 I $D(II),$D(DATA) S II=II+1,@DATA@(II)=$C(31)
 Q
 ;
SVUP(DATA,VIEW,OWNR,PLIEN,CMVIEW,SEARCH,PTVIEW,RCVIEW,CMTIP,CMFIL,TRTIP,TRFIL,FLTIP,FLFIL) ;EP -- BQI SET USER PREFS
 ;
 ; Input
 ;  VIEW   - the default view code
 ;  OWNR   - the owner
 ;  PLIEN  - the panel ien (if default view is specified panel)
 ;  CMVIEW - the default CMET view (if default view is CMET or Split View CMET)
 ;  SEARCH - Patient search criteria
 ;  PTVIEW - Patient View opens with this tab
 ;  RCVIEW - View is opened maximized or minimized
 ;  CMTIP  - Queued Tip show or hide
 ;  CMFIL  - Queued Filter show or hide
 ;  TRTIP  - Tracked Tip show or hide
 ;  TRFIL  - Tracked Filter show or hide
 ;  FLTIP  - Followup Tip show or hide
 ;  FLFIL  - Followup Filter show or hide
 ;Output
 ;  DATA  - name of global (passed by reference) in which the data
 ;          is stored
 ;Assumes DUZ the user signed onto iCare
 ;
 NEW UID,II,X
 S UID=$S($G(ZTSK):"Z"_ZTSK,1:$J)
 S DATA=$NA(^TMP("BQIPLUSR",UID))
 K @DATA
 ;
 I '$$OWNR(DUZ) S BMXSEC="There is a problem with your entry." Q
 ;
 S II=0
 S @DATA@(II)="I00010RESULT"_$C(30)
 ;
 NEW $ESTACK,$ETRAP S $ETRAP="D ERR^BQIPLUSR D UNWIND^%ZTER" ; SAC 2006 2.2.3.3.2
 ;
 S VIEW=$G(VIEW,"")
 S SEARCH=$G(SEARCH,""),CMVIEW=$G(CMVIEW,""),CMTIP=$G(CMTIP,""),CMFIL=$G(CMFIL,"")
 S TRTIP=$G(TRTIP,""),TRFIL=$G(TRFIL,""),FLTIP=$G(FLTIP,""),FLFIL=$G(FLFIL,"")
 S PTVIEW=$G(PTVIEW,"") S:PTVIEW="" PTVIEW=$O(^BQI(90506.4,"B","Cover Sheet",""))
 S RCVIEW=$G(RCVIEW,"") S:RCVIEW="" RCVIEW="N"
 ;
 S BQIUPD(90505,DUZ_",",.02)=VIEW
 S BQIUPD(90505,DUZ_",",.05)=SEARCH
 S BQIUPD(90505,DUZ_",",.07)=PTVIEW
 S BQIUPD(90505,DUZ_",",.08)=RCVIEW
 S BQIUPD(90505,DUZ_",",.1)=CMTIP
 S BQIUPD(90505,DUZ_",",.11)=CMFIL
 S BQIUPD(90505,DUZ_",",.12)=TRTIP
 S BQIUPD(90505,DUZ_",",.13)=TRFIL
 S BQIUPD(90505,DUZ_",",.14)=FLTIP
 S BQIUPD(90505,DUZ_",",.15)=FLFIL
 ;
 I $G(PLIEN)'="" D
 . NEW DA,IENS
 . S DA(1)=OWNR,DA=PLIEN,IENS=$$IENS^DILF(.DA)
 . S BQIUPD(90505,DUZ_",",.03)=$$GET1^DIQ(90505.01,IENS,.01,"E")
 . S BQIUPD(90505,DUZ_",",.04)=OWNR
 ;
 ;Save the Default CMET view
 S:CMVIEW]"" BQIUPD(90505,DUZ_",",.09)=CMVIEW
 ;
 K ERROR
 D FILE^DIE("","BQIUPD","ERROR")
 K BQIUPD
 S II=II+1
 I '$D(ERROR) S @DATA@(II)="1"_$C(30)
 I $D(ERROR) S @DATA@(II)="-1"_$C(30)
 S II=II+1,@DATA@(II)=$C(31)
 Q
