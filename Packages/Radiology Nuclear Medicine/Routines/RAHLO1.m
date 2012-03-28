RAHLO1 ;HIRMFO/GJC/BNT-File rpt (data from bridge program) ;05/21/99  14:30
 ;;5.0;Radiology/Nuclear Medicine;**4,5,12,17,21,27**;Mar 16, 1998
FILE ;Create Entry in File 74 and File Data
 N RADATIME S RADATIME=$$NOW^XLFDT() I $L($P(RADATIME,".",2))>4 S RADATIME=$P(RADATIME,".",1)_"."_$E($P(RADATIME,".",2),1,4) S RADATIME=+RADATIME
 S RADPIECE=$S($D(^VA(200,"ARC","S",+$G(RAVERF))):15,$D(^VA(200,"ARC","R",+$G(RAVERF))):12,1:"")
 N:'$D(RAPRTSET) RAPRTSET N:'$D(RAMEMARR) RAMEMARR
 D EN2^RAUTL20(.RAMEMARR) ; 04/30/99 always recalculate RAPRTSET
 ; If rpt already exists, skip creating a new file 74 entry
 I RARPT,$D(^RARPT(RARPT,0)) S RASAV=RARPT D FILETST^RAHLO4 Q:$D(RAERR)  D  S RARPT=RASAV K RASAV G LOCK1
 . ; must save off RARPT, RAVERF and other RA* variables because
 . ; they are being killed off somewhere in the 'Unverify A Report'
 . ; option.
 . N RADFN,RADTI,RACNI,RARPTSTS,RASSN,RADATE,RALONGCN,RAVERF
 . S RAEDIT=1 N RAEDIT
 . I $D(RADENDUM)#2,($P(^RARPT(RARPT,0),"^",5)="V") D  Q  ; edit on current record (for activity log)
 .. S ZTQUEUED="1^dummy to supress screen displays in UP2^RAUTL1"
 .. D UNVER^RARTE1(RARPT) K ZTQUEUED
 .. Q
 . K ^RARPT(RARPT,"I"),^("R"),^("H")
 . Q
 I RAPRTSET L +^RADPT(RADFN,"DT",RADTI):0 G:$T NEW1 S RAERR="ANOTHER USER IS CURRENTLY EDITING THIS PRINTSET. TRY LATER." Q
NEW1 S I=$P(^RARPT(0),"^",3)
LOCK S I=I+1 L +^RARPT(I):1 I '$T!($D(^RARPT(I)))!($D(^RARPT("B",I))) L -^RARPT(I) G LOCK
 S ^RARPT(I,0)=RALONGCN,RARPT=I,^(0)=$P(^RARPT(0),"^",1,2)_"^"_I_"^"_($P(^(0),"^",4)+1)
 ;if case is member of a print set, then create sub-recs for file #74
 G:'RAPRTSET LOCK1
 I '$D(RARPTN) N RARPTN S RARPTN=RALONGCN
 N RAXIT D PTR^RARTE2 ;create corresponding subrecs in ^RARPT()
 I $D(RAERR) L -^RADPT(RADFN,"DT",RADTI) Q  ;unlock before quiting 122000
LOCK1 I $D(RAESIG) S X=RAESIG,X1=$G(RAVERF),X2=RARPT D EN^XUSHSHP S RAESIG=X
 K DA,DIE,DQ,DR S DA=RARPT,DIE="^RARPT("
 S DR="5////"_RARPTSTS ; rpt status
 ;Verifier & Verified date will be set if RAVERF exists for new
 ;reports, edits, and addendums.  Date rpt entered and reported date
 ;will be set for new reports, and not reset for edits and addendums
 S DR=DR_";6////"_$S($D(RAEDIT):"",1:RADATIME) ; date/time rpt entered
 S DR=DR_";7////"_$S($G(RAVERF)&(RARPTSTS="V"):RADATIME,1:"") ; v'fied date/time
 S DR=DR_";8////"_$S($D(RAEDIT):"",1:RADATE) ; reported date
 S DR=DR_";9////"_$S($G(RAVERF)&(RARPTSTS="V"):RAVERF,1:"") ; v'fying phys
 S DR=DR_";11////"_$S($G(RATRANSC):RATRANSC,$G(RAVERF):RAVERF,1:"") ; transcriptionist
 I $G(RAVERF),(RARPTSTS="V") S DR=DR_";17////"_$G(^TMP("RARPT-REC",$J,RASUB,"RAWHOCHANGE")) ;status changed to 'verified' by
 ; D ^DIE K DA,DR ;BNT- Moved the DIE call down three lines due to a 
 ; problem found at Indy while testing PowerScribe.  Site was doing a
 ; local MUMPS cross reference on one of the nodes that are set below.
 S $P(^RARPT(RARPT,0),"^",2)=RADFN,$P(^(0),"^",3)=(9999999.9999-RADTI),$P(^(0),"^",4)=$P(RALONGCN,"-",2) ;must set manually due uneditable
 S $P(^RARPT(RARPT,0),"^",10)=$S($D(RAESIG)&(RARPTSTS="V"):RAESIG,1:"") ; hard set because Elec Sig Code may contain a semi-colon which causes errors in DIE
 D ^DIE K DA,DR
 I $D(RADX),('$D(RADENDUM)#2) D
 . K DIE,DA,DR S DA=RACNI,DA(1)=RADTI,DA(2)=RADFN
 . S DIE="^RADPT("_DA(2)_",""DT"","_DA(1)_",""P"","
 . S DR="13////"_RADX D ^DIE K DIE,DA,DR
 . S:$P(^RA(78.3,+RADX,0),"^",4)="y" RAAB=1
 . Q
 I '$D(RADENDUM)#2,($G(^TMP("RARPT-REC",$J,RASUB,"RASTAFF"))!$G(^("RARESIDENT"))) D
 . K DIE,DA S DR=""
 . S RAPRIMAR=+$G(^TMP("RARPT-REC",$J,RASUB,"RARESIDENT")) I $D(^VA(200,"ARC","R",RAPRIMAR)) S DR="12////"_RAPRIMAR
 . S RAPRIMAR=+$G(^TMP("RARPT-REC",$J,RASUB,"RASTAFF")) I $D(^VA(200,"ARC","S",RAPRIMAR)) S DR=$S(DR]"":DR_";",1:"")_"15////"_RAPRIMAR
 . Q:'$G(DR)
 . S DA=RACNI,DA(1)=RADTI,DA(2)=RADFN
 . S DIE="^RADPT("_DA(2)_",""DT"","_DA(1)_",""P"","
 . D ^DIE K DIE,DA,DR
 . Q
 ;
 S $P(^RADPT(RADFN,"DT",RADTI,"P",RACNI,0),"^",17)=RARPT I $G(RADPIECE),$P(^(0),"^",RADPIECE)="",('$D(RADENDUM)#2) D SETPHYS^RAHLO4
 ; file impression text if present & not an addendum
 I '$D(RADENDUM) D
 . S J=0 I $O(^TMP("RARPT-REC",$J,RASUB,"RAIMP",0)) S I=0 F J=0:1 S I=$O(^TMP("RARPT-REC",$J,RASUB,"RAIMP",I)) Q:I'>0  I $D(^(I)) S ^RARPT(RARPT,"I",(J+1),0)=$G(^TMP("RARPT-REC",$J,RASUB,"RAIMP",I))
 . S:J ^RARPT(RARPT,"I",0)="^^"_J_"^"_J_"^"_RADATE
 . Q
 ; file report text if present & not an addendum
 I '$D(RADENDUM) D
 . S J=0 I $O(^TMP("RARPT-REC",$J,RASUB,"RATXT",0)) S I=0 F J=0:1 S I=$O(^TMP("RARPT-REC",$J,RASUB,"RATXT",I)) Q:I'>0  I $D(^(I)) S ^RARPT(RARPT,"R",(J+1),0)=$G(^TMP("RARPT-REC",$J,RASUB,"RATXT",I))
 . S:J ^RARPT(RARPT,"R",0)="^^"_J_"^"_J_"^"_RADATE
 . Q
 ; if addendum, add addendum text to impression or report
 I $D(RADENDUM),($O(^TMP("RARPT-REC",$J,RASUB,"RAIMP",0))!$O(^TMP("RARPT-REC",$J,RASUB,"RATXT",0))) D ADENDUM^RAHLO2 ; store new lines at the end of existing text
 ;
 ;
 ; Check for History from Dictation
 ; If history sent, check if previous history exists.  If previous
 ; history then current history will follow adding 'Addendum:' before 
 ; the text.
 I $O(^TMP("RARPT-REC",$J,RASUB,"RAHIST",0)) D
 . S RACNT=+$O(^RARPT(RARPT,"H",9999999),-1),RAHSTNDE=RACNT+1
 . S RANEW=$S(RACNT>0:0,1:1)
 . S I=0 F  S I=$O(^TMP("RARPT-REC",$J,RASUB,"RAHIST",I)) Q:I'>0  D
 . . S RACNT=RACNT+1
 . . S RALN=$G(^TMP("RARPT-REC",$J,RASUB,"RAHIST",I))
 . . S:'RANEW&(I=$O(^TMP("RARPT-REC",$J,RASUB,"RAHIST",0))) RALN="Addendum: "_RALN ; if the first line, append 'Addendum:'
 . . I (RAHSTNDE=RACNT),(RACNT>1) S ^RARPT(RARPT,"H",RACNT,0)=" ",RACNT=RACNT+1
 . . S ^RARPT(RARPT,"H",RACNT,0)=RALN
 . . Q
 . S ^RARPT(RARPT,"H",0)="^^"_RACNT_"^"_RACNT_"^"_RADATE
 . Q
 ;
 ;
 I $P(^RARPT(RARPT,0),U,5)="V",$T(CREATE^WVRALINK)]"" D CREATE^WVRALINK(RADFN,RADTI,RACNI) ; women's health
 D KILSECDG^RAHLO4 ;kill sec diag nodes for this case
 G:'RAPRTSET UPACT ; the next section is for printsets only
 N RACNISAV,RA7
 N RA13,RA12,RA15 ;prim dx, prim resid, prim staff, rpt pointer
 S RACNISAV=RACNI,RA7=0
 S RA13=$P(^RADPT(RADFN,"DT",RADTI,"P",RACNI,0),U,13),RA12=$P(^(0),U,12),RA15=$P(^(0),U,15)
 F  S RA7=$O(RAMEMARR(RA7)) Q:RA7=""  I RACNISAV'=RA7 S RACNI=RA7 D UPMEM^RAHLO4,KILSECDG^RAHLO4
 S RACNI=RACNISAV
 L -^RADPT(RADFN,"DT",RADTI) ;unlock after pce 17 is set in all cases of this printset
 ;Update Activity Log
UPACT S DA=RARPT,DIE="^RARPT(",DR="100///""NOW""",DR(2,74.01)="2////"_$S(RARPTSTS="V":"V",$D(RAEDIT):"E",1:"I")_";3////"_$S($G(RATRANSC):RATRANSC,$G(RAVERF):RAVERF,1:"") D ^DIE K DA,DR,DE,DQ,DIE
 ; use ix^dik to kill before setting xrefs
 S DA=RARPT,DIK="^RARPT(",RAQUEUED=1 D IX^DIK I $D(RAMDV),RAMDV'="" D UPSTAT^RAUTL0
 L -^RARPT(RARPT) D:'$D(RAERR)&($G(^TMP("RARPT-REC",$J,RASUB,"VENDOR"))'="KURZWEIL") GENACK^RAHLTCPB ; generate 'ACK' message
 ; line pacs is for 2 tasks: hl7 msg'g  &  voice verified rpt printout
PACS I ($P(^RARPT(RARPT,0),U,5)="V")!($P(^(0),U,5)="R") D TASK^RAHLO4,VOICE^RAHLO4
 K RAAB,RAEDIT,RAESIG,RAQUEUED,RARPT,RAHIST
 Q
