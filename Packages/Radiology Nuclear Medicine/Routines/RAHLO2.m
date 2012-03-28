RAHLO2 ;HIRMFO/GJC-File rpt (data from bridge program) ;10/30/97  09:02
 ;;5.0;Radiology/Nuclear Medicine;;Mar 16, 1998
ADENDUM ; store new lines at the end of existing text
 F A="I","R" D
 . I $O(^TMP("RARPT-REC",$J,RASUB,$S(A="I":"RAIMP",1:"RATXT"),0)) D
 .. S RACNT=+$O(^RARPT(RARPT,A,9999999),-1),RASTRNDE=RACNT+1
 .. ; Check if the impression an/or report text sent with the addendum
 .. ; is to be the initial text added to the word processing multiples.
 .. ; RASTRNDE=the first subscript where impression/report data is to
 .. ; be stored.  If no existing impression/report text data, RASTRNDE
 .. ; equals one.  If one & RACNT equals one, don't add a blank line
 .. ; before adding addendum text.  If RASTRNDE & RACNT both >1, add
 .. ; the blank line.
 .. S I=0 F  S I=$O(^TMP("RARPT-REC",$J,RASUB,$S(A="I":"RAIMP",1:"RATXT"),I)) Q:I'>0  D
 ... S RACNT=RACNT+1,L=$G(^TMP("RARPT-REC",$J,RASUB,$S(A="I":"RAIMP",1:"RATXT"),I))
 ... S:I=$O(^TMP("RARPT-REC",$J,RASUB,$S(A="I":"RAIMP",1:"RATXT"),0)) L="Addendum: "_L ; if the first line, append 'addendum:'
 ... I (RASTRNDE=RACNT),(RACNT>1) S ^RARPT(RARPT,A,RACNT,0)=" ",RACNT=RACNT+1
 ... S ^RARPT(RARPT,A,RACNT,0)=L
 ... Q
 .. S ^RARPT(RARPT,A,0)="^^"_RACNT_"^"_RACNT_"^"_RADATE
 .. Q
 . Q
 K A,I,L,RACNT,RASTRNDE
 Q
ERR(A) ; Invalid impression/report text message.
 ; Input: 'A' - either "I" for impression, or "R" for report
 ; Output: the appropriate error message
 Q "Invalid "_$S(A="I":"Impression",1:"Report")_" Text"
 ;
DIAG    ; Check if the Diagnostic Codes passed are valid.  Set RADX equal
 ; to primary Dx code pntr value.  Set RASECDX(x) to the secondary
 ; Dx code(s) if any.
 S I=0
 F  S I=$O(^TMP("RARPT-REC",$J,RASUB,"RADX",I)) Q:I'>0  D  Q:$D(RAERR)
 . S RADIAG=$G(^TMP("RARPT-REC",$J,RASUB,"RADX",I))
 . S:RADIAG']"" RAERR="Missing Diagnostic Code" Q:$D(RAERR)
 . ; If RADXIEN is a number, set RADXIEN to what is assumed to be a
 . ; valid pointer (ien) for file 78.3
 . I +RADIAG=RADIAG S RADXIEN=RADIAG
 . ; If RADIAG is in a free text format, convert the external value
 . ; into the ien for file 78.3
 . I +RADIAG'=RADIAG S RADXIEN=$$FIND1^DIC(78.3,"","X",RADIAG)
 . I '$D(^RA(78.3,RADXIEN,0)) S RAERR="Invalid Diagnostic Code" Q
 . IF I=1 S RADX=RADXIEN Q  ; RADX=pri. Dx Code
 . ; are any of the sec. Dx codes equal to our pri. Dx code?
 . S:RADXIEN=RADX RAERR="Secondary Dx codes must differ from the primary Dx code." Q:$D(RAERR)
 . S:$D(RASECDX(RADXIEN))#2 RAERR="Duplicate secondary Dx codes." Q:$D(RAERR)
 . S RASECDX(RADXIEN)="" ; set the sec. Dx array
 . Q
 K I,RADIAG,RADXIEN
 Q
SECDX ; Kill old sec. Dx nodes, and add the new ones into the 70.14 multiple
 ; called from RAHLO.  Needs RADFN,RADTI & RACNI to function.
 Q:'$D(RADFN)!('$D(RADTI))!('$D(RACNI))
 I $O(^RADPT(RADFN,"DT",RADTI,"P",RACNI,"DX",0)) D KILSECDG^RAHLO4
 K RAFDA N RAX S RAX=0,RAFDA(70,"?1,",.01)=RADFN
 S RAFDA(70.02,"?2,?1,",.01)=(9999999.9999-RADTI)
 S RAFDA(70.03,"?3,?2,?1,",.01)=$P($G(^RADPT(RADFN,"DT",RADTI,"P",RACNI,0)),"^")
 F  S RAX=$O(RASECDX(RAX)) Q:RAX'>0  D
 . S RAFDA(70.14,"+"_RAX_"9,?3,?2,?1,",.01)=RAX
 . Q
 D UPDATE^DIE("","RAFDA")
 Q
IMPTXT ; Check if the impression text consists only of the string
 ; 'impression:".  If 'impression:' is the only set of characters,
 ; (spaces are excluded) then delete the "RAIMP" node.
 N RA1 S RA1=$O(^TMP("RARPT-REC",$J,RASUB,"RAIMP",0))
 Q:'RA1  N RAIMP S RAIMP=$G(^TMP("RARPT-REC",$J,RASUB,"RAIMP",RA1))
 I $$UP^XLFSTR($E(RAIMP,1,11))="IMPRESSION:" D
 . S $E(RAIMP,1,11)="" ; strip out 'impression:' if it is the first
 . ;                     eleven chars of the impression text
 . ; now strip off leading spaces from the remaining
 . ; text that led with 'impression:' if present
 . F I1=1:1 S:$E(RAIMP,I1)'=" " RAIMP=$E(RAIMP,I1,99999) Q:$E(RAIMP)'=" "
 . S ^TMP("RARPT-REC",$J,RASUB,"RAIMP",RA1)=RAIMP
 . Q
 Q:$O(^TMP("RARPT-REC",$J,RASUB,"RAIMP",RA1))  ; more imp. text follows
 K:$G(^TMP("RARPT-REC",$J,RASUB,"RAIMP",RA1))="" ^TMP("RARPT-REC",$J,RASUB,"RAIMP",RA1) ; if only "RAIMP" node null, delete "RAIMP" node
 Q
