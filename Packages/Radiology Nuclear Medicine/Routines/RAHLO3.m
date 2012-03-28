RAHLO3 ;HIRMFO/GJC-Process data set from the bridge program ;11/18/97  12:13
 ;;5.0;Radiology/Nuclear Medicine;**4**;Mar 16, 1998
RPTSTAT ; Determine the status to set this report to.
 K RARPTSTS S:$D(RAESIG) RARPTSTS="V" Q:$D(RARPTSTS)
 ; $D(RAESIG)=0 now figure out report status
 S RASTAT=$E($G(^TMP("RARPT-REC",$J,RASUB,"RASTAT")))
 I RASTAT="A" S RARPTSTS="V" Q
 I RASTAT]"",("FR"[RASTAT) D
 . S:RASTAT="F" RARPTSTS="V" Q:$D(RARPTSTS)
 . ; do we allow 'Released/Unverified' reports for this location?
 . S RARPTSTS=$S($P($G(^RA(79.1,RAMLC,0)),"^",17)="Y":"R",1:"D")
 . Q
 ; if no status, & there's physician data (verifier/primary),set status
 I '$D(RARPTSTS),($G(RAVERF)!$G(^TMP("RARPT-REC",$J,RASUB,"RASTAFF"))!$G(^("RARESIDENT"))) S RARPTSTS=$S($P($G(^RA(79.1,RAMLC,0)),"^",17)="Y":"R",1:"D")
 ; if still no status, default to draft
 S:'$D(RARPTSTS) RARPTSTS="D"
 K RASTAT
 Q
TEXT(X) ; Check if the Impression Text and the Report Text contain
 ; valid characters.
 ; Input : X = "I" if Impr Text is being checked, "R" if Rpt Text
 ; Output: 0=invalid, 1=valid
 N CNT,DATA,FLAG,I,I1,J,Y S (FLAG,I)=0
 F  S I=$O(^TMP("RARPT-REC",$J,RASUB,$S(X="I":"RAIMP",1:"RATXT"),I)) Q:I'>0  D  Q:FLAG
 . S CNT=0,DATA=$G(^TMP("RARPT-REC",$J,RASUB,$S(X="I":"RAIMP",1:"RATXT"),I)) Q:DATA']""
 . F J=1:1:$L(DATA) D  Q:FLAG
 .. S:$E(DATA,J)?1AN CNT=CNT+1
 .. S:$E(DATA,J)'?1AN&(CNT>0) CNT=0
 .. S:CNT=2 FLAG=1
 .. Q
 . Q
 Q FLAG
 ;
VERCHK ; Check if our provider can verify reports.
 ; Examine the following four (4) conditions if $D(RAESIG)
 ; 1) Does this person have a resident or staff classification?
 ; 2) If a resident, does the division parameter allow resident
 ;    verification?
 ; 3) Does this person hold the "RA VERIFY" key?
 ; 4) Is this person an activate Rad/Nuc Med user?
 ; 5) Can this person verify reports without staff review?
 ; If 'No' to any of the above questions, kill RAESIG & set the variable
 ; RAERR to the appropriate error message.
 I '$D(^VA(200,"ARC","R",+$G(RAVERF))),('$D(^VA(200,"ARC","S",+$G(RAVERF)))) D  Q
 . ; neither a resident or staff
 . K RAESIG S RAERR="Provider not classified as resident or staff."
 . Q
 I $D(^VA(200,"ARC","R",+$G(RAVERF))),('$P(RAMDV,"^",18)) D  Q
 . ; residents can't verify reports linked to this division
 . K RAESIG S RAERR="Residents are not permitted to verify reports."
 . Q
 I '$D(^XUSEC("RA VERIFY",+$G(RAVERF))) D  Q
 . ; verifier MUST have the RA VERIFY key.
 . K RAESIG S RAERR="Provider does not meet security requirements to verify report."
 . Q
 I $P($G(^VA(200,+$G(RAVERF),"RA")),"^",3),($P(^("RA"),"^",3)'>$$DT^XLFDT()) D
 . ; Rad/Nuc Med user has been inactivated.
 . K RAESIG S RAERR="Inactive Rad/Nuc Med Classification for Interpreting Physician."
 . Q
 I '$S('$D(^VA(200,+$G(RAVERF),"RA")):1,$P(^("RA"),"^")'="Y":1,1:0) D
 . K RAESIG S RAERR="Staff review required to verify report."
 . Q
 Q
