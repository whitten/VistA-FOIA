RAHLTCPB ;HIRMFO/REL,GJC,BNT - Rad/Nuc Med HL7 TCP/IP Bridge;05/21/99
 ;;5.0;Radiology/Nuclear Medicine;**12,17,25**;Mar 16, 1998
EN1 ; Build the ^TMP("RARPT-REC" global when we receive the
 ; message from HL7.
 S RASUB=HL("MID") K RAERR
 K ^TMP("RARPT-HL7",$J) ; clean area that holds data from HL7
 K ^TMP("RARPT-REC",$J,RASUB) ; kill storage area for new HL7 message id
 S ^TMP("RARPT-REC",$J,RASUB,"RADATE")=$$DT^XLFDT()
 F I=1:1 X HLNEXT Q:HLQUIT'>0  S ^TMP("RARPT-HL7",$J,I)=HLNODE,J=0 F  S J=$O(HLNODE(J)) Q:'J  S ^TMP("RARPT-HL7",$J,I,J)=HLNODE(J)
 S CNT=2,SEGMNT=$G(^TMP("RARPT-HL7",$J,CNT))
 S VNDR=$G(HL("SAN"))
 S ^TMP("RARPT-REC",$J,RASUB,"VENDOR")=$G(HL("SAN"))
PID ; Pick data off the 'PID' segment.
 I $P(SEGMNT,HL("FS"))="PID" D
 . S SEGMNT=$P(SEGMNT,HL("FS"),2,99999)
 . I $P($P(SEGMNT,HL("FS"),3),$E(HL("ECH")))]"" D
 .. S ^TMP("RARPT-REC",$J,RASUB,"RADFN")=$P($P(SEGMNT,HL("FS"),3),$E(HL("ECH")))
 .. Q
 . I $P(SEGMNT,HL("FS"),19)]"" D
 .. S ^TMP("RARPT-REC",$J,RASUB,"RASSN")=$P(SEGMNT,HL("FS"),19)
 .. Q
 . Q
 E  S RAERR="Missing PID segment" D XIT Q
 I '(+$G(^TMP("RARPT-REC",$J,RASUB,"RADFN"))) D  Q
 .S RAERR="Invalid Patient ID"
 .D XIT
 ; Save off E-Sig information (if it exists)
 S:$D(HL("ESIG")) ^TMP("RARPT-REC",$J,RASUB,"RAESIG")=HL("ESIG")
 ;
OBR ; Pick data off the 'OBR' segment.
 K SEGMNT F  S CNT=$O(^TMP("RARPT-HL7",$J,CNT)) Q:CNT=""  S SEGMNT=$G(^(CNT)) Q:$P(SEGMNT,HL("FS"))="OBR"  ; find the 'OBR' segment
 I $P($G(SEGMNT),HL("FS"))'="OBR" S RAERR="Missing OBR segment" D XIT Q
 S SEGMNT=$P(SEGMNT,HL("FS"),2,99999)
 I $P(SEGMNT,HL("FS"),3)]"" D
 . N RADTCN S RADTCN=$P(SEGMNT,HL("FS"),3)
 . S:$P($P(RADTCN,$E(HL("ECH"))),"-")]"" ^TMP("RARPT-REC",$J,RASUB,"RADTI")=$P($P(RADTCN,$E(HL("ECH"))),"-")
 . S:$P($P(RADTCN,$E(HL("ECH"))),"-",2)]"" ^TMP("RARPT-REC",$J,RASUB,"RACNI")=$P($P(RADTCN,$E(HL("ECH"))),"-",2)
 . S:$P(RADTCN,$E(HL("ECH")),2)["&L" RADTCN=$TR(RADTCN,"&","^")
 . S:$P(RADTCN,$E(HL("ECH")),2)]"" ^TMP("RARPT-REC",$J,RASUB,"RALONGCN")=$P(RADTCN,$E(HL("ECH")),2)
 . Q
 S RAHLD=$$PCEXTR^RAHLO4(CNT,SEGMNT,25,HL("FS")) K RAHL70
 I RAHLD="" S RAERR="Missing Report Status" D XIT Q
 I "AFR"'[RAHLD S RAERR="Invalid Report Status" D XIT Q
 S ^TMP("RARPT-REC",$J,RASUB,"RASTAT")=RAHLD
 S RAHLD=$$PCEXTR^RAHLO4(CNT,SEGMNT,32,HL("FS")) K RAHL70
 I RAHLD']"" S RAERR="Missing Provider ID" D XIT Q
 S RAVERF=RAHLD
 ; -----   Check the validity of the provider name   -----
 I '$D(^VA(200,"B",RAVERF)) D  ; check for a partial match in file 200
 . D VFIER ; if one partial match found, return the entry ien
 . Q
 E  D  ; $D(^VA(200,"B",RAVERF)) true, get the entry ien
 . S RAVERF=$O(^VA(200,"B",RAVERF,0))
 . S:'RAVERF RAERR="Invalid Provider Name"
 . Q
 I $D(RAERR) D XIT Q
 ; can't get resident info from medspeak
 S RAHLD="",RAHLD=$$PCEXTR^RAHLO4(CNT,SEGMNT,33,HL("FS")),RARSDNT="" K RAHL70
 I RAHLD]"" D
 . S RARSDNT=$P(RAHLD,$E(HL("ECH"),4)) I '$D(^VA(200,+RARSDNT,0)) S RARSDNT=""
 S RAHLD="",RAHLD=$$PCEXTR^RAHLO4(CNT,SEGMNT,35,HL("FS")),RATRSCRP="" K RAHL70
 I RAHLD]"" D
 . S RATRSCRP=$P(RAHLD,$E(HL("ECH"),4)) I '$D(^VA(200,+RATRSCRP,0)) S RATRSCRP=""
 S ^TMP("RARPT-REC",$J,RASUB,"RAVERF")=RAVERF
 S ^TMP("RARPT-REC",$J,RASUB,"RATRANSCRIPT")=$S(RATRSCRP]"":RATRSCRP,RARSDNT]"":RARSDNT,1:RAVERF)
 S:$G(RARSDNT) ^TMP("RARPT-REC",$J,RASUB,"RARESIDENT")=RARSDNT
 S ^TMP("RARPT-REC",$J,RASUB,"RASTAFF")=RAVERF,^("RAWHOCHANGE")=RAVERF
 D ESIG
 K RAHLD,RARSDNT,RATRSCRP
 ;
OBX ; Pick data off the 'OBX' segments
 K SEGMNT F  S CNT=$O(^TMP("RARPT-HL7",$J,CNT)) Q:CNT=""  S SEGMNT=$G(^(CNT)) D:$P(SEGMNT,HL("FS"))="OBX"  Q:$D(RAERR)
 . S SEGMNT=$P(SEGMNT,HL("FS"),2,9999)
 . I $P(SEGMNT,HL("FS"),3)']"" S RAERR="Missing Observation Identifier" Q
 . S OBXTYP=$P(SEGMNT,HL("FS"),3),OBXTYP=$E(OBXTYP,$F(OBXTYP,"&"))
 . S OBX2CE=""
 . S:OBXTYP="" OBXTYP=" "
 . I OBXTYP=" "&($P(SEGMNT,HL("FS"),2)="CE") D
 . . I $P(SEGMNT,HL("FS"),5)=" " S OBXTYP="F" Q
 . . S OBX2CE=1,OBXTYP="D" Q
 . I "IDRF"'[OBXTYP S RAERR="Invalid Observation Identifier" Q
 . D RPT Q
XIT I $D(RAERR) D EN1^RAHLEXF,GENACK G XIT1
 I $D(^TMP("RARPT-REC",$J)) D EN1^RAHLO I $D(RAERR) D EN1^RAHLEXF,GENACK
XIT1 K ^TMP("RARPT-REC",$J) ; kill storage area for current HL7 message id
 K ^TMP("RARPT-HL7",$J) ; clean up HL7 storage
 K CNT,OBXTYPE,X1,LIN,RADATE,RADTCN,RAERR,RAESIG,RAHLD,RANODE,RARCNT
 K RAVERF,RASUB,SEGMNT,VNDR,MSA1,OBX2CE,RADX,RADX1,RADX2,RADX3
 Q
RPT ; Save off Report Text data.
 S RANODE=$S(OBXTYP="D":"RADX",OBXTYP="I":"RAIMP",1:"RATXT"),LIN=""
 I OBX2CE D  Q
 . S X=$P(SEGMNT,HL("FS"),5),RADX1=$P(X,$E(HL("ECH")))
 . S LIN=RADX1,L=999 D P2 S LIN=X
 . Q:X'["~"  F J=0:0 S J=$O(^TMP("RARPT-HL7",$J,CNT,J)) Q:'J  S X1=^(J),LIN=LIN_X1 Q
 . S RADX=LIN,RADX2=$P($P(RADX,"~",2),"^") S:RADX2]"" LIN=RADX2 D P2
 . S RADX3=$P($P(RADX,"~",3),"^") Q:RADX3']""  S LIN=RADX3 D P2 Q
 S X=$P(SEGMNT,HL("FS"),5)
 I X["\S\"!(X["\R\")!(X["\E\")!(X["\T\") D FORMAT
 D PAR
 F J=0:0 S J=$O(^TMP("RARPT-HL7",$J,CNT,J)) Q:'J  S X1=^(J),X=$E(X1,1,125) D PAR I $L(X1)>125 S X=$E(X1,126,999) D PAR
 I X=""!(LIN'="") S L=999 D P2
 Q
FORMAT ; Format report text for Escape Character delimited codes.
 S Y=X N T,Q
 I Y["\S\" S Q=$F(Y,"\S\"),T=Q-4,X=$E(Y,1,T)_$E(HL("ECH"))_$E(Y,Q,$L(X)),Y=X
 I Y["\R\" S Q=$F(Y,"\R\"),T=Q-4,X=$E(Y,1,T)_$E(HL("ECH"),2)_$E(Y,Q,$L(X)),Y=X
 I Y["\E\" S Q=$F(Y,"\E\"),T=Q-4,X=$E(Y,1,T)_$E(HL("ECH"),3)_$E(Y,Q,$L(X)),Y=X
 I Y["\T\" S Q=$F(Y,"\T\"),T=Q-4,X=$E(Y,1,T)_$E(HL("ECH"),4)_$E(Y,Q,$L(X)),Y=X
 I X["\S\"!(X["\R\")!(X["\E\")!(X["\T\") D FORMAT
 Q
PAR ; Build text paragraph
 S LIN=LIN_X
P1 I $L(LIN)<80 Q
 F L=80:-1:1 Q:$E(LIN,L)=" "
 D P2 S LIN=$E(LIN,L+1,999) G P1
P2 ; Set node
 S RARCNT(OBXTYP)=$G(RARCNT(OBXTYP))+1
 S ^TMP("RARPT-REC",$J,RASUB,RANODE,RARCNT(OBXTYP))=$E(LIN,1,L-1) Q
 ;
GENACK ; Compile the 'ACK' segment, generate the 'ACK' message.
 S MSA1="AA"
 Q:$E($G(VNDR),1,3)'="RA-"  ; Don't allow non RA namespaced interfaces
 I $D(RAERR) S MSA1=$S(VNDR="RA-PSCRIBE-TCP":"AE",1:"AR")
 ; Added next line to support MedSpeak interface.  Must re-initialize
 ; FS and EC's before sending ACK.
 D:VNDR="RA-CLIENT-TCP" INIT^HLFNC2("RA VOICE TCP SERVER RPT",.HL)
 S HLA("HLA",1)="MSA"_HL("FS")_MSA1_HL("FS")_HL("MID")_$S($D(RAERR):HL("FS")_RAERR,1:"")
 S HLEID=HL("EID"),HLEIDS=HL("EIDS"),HLARYTYP="LM",HLFORMAT=1,HLRESLTA=HL("MID")
 D GENACK^HLMA1(HLEID,HLMTIENS,HLEIDS,HLARYTYP,HLFORMAT,.HLRESTLA)
 Q
VFIER ; Check if the RAVERF string is a partial match to an entry in file
 ; 200.  If if is, check to see that is a partial match to only ONE
 ; active provider entry in file 200.
 I '$L(RAVERF) S RAERR="Missing Provider information" Q
 K RAVCNT,RAVIEN,RAVLGTH,RAVPS
 S RAVLGTH=$L(RAVERF) ; length of the RAVERF string
 S RAVCNT=0,RAVS1=RAVERF,RAVIEN=""
 F  S RAVS1=$O(^VA(200,"B",RAVS1)) Q:RAVS1=""!($E(RAVS1,1,RAVLGTH)'=RAVERF)  D  Q:RAVCNT>1
 . ; return subscripts that have the RAVERF string as the first
 . ; 1 - RAVLGTH chars of RAVS1
 . S RAVIEN=0
 . F  S RAVIEN=$O(^VA(200,"B",RAVS1,RAVIEN)) Q:RAVIEN'>0  D  Q:RAVCNT>1
 .. S RAVPS=$G(^VA(200,RAVIEN,"PS"))
 .. S:'$P(RAVPS,"^",4)!($P(RAVPS,"^",4)>DT) RAVCNT=RAVCNT+1
 .. I RAVCNT=1,('$D(RAVIEN(RAVCNT))#2) S RAVIEN(RAVCNT)=RAVIEN ; when
 .. ; we find the first active provider save the provider ien off
 .. ; in a local array.
 .. Q
 . Q
 ; Added for PowerScribe
 I RAVIEN']"" D
 . S RAVIEN=$P(RAVERF,$E(HL("ECH"),4))
 . S RAVPS=$G(^VA(200,RAVIEN,"PS"))
 . S:'$P(RAVPS,"^",4)!($P(RAVPS,"^",4)>DT) RAVCNT=RAVCNT+1
 . I RAVCNT=1,('$D(RAVIEN(RAVCNT))#2) S RAVIEN(RAVCNT)=RAVIEN
 . Q
 I RAVCNT=0 S RAERR="Invalid Provider Name" Q  ; partial match not found
 I RAVCNT>1 S RAERR="Non-Unique Provider Name" Q  ; >1 partial match
 S RAVERF=$G(RAVIEN(1)) S:'RAVERF RAERR="Provider Name Entry Error"
 K RAVCNT,RAVIEN,RAVLGTH,RAVPS
 Q
 ;
ESIG ; Added for COTS E-Sig capability
 ;
 I $D(RAERR) D XIT Q
 Q:"FA"'[^TMP("RARPT-REC",$J,RASUB,"RASTAT")!('$D(^("RAVERF")))!($D(^("RAESIG")))
 S RADFN=+$G(^TMP("RARPT-REC",$J,RASUB,"RADFN"))
 S RADTI=+$G(^TMP("RARPT-REC",$J,RASUB,"RADTI"))
 S RADIV=$P($G(^RADPT(RADFN,"DT",RADTI,0)),"^",3)
 Q:RADIV=""  ; exam has been deleted - will be rejected
 ; Check division parameters for ALLOW E-SIG ON COTS REPORT in file 79
 ; for the division that ordered this procedure.
 I $P(^RA(79,RADIV,.1),"^",27)["Y" D
 . S RAESIG=$$GET1^DIQ(200,RAVERF,20.2)
 . S:RAESIG]"" ^TMP("RARPT-REC",$J,RASUB,"RAESIG")=RAESIG
 . Q
 Q
