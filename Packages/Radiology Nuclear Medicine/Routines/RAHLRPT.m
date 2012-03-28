RAHLRPT ;HISC/CAH AISC/SAW-Compiles HL7 'ORU' Message Type ; 4/26/01 10:40am
 ;;5.0;Radiology/Nuclear Medicine;**2,12,10,25**;Mar 16, 1998
EN ; Called from RA RPT and RA RPT 2.3 protocol entry action
 ; Input variables:
 ;   RADFN=file 2 IEN (DFN)
 ;   RADTI=file 70 Exam subrecord IEN (reverse date/time)
 ;   RACNI=file 70 Case subrecord IEN
 ;   RARPT=file 74 Report IEN
 ; Output variables:
 ;   HLA("HLS", array containing HL7 msg
 ;
 N RASET,RACN0
 S RASET=0
 S RACN0=^RADPT(RADFN,"DT",RADTI,"P",RACNI,0)
 I +$P(RACN0,U,25)=2 D  Q  ; printset
 .; loop through all cases in set and create message
 .S RASET=1
 .N RACNI,RAII S RAII=0
 .F  S RAII=$O(^RADPT(RADFN,"DT",RADTI,"P",RAII)) Q:RAII'>0  D
 .. Q:$P(^RADPT(RADFN,"DT",RADTI,"P",RAII,0),U,25)'=2
 .. S RACNI=RAII
 .. D NEW
NEW ; new variables
 S:$D(ZTQUEUED) ZTREQ="@" ; delete task from task global
 N DFN,DIWF,DIWL,DIWR,RACPT,RACPTNDE,RADTECN,RADTE0,RADTV,RAI,RAN,RAOBR4,RAPRCNDE,RAPROC,RAPROCIT,RAPRV,RARPT0,VADM,VAERR,X,X1,X2,XX2,Y,X0,OBR36
 N EID,HL,INT,HLQ,HLFS,HLECH,HLA,RAN K RAVADM
 D INIT^RAHLR ;initialize HL7 variables
 Q:$O(HL(""))=""  ;failed return from init^hlfnc2
 S DFN=RADFN D DEM^VADPT
 I VADM(1)']"" S HLP("ERRTEXT")="Invalid Patient Identifier" G EXIT
 S RAN=0
 S RAVADM(3)=$S($E(+VADM(3),6,7)="00":"",1:+VADM(3)) ; NOTE: Check
 ; for an inexact date of birth.  If inexact, pass null for DOB in
 ; the 'PID' segment.  Some COTS systems can't handle inexact DOB's.
 D SETUP,PID,OBR,OBXPRC,OBXIMP,OBXDIA,OBXRPT,OBXMOD,OBXTCM
EXIT ; set HL7 message type & return to RA RPT protocol
 S HL("MTN")="ORU"
 N HLEID,HLARYTYP,HLFORMAT,HLMTIEN,HLP
 S HLEID=EID,HLARYTYP="LM",HLFORMAT=1,HLMTIEN="",HLP("PRIORITY")="I"
 D GENERATE^HLMA(HLEID,HLARYTYP,HLFORMAT,.HLRESLT,HLMTIEN,.HLP)
 K RAVADM
 Q
 ;
OBR ;Compile 'OBR' Segment
 S RAOBR4=$P(RACPTNDE,U)_$E(HLECH)_$P(RACPTNDE,U,2)_$E(HLECH)_"C4"_$E(HLECH)_+RAPROC_$E(HLECH)_$P(RAPRCNDE,U)_$E(HLECH)_"99RAP"
 ; Replace above with following when Imaging can cope with ESC chars
 ; S RAOBR4=$P(RACPTNDE,U)_$E(HLECH)_$$ESCAPE^RAHLRU($P(RACPTNDE,U,2))_$E(HLECH)_"C4"_$E(HLECH)_+RAPROC_$E(HLECH)_$$ESCAPE^RAHLRU($P(RAPRCNDE,U))_$E(HLECH)_"99RAP"
 ; Have to use LOCAL code if Broad Procedure - no CPT code
 I $P(RAOBR4,$E(HLECH))=""!($P(RAOBR4,$E(HLECH),2)="") S $P(RAOBR4,$E(HLECH),1,3)=$P(RAOBR4,$E(HLECH),4,5)_$E(HLECH)_"LOCAL"
 S X1="OBR"_HLFS_HLFS_HLFS_RADTI_"-"_RACNI_$E(HLECH)_RADTECN_$E(HLECH)_"L"_HLFS_RAOBR4_HLFS_HLFS_HLFS_RADTE0_HLFS_HLQ_HLFS_HLQ_HLFS_HLFS_HLFS_HLFS_HLFS,Y=$$HLDATE^HLFNC($P(RARPT0,"^",6)) S X1=X1_Y_HLFS_HLFS
 S RAPRV=$$GET1^DIQ(200,+$P(RACN0,"^",14),.01)
 S Y=$$HLNAME^HLFNC(RAPRV) S X1=X1_$S(Y]"":+$P(RACN0,"^",14)_$E(HLECH)_Y,1:"")
 S $P(X1,HLFS,19)=$S($D(^DIC(42,+$P(RACN0,"^",6),0)):$P(^(0),"^"),$D(^SC(+$P(RACN0,"^",8),0)):$P(^(0),"^"),1:"Unknown")
 ; PCE 21 -> ien file #79.1~name of img loc~stn #~stn name
 N RACN00,RA20 S RACN00=$G(^RADPT(RADFN,"DT",RADTI,0))
 S RA20=+$G(^RA(79.1,+$P(RACN00,U,4),0))
 S $P(X1,HLFS,21)=$P(RACN00,"^",4)_$E(HLECH)_$P($G(^SC(RA20,0)),"^")_$E(HLECH)_$P(RACN00,"^",3)_$E(HLECH)_$P($G(^DIC(4,$P(RACN00,U,3),0)),"^")
 S $P(X1,HLFS,21)=$P(X1,HLFS,21)
 ; Replace above with following when Imaging can cope with ESC chars
 ; S $P(X1,HLFS,21)=$$ESCAPE^RAHLRU($P(X1,HLFS,21))
 ;
 S OBR36=9999999.9999-RADTI
 S $P(X1,HLFS,37)=$$FMTHL7^XLFDT(OBR36)
 ;
 S RADTV=HLDT1 I $P(RARPT0,"^",5)="V",$P(RARPT0,"^",7) K RADTV S RADTV=$$HLDATE^HLFNC($P(RARPT0,"^",7))
 S $P(X1,HLFS,23)=RADTV,$P(X1,HLFS,26)=$S($P(RARPT0,"^",5)="V":"F",1:"R")
 ;Principal Result Interpreter = Verifying Physician
 S $P(X1,HLFS,33)="" I $P(RARPT0,"^",9) D
 .S X2=$$GET1^DIQ(200,$P(RARPT0,"^",9),.01) Q:X2']""
 .S Y=$$HLNAME^HLFNC(X2) Q:Y']""
 .S $P(X1,HLFS,33)=$P(RARPT0,"^",9)_$E(HLECH)_Y
 ;Assistant Result Interpreter = Primary Interpreting Staff OR Resident
 S $P(X1,HLFS,34)="" I $P(RACN0,"^",15) D
 .S X2=$$GET1^DIQ(200,$P(RACN0,"^",15),.01) Q:X2']""
 .S Y=$$HLNAME^HLFNC(X2) Q:Y']""
 .S $P(X1,HLFS,34)=$P(RACN0,"^",15)_$E(HLECH)_Y
 I $P(RACN0,"^",12) D
 .S X2=$$GET1^DIQ(200,$P(RACN0,"^",12),.01) Q:X2']""
 .S Y=$$HLNAME^HLFNC(X2) Q:Y']""
 .S $P(X1,HLFS,34)=$P(RACN0,"^",12)_$E(HLECH)_Y
 ;Technician = Technologist
 S $P(X1,HLFS,35)="" I $O(^RADPT(RADFN,"DT",RADTI,"P",RACNI,"TC",0)) D
 .S X2=$O(^RADPT(RADFN,"DT",RADTI,"P",RACNI,"TC",0)) I X2']"" Q
 .S X2=$G(^RADPT(RADFN,"DT",RADTI,"P",RACNI,"TC",X2,0)) I X2']"" Q
 .S XX2=$$GET1^DIQ(200,X2,.01) Q:XX2']""
 .S Y=$$HLNAME^HLFNC(XX2) I Y']"" Q
 .S $P(X1,HLFS,35)=X2_$E(HLECH)_Y
 ;Transcriptionist
 S $P(X1,HLFS,36)="" I $G(^RARPT(RARPT,"T")) D
 .S X2=$$GET1^DIQ(200,^RARPT(RARPT,"T"),.01) I X2']"" Q
 .S Y=$$HLNAME^HLFNC(X2) I Y']"" Q
 .S $P(X1,HLFS,36)=^RARPT(RARPT,"T")_$E(HLECH)_Y
 ;
 ; if long str, break so 2nd str begins with separator to avoid abend
 I $L(X1)>245 N RAPART F RAPART=5:1:18 S RAPART(1)=$P(X1,HLFS,1,RAPART),RAPART(2)=$P(X1,HLFS,RAPART+1,99) Q:$L(RAPART(1))<245&($L(RAPART(2))<245)&($P(RAPART(2),HLFS)="")
 I $D(RAPART) K:RAPART=18 RAPART ;if RAPART reaches 18, then something's wrong, so kill RAPART to allow abend due "string too long"
 S RAN=RAN+1
 I $D(RAPART) S HLA("HLS",RAN)=$P(RAPART(1),HLFS)_HLFS,HLA("HLS",RAN,1)=$P(RAPART(1),HLFS,2,99)_HLFS,HLA("HLS",RAN,2)=RAPART(2) K RAPART Q
 S HLA("HLS",RAN)=X1
 Q
OBXDIA ;Compile 'OBX' Segment for Diagnostic Code
 S RAI=$P($G(^RA(78.3,+$P(RACN0,"^",13),0)),"^") I RAI]"" D
 .S RAN=RAN+1
 .I $$PATCH^XPDUTL("MAG*2.5*1")!(+$$VERSION^XPDUTL("MAG")>2.5) D
 ..S HLA("HLS",RAN)="OBX"_HLFS_HLFS_"CE"_HLFS_"D"_$E(HLECH)_"DIAGNOSTIC CODE"_$E(HLECH)_"L"_HLFS_HLFS_+$P(RACN0,"^",13)_$E(HLECH)_RAI_$E(HLECH)_"L"
 ..; Replace above with following when Imaging can cope with ESC chars
 ..; S HLA("HLS",RAN)="OBX"_HLFS_HLFS_"CE"_HLFS_"D"_$E(HLECH)_"DIAGNOSTIC CODE"_$E(HLECH)_"L"_HLFS_HLFS_+$P(RACN0,"^",13)_$E(HLECH)_$$ESCAPE^RAHLRU(RAI)_$E(HLECH)_"L"
 .E  D
 ..S HLA("HLS",RAN)="OBX"_HLFS_HLFS_"ST"_HLFS_"D"_$E(HLECH)_"DIAGNOSTIC CODE"_$E(HLECH)_"L"_HLFS_HLFS_RAI
 .D OBX11^RAHLRU
 Q:'$O(^RADPT(RADFN,"DT",RADTI,"P",RACNI,"DX",0))  ;any secondary dx
 S X2=0
OBXDIA2 S X2=$O(^RADPT(RADFN,"DT",RADTI,"P",RACNI,"DX",X2)) Q:'X2
 S Y=+^(X2,0),X=$P($G(^RA(78.3,+Y,0)),U)
 I X]"" D
 .S RAN=RAN+1
 .I $$PATCH^XPDUTL("MAG*2.5*1")!(+$$VERSION^XPDUTL("MAG")>2.5) D
 ..S HLA("HLS",RAN)="OBX"_HLFS_HLFS_"CE"_HLFS_"D"_$E(HLECH)_"DIAGNOSTIC CODE"_$E(HLECH)_"L"_HLFS_HLFS_Y_$E(HLECH)_X_$E(HLECH)_"L"
 ..; Replace above with following when Imaging can cope with ESC chars
 ..; S HLA("HLS",RAN)="OBX"_HLFS_HLFS_"CE"_HLFS_"D"_$E(HLECH)_"DIAGNOSTIC CODE"_$E(HLECH)_"L"_HLFS_HLFS_Y_$E(HLECH)_$$ESCAPE^RAHLRU(X)_$E(HLECH)_"L"
 .E  D
 ..S HLA("HLS",RAN)="OBX"_HLFS_HLFS_"ST"_HLFS_"D"_$E(HLECH)_"DIAGNOSTIC CODE"_$E(HLECH)_"L"_HLFS_HLFS_X
 .D OBX11^RAHLRU
 G OBXDIA2
 ;
OBXIMP ;Compile 'OBX' segment for Impression
 I '$O(^RARPT(RARPT,"I",0)) S RAN=RAN+1,HLA("HLS",RAN)="OBX"_HLFS_HLFS_"TX"_HLFS_"I"_$E(HLECH)_"IMPRESSION"_$E(HLECH)_"L"_HLFS_HLFS_"None Entered" D OBX11^RAHLRU Q
 K ^UTILITY($J,"W") S DIWF="",DIWR=80,DIWL=1 F RAI=0:0 S RAI=$O(^RARPT(RARPT,"I",RAI)) Q:'RAI  I $D(^(RAI,0)) S X=^(0) D ^DIWP
 F RAI=0:0 S RAI=$O(^UTILITY($J,"W",DIWL,RAI)) Q:'RAI  I $D(^(RAI,0)) S RAN=RAN+1,HLA("HLS",RAN)="OBX"_HLFS_HLFS_"TX"_HLFS_"I"_$E(HLECH)_"IMPRESSION"_$E(HLECH)_"L"_HLFS_HLFS_^(0) D OBX11^RAHLRU
 Q
OBXMOD ;Compile 'OBX' Segment for Modifiers
 S RAN=RAN+1 D OBXMOD^RAHLRU
 Q
OBXPRC ;Compile 'OBX' Segment for Procedure
 S RAN=RAN+1 D OBXPRC^RAHLRU
 Q
OBXTCM ; Compile 'OBX' Segment for Tech Comments
 D OBXTCM^RAHLRU
 Q
OBXRPT ;Compile 'OBX' Segment for Radiology Report Text
 I '$O(^RARPT(RARPT,"R",0)) S RAN=RAN+1,HLA("HLS",RAN)="OBX"_HLFS_HLFS_"TX"_HLFS_"R"_$E(HLECH)_"REPORT"_$E(HLECH)_"L"_HLFS_HLFS_"None Entered" D OBX11^RAHLRU Q
 K ^UTILITY($J,"W") S DIWF="",DIWR=80,DIWL=1 F RAI=0:0 S RAI=$O(^RARPT(RARPT,"R",RAI)) Q:'RAI  I $D(^(RAI,0)) S X=^(0) D ^DIWP
 F RAI=0:0 S RAI=$O(^UTILITY($J,"W",DIWL,RAI)) Q:'RAI  I $D(^(RAI,0)) S RAN=RAN+1,HLA("HLS",RAN)="OBX"_HLFS_HLFS_"TX"_HLFS_"R"_$E(HLECH)_"REPORT"_$E(HLECH)_"L"_HLFS_HLFS_^(0) D OBX11^RAHLRU
 ; Replace above with following when Imaging can cope with ESC chars
 ; F RAI=0:0 S RAI=$O(^UTILITY($J,"W",DIWL,RAI)) Q:'RAI  I $D(^(RAI,0)) S RAN=RAN+1,HLA("HLS",RAN)="OBX"_HLFS_HLFS_"TX"_HLFS_"R"_$E(HLECH)_"REPORT"_$E(HLECH)_"L"_HLFS_HLFS_$$ESCAPE^RAHLRU(^(0)) D OBX11^RAHLRU
 Q
PID ;Compile 'PID' Segment
 I HL("VER")']"2.2" D
 .S X1="",X1="PID"_HLFS_HLFS_$G(VA("PID"))_HLFS_Y_HLFS_HLFS S X=VADM(1),Y=$$HLNAME^HLFNC(X) S X1=X1_Y_HLFS_HLFS
 .S X=RAVADM(3),Y=$$HLDATE^HLFNC(X) S X1=X1_Y_HLFS_$S(VADM(5)]"":$S("MF"[$P(VADM(5),"^"):$P(VADM(5),"^"),1:"O"),1:"U") S:$P(VADM(2),"^")]"" $P(X1,HLFS,20)=$P(VADM(2),"^") S RAN=RAN+1,HLA("HLS",RAN)=X1
 I HL("VER")]"2.2" S RAN=RAN+1,HLA("HLS",RAN)=$$EN^VAFHLPID(DFN,"2,3,5,7,8,19,20")
 Q
SETUP ; Setup basic examination information
 S:RASET RACN0=^RADPT(RADFN,"DT",RADTI,"P",RACNI,0)
 S RADTE0=9999999.9999-RADTI,RADTECN=$E(RADTE0,4,7)_$E(RADTE0,2,3)_"-"_+RACN0,RARPT0=^RARPT(RARPT,0)
 S RAPROC=+$P(RACN0,U,2),RAPROCIT=+$P($G(^RAMIS(71,RAPROC,0)),U,12),RAPROCIT=$P(^RA(79.2,RAPROCIT,0),U,1)
 S RAPRCNDE=$G(^RAMIS(71,+RAPROC,0)),RACPT=+$P(RAPRCNDE,U,9)
 S RACPTNDE=$$NAMCODE^RACPTMSC(RACPT,DT)
 S Y=$$HLDATE^HLFNC(RADTE0) S RADTE0=$S(Y:Y,1:HLQ),Y=$$M11^HLFNC(RADFN)
 Q
