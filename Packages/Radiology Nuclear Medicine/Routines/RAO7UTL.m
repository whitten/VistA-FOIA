RAO7UTL ;HISC/GJC,SS-Utilities for HL7 messages. ;9/5/97  08:55
 ;;5.0;Radiology/Nuclear Medicine;**18**;Mar 16, 1998
 ;modified by SS JUN 19,2000 for P18
EN1 ; Entry point to define some basic HL7 variables
 N I S RAHLFS="|",RAECH="^~\&"
 S $P(RAHLFS(0),RAHLFS,51)=""
 F I=1:1:$L(RAECH) S RAECH(I)=$E(RAECH,I)
 Q
CMEDIA(RAIEN71) ;Called from RAO7MFN.  RAIEN71-> ien of proc. in file 71
 ;Passes back some combination of the following indicators, or
 ;NULL if none of the indicators apply to this procedure:
 ;  B = Barium used in procedure
 ;  C = Cholecystogram procedure
 ;  M = Contrast media used in procedure
 ;The procedure's AMIS category determines which indicators apply.
 Q:'+$O(^RAMIS(71,RAIEN71,2,0)) "" ; no data, null
 N I,J,K,L S I=0,K=""
 ; I-> ien of multiple, J-> 0 node of multiple, K-> data string (return)
 ; L-> iens of Rad/Nuc Med AMIS codes linked to Contrast Media
 F  S I=$O(^RAMIS(71,RAIEN71,2,I)) Q:I'>0  D
 . S J=+$G(^RAMIS(71,RAIEN71,2,I,0))
 . I J=9,(K'["B") S K=K_"B"
 . I J=11,(K'["C") S K=K_"C"
 . F L=4,10,11,12,14,15,16,17,18,19,20 D  Q:J=L
 .. I J=L,(K'["M") S K=K_"M"
 .. Q
 . Q
 Q K
MSH(X) ; Set up the 'MSH' segment.
 ; 'X' is passed in and identifies the message type.
 S:X']"" X="Message Type Error"
 Q "MSH"_RAHLFS_RAECH_RAHLFS_"RADIOLOGY"_RAHLFS_$P($G(^DIC(4,+$G(DUZ(2)),99)),"^")_$$STR(3)_$$HLDATE^HLFNC($$NOW^XLFDT(),"TS")_$$STR(2)_X
 ;
MSA(X,Y) ; Set up the 'MSA' segment. P18 
 ; 'X' is passed in and identifies the message ID.
 ; 'Y' is acknowledgement code
 S:X']"" X="Message ID Error"
 Q "MSA"_RAHLFS_Y_RAHLFS_$E(X,1,20)_$$STR(4)
MFI(X) ; Set up the 'MFI' segment
 S @(RAVAR_RACNT_")")="MFI"_RAHLFS_RAFNUM
 S @(RAVAR_RACNT_")")=@(RAVAR_RACNT_")")_RAECH(1)_RAFNAME_RAECH(1)
 S @(RAVAR_RACNT_")")=@(RAVAR_RACNT_")")_"99DD"_RAHLFS_RAHLFS_X ;P18
 S @(RAVAR_RACNT_")")=@(RAVAR_RACNT_")")_RAHLFS_RAHLFS_RAHLFS_"ER"
 X RAINCR ; increment counter
 Q
PID(Y) ; Create 'pid' segment
 Q "PID"_$$STR(3)_+$P(Y,"^")_$$STR(2)_$P($G(^DPT(+$P(Y,"^"),0)),"^")
 ;
PV1(Y) ; Create 'pv1' segment
 N DFN,RA,RARMBED,RAWARD,VAIP
 S DFN=+$P(Y,"^"),VAIP("D")=$P(Y,"^",21)
 S RA("PV1",2)="O",RA("PV1",3)=+$P(Y,"^",22)
 D IN5^VADPT S RAWARD=$G(VAIP(5)),RARMBED=$G(VAIP(6))
 I RAWARD]"" D
 . S RA("PV1",2)="I",RAWARD(44)=$P($G(^DIC(42,+RAWARD,44)),"^")
 . S RA("PV1",3)=+RAWARD(44)_U_$P(RARMBED,"^",2)
 . Q
 Q "PV1"_$$STR(2)_RA("PV1",2)_RAHLFS_RA("PV1",3)_$$STR(16) ;_"Visit #" was truncated for P18
 ;
PURGE K RAHLFS,RACNT,RAECH,RAFNAME,RAFNUM,RAINCR,RASUB,RATSTMP,RAVAR,RAXIT
PURGE1 ; kill only whole file update variables
 K RA71,RA713,RACMCODE,RACMNOR,RACOST,RACPT,RAIEN71,RAIMGAB,RAMFE,RAMULT
 K RAPHYAP,RAPRCTY,RAXT71
 Q
DIAG(X,Y,Z) ; Pass back an "A" if any Dx code has 'Yes' in the 'Generate
 ;         Abnormal Alert' field.
 N A,AAH,RA7003,RA783 S AAH=""
 S RA7003=$G(^RADPT(X,"DT",Y,"P",Z,0)),RA7003(13)=+$P(RA7003,"^",13)
 S RA783(0)=$G(^RA(78.3,RA7003(13),0))
 S RA783(4)=$$UP^XLFSTR($P(RA783(0),"^",4))
 S:RA783(4)="Y" AAH="A"
 Q:AAH]"" AAH
 S A=0 F  S A=$O(^RADPT(X,"DT",Y,"P",Z,"DX",A)) Q:A'>0  D  Q:AAH]""
 . S RA783=+$G(^RADPT(X,"DT",Y,"P",Z,"DX",A,0))
 . S RA783(0)=$G(^RA(78.3,RA783,0))
 . S RA783(4)=$$UP^XLFSTR($P(RA783(0),"^",4))
 . I RA783(4)="Y" S AAH="A"
 . Q
 Q AAH
PROCNDE(X) ; Check if the procedure has both an I-Type & Proc. Type
 ;         assigned. Pass back '1' if either the I-Type -or- Proc. Type
 ;         data is missing.  '0' if everything is ok.
 I $P(X(0),U,6)]"",($P(X(0),U,12)]"") Q 0
 Q 1
STR(X) ; Pass back a predetermined # of '|' or other field separator
 Q:$G(RAHLFS(0))']""!(+X=0) "" ; Quit if parent string i.e, 'RAHLFS(0)'
 ;                               does not exist or +X evaluates to null.
 ;
 S:X<0 X=$$ABS^XLFMTH(X) ;       If passed in negative, take absolute
 ;                               value.  Quit if 'X' is greater than the
 ;                               length of our parent string.
 ;
 S:X["." X=X\1 ;                 If a non-integer, remove mantissa.
 ;
 Q:X>($L(RAHLFS(0))) "" ;        If parameter greater than length of
 ;                               string, pass back null.
 Q $E(RAHLFS(0),1,X)
 ;
CHKUSR(RADUZ) ; Check user status to 'DC' an order.
 ; pass back '0' if non-active Rad/Nuc Med user
 ; pass back '1' if active Rad/Nuc Med user
 N RAINADT S RAINADT=+$P($G(^VA(200,RADUZ,"PS")),"^",4) ;inactivation DT
 Q $S('($D(RADUZ)#2):0,'$D(^VA(200,RADUZ,0)):0,'$D(^("RAC")):0,'RAINADT:1,'$D(DT):0,DT'>RAINADT:1,1:0)
 ;
ERR(RATXT,RAMSG,RAVAR) ; Call CPRS utility to log 'soft' errors.
 ; Input: RATXT-text description of the error
 ;        RAMSG-HL7 message array
 ;        RAVAR-variables to be saved off
 D EN^ORERR(RATXT,.RAMSG,.RAVAR)
 Q
 ;
MSG(RAPROTO,RAMSG) ; ship HL7 messages to CPRS from this entry point
 ; input: RAPROTO - protocol to execute
 ;          RAMSG - message (in HL7 format)
 D MSG^XQOR(RAPROTO,.RAMSG)
 Q
