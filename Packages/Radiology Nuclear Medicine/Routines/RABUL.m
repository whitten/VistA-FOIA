RABUL ;HISC/FPT,GJC AISC/DMK-Generate 'RAD/NUC MED REQUEST CANCELLED' or 'RAD/NUC MED REQUEST HELD' Bulletin ;9/9/94  09:53
 ;;5.0;Radiology/Nuclear Medicine;**2,15**;Mar 16, 1998
 ; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 ; The variables DA and RAOSTS must be defined.
 ; The variable DA must be greater than 0, and RAOSTS must
 ; equal 1 (cancelled) or 3 (held) for the RAD/NUC MED REQUEST CANCELLED
 ; or the  RAD/NUC MED REQUEST HELD bulletin to execute.
 ; Called from: ^DD(75.1,5,0) fifth piece
 ; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 ;                     ***** Variable List *****
 ; 'RADFN' -> IEN of the patient in the PATIENT file (2)
 ; 'RAFN1' -> internal format of a FM date/time data element
 ; 'RAFN2' -> FM data definition for RAFN1, used in XTERNAL^RAUTL5
 ; 'Z'     -> Zero node of the RADIOLOGY/NUCLEAR MEDICINE ORDERS
 ;            file (75.1)
 ; Format: Data to be fired;local var name;XMB array representation
 ; Patient ; RANAME ; XMB(1)      <---> Req. Physician ; RARPHY ; XMB(5)
 ; Patient SSN ; RASSN ; XMB(2)   <---> Req. Location ; RARLOC ; XMB(6)
 ; Procedure ; RAPNAM ; XMB(3)    <---> Reason ; RARCR ; XMB(7)
 ; Date Desired ; RADTDS ; XMB(4) <---> Current User ; RAUSER ; XMB(8)
 ;
EN(RAX) ; Pass in the report status (RAX)
 ; also called during request status edit.
 Q:+$G(DA)'>0
 Q:+$G(RAOSTS)'=1&(+$G(RAOSTS)'=3)
 N RADFN,RADTDS,RAFN1,RAFN2,RASSN,RAUSER,RAXMB,Z
 S RAXMB="RAD/NUC MED REQUEST",Z=$G(^RAO(75.1,DA,0))
 S (RADFN,RANAME)=+$P(Z,U)
 S RANAME=$S($D(^DPT(RANAME,0)):$P(^(0),U),1:"Unknown")
 S RASSN=$$SSN^RAUTL(),RAPNAM=+$P(Z,U,2)
 S RAPNAM=$S($D(^RAMIS(71,RAPNAM,0)):$P(^(0),U),1:"Unknown")
 S RARLOC=+$P(Z,U,22)
 S RARLOC=$S($D(^SC(RARLOC,0)):$P(^(0),U),1:"Unknown")
 S RAFN1=$P(Z,U,21),RAFN2=$P($G(^DD(75.1,21,0)),U,2)
 S RADTDS=$$XTERNAL^RAUTL5(RAFN1,RAFN2)
 S:RADTDS']"" RADTDS="Unknown" S RARPHY=+$P(Z,U,14)
 S RARPHY=$S($D(^VA(200,RARPHY,0)):$P(^(0),U),1:"Unknown")
 S RARCR=+$P(Z,U,10)
 S RARCR=$S($D(^RA(75.2,RARCR,0)):$P(^(0),U),1:"Unknown")
 S:RARCR="Unknown" RARCR=$S($P(Z,U,27)]"":$P(Z,U,27),1:"Unknown")
 S RAUSER=$S($D(^VA(200,DUZ,0)):$P(^(0),U),1:"Unknown")
 S XMB(1)=RANAME,XMB(2)=RASSN,XMB(3)=RAPNAM,XMB(4)=RADTDS
 S XMB(5)=RARPHY,XMB(6)=RARLOC,XMB(7)=RARCR,XMB(8)=RAUSER
 S XMB=RAXMB_$S(RAX=1:" CANCELLED",1:" HELD")
 D ^XMB:$D(^XMB(3.6,"B",XMB))
 K RANAME,RAPNAM,RARLOC,RARPHY,RARCR,XMB,XMB0,XMC0,XMDT,XMM,XMMG
 Q
OE3(DA) ; Trigger the Rad/Nuc Med Request Cancelled bulletin when
 ; the order is discontinued through CPRS (frontdoor).
 ; Input: DA-ien of the Rad/Nuc Med order record
 S RAOSTS=$P($G(^RAO(75.1,DA,0)),"^",5)
 I RAOSTS'=1 K RAOSTS Q  ; status must be discontinued
 D EN(RAOSTS) K RAOSTS
 Q
