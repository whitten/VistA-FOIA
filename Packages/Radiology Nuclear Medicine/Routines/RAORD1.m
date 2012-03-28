RAORD1 ;HISC/CAH,FPT,GJC AISC/RMO-Request An Exam ;3/13/98  12:16
 ;;5.0;Radiology/Nuclear Medicine;**10**;Mar 16, 1998
 S RAPKG="" N RAPTLKUP,RAGMTS
 G ADDORD:$D(RAVSTFLG)&($D(RALIFN))&($D(RAPIFN))
 I '$D(RAREGFLG),'$D(RAVSTFLG) N RAPTLOCK K RAWARD D  G:'RAPTLKUP Q
PAT . S DIC="^DPT(",DIC(0)="AEMQ" W ! D ^DIC K DIC
 . I Y<0 S RAPTLKUP=0 Q
 . I $$ORVR^RAORDU()'<3 D  G:'RAPTLOCK PAT
 .. S RAPTLOCK=$$LK^RAUTL19(+Y_";DPT(")
 .. Q
 . S (DFN,RADFN)=+Y,(VA200,RAPTLKUP)=1
 . W ! D IN5^VADPT S:VAIP(1) RAWARD=$P(VAIP(5),"^",2)
 . Q
PL S DIC("A")="Patient Location: ",DIC("B")=$S($D(RAWARD):RAWARD,1:""),DIC="^SC(",DIC(0)="AEMQ",DIC("S")="I $$SCREEN^RAORD1A()"
 D ^DIC K DIC K:'$D(RAREGFLG) RAWARD G Q:Y<0 S RALIFN=+Y
 ;S DIC("A")="Person Requesting Order: ",DIC("S")="I $S('$D(^(""PS"")):1,'$P(^(""PS""),U,4):1,DT'>$P(^(""PS""),U,4):1,1:0)&($D(^XUSEC(""PROVIDER"",Y)))",DIC="^VA(200,",DIC(0)="AEMQ" S:$D(^XUSEC("PROVIDER",DUZ)) DIC("B")=$P(^VA(200,DUZ,0),"^",1)
 S DIC("A")="Person Requesting Order: "  ;IHS/ITSC/CLS 02/03/2004 screen and display default based on authorized to write med orders
 S DIC("S")="I $S('$D(^(""PS"")):1,'$P(^(""PS""),U,4):1,DT'>$P(^(""PS""),U,4):1,1:0)&($P($G(^VA(200,Y,""PS"")),""^""))"  ;IHS/ITSC/CLS 02/03/2004
 S DIC="^VA(200,",DIC(0)="AEMQ" S:$G(^VA(200,DUZ,"PS"),"^") DIC("B")=$P(^VA(200,DUZ,0),"^",1)  ;IHS/ITSC/CLS 02/03/2004
 D ^DIC K DIC G Q:Y<0 S RAPIFN=+Y K DD,DO,VA200,VAERR,VAIP G ADDORD:$D(RAVSTFLG)
 ;
ENADD ;OE/RR Entry Point for the ACTION Option
 K ORSTOP,ORTO,ORCOST,ORPURG
 I '$D(RAPKG) G Q:'$D(ORVP)!('$D(ORL))!('$D(ORNP)) S (DFN,RADFN)=+ORVP,RALIFN=+ORL,RAPIFN=$S(+ORNP:+ORNP,$D(RAPIFN):RAPIFN,1:+ORNP),RAFOERR=""
 ; RAFOERR is used as a flag to track when a user enters this option
 ; from OE/RR (frontdoor).  If this variable exists when a request is
 ; being printed, exam information is omitted from the request.
 S RANME=^DPT(RADFN,0),RASEX=$P(RANME,"^",2),RANME=$P(RANME,"^") D EXAM^RADEM1:'$D(RAREGFLG)&($D(RAPKG)) I '$D(RAREGFLG) S VA200=1 D IN5^VADPT S:VAIP(1) RAWARD=$P(VAIP(5),"^",2)
 D SAVE ; save off original value of RAMDV!
 S RAL0=$S($D(^SC(RALIFN,0)):^(0),1:0)
 S RADIV=+$$SITE^VASITE(DT,+$P(RAL0,"^",15)) S:RADIV<0 RADIV=0
 S RADIV=$S($D(^RA(79,RADIV,0)):RADIV,1:$O(^RA(79,0)))
 S RAMDV=$TR($G(^RA(79,+RADIV,.1)),"YyNn","1100")
 S RACAT=$S($D(RACAT):RACAT,$D(RAWARD):"INPATIENT",$P(RAL0,"^",2)="PERSONNEL HEALTH":"EMPLOYEE",'$D(^RADPT(RADFN,0)):"OUTPATIENT",$P(^(0),"^",4)]"":$P($P(^DD(70,.04,0),$P(^RADPT(RADFN,0),"^",4)_":",2),";"),1:"OUTPATIENT")
 I "IO"[$E(RACAT,1) D
 . S RASTRNG=$$MATCH^RAORD1A(RACAT,RALIFN)
 . ;if necessary, change category of exam to match type of requesting
 . ;location and display msg to user
 . S RACAT=$P(RASTRNG,"^"),RAWARD=$P(RASTRNG,"^",2)
 . Q
 K:$D(RAWARD)&($E(RACAT,1)="O") RAWARD
 K RASTRNG
 ; clear clin hist if:
 ;   rad backdoor, or
 ;   oe/rr's first order (quick or not)
 I $D(RAPKG) K ^TMP($J,"RAWP")
 I '$D(RAPKG),$G(XQORS)>1,$G(^TMP("XQORS",$J,XQORS-1,"ITM"))=1 K ^TMP($J,"RAWP")
 ;
ADDORD I $D(RADR1) D ALLERGY,CREATE1 G Q
 ; Set flag variable 'RASTOP' to track if procedure messages (if any)
 ; have been displayed.  Value altered in EN2+1^RAPRI & DISP+12^RAORDU1.
 D DISP^RAPRI G:RAIMGTYI'>0 Q
ADDORD1 W !,"Select Procedure",$S(RACNT:" (1-"_RACNT_") ",1:" "),"or enter '?' for help: "
 R RARX:DTIME
 S:'$T RARX="^" G Q:RARX=""!($E(RARX)="^")
 S:RARX=" " RARX=$S($D(RASX):RASX,1:RARX)
 I $E(RARX)="?"!(RARX=0)!(RARX=" ")!(RARX?.E1N1"-"1N.E)!(RARX?.E1".".E) D HELP^RAPRI G Q:Y'=1 D DISP1^RAPRI G ADDORD1
 S RAEXMUL=1 K RAHSMULT
 F RAJ=1:1 S X=$P(RARX,",",RAJ) Q:X=""  S RASTOP=0 W !!!,"Processing procedure: ",$S(+X&(+X'>RACNT):$P($G(RAPRC(X)),"^"),$E(X)'="`":X,1:"") D LOOKUP^RAPRI Q:$D(RAOUT)  S:RAPRI>0 RASX="`"_RAPRI D:RAPRI>0 ALLERGY,CREATE Q:$D(RAOUT)  K RAPRI
 I $D(RAREASK),'$D(RAOUT) K RAREASK D DISP1^RAPRI G ADDORD1
Q ; Kill, unlock if locked, and quit
 D KILL^RAORD
 D SAVE ; reset RAMDV to its original value!
 I $$ORVR^RAORDU()'<3,(+$G(RAPTLOCK)),(+$G(RADFN)) D
 . D ULK^RAUTL19(RADFN_";DPT(")
 K:'$D(RAREGFLG)&('$D(RAVSTFLG)) RACAT,RADFN,RANME,RAWARD
 I '$D(RAPKG) K RAMDIV,RAMDV,RAMLC
 I $D(RAPKG) K ORIFN,ORIT,ORL,ORNP,ORNS,ORPCL,ORPK,ORPV,ORPURG,ORSTS,ORTX,ORVP,RAPKG
 K RAHSMULT,RAPOP,RAIMAG,RAREQLOC
 K C,DI,DIG,DIH,DISYS,DIU,DIW,DIWF,DIWL,DIWR,DIWT,DN,I,ORCHART,POP,RAMDVZZ,RASCI,RASERIES
 Q
CREATE S RACT=0 D MODS Q:$D(RAOUT)
CREATE1 S RAWHEN=$$DESDT^RAUTL12(RAPRI) Q:$D(RAOUT)
 ; Ask pregnant if age is between 12 & 55.  Ask once for mult requests
 ; RASKPREG is the variable used to track if the pregnant prompt has
 ; been asked.  Ask only once for multiple requests.
 S:'$D(RASKPREG) RAPREG=$$PREG^RAORD1A(RADFN,$G(DT)),RASKPREG="" Q:$D(RAOUT)
 D CH^RAUTL5 Q:$D(RAOUT)  I '$D(RADR1) D DISP^RAORDU1 Q:$D(RAOUT)
 S X=RADFN,DIC="^RAO(75.1,",DIC(0)="L",DLAYGO=75.1 D FILE^DICN K DIC Q:Y<0  S RAOIFN=+Y K DLAYGO
 I $D(RAREGFLG)!($D(RAVSTFLG)) S RANUM=$S('$D(RANUM):1,1:RANUM+1),RAORDS(RANUM)=RAOIFN
 I $D(^RA(79,+RADIV,.1)),$P(^(.1),"^",21)="y" S RALOCFLG=""
 W ! S DA=RAOIFN,DIE="^RAO(75.1,",DIE("NO^")="OUTOK",DR=$S($D(RADR1):"[RA QUICK EXAM ORDER]",$D(RADR2):"[RA ORDER EXAM]",$D(RAEXMUL)&($D(RAFIN1)):"[RA QUICK EXAM ORDER]",1:"[RA ORDER EXAM]") D ^DIE K DIE("NO^"),DE,DQ,DIE,DR,RADR1,RADR2
 I $D(RAFIN),$D(^RAO(75.1,RAOIFN,0)) S RAORD0=^(0) D SETORD^RAORDU D OERR^RAORDU:'$D(RAPKG) D ^RAORDQ:$D(RAPKG) K RAORD0
 I '$D(RAFIN) W !?3,*7,"Request not complete. Must Delete..." S DA=RAOIFN,DIK="^RAO(75.1," D ^DIK W "...deletion complete!" I $D(RAREGFLG)!($D(RAVSTFLG)) K RAORDS(RANUM)
 I '$D(RAFIN),('$D(^RAO(75.1,RAOIFN,0))#2) Q  ; record deleted!
 K RAFIN
 ; check if the 'stat' or 'urgent' alert is to be sent.
 N RALOC,RAORD0
 S RAORD0=$G(^RAO(75.1,RAOIFN,0)),RALOC=+$P(RAORD0,"^",20)
 Q:'RALOC  ; if no 'SUBMIT TO' location, can't send stat/urgent alerts
 I $P(RAORD0,"^",6)=1!(($P(RAORD0,"^",6)=2)&($P(^RA(79.1,RALOC,0),"^",20)="Y")) D
 . ; If 6th piece of RAORD0=1 *stat*, =2 *urgent*
 . Q:$$ORVR^RAORDU()<3
 . ; needs OE/RR 3.0 or greater for stat/urgent alerts to fire
 . D OENO^RAUTL19(RAOIFN)
 . Q
 Q
 ;
MODS ;RAPRI= Procedure IEN, RAIMAG=Imaging Type for the procedure.
 ;Edited 4/19/94, Type of Imaging is now a multiple in file 71.2. CEW
 S RAIMAG=+$$ITYPE^RASITE(RAPRI),DIC(0)="AEQMZ",DIC="^RAMIS(71.2,",DIC("A")="Select "_$P($G(^DIC(71.2,0)),"^")_": "
 S DIC("S")="I +$D(^RAMIS(71.2,""AB"",RAIMAG,+Y)),$S('$G(RASERIES):1,$P(^RAMIS(71.2,+Y,0),U,2)="""":1,1:0),$$INIMOD^RAORD1A($P($G(^RAMIS(71.2,+Y,0)),""^""))"
 D ^DIC K DIC,RAIMAG S:$D(DTOUT)!($D(DUOUT)) RAOUT=1 Q:$D(RAOUT)!(X="^")!(X="")  I Y<1 W *7,"  ??" G MODS
 S RACT=RACT+1,RAMOD(RACT)=$P(Y,"^",2) G MODS
 Q
ALLERGY ; If patient has had a previous contrast media allergic reaction
 ; check procedure RAPRI for amis codes 4,10-12,14-22 & write warning
 I $$ORCHK^GMRAOR(RADFN,"CM") D
 . N AMIS
 . F AMIS=4,10:1:12,14:1:22 I $O(^RAMIS(71,"AC",AMIS,RAPRI,0)) D  Q
 .. W $C(7),!!?3,"*** WARNING: This patient has had a previous contrast media reaction ***",!,$C(7)
 .. Q
 . Q
 Q
SAVE ; Save original value of RAMDV before it is altered in the ENADD sub-
 ; routine.  This code will also reset RAMDV to the sign-on value.
 Q:'$D(RAPKG)  ; entered through OE/RR (RAMDV will not be set)
 Q:'$D(RAMDV)&('$D(RAMDVZZ))  ;entered through 'Request an Exam' option used stand-alone outside of Rad/NM pkg
 I '$D(RAMDVZZ) S RAMDVZZ=RAMDV
 E  S RAMDV=RAMDVZZ
 Q
