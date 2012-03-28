TIURTITL ; SLC/JER - Review Documents by TITLE ;2/26/01
 ;;1.0;TEXT INTEGRATION UTILITIES;**58,100**;Jun 20, 1997
 ;12/6/00 split, moving GATHER,PUTLIST,ADDELMNT, etc to new rtn TIURTIT1
 ;IHS/ITSC/LJF 02/27/2003  changed test for DIROUT to test for DIRUT
 ;
MAKELIST(TIUCLASS) ; Get Search Criteria
 N TIUI,SCREEN,STATUS,TIUTYP,TIUSTAT,TIUEDFLT,TIUDCL,TIUQUIT
 N STATWORD,STATIFN,NOWFLAG
 D INITRR^TIULRR(0)
STATUS S STATUS=$$SELSTAT^TIULA(.TIUSTAT,"F","ALL")
 I +STATUS<0 S VALMQUIT=1 Q
 S TIUI=0
 F  S TIUI=$O(TIUSTAT(TIUI)) Q:'TIUI  D
 . S STATIFN=$O(^TIU(8925.6,"B",$$UPPER^TIULS($P(TIUSTAT(TIUI),U,3)),0))
 . Q:'STATIFN
 . S STATUS("IFNS")=$G(STATUS("IFNS"))_STATIFN_";"
 S TIUI=1,STATWORD=$$UPPER^TIULS($P(TIUSTAT(1),U,3))
 I +$G(TIUSTAT(4))'>0 F  S TIUI=$O(TIUSTAT(TIUI)) Q:+TIUI'>0  D
 . S STATWORD=STATWORD_$S(TIUI=+TIUSTAT(1):" & ",1:", ")_$$UPPER^TIULS($P(TIUSTAT(TIUI),U,3))
 I +$G(TIUSTAT(4))>0 S STATWORD=$S($P(TIUSTAT(4),U,4)="ALL":"ALL",1:STATWORD_", OTHER")
 S STATUS("WORDS")=STATWORD
DOCTYPE ; Select Document Type(s)
 N TIUDCL
 D TITLPICK^TIULA3(.TIUTYP,TIUCLASS)
 I +$D(TIUQUIT) S VALMQUIT=1 Q
 I +$G(TIUTYP)'>0,'$D(TIUQUIK) G STATUS
SCREEN ;
 N TIUNAME
 S TIUNAME=$P($G(^VA(200,+DUZ,0)),U)
 S SCREEN=1,SCREEN(1)="ALL^ANY"
 D CHECKADD(.TIUTYP)
ERLY S TIUEDFLT=$S(TIUCLASS=3:"T-2",TIUCLASS=244:"T-30",1:"T-7")
 S TIUEDT=$S($D(TIUQUIK):1,1:$P($$EDATE^TIULA("Reference","",TIUEDFLT),U))
 ;I +$G(DIROUT) S VALMQUIT=1 Q    ;IHS/ITSC/LJF 02/27/2003
 I +$G(DIRUT) S VALMQUIT=1 Q      ;IHS/ITSC/LJF 02/27/2003 changed logic to check more cases
 I TIUEDT'>0 G SCREEN
 S TIULDT=$S($D(TIUQUIK):9999999,1:$P($$LDATE^TIULA("Reference"),U))
 ;I +$G(DIROUT) S VALMQUIT=1 Q    ;IHS/ITSC/LJF 02/27/2003
 I +$G(DIRUT) S VALMQUIT=1 Q      ;IHS/ITSC/LJF 02/27/2003 changed logic to check more cases
 I TIULDT'>0 G ERLY
 I TIUEDT>TIULDT D SWAP^TIUR(.TIUEDT,.TIULDT)
 I $L(TIULDT,".")=1 D EXPRANGE^TIUR(.TIUEDT,.TIULDT)
 ; -- Reset late date to NOW on rebuild:
 S NOWFLAG=$S(TIULDT-$$NOW^XLFDT<.0001:1,1:0)
 I '$G(TIURBLD) W !,"Searching for the documents."
 D BUILD(TIUCLASS,.STATUS,.TIUTYP,.SCREEN,TIUEDT,TIULDT,NOWFLAG)
 ; -- If changed view while attaching ID note, update video for note: --
 I $G(TIUGLINK) D RESTOREG^TIULM(.TIUGLINK)
 Q
CHECKADD(TYPES) ; Checks whether Addendum is included in the list of types
 N TIUI,HIT S (TIUI,HIT)=0
 F  S TIUI=$O(TYPES(TIUI)) Q:+TIUI'>0!+HIT  I $$UP^XLFSTR(TYPES(TIUI))["ADDENDUM" S HIT=1
 I +HIT'>0 S TYPES(TYPES+1)=+TYPES(TYPES)+1_U_"81^Addendum^NOT PICKED",TYPES=TYPES+1
 Q
BUILD(TIUCLASS,STATUS,TYPES,SCREEN,EARLY,LATE,NOWFLAG) ; Build List
 N TIUCNT,TIUDT,TIUI,TIUJ,TIUK,TIUQ,TIUIFN,TIUREC
 N XREF,TIUS,TIUPREF
 S TIUPREF=$$PERSPRF^TIULE(DUZ),(TIUK,VALMCNT)=0
 K ^TMP("TIUR",$J),^TMP("TIURIDX",$J),^TMP("TIUI",$J),^TMP("TIUTYP",$J)
 ; If user entered NOW at first build, update NOW for rebuild;
 ; Save data in ^TMP("TIURIDX",$J,0) for rebuild:
 I $G(TIURBLD),$G(NOWFLAG) S LATE=$$NOW^XLFDT
 S ^TMP("TIURIDX",$J,0)=+EARLY_U_+LATE_U_$G(STATUS("IFNS"))_U_NOWFLAG
 ; Save docmt types in ^TMP("TIUTYP",$J) for rebuild:
 M ^TMP("TIUTYP",$J)=TYPES
 S ^TMP("TIUR",$J,"RTN")="TIURTITL"
 I '$D(TIUPRM0)!'$D(TIUPRM0) D SETPARM^TIULE
 S EARLY=9999999-+$G(EARLY),LATE=9999999-$S(+$G(LATE):+$G(LATE),1:3333333)
 F  S TIUK=$O(SCREEN(TIUK)) Q:TIUK'>0  D
 . S XREF=$P(SCREEN(TIUK),U)
 . I XREF'="ASUB" D
 . . S TIUI=$S(XREF'="APRB":$P(SCREEN(TIUK),U,2),1:$$UPPER^TIULS($P(SCREEN(TIUK),U,3)))
 . . D GATHER^TIURTIT1(TIUI,TIUPREF,TIUCLASS,STATUS("IFNS"),EARLY,LATE,XREF)
 . I XREF="ASUB" D
 . . S TIUI=$O(^TIU(8925,XREF,$P(SCREEN(TIUK),U,2)),-1)
 . . F  S TIUI=$O(^TIU(8925,XREF,TIUI)) Q:TIUI=""!(TIUI'[$P(SCREEN(TIUK),U,2))  D GATHER^TIURTIT1(TIUI,TIUPREF,TIUCLASS,STATUS("IFNS"),EARLY,LATE,XREF)
 D PUTLIST^TIURTIT1(TIUPREF,TIUCLASS,.STATUS,.SCREEN)
 Q
CLEAN ; Clean up your mess!
 K ^TMP("TIUR",$J),^TMP("TIURIDX",$J) D CLEAN^VALM10,KILLRR^TIULRR
 K VALMY,^TMP("TIUTYP",$J)
 Q
 ;
RBLD ; Rebuild list after actions
 N TIUEXP,TIUR0,TIURIDX0,TIUSCRN,TMP,TIUEDT,TIULDT,TIUSTAT
 N TIURBLD,TIUI,TIUCLASS,TIUTYP,NOWFLAG
 S TIURBLD=1
 D FIXLSTNW^TIULM ;restore video for elements added to end of list
 I +$O(^TMP("TIUR",$J,"EXPAND",0)) D
 . M TIUEXP=^TMP("TIUR",$J,"EXPAND")
 M TIUTYP=^TMP("TIUTYP",$J)
 S TIUR0=^TMP("TIUR",$J,0),TIURIDX0=^TMP("TIURIDX",$J,0)
 S TIUSCRN=$P(TIUR0,U,3,99),TIUCLASS=^TMP("TIUR",$J,"CLASS")
 S TIUI=1
 F  S TMP=$P(TIUSCRN,";",TIUI) Q:TMP=""  D
 . S TIUSCRN(TIUI)=TMP,TIUI=TIUI+1
 S TIUSCRN=$L(TIUSCRN,";")
 S STATUS("WORDS")=$P(TIUR0,U,2)
 S STATUS("IFNS")=$P(TIURIDX0,U,3)
 S TIUEDT=$P(TIURIDX0,U),TIULDT=$P(TIURIDX0,U,2),NOWFLAG=+$P(TIURIDX0,U,4)
 D BUILD(TIUCLASS,.STATUS,.TIUTYP,.TIUSCRN,TIUEDT,TIULDT,NOWFLAG)
 ; Reexpand previously expanded items:
 D RELOAD^TIUROR1(.TIUEXP)
 D BREATHE^TIUROR1(1)
 Q
