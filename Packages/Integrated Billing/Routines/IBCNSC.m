IBCNSC	;ALB/NLR - INSURANCE COMPANY EDIT ; 12-MAR-1993
	;;Version 2.0 ; INTEGRATED BILLING ;; 21-MAR-94
	;;Per VHA Directive 10-93-142, this routine should not be modified.
	;
EN	; -- main entry point for IBCNS INSURANCE COMPANY, IBCNS VIEW INS CO
	K VALMQUIT,VALMEVL,XQORS,^TMP("XQORS",$J),IBCNS
	S IBCHANGE="OKAY"
	I '$G(IBVIEW) D EN^VALM("IBCNS INSURANCE COMPANY") G ENQ
	D EN^VALM("IBCNS VIEW INS CO")
ENQ	Q
	;
HDR	; -- header code
	S VALMHDR(1)="Insurance Company Information for: "_$E($P(^DIC(36,IBCNS,0),"^"),1,30)
	S VALMHDR(2)="Type of Company: "_$E($P($G(^IBE(355.2,+$P($G(^DIC(36,+IBCNS,0)),"^",13),0)),"^"),1,20)_"                     Currently "_$S(+($P($G(^DIC(36,+IBCNS,0)),"^",5)):"Inactive",1:"Active")
	Q
	;
INIT	; -- init variables and list array
	K VALMQUIT
	S VALMCNT=0,VALMBG=1
	I '$D(IBCNS) D INSCO Q:$D(VALMQUIT)
	D BLD,HDR
	Q
BLD	; -- list builder
	K ^TMP("IBCNSC",$J)
	D KILL^VALM10()
	F I=1:1:65 D BLANK(.I)
	S VALMCNT=61
	D PARAM^IBCNSC01,MAIN^IBCNSC01,CLAIMS1^IBCNSC0,CLAIMS2^IBCNSC0,PRESCR^IBCNSC1,APPEALS,INQUIRY,REMARKS^IBCNSC01,SYN^IBCNSC01
	S VALMCNT=61+$G(IBLCNT)
	Q
	;
APPEALS	;
	N OFFSET,START,IBCNS14,IBADD
	S IBCNS14=$$ADDRESS^IBCNSC0(IBCNS,.14,7)
	S START=40,OFFSET=2
	D SET^IBCNSP(START,OFFSET+25," Appeals Office Information ",IORVON,IORVOFF)
	D SET^IBCNSP(START+1,OFFSET," Company Name: "_$P($G(^DIC(36,+$P(IBCNS14,"^",7),0)),"^",1))
	D SET^IBCNSP(START+2,OFFSET,"       Street: "_$P(IBCNS14,"^",1))
	D SET^IBCNSP(START+3,OFFSET,"     Street 2: "_$P(IBCNS14,"^",2))
	N OFFSET S OFFSET=45
	D SET^IBCNSP(START+1,OFFSET,"     Street 3: "_$P(IBCNS14,"^",3)) S IBADD=1
	D SET^IBCNSP(START+1+IBADD,OFFSET,"   City/State: "_$E($P(IBCNS14,"^",4),1,15)_$S($P(IBCNS14,"^",4)="":"",1:", ")_$P($G(^DIC(5,+$P(IBCNS14,"^",5),0)),"^",2)_" "_$E($P(IBCNS14,"^",6),1,5))
	D SET^IBCNSP(START+2+IBADD,OFFSET,"        Phone: "_$P(IBCNS14,"^",8))
	D SET^IBCNSP(START+3+IBADD,OFFSET,"          Fax: "_$P(IBCNS14,"^",9))
	Q
	;
INQUIRY	;
	;
	N OFFSET,START,IBCNS15,IBADD
	S IBCNS15=$$ADDRESS^IBCNSC0(IBCNS,.15,8)
	S START=47,OFFSET=2
	D SET^IBCNSP(START,OFFSET+25," Inquiry Office Information ",IORVON,IORVOFF)
	D SET^IBCNSP(START+1,OFFSET," Company Name: "_$P($G(^DIC(36,+$P(IBCNS15,"^",7),0)),"^",1))
	D SET^IBCNSP(START+2,OFFSET,"       Street: "_$P(IBCNS15,"^"))
	D SET^IBCNSP(START+3,OFFSET,"     Street 2: "_$P(IBCNS15,"^",2))
	N OFFSET S OFFSET=45
	D SET^IBCNSP(START+1,OFFSET,"     Street 3: "_$P(IBCNS15,"^",3)) S IBADD=1
	D SET^IBCNSP(START+1+IBADD,OFFSET,"   City/State: "_$E($P(IBCNS15,"^",4),1,15)_$S($P(IBCNS15,"^",4)="":"",1:", ")_$P($G(^DIC(5,+$P(IBCNS15,"^",5),0)),"^",2)_" "_$E($P(IBCNS15,"^",6),1,5))
	D SET^IBCNSP(START+2+IBADD,OFFSET,"        Phone: "_$P(IBCNS15,"^",8))
	D SET^IBCNSP(START+3+IBADD,OFFSET,"          Fax: "_$P(IBCNS15,"^",9))
	Q
	;
HELP	; -- help code
	S X="?" D DISP^XQORM1 W !!
	Q
	;
EXIT	; -- exit code
	K VALMQUIT,IBCNS,IBCHANGE
	D CLEAN^VALM10
	Q
	;
INSCO	; -- select insurance company
	I '$D(IBCNS) D  G:$D(VALMQUIT) INSCOQ
	.S DIC="^DIC(36,",DIC(0)="AEQMZ"
	.I '$G(IBVIEW) S DLAYGO=36,DIC(0)=DIC(0)_"L"
	.D ^DIC K DIC
	.S IBCNS=+Y
	I $G(IBCNS)<1 K IBCNS S VALMQUIT="" G INSCOQ
INSCOQ	;
	K DIC
	Q
	;
BLANK(LINE)	; -- Build blank line
	D SET^VALM10(.LINE,$J("",80))
	Q
CC	; -- change insurance company
	S IBCNS1=IBCNS D INSCO
	Q
