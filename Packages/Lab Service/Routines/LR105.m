LR105 ; IHS/DIR/FJE - LR*5.2*105 PATCH ENVIRNMENT CHECK ROUTINE ;
 ;;5.2;LR;;NOV 01, 1997
 ;
 ;;5.2;LAB SERVICE;**105**;Feb 14, 1996
EN ; Does not prevent loading of the transport global.
 ;Envirnment check is done only during the install.
 Q:'$G(XPDENV)
 I $S('$G(IOM):1,'$G(IOSL):1,$G(U)'="^":1,1:0) W !,$$CJ^XLFSTR("Terminal Device in not defined",80),!!
 I $S('$G(DUZ):1,$D(DUZ)[0:1,$D(DUZ(0))[0:1,1:0) W !!,$$CJ^XLFSTR("Please Log in to set local DUZ... variables",80),! S XPDQUIT=2
 I '$D(^VA(200,$G(DUZ),0))#2 W !,$$CJ^XLFSTR("You are not a valid user on this system",80),! S XPDQUIT=2
 L +^LRO(64.03):4 I '$T W !,$$CJ^XLFSTR("Not able to lock ^LRO(64.03) GLOBAL ",80),! S XPDQUIT=2
 L +^LAB(64.2):4 I '$T W !,$$CJ^XLFSTR("Not able to lock ^LAB(64.R) GLOBAL ",80),! S XPDQUIT=2
 I +$G(^LAM("VR"))'>5.1 W !,$$CJ^XLFSTR("You must have LAB V5.2 or greater Installed",80),! S XPDQUIT=2
 I $G(XPDQUIT) W !!,$$CJ^XLFSTR("Install environment check FAILED",80)
 I '$G(XPDQUIT) W !!,$$CJ^XLFSTR("Envirnment Check is Ok ---",80)
 Q
