ACHSEOBM ; IHS/ITSC/TPF/PMF - BUILD CHS EOBR MESSAGE FILE ;  
 ;;3.1;CONTRACT HEALTH MGMT SYSTEM;**15,16**;JUN 11, 2001
 ;ACHS*3.1*15 1.26.2009 IHS/OIT/FCJ REMOVED DLAYGO WHEN ADDING MESSAGES
 ;ACHS*3.1*16 IHS.OIT.FCJ FIXED DUPLICATE MESSAGE ISSUE 
 ;
 S (ACHSRCT,ACHSMCNT,ACHSMFLG)=0,ACHSOMSG=""
 K ACHSAEND
 U IO(0)
 W !,"BUILDING CHS EOBR MESSAGE FILE...",!!
A1 ; Read past the FI header info at the top of the file.
 U IO
 F  R ACHSEOBR:5 G:ACHSEOBR="" B0A S:ACHSISAO ACHSEOBR=$E(ACHSEOBR,3,85) S ACHSRCT=ACHSRCT+1 G:ACHSRCT>100 END1^ACHSEOB1 Q:$E(ACHSEOBR,1,2)="$$"  ; note for SAC: this is a file read, not an interactive read
 ;
B0 ;
 I +$E(ACHSEOBR,3,4)<1 G B0A
 S ACHSRCT=0,X=$S(ACHSISAO=0:9,ACHSISAO=1:8),ACHSEOBD=$E(ACHSEOBR,3,X)
 I 'ACHSISAO S ACHSEBSQ=+$P(ACHSEOBR," ",2)
 I ACHSISAO S X=ACHSEOBD,X=$E(X,5,6)_$E(X,1,4),ACHSEOBD=$S($E(X,1,2)>50:2,1:3)_X,ACHSAOSQ=+$P(ACHSEOBR," ",3)
B2 ;
 U IO
 R ACHSEOBR:5
 I ACHSISAO S ACHSEOBR=$E(ACHSEOBR,3,85)
 I ACHSRCT=0,$E(ACHSEOBR,1,2)="$$" U IO(0) W "NO MESSAGE RECORDS TO PROCESS" Q
 S ACHSRCT=ACHSRCT+1
 I $E(ACHSEOBR,1,2)="$$" G MSGEND
 S ACHSSEQ=$E(ACHSEOBR,1,3),ACHSMSG=$E(ACHSEOBR,4,7)
 I ACHSOMSG=ACHSMSG G B3
 ;ACHS*3.1*15 1.26.2009 IHS/OIT/FCJ REMOVED DLAYGO
 ;S DIC="^ACHSEOBM(",DIC(0)="ZML",X=ACHSMSG,DLAYGO=9002076
 S DIC="^ACHSEOBM(",DIC(0)="ZML",X=ACHSMSG
 U IO(0)
 S Y=""       ;ACHS*3.1*16 IHS.OIT.FCJ ADDED LINE
 I $D(^ACHSEOBM("B",ACHSMSG)) S Y=$O(^ACHSEOBM("B",ACHSMSG,Y))  ;ACHS*3.1*16 IHS.OIT.FCJ NEW LINE CHANGED NXT LINE TO ELSE
 E  D ^DIC
 ;K DLAYGO     ;ACHS*3.1*16 IHS.OIT.FCJ DLAYGO NO LONGER BEING SET
 I +Y<1 U IO(0) W !,ACHSMSG," ADD TO EOBR MESSAGE FILE <FAILED> - NOTIFY SUPERVISOR" S ACHSTERR=1 D RTRN^ACHS G B2
 S DA=+Y,ACHSMCNT=ACHSMCNT+1,ACHSMLN=0
 U IO(0)
 W $J(ACHSMSG,10)
 K ^ACHSEOBM(DA,1) ; Remove old message text.
B3 ;
 I 'ACHSISAO S ACHSM3=$E(ACHSEOBR,8,85) G B4
 I ACHSSEQ#2'=0 S ACHSM1=$E(ACHSEOBR,8,46),ACHSOMSG=ACHSMSG G B2
 S ACHSM2=$E(ACHSEOBR,8,46),ACHSM3=ACHSM1_ACHSM2
B4 ;
 S ACHSMLN=ACHSMLN+1,^ACHSEOBM(DA,1,ACHSMLN,0)=$$SB^ACHS($$RPL^ACHS(ACHSM3,"  "," "))
 S $P(^ACHSEOBM(DA,1,0),U,3,4)=ACHSMLN_U_ACHSMLN
 S $P(^ACHSEOBM(DA,0),U,2)=DT,ACHSOMSG=ACHSMSG
 G B2
 ;
MSGEND ;
 U IO(0)
 W !
 Q:ACHSMCNT=0
 W !!,ACHSMCNT," EOBR MESSAGES ADDED/UPDATED",!!
 Q:'ACHSISAO
 Q:+$$AOP^ACHS(2,9)=0
 I $$AOP^ACHS(2,9)="999" S $P(^ACHSAOP(DUZ(2),2),U,9)=0
 I $$AOP^ACHS(2,9)+1'=ACHSAOSQ S ACHSMFLG=2
 Q
 ;
FAC ;
 I $D(^ACHSF(DUZ(2),17,"B",ACHSEOBD)) S ACHSMFLG=1 Q
 S X=""
 I ACHSFCSQ+1'=ACHSEBSQ S ACHSMFLG=2 Q
 Q
 ;
B0A ;
 U IO(0)
 W *7,!,"PROCESSING ERROR ENCOUNTERED FOR EOBR FILE"
 D RTRN^ACHS
 G ABEND^ACHSEOB
 ;
