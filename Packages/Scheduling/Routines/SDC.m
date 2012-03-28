SDC ;MAN/GRR,ALB/LDB - CANCEL A CLINIC'S AVAILABILITY ; [ 01/09/2003  1:52 PM ]
 ;;5.3;Scheduling;**15,32,79,132,167,1003**;Aug 13, 1993
 ;IHS/ANMC/LJF  8/18/2000 added DIC("W") to warn if clinic inactivated
 ;             12/13/2000 added setting of cancellation comment into
 ;                          each patient's record
 ;             12/18/2002 added check so only onwers could cancel clinic
 ;IHS/ITSC/LJF 06/09/2005 PATCH 1003 added message to whole day cancellations
 ;
 N SDATA,SDCNHDL ; for evt dvr
SDC1 K SDLT,SDCP S NOAP="" D LO^DGUTL
 ;S DIC=44,DIC(0)="MEQA",DIC("S")="I $P(^(0),""^"",3)=""C"",'$G(^(""OOS""))",DIC("A")="Select CLINIC NAME: " D ^DIC K DIC("S"),DIC("A") G:'$D(^SC(+Y,"SL")) END^SDC0  ;IHS/ANMC/LJF 8/18/2000
 S DIC=44,DIC(0)="MEQA",DIC("S")="I $P(^(0),""^"",3)=""C"",'$G(^(""OOS""))",DIC("A")="Select CLINIC NAME: ",DIC("W")=$$INACTMSG^BSDU D ^DIC K DIC("S"),DIC("A") G:'$D(^SC(+Y,"SL")) END^SDC0  ;IHS/ANMC/LJF 8/18/2000
 ;1/9/2003 WAR per P46,LJF37
 I '$$OWNER^BSDU(+Y,+$G(DUZ)) D MSG^BDGF("You are NOT an owner of this clinic.  Please select again.",2,1) G SDC1   ;IHS/ANMC/LJF 12/18/2002
 I $D(^SC(+Y,0))  ;IHS/ANMC/LJF 12/18/2002 restoring naked reference
 S SC=+Y,SL=^("SL"),%DT="AEXF",%DT("A")="CANCEL '"_$P(Y,U,2)_"' FOR WHAT DATE: " D ^%DT K %DT G:Y<0 END^SDC0 ;NAKED REFERNCE - ^SC(IFN,"SL")
 S (SD,CDATE)=Y,%=$P(SL,U,6),SI=$S(%="":4,%<3:4,%:%,1:4),%=$P(SL,U,3),STARTDAY=$S($L(%):%,1:8) D NOW^%DTC S SDTIME=%
 K SDRE,SDIN,SDRE1 I $D(^SC(SC,"I")) S SDIN=+^("I"),SDRE=+$P(^("I"),"^",2),Y=SDRE D:Y DTS^SDUTL S SDRE1=$S(SDRE:" to "_Y,1:"")
 I $S('$D(SDIN):0,SDIN'>0!(SDIN>SD):0,SDRE'>SD&(SDRE):0,1:1) W !,*7,"Clinic is inactive ",$S('SDRE:"as of ",1:"from ") S Y=SDIN D DTS^SDUTL W Y,SDRE1 G SDC1
 I '$D(^SC(SC,"ST",SD,1)) S DH="" D B S ^SC(SC,"ST",SD,1)=$P("SU^MO^TU^WE^TH^FR^SA",U,DOW+1)_" "_$E(SD,6,7)_$J("",SI+SI-6)_DH,^(0)=SD G N
 I ^(1)["CANCELLED" W !,"APPOINTMENTS HAVE ALREADY BEEN CANCELLED",!,*7 S ANS="N",SDTIME="*",SDV1=$S($P(^SC(SC,0),"^",15):$P(^(0),"^",15),1:+$O(^DG(40.8,0))) K SDX G ASKL^SDC0 ;NAKED REFERENCE - ^SC(IFN,"ST",Date,1)
N I '$F(^SC(SC,"ST",SD,1),"[") K:^(1)?2U.E ^SC(SC,"ST",SD) W !,*7,"CLINIC DOES NOT MEET ON THAT DAY" G SDC1 ; KILLs node if not holiday
 I $O(^SC(SC,"S",SD))\1-SD W *7,!?5,"NO APPOINTMENTS SCHEDULED" S NOAP=1 G W
 W !,"FIRST, I'LL LIST THE EXISTING APPOINTMENTS",!
 K DUOUT,DTOUT D ^SDC1 I $D(DUOUT)!$D(DTOUT) D END^SDC0 Q
 I ^SC(SC,"ST",SD,1)["X" G ^SDC2
W S DH=0,%="" W !,"WANT TO CANCEL THE WHOLE DAY" D YN^DICN I '% W !,"REPLY YES (Y) OR NO (N)" G W
 I %=1 G WP:$$COED^SDC4(SC,SD,SD+.2359,1),ALL
 Q:%<1
WP S %="" W !,"WANT TO CANCEL PART OF THE DAY" D YN^DICN I '% W !,"REPLY YES (Y) OR NO (N)" G WP
 Q:(%-1)
F R !,"STARTING TIME: ",X:DTIME Q:U[X  D TC^SDC2 G F:Y<0 S FR=Y,ST=%
T R !,"ENDING TIME: ",X:DTIME Q:U[X  D TC^SDC2 G T:Y<0 S SDHTO=X,TO=Y I TO'>FR W !,"Ending time must be greater than starting time",*7 G T
 I $$COED^SDC4(SC,FR,TO,1) K FR,SDHTO,TO,ST W ! G F
ROPT R !,"(OPTIONAL) MESSAGE: ",I:DTIME I I?1"?".E W !,"YOU MAY ENTER A MESSAGE CONCERNING THE CANCELLATION HERE" G ROPT
 Q:I["^"  I '$D(^SC(SC,"SDCAN",0)) S ^SC(SC,"SDCAN",0)="^44.05D^"_FR_"^1" G SKIP
 NEW BSDMSG I I]"" S BSDMSG=I   ;IHS/ANMC/LJF 12/13/2000
 S A=^SC(SC,"SDCAN",0),SDCNT=$P(A,"^",4),^SC(SC,"SDCAN",0)=$P(A,"^",1,2)_"^"_FR_"^"_(SDCNT+1)
SKIP S ^SC(SC,"SDCAN",FR,0)=FR_"^"_SDHTO
 S NOAP=$S($O(^SC(SC,"S",(FR-.0001)))'>0:1,$O(^SC(SC,"S",(FR-.0001)))>TO:1,1:0) I 'NOAP S NOAP=$S($O(^SC(SC,"S",+$O(^SC(SC,"S",(FR-.0001))),0))="MES":1,1:0)
 S ^SC(SC,"S",FR,0)=FR,^("MES")="CANCELLED UNTIL "_X_$S(I?.P:"",1:" ("_I_")") D S S I=^(1),I=I_$J("",%-$L(I)),Y=""
 F X=0:2:% S DH=$E(I,X+SI+SI),P=$S(X<ST:DH_$E(I,X+1+SI+SI),X=%:$S(Y="[":Y,1:DH)_$E(I,X+1+SI+SI),1:$S(Y="["&(X=ST):"]",1:"X")_"X"),Y=$S(DH="]":"",DH="[":DH,1:Y),I=$E(I,1,X-1+SI+SI)_P_$E(I,X+2+SI+SI,999)
 S:'$F(I,"[") I5=$F(I,"X"),I=$E(I,1,(I5-2))_"["_$E(I,I5,999) K I5
 S DH=0,^(1)=I,FR=FR-.0001 G C ;NAKED REFERENCE - ^SC(IFN,"ST",Date,1)
 ;
S S ^("CAN")=^SC(SC,"ST",SD,1) Q
 ;
 ;IHS/ITSC/LJF 6/9/2005 PATCH 1003 add message to stored cancel message
ALL ;D S S ^(1)="   "_$E(SD,6,7)_"    **CANCELLED**",FR=SD,TO=SD+.9 ;NAKED REFERENCE - ^SC(IFN,"ST",Date,1)
 NEW BSDMSG S BSDMSG=$$READ^BDGF("FO^1:50","(OPTIONAL) MESSAGE")  ;IHS/ANMC/LJF 12/13/2000
 D S S ^SC(SC,"ST",SD,1)="   "_$E(SD,6,7)_"    **CANCELLED** "_BSDMSG,FR=SD,TO=SD+.9
 ;end of PATCH 1003 changes
 ;
C S FR=$O(^SC(SC,"S",FR)) I FR<1!(FR'<TO) W !!,"CANCELLED!  " K SDX G CHKEND^SDC0
 F I=0:0 S I=$O(^SC(SC,"S",FR,1,I)) Q:I'>0  D
 .S DFN=+^SC(SC,"S",FR,1,I,0),SDCNHDL=$$HANDLE^SDAMEVT(1)
 .D BEFORE^SDAMEVT(.SDATA,DFN,FR,SC,I,SDCNHDL)
 .S $P(^SC(SC,"S",FR,1,I,0),"^",9)="C"
 .I $D(^DPT(DFN,"S",FR,0)),$P(^(0),"^",2)'["C" S $P(^(0),"^",2)="C",$P(^(0),"^",12)=DUZ,$P(^(0),"^",14)=SDTIME,DH=DH+1 D MORE
 G C
 ;
B S X=SD D DOW^SDM0 S DOW=Y,SS=+$O(^SC(SC,"T"_Y,X)) I $D(^(SS,1)),^(1)]"" S DH=^(1),DO=X+1,DA(1)=SC
 Q
MORE I $D(^SC("ARAD",SC,FR,DFN)) S ^(DFN)="N"
 I $G(BSDMSG)]"" S ^DPT(DFN,"S",FR,"R")=BSDMSG  ;IHS/ANMC/LJF 12/13/2000
 S SDIV=$S($P(^SC(SC,0),"^",15)]"":$P(^(0),"^",15),1:" 1"),SDV1=$S(SDIV:SDIV,1:+$O(^DG(40.8,0))) I $D(^DPT("ASDPSD","C",SDIV,SC,FR,DFN)) K ^(DFN)
 S SDH=DH,SDTTM=FR,SDSC=SC,SDPL=I,SDRT="D" D RT^SDUTL
 S DH=SDH K SDH D CK1,EVT
 K SD1,SDIV,SDPL,SDRT,SDSC,SDTTM,SDX Q
CK1 S SDX=0 F SD1=FR\1:0 S SD1=$O(^DPT(DFN,"S",SD1)) Q:'SD1!((SD1\1)'=(FR\1))  I $P(^(SD1,0),"^",2)'["C",$P(^(0),"^",2)'["N" S SDX=1 Q
 Q:SDX  F SD1=2,4 I $D(^SC("AAS",SD1,FR\1,DFN)) S SDX=1 Q
 Q:SDX  IF $D(^SCE(+$$EXAE^SDOE(DFN,FR\1,FR\1),0)) S SDX=1
 Q:SDX  K ^DPT("ASDPSD","B",SDIV,FR\1,DFN) Q
 ;
EVT ; -- separate tag if need to NEW vars
 ; -- cancel event
 N FR,I,SDTIME,DH,SC
 D CANCEL^SDAMEVT(.SDATA,DFN,SDTTM,SDSC,SDPL,0,SDCNHDL) K SDATA,SDCNHDL
 Q
