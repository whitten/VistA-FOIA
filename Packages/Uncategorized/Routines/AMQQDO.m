AMQQDO ; IHS/CMI/THL - GENERATE OUTPUT ; 09 Mar 2010  3:19 PM
 ;;2.0;IHS PCC SUITE;**4**;MAY 14, 2009
 ;-----
 ; SPECIAL AMQP VARIABLES:
 ;AMQP(0)=PATIENT #
 ;AMQP(1)=VISIT #
 ;AMQP(2)=VISIT DATE
 ;AMQP(3)=V POV #
 ;AMQP(4)= V MED #
 ;AMQP(5) = PROVIDER #
 ;AMQP(6)=V PROCEDURE #
 S AMQQOV=$S(AMQQCCLS="P":0,AMQQCCLS="D":3,AMQQCCLS="H":5,1:1)
 I $D(AMQQBACK),$D(AMQQDIBT) S ^DIBT(AMQQDIBT,1,AMQP(AMQQOV))="" Q
 I $D(AMQQEN3),$D(AMQQDIBT),$D(AMQQND) S ^DIBT(AMQQDIBT,1,AMQP(AMQQOV))="" W "." Q
 I '$D(AMQQLABB) S AMQQLABB="" I $D(DUZ(2)),$D(^AUTTLOC(DUZ(2),0)) S AMQQLABB=$E($P(^(0),U,2),1,6)
 I $G(AMQQMULL),$D(^UTILITY("AMQQ",$J,"AG",AMQQMULL)) D MULT G EXIT
 D DISPLAY
EXIT K AMQQSVAR,AMQQOV,^UTILITY("AMQQ",$J,"AG"),AMQQLDFN,%,A,I,J,Z,W,X,Y
 Q
 ;
MULT ; ENTRY POINT FROM AMQQCMPS
 F AMQQHOLD=0:0 S AMQQHOLD=$O(^UTILITY("AMQQ",$J,"AG",AMQQMULL,AMQQHOLD)) Q:'AMQQHOLD  S %=^(AMQQHOLD) D M1 I AMQP(AMQQOV)=99999999999 Q
 K ^UTILITY("AMQQ",$J,"AG",AMQQMULL)
 Q
 ;
M1 ;
 I $P(^AMQQ(1,+$G(^UTILITY("AMQQ",$J,"Q",AMQQMULL)),0),U,3)=9000010.01 NEW AMQQVMDA S AMQQVMDA=$P(%,U,4)
 I $G(AMQV(+$O(AMQT(999),-1)))["AUPNVHF" N AMQQDA S AMQQDA=$P(%,U,4)
 I '$G(AMQQDA),$G(AMQV(+$O(AMQT(999),-1)-1))["AUPNVHF" N AMQQDA S AMQQDA=$P(%,U,4)
 S Z=(AMQQMUFV+AMQQMUNV-1)
 F X=AMQQMUFV:1:Z I $D(^UTILITY("AMQQ",$J,"VAR NAME",X)) S Y=^(X) D
 .S A=$P(Y,U,2)
 .Q:+Y'=+$G(^UTILITY("AMQQ",$J,"Q",AMQQMULL))
 .Q:'A
 .S AMQP(X)=$P(%,U,A)
 I $D(AMQQYY(0)) Q
 I 'AMQQOV,'$D(^DPT(AMQP(0),0)) W !,"BAD POINTER FOR PATIENT NUMBER ",AMQP(AMQQOV) Q
 D DISPLAY
 Q
 ;
DISPLAY S:'$D(AMQQTOT) AMQQTOT=0 S AMQQTOT=AMQQTOT+1
 I $D(AMQQRMFL) D @AMQQRMFL Q
 I $D(AMQV("OPTION")),AMQV("OPTION")="COUNT" W:$E(IOST,1,2)'="P-" $C(13),AMQQTOT Q
 I $D(AMQQDIBT) S ^DIBT(AMQQDIBT,1,AMQP(AMQQOV))=""
 I '$G(AMQQDLIM),AMQQTOT#(IOSL-6-(5*($E(IOST,1,2)="P-")))=1 D ^AMQQDOH I AMQP(AMQQOV)=99999999999 Q
 I AMQQCCLS="D" D DD Q
 I AMQQCCLS="H" D DH Q
 I AMQQCCLS="V" D DV Q
 I $P($G(^DPT(AMQP(AMQQOV),0)),U)="" W !,"MISSING DATA FOR """_$S($G(AMQP(.1))'="":AMQP(.1),1:("#"_AMQP(AMQQOV)))_""".  HAVE SITE MANAGER CHECK ""B"" INDEX!" S AMQQTOT=AMQQTOT-1 Q
 S %=$E($P(^DPT(AMQP(0),0),U),1,$S('$G(AMQQDLIM):16,1:99))
 I '$G(AMQQDLIM),$D(^DPT(AMQP(0),.01,1)) S %=$E(%,1,15)_"*"
 E  I $D(^DPT(AMQP(0),.01,1)) S %=%_"*"
 I $D(AMQQSUPF) S %="*****"
 W !,% W $S('$G(AMQQDLIM):" ",1:U)
 I $D(DUZ(2)),$D(^AUPNPAT(AMQP(AMQQOV),41,DUZ(2),0)) D
 .I '$G(AMQQDLIM) W ?17,$P(^AUPNPAT(AMQP(AMQQOV),41,DUZ(2),0),U,2)
 .E  W $S($P(^AUPNPAT(AMQP(AMQQOV),41,DUZ(2),0),U,2)]"":$P(^(0),U,2),1:"NO HRN"),U
 I $D(DUZ(2)),$G(AMQQDLIM),'$D(^AUPNPAT(AMQP(AMQQOV),41,DUZ(2),0)) W "NO HRN",U
DIS S J=$$CHKVA(24)
 F I=9:0 S I=$O(^UTILITY("AMQQ",$J,"VAR NAME",I)) Q:'I  I $D(AMQP(I)) D FORMAT
 I $G(AMQQDVQU),$P(^AMQQ(1,+%,0),U,3)=9000010.01 W ?J,$$QUAL(AMQQVMDA) S J=J+20
 I '$G(AMQQDLIM),$D(^TMP(+$G(AMQQJOB),"AMQQAPT")) D
 .S AMQQEDT=+^TMP(AMQQJOB,"AMQQAPT")
 .D APT^AMQQAPT(AMQP(AMQQOV),DT,AMQQEDT,$J)
 Q
 ;
DV S Y=+^AUPNVSIT(AMQP(1),0)
 X ^DD("DD")
 I '$G(AMQQDLIM) W !,AMQP(1),?9,Y
 E  W !,AMQP(1),U,Y,U
 S J=$$CHKVA(29)
 F I=9:0 S I=$O(AMQP(I)) Q:'I  I $D(^UTILITY("AMQQ",$J,"VAR NAME",I)) D FORMAT
 Q
 ;
DD I '$G(AMQQDLIM) W !,AMQP(3)
 E  W !,AMQP(3),U
 S J=9
 F I=9:0 S I=$O(AMQP(I)) Q:'I  I $D(^UTILITY("AMQQ",$J,"VAR NAME",I)) D FORMAT
 Q
 ;
DH S %=$P(@AMQQ200(16)@(AMQP(5),0),U)
 S Y=$P($G(@AMQQ200(6)@(AMQP(5),9999999)),U,2)
 I '$G(AMQQDLIM) W !,$E(%,1,18),?19,$E(Y,1,4)
 E  W !,$E(%,1,18),U,$E(Y,1,4),U
 D DIS
 Q
 ;
FORMAT S X=AMQP(I)
 S %=^UTILITY("AMQQ",$J,"VAR NAME",I)
 S Y=1
 S A=$P(%,U,2)
 S:'A A=1
 I $P(%,U,5)="EXISTS" S X="+"
 I $P(%,U,5)="INVERSE" S X="-"
 D LABCONV
 S Z=^AMQQ(1,+%,4,A,0)
 S Z=$P(Z,U,6)
 I X="" S X="-"
 I $P(%,U,3)'="" S Z=$P(%,U,4)
 I $D(AMQQTOTF(I)) K AMQQTOTF(I) S Y=0 G FOR1
 I $D(^AMQQ(1,+%,4,A,1)),X'?1P,X'="SAVED",X'="NULL",Y X ^(1)
 I $G(AMQQDA),$D(^AMQQ(1,+%,4,A,1)),X'?1P,X'="SAVED",X'="NULL",Y,^(1)["AUTTHF" D
 .N AMQQZ
 .S AMQQZ=$P($G(^AUPNVHF(AMQQDA,0)),U,6)
 .I AMQQZ,$D(AMQQ(423)) D  Q:'AMQQZ
 ..N COND,VALUE,T
 ..S COND=$P(AMQQ(423),U)
 ..S VALUE=$P(AMQQ(423),U,2)
 ..S T="I "_AMQQZ_COND_VALUE
 ..X T
 ..S:'$T AMQQZ=""
 .S:AMQQZ]"" X=$E(X,1,Z-4)_$J($E(AMQQZ,1,4),4)
FOR1 I '$G(AMQQDLIM) W ?J,$E(X,1,Z)
 E  W $E(X,1,Z),U
 I "^765^766^767^"[(U_+$G(%)_U) W " days"
 S J=J+2+Z
 Q
QUAL(Z) ;
 I $G(Z)="" Q ""
 NEW A,B,C
 S A=""
 S B=0 F  S B=$O(^AUPNVMSR(Z,5,B)) Q:B'=+B  D
 .S C=$P($G(^AUPNVMSR(Z,5,B,0)),U) I C S A=A_$P(^GMRD(120.52,C,0),U,2)_" "
 Q A
LABCONV ;EP;CONVERT FOR TEMP LAB GLOBAL
 Q:+%<1000!(+%'[".")
 Q:$D(^AMQQ(1,+%,4))
 S $P(%,U)=$P(+%,".")_($J/100000)
 Q:$D(^AMQQ(1,+%,4))
 N AMQQATN,AMQQATNM,AMQQXX
 S AMQQXX=""
 S AMQQATN=$P(%,".")
 S AMQQATNM="LAB"
 N A,I,J,%
 D SETLAB^AMQQATAL
 Q
 ;
EXP ; ENTRY POINT FROM METADICTIONARY
 N J,Y,Z,%,SITE,VLAB
 S J=$G(AMQQLDFN)
 I 'J Q
 S Y=$P(^LAB(60,J,0),U)
 S Y=$P(Y,"(",2)
 S:Y'="" Y="  ("_$E(Y,1,16)
 S %=^UTILITY("AMQQ",$J,"AG",AMQQMULL,AMQQHOLD)
 S Z=$P(%,U,4)
 S VLAB=Z
 S Z=$P($G(^AUPNVLAB(Z,11)),U)
 S %=$P(%,U)
 S %=$E(%,$L(%)-1,$L(%)),%=$S(%="L*":" ",%="H*":" ",%=" H":"  ",%=" L":"  ",1:"    ")
 S Z=%_Z
 S SITE="NO SITE RECORDED"
 S %=$P($G(^AUPNVLAB(VLAB,11)),U,3)
 S:$G(^LAB(61,+%,0))'="" SITE=$P(^LAB(61,%,0),U)
 S X=X_Z_"   "_SITE_Y
 Q
 ;
SUOUT ; Output transform for CHART SERVICE UNIT attribute; prints chart #s/su
 N %
 S X=""
 S %=0
 F  S %=$O(^AUPNPAT(AMQP(0),41,%)) Q:'%  D
 .N %A
 .S %A=$P(^AUTTLOC(%,0),U,5)
 .I %'=DUZ(2),$D(^UTILITY("AMQQ TAX",$J,AMQP(4101),%A))!($D(^("*"))) S:X'="" X=X_"," S X=X_$P(^AUTTLOC(%,0),U,7)_$P(^AUPNPAT(AMQP(0),41,%,0),U,2)
 Q
 ;
CHKVA(C) ; RETURN C+3 IF VA, ELSE C
 Q $S('$D(DUZ("AG")):C,$E(DUZ("AG"))="V":C+3,1:C)
