ACRFTXTP ;IHS/OIRM/DSD/THL,AEF - LIST OF TRANSACTION TYPES; [ 11/01/2001   9:44 AM ]
 ;;2.1;ADMIN RESOURCE MGT SYSTEM;;NOV 05, 2001
 ;;ROUTIN TO LIST OF TRANSACTION TYPES
EN K ACRQUIT,ACRTXTP
 F  D EN1 Q:$D(ACRQUIT)!$D(ACROUT)
EXIT K ACRX,ACRTXNAM,ACRTX,ACRQUIT,ACRDOCDA,ACRTXDOC,ACRTT,ACRDUZ,ACRJ,ACRK,ACRI,ACRQK,ACRCNT,ACRLI,ACRUSER,ACR,ACRQK1,ACRZ
 Q
EN1 D DISPLAY
 D SELECT
 Q
DISPLAY ;EP;TO DISPLAY TRANSACTION TYPES
 W @IOF
 I $D(ACRXDRCL) D
 .W !?80-$L(ACRXDRCL)\2,ACRXDRCL
 .W !?80-$L(ACRXDRCL)\2
 .F ACRI=1:1:$L(ACRXDRCL) W "="
 K ACRXDRCL,ACRI
 W !!?2,"NO."
 W ?7,"TRANSACTION TYPE"
 W ?42,"NO."
 W ?47,"TRANSACTION TYPE"
 W !?2,"---"
 W ?7,"------------------------------"
 W ?42,"---"
 W ?47,"------------------------------"
 S (ACRJ,ACRTXDO)=0
 F  S ACRTXDO=$O(^ACRTXTYP("DO",ACRTXDO)) Q:'ACRTXDO  D
 .S ACRTXDA=0
 .F  S ACRTXDA=$O(^ACRTXTYP("DO",ACRTXDO,ACRTXDA)) Q:'ACRTXDA  D
 ..I $D(ACRNEWOB),$D(ACRFDNO),$D(^ACRLOCB(ACRFDNO,2,"B",ACRTXDA)) D  Q
 ...S ACRTXNAM=$P(^ACRTXTYP(ACRTXDA,0),U)
 ...D DISP1
 ..I $D(ACRNEWOB),$D(ACRFDNO),'$D(^ACRLOCB(ACRFDNO,2,"B",ACRTXDA)) Q
 ..S ACRTXNAM=$P(^ACRTXTYP(ACRTXDA,0),U)
 ..I $D(ACRLBTX),$E(ACRTXNAM,1,3)="REQ" D DISP1 Q
 ..D DISP1
 D DISP2
 Q
DISP1 I ACRTXDA=19&'$D(ACRTT) Q
 S ACRJ=ACRJ+1
 S ACRTX=ACRTXDA_"^"_^ACRTXTYP(ACRTXDA,0)
 S ACRTX("DT")=^ACRTXTYP(ACRTXDA,"DT")
 I $D(ACRFDNO),'$D(ACRCSI),'$D(ACRTT),$D(^ACRLOCB(ACRFDNO,2,"B",ACRTXDA)) D
 .S ACRTXDAX=$O(^ACRLOCB(ACRFDNO,2,"B",ACRTXDA,0))
 .I ACRTXDAX D  I 1
 ..S ACRTXLIM=$P(^ACRLOCB(ACRFDNO,2,ACRTXDAX,0),U,2)
 ..S ACRTXLIM=$P(ACRTXLIM,".")
 E  S ACRTXLIM=0
 S ACRTX(ACRJ)=ACRTX_"^LIM^"_ACRTXLIM
 S ACRTX(ACRJ,"DT")=ACRTX("DT")
 Q
DISP2 S ACRJJ=$S(ACRJ>1:ACRJ\2+(ACRJ#2),1:ACRJ)
 F ACRI=1:1:ACRJJ D
 .W:ACRI<ACRJJ!(ACRI=ACRJJ) !
 .S ACRJJJ=ACRI
 .D LBTX
 .W ?2,ACRI
 .W ?$X+3+$S($L(ACRI)=1:1,1:0),$P(ACRTX(ACRI),U,2)
 .I $D(ACRTX(ACRI+ACRJJ)) D
 ..S ACRJJJ=ACRI+ACRJJ
 ..D LBTX
 ..W ?42,ACRJJJ
 ..W ?$X+3+$S($L(ACRJJJ)=1:1,1:0),$P(ACRTX(ACRJJJ),U,2)
 Q
LBTX I $D(ACRLBTX),$D(ACRZDA),$D(^ACRLOCB(ACRZDA,2,+ACRTX(ACRJJJ))) D
 .I ACRI=ACRJJJ W ""
 .E  W ?40
 .W "**"
 Q
SELECT ;EP;
 S DIR(0)="NO^1:"_ACRJ
 S DIR("A")="Which Transaction Type ==> "
 W !
 D DIR^ACRFDIC
 I U[$E(X)!(X="")!(+Y<1) S (ACRQUIT,ACRTXTP)="" Q
 S (ACRY,ACRX)=+Y
 S ACRTXDA=$P(ACRTX(ACRX),U)
 S ACRREFDA=$P(ACRTX(ACRX),U,3)
 S ACRTXLIM=$S($P(ACRTX(ACRX),"^LIM^",2)]"":$P(ACRTX(ACRX),"^LIM^",2),1:"UNSPEC")
 S ACRTXOBJ=$P(ACRTX(ACRX),U,4)
 S ACRQUIT=""
 S ACRTXPFX=$P(ACRTX(ACRX,"DT"),U,2)
 D OC:ACRTXDA'=35&(ACRTXDA'=31)
 Q
OC ;DISPLAY RELATED OBJECT CLASS CODES
 S DIR(0)="YO"
 S DIR("A")="Display Related Object Class Codes"
 S DIR("B")="NO"
 W !
 D DIR^ACRFDIC
 I U[$E(X) S (ACRQUIT,ACROUT)="" Q
 I +Y'=1 S ACRQUIT="" Q
 W @IOF,!?5,"Object Class Codes for ",$P(^ACRTXTYP(ACRTXDA,0),U)
 W !?5,"----  ----------------------------------------------------------"
 W !?5
 I ACRTXDA=31 D
 .W !,"Applicable Object Class Codes for a REQ FOR CALL AGAINST A BPA"
 .W !,"will depend on the original BPA."
 N ACRX
 S ACRX=0
 F  S ACRX=$O(^ACRTXTYP(ACRTXDA,10,ACRX)) Q:'ACRX!$D(ACRQUIT)!$D(ACROUT)  D:$D(^AUTTOBJC(ACRX,0))
 .S ACROC=^AUTTOBJC(ACRX,0)
 .W !?5,$P(ACROC,U),?11,$P(ACROC,U,3)
 .I $Y>(IOSL-4) D PAUSE^ACRFWARN W @IOF
 D PAUSE^ACRFWARN
 K ACRQUIT,ACROUT
 Q
