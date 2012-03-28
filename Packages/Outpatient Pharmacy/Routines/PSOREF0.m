PSOREF0 ;IHS/JCM - REFILL CON'T ;21-Nov-2010 19:44;SM
 ;;7.0;OUTPATIENT PHARMACY;**14,152,180,1003,1005,1009**;DEC 1997
 ;External reference to ^PSDRUG supported by DBIA 221
 ; Modified - IHS/CIA/PLS - 01/06/04 - Line PROCESS+5 and PROCESS+8
 ;                          08/30/05 - Line PROCESS+7
 ;                          01/22/07 - Line PROCESS+6
 ;                          01/25/07 - Line PROCESS+10 - Added XFROM conditional
 ;                          11/20/10 - Line CHECK+3
PROCESS ;
 K PSODF S PSOREF("RX0")=^PSRX(PSOREF("IRXN"),0),PSOREF("RX2")=^(2),PSOREF("RX3")=^(3),PSOREF("STA")=+$G(^("STA")),PSOREF("SIG")=$P($G(^("SIG")),"^"),PSOREF("PSODFN")=$P(PSOREF("RX0"),"^",2)
 S PSOREF("DAYS SUPPLY")=$P(PSOREF("RX0"),"^",8)
 I $D(PSORX("BAR CODE")),PSODFN'=PSOREF("PSODFN") D NEWPT
 W !,"Now refilling Rx# ",$P(PSOREF("RX0"),"^")_"   Drug: "_$P(^PSDRUG($P(PSOREF("RX0"),"^",6),0),"^")
 ; IHS/MSC/PLS - 01/22/07 - Added next line - Patch 1005
 W !,"Patient: "_$$GET1^DIQ(2,PSODFN,.01)_"   HRN: "_$$HRN^AUPNPAT(PSODFN,DUZ(2))_"   LFDT: "_$$FMTE^XLFDT(+PSOREF("RX3"),"5Z")
 D PRINT^APSQLAB   ; IHS/CIA/PLS - 01/06/04 - Display appropriate lab results
 S PSOREF("DFLG")=0 D DSPLY G:PSOREF("DFLG") PROCESSX
 ;IHS/CIA/PLS - 08/30/205 - Added logic to populate IHS fields
 ;IHS/MSC/PLS - 01/25/2007 - Added XFROM condition for AudioCare processing
 D:$G(XFROM)="BATCH"!('$D(PSOREF("AWP"))) IHSSET^PSOREF1
 D CHECK G:$G(PSODF) PROCESS G:PSOREF("DFLG") PROCESSX D EN^PSOR52(.PSOREF)
 ; IHS/CIA/PLS - 02/10/04 - Paperless refill
 ;               04/29/05 - PCC logic moved to APSP namespace
 ;S X="CIAZPRX1" X ^%ZOSF("TEST") I $T D EN^CIAZPRX1(PSOREF("PSODFN"),PSOREF("IRXN"))
 D EN^APSPPCC1(PSOREF("PSODFN"),PSOREF("IRXN"))
 S:$G(PSOREF("MAIL/WINDOW"))["W" BINGRTE="W",BINGCRT=1
PROCESSX D:$G(PSOREF("OLD FILL DATE"))]"" SUSDATEK^PSOUTIL(.PSOREF)
 Q
DSPLY ;W !!,$P(PSOREF("RX0"),"^"),?12," ",$P(^PSDRUG($P(PSOREF("RX0"),"^",6),0),"^"),?45," SIG: "_PSOREF("SIG"),?60," QTY: ",$P(PSOREF("RX0"),"^",7)
 K FSIG,BSIG I $P($G(^PSRX(PSOREF("IRXN"),"SIG")),"^",2) D FSIG^PSOUTLA("R",PSOREF("IRXN"),54) F PSREV=1:1 Q:'$D(FSIG(PSREV))  S BSIG(PSREV)=FSIG(PSREV)
 K FSIG,PSREV I '$P($G(^PSRX(PSOREF("IRXN"),"SIG")),"^",2) D EN2^PSOUTLA1(PSOREF("IRXN"),54)
 W !!,"Qty: ",$P(PSOREF("RX0"),"^",7),?19,"Sig: ",$G(BSIG(1))
 I $O(BSIG(1)) F PSREV=1:0 S PSREV=$O(BSIG(PSREV)) Q:'PSREV  W !?24,$G(BSIG(PSREV))
 K BSIG,PSREV
DSPLYX Q
CHECK ;
 I '$P(PSOPAR,"^",11),$G(^PSDRUG($P(PSOREF("RX0"),"^",6),"I"))]"",DT>$G(^("I")) D  G CKQ
 .W $C(7),!!," *** Drug is inactive for Rx # "_$P(PSOREF("RX0"),"^")_" cannot be refilled ***",!
 I '$$SCREEN^APSPMULT(+$P(PSOREF("RX0"),"^",6),,1) W $C(7),!!,"** Drug is not selectable for Rx # "_$P(PSOREF("RX0"),"^")_" cannot be refilled ***",! G CKQ  ;IHS/MSC/JDS - 11/20/10
 I '$D(PSORX("BAR CODE")),PSOREF("PSODFN")'=PSODFN W !!,?5,$C(7),"Can't refill Rx # "_$P(PSOREF("RX0"),"^")_", it is not for this patient." G CKQ
 S (PSOX,PSOY,STA)=""
 I $G(PSOSD) F  S STA=$O(PSOSD(STA)) Q:STA=""  F  S PSOX=$O(PSOSD(STA,PSOX)) Q:PSOX']""!(PSOREF("DFLG"))  I PSOREF("IRXN")=+PSOSD(STA,PSOX) S PSOY=PSOSD(STA,PSOX) I $P(PSOY,"^",4)]"" D
 . S PSOREF("DFLG")=1 W:'$G(PSOERR) !,$C(7),"Cannot refill Rx # "_$P(PSOREF("RX0"),"^") S PSOREA=$P(PSOY,"^",4),PSOSTAT=PSOREF("STA")
 . D STATUS^PSOUTIL(PSOREA,PSOSTAT) K PSOREA,PSOSTAT
 . Q
 I PSOY="" W !,$C(7),"Cannot refill, Rx is discontinued or expired.  Later Rx may exist.",! D  I $G(PSODF) Q
 .D LOOK^PSOREF2 I $G(PSODF) Q
 .S PSOREF("DFLG")=1
 K PSOX,PSOY G:PSOREF("DFLG") CHECKX
 I $O(^PS(52.5,"B",PSOREF("IRXN"),0)),'$G(^PS(52.5,+$O(^PS(52.5,"B",PSOREF("IRXN"),0)),"P")) W !,$C(7),"Rx is in suspense and cannot be refilled" S PSOREF("DFLG")=1 G CHECKX
 ;
 S PSOREF("RXSTATUS")=PSOREF("STA")
 I PSOREF("RXSTATUS"),PSOREF("RXSTATUS")'=6 D  G CHECKX
 . S PSOY=";"_PSOREF("RXSTATUS"),PSOX=$P(^DD(52,100,0),"^",3),PSOY=$F(PSOX,PSOY),PSOY=$P($E(PSOX,PSOY,999),";",1)
 . W !,$C(7),"Rx is in "_PSOY_" status, cannot be refilled" S PSOREF("DFLG")=1
 D CHKDIV G:PSOREF("DFLG") CHECKX
 D NUMBER I PSOREF("NUMBER")>$P(PSOREF("RX0"),"^",9) W !?5,"Can't refill, no refills remaining." S PSOREF("DFLG")=1 G CHECKX
 D DATES
CHECKX Q
CKQ ;
 S PSOREF("DFLG")=1 D PAUSE^VALM1 G CHECKX
 Q
 ;
CHKDIV G:$P(PSOREF("RX2"),"^",9)=+PSOSITE CHKDIVX
 W !?5,$C(7),"RX # ",$P(PSOREF("RX0"),"^")," is for (",$P(^PS(59,$P(PSOREF("RX2"),"^",9),0),"^"),") division."
 I '$P($G(PSOSYS),"^",2) S PSOREF("DFLG")=1 G CHKDIVX
 D:$P($G(PSOSYS),"^",3) DIR
CHKDIVX Q
NUMBER K PSOX,PSOY S PSOREF("# OF REFILLS")=0
 I $G(^PSRX(PSOREF("IRXN"),1,0))]"" F PSOX=0:0 S PSOX=$O(^PSRX(PSOREF("IRXN"),1,PSOX)) Q:'PSOX  S PSOREF("# OF REFILLS")=PSOX
 S PSOREF("NUMBER")=PSOREF("# OF REFILLS")+1
 Q
 ;
DATES S PSOREF("STOP DATE")=$P(PSOREF("RX2"),"^",6) D NEXT^PSOUTIL(.PSOREF)
 D:$G(PSOBBC("QFLG"))&($P(PSOPAR,"^",6)) EDATE Q:$G(PSOREF("DFLG"))
 S PSOREF("FILL DATE")=$S($G(PSOREF("FILL DATE")):PSOREF("FILL DATE"),1:DT)
 I $P(PSOPAR,"^",6),PSOREF("FILL DATE")<$P(PSOREF("RX3"),"^",2) D SUSDATE^PSOUTIL(.PSOREF)
 ;
 I PSOREF("FILL DATE")>PSOREF("STOP DATE") D
 . W !!?5,$C(7),"Can't refill, Refill Date ",$E(PSOREF("FILL DATE"),4,5),"/",$E(PSOREF("FILL DATE"),6,7),"/"
 . W $E(PSOREF("FILL DATE"),2,3)," is past Expiration Date ",$E(PSOREF("STOP DATE"),4,5),"/",$E(PSOREF("STOP DATE"),6,7),"/"
 . W $E(PSOREF("STOP DATE"),2,3) S PSOREF("DFLG")=1
EDATE S PSOREF("LAST REFILL DATE")=$P(PSOREF("RX3"),"^",1)
 I PSOREF("LAST REFILL DATE"),PSOREF("FILL DATE")=PSOREF("LAST REFILL DATE") D  G DATESX
 . W !?5,"Can't refill, Fill Date already exists for ",$E(PSOREF("FILL DATE"),4,5),"/",$E(PSOREF("FILL DATE"),6,7),"/",$E(PSOREF("FILL DATE"),2,3)
 . S PSOREF("DFLG")=1
 I PSOREF("LAST REFILL DATE"),PSOREF("FILL DATE")<PSOREF("LAST REFILL DATE") D  G DATESX
 . W !?5,"Can't refill, later Refill Date already exists for ",$E(PSOREF("LAST REFILL DATE"),4,5),"/",$E(PSOREF("LAST REFILL DATE"),6,7),"/",$E(PSOREF("LAST REFILL DATE"),2,3)
 . S PSOREF("DFLG")=1
 I '$P(PSOPAR,"^",6),'$D(PSOREF("EAOK")),$P(PSOREF("RX3"),"^",2)>PSOREF("FILL DATE") D
 . S PSOX1=(PSOREF("NUMBER")+1)*PSOREF("DAYS SUPPLY")-10
 . W !?5,$C(7),"LESS THAN ",PSOX1," DAYS FOR ",PSOREF("NUMBER")+1," FILLS",! D DIR K PSOX1
 I '$P(PSOPAR,"^",6),$G(PSOREF("EAOK"))=0,$P(PSOREF("RX3"),"^",2)>PSOREF("FILL DATE") D
 . S Y=$P(PSOREF("RX3"),"^",2) D DD^%DT W !!,$C(7),"Cannot be refilled until "_Y_"." S PSOREF("DFLG")=1 K Y
DATESX Q
DIR K DIR,X,Y S DIR(0)="Y",DIR("A")="Continue ",DIR("B")="N",DIR("?")="Answer YES to Refill, NO to bypass"
 D ^DIR K DIR S:$D(DIRUT)!('Y) PSOREF("DFLG")=1 K DIRUT,DTOUT,DUOUT,X,Y
 Q
NEWPT S PSOQFLG=0,(DFN,PSODFN)=PSOREF("PSODFN") D ^PSOPTPST I PSOQFLG S PSOREF("DFLG")=1,PSOQFLG=0 G NEWPTX
 D PROFILE^PSOREF1
NEWPTX Q
 ;
EN(PSOREF) ; Entry Point for Batch Barcode Option
 D PROCESS K DRUG,PSODF
 Q
