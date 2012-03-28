ACMRL0 ;cmi/anch/maw - SCREEN LOGIC ; 
 ;;2.0;ACM CASE MANAGEMENT SYSTEM;**1,4,6,7**;AUG 27, 2007
 ;IHS/CMI/LAB - TMP TO XTMP
 ;
 ;
SELECT ;EP
 S ACMANS=Y,ACMC="" F ACMI=1:1 S ACMC=$P(ACMANS,",",ACMI) Q:ACMC=""  S ACMCRIT=ACMSEL(ACMC) D
 .S ACMTEXT=$P(^ACM(58.1,ACMCRIT,0),U)
 .S ACMVAR=$P(^ACM(58.1,ACMCRIT,0),U,6) K ^ACM(58.8,ACMRPT,11,ACMCRIT),^ACM(58.8,ACMRPT,11,"B",ACMCRIT)
 .W !!,ACMC,")  ",ACMTEXT," Selection."
 .I $P(^ACM(58.1,ACMCRIT,0),U,2)]"" S ACMCNT=0,^ACM(58.8,ACMRPT,11,0)="^9002258.81101PA^0^0" D @$P(^ACM(58.1,ACMCRIT,0),U,2)
 .Q
 Q
PSELECT ;EP
 S ACMANS=Y,ACMC="" F ACMI=1:1 S ACMC=$P(ACMANS,",",ACMI) Q:ACMC=""  S ACMCRIT=ACMSEL(ACMC),ACMPCNT=ACMPCNT+1 D
 .I ACMCTYP="F" S Y=0 G SET
 .S DIR(0)="N^2:80:0",DIR("A")="Enter Column width for "_$P(^ACM(58.1,ACMCRIT,0),U)_" (suggested: "_$P(^ACM(58.1,ACMCRIT,0),U,7)_")",DIR("B")=$P(^(0),U,7) D ^DIR K DIR S:$D(DUOUT) DIRUT=1
 .I $D(DIRUT) S Y=$P(^ACM(58.1,ACMCRIT,0),U,7)
SET .S ^ACM(58.8,ACMRPT,12,0)="^9002258.81102PA^1^1" ;IHS/CMI/LAB
 .I $D(^ACM(58.8,ACMRPT,12,"B",ACMCRIT)) S X=$O(^ACM(58.8,ACMRPT,12,"B",ACMCRIT,"")),ACMTCW=ACMTCW-$P(^ACM(58.8,ACMRPT,12,X,0),U,2)-2,^ACM(58.8,ACMRPT,12,X,0)=ACMCRIT_U_Y D  Q
 ..Q
 .S ^ACM(58.8,ACMRPT,12,ACMPCNT,0)=ACMCRIT_U_Y,^ACM(58.8,ACMRPT,12,"B",ACMCRIT,ACMPCNT)="",ACMTCW=ACMTCW+Y+2
 .I ACMCTYP="P" W !!?15,"Total Report width (including column margins - 2 spaces):   ",ACMTCW ;IHS/CMI/LAB
 .Q
 Q
I ;DIC CALL WITH SCREEN
 K DIC,DA,DR
 S DIC=$P(^ACM(58.1,ACMCRIT,0),U,13) I DIC="" W !!,"ERROR IN CONTROL TABLE." H 4 Q
 S DIC(0)="AEMQ",DIC("A")="ENTER "_$P(^ACM(58.1,ACMCRIT,0),U)_":  "
 S ACMDIC2="^ACM("_$E(DIC,6,99)_")"
 S DIC("S")="I $D(@ACMDIC2@(+Y,""RG"",""B"",ACMRG))"
 D ^DIC K DIC,DA,DR
 I Y=-1 Q
 S ^ACM(58.8,ACMRPT,11,ACMCRIT,0)=ACMCRIT,^ACM(58.8,ACMRPT,11,"B",ACMCRIT,ACMCRIT)=""
 S ACMCNT=ACMCNT+1,^ACM(58.8,ACMRPT,11,ACMCRIT,11,ACMCNT,0)=$P(Y,U),^ACM(58.8,ACMRPT,11,ACMCRIT,11,"B",$P(Y,U),ACMCNT)="",^ACM(58.8,ACMRPT,11,ACMCRIT,11,0)="^9002258.8110101A^"_ACMCNT_"^"_ACMCNT
 G I
 Q
Q ;EP
 K ^XTMP("ACMRL",$J,"QMAN"),^UTILITY("AMQQ TAX",$J)
 K DIC,X,Y,DD S X=$P(^ACM(58.1,ACMCRIT,0),U,3),DIC="^AMQQ(5,",DIC(0)="EQXM",DIC("S")="I $P(^(0),U,14)" D ^DIC K DIC,DA,DINUM,DICR I Y=-1 W "OOPS - QMAN NOT CURRENT - QUITTING" Q
 S ACMQMAN=+Y
 D ^AMQQGTX0(ACMQMAN,"^XTMP(""ACMRL"",$J,""QMAN"",")
 I '$D(^XTMP("ACMRL",$J,"QMAN")) W !!,$C(7),"** No ",$P(^ACM(58.1,ACMCRIT,0),U)," selected, all will be included." Q
 I $D(^XTMP("ACMRL",$J,"QMAN","*")) K ^XTMP("ACMRL",$J,"QMAN")
 S ^ACM(58.8,ACMRPT,11,ACMCRIT,0)=ACMCRIT,^ACM(58.8,ACMRPT,11,"B",ACMCRIT,ACMCRIT)=""
 S X="",Y=0 F  S X=$O(^XTMP("ACMRL",$J,"QMAN",X)) Q:X=""  S Y=Y+1,^ACM(58.8,ACMRPT,11,ACMCRIT,11,Y,0)=X,^ACM(58.8,ACMRPT,11,ACMCRIT,11,"B",X,Y)="",^ACM(58.8,ACMRPT,11,ACMCRIT,11,0)="^9002258.8110101A^"_Y_"^"_Y
 K X,Y,Z,ACMQMAN,V
 K ^XTMP("ACMRL",$J,"QMAN")
 Q
R ;EP
 S DIR(0)=$P(^ACM(58.1,ACMCRIT,0),U,4)_"O",DIR("A")="ENTER "_$P(^(0),U) D ^DIR K DIR S:$D(DUOUT) DIRUT=1
 Q:$D(DIRUT)
 I Y="" Q
 S ^ACM(58.8,ACMRPT,11,ACMCRIT,0)=ACMCRIT,^ACM(58.8,ACMRPT,11,"B",ACMCRIT,ACMCRIT)=""
 S ACMCNT=ACMCNT+1,^ACM(58.8,ACMRPT,11,ACMCRIT,11,ACMCNT,0)=$P(Y,U),^ACM(58.8,ACMRPT,11,ACMCRIT,11,"B",$P(Y,U),ACMCNT)="",^ACM(58.8,ACMRPT,11,ACMCRIT,11,0)="^9002258.8110101A^"_ACMCNT_"^"_ACMCNT
 G R
 Q
D ;EP;DATE RANGE
BD ;get beginning date
 ;cmi/anch/maw 9/6/2007 patch 7 begin mods
 ;W ! S DIR(0)="D^::EP",DIR("A")="Enter beginning "_ACMTEXT_" for Search" D ^DIR K DIR S:$D(DUOUT) DIRUT=1
 W ! S %DT="AE",%DT("A")="Enter beginning "_ACMTEXT_" for Search:  " D ^%DT
 ;I $D(DIRUT) Q
 Q:'Y  
 S ACMBD=Y
 ;cmi/anch/maw 9/6/2007 patch 7 end mods
ED ;get ending date IHS/CMI/TMJ PATCH #6 date display
 ;cmi/maw patch 7 10/1/2006 changed date display back to the way it was at the date
 ;shown in parens is a fileman thing that cannot be controlled.  and the patch 6 date
 ;control did not work
 ;cmi/anch/maw 9/6/2007 patch 7 begin mods
 ;W ! S DIR(0)="D^"_ACMBD_"::EX",DIR("A")="Enter ending "_ACMTEXT_" for Search" S Y=ACMBD D DD^%DT S DIR("B")=Y,Y="" D ^DIR K DIR S:$D(DUOUT) DIRUT=1
 W ! S %DT="AE",%DT(0)=ACMBD,%DT("A")="Enter ending "_ACMTEXT_" for Search:  ",%DT("B")=$$FMTE^XLFDT(ACMBD) D ^%DT
 ;I $D(DIRUT) G BD
 G BD:'Y
 ;cmi/anch/maw 9/6/2007 end of mods
 S ACMED=Y
 S X1=ACMBD,X2=-1 D C^%DTC S ACMSD=X
 ;
 S ^ACM(58.8,ACMRPT,11,ACMCRIT,0)=ACMCRIT,^ACM(58.8,ACMRPT,11,"B",ACMCRIT,ACMCRIT)=""
 S ACMCNT=0,^ACM(58.8,ACMRPT,11,ACMCRIT,11,ACMCNT,0)="^9002258.8110101A^1^1" S ACMCNT=ACMCNT+1,^ACM(58.8,ACMRPT,11,ACMCRIT,11,1,0)=ACMBD_U_ACMED,^ACM(58.8,ACMRPT,11,ACMCRIT,11,"B",ACMBD,ACMCNT)=""
 Q
N ;
 D N^ACMRL01
 Q
F ;FREE TEXT RANGE
 D F^ACMRL01
 Q
J ;
 D J^ACMRL01
 Q
Y ;
 D Y^ACMRL01
 Q
W ;EP - contains
 K DIR,DTOUT,DUOUT,DIRUT
 W !!,?5,"What phrase do you want to search for in the ",$P(^ACM(58.1,ACMCRIT,0),U),"?",!
 S DIR(0)="FO^2:40",DIR("A")=$P(^ACM(58.1,ACMCRIT,0),U)_" - CONTAIN" D ^DIR K DIR S:$D(DUOUT) DIRUT=1
 Q:$D(DIRUT)
 Q:Y=""
 S ^ACM(58.8,ACMRPT,11,ACMCRIT,0)=ACMCRIT,^ACM(58.8,ACMRPT,11,"B",ACMCRIT,ACMCRIT)=""
 S ACMCNT=ACMCNT+1,^ACM(58.8,ACMRPT,11,ACMCRIT,11,ACMCNT,0)=$P(Y,U),^ACM(58.8,ACMRPT,11,ACMCRIT,11,"B",$P(Y,U),ACMCNT)="",^ACM(58.8,ACMRPT,11,ACMCRIT,11,0)="^9002258.8110101A^"_ACMCNT_"^"_ACMCNT
 G W
 ;
 Q
