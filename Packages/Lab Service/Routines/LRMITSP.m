LRMITSP ; IHS/DIR/FJE - MICRO TREND PROCESS 16:59 ; 
 ;;5.2;LR;**1013**;JUL 15, 2002
 ;
 ;;5.2;LAB SERVICE;**96**;Sep 27, 1994
 ; LRATS  inverse start time
 ; LRFBEG  formatted begin time
 ; LRFEND  formatted end time
 ; LRTSAL  inverse end time
 ;
DQ ; dequeued from LRMITS
 ; initialize variables
 S:$D(ZTQUEUED) ZTREQ="@" K ^TMP($J)
 S LRFBEG=$$FMTE^XLFDT(LRTBEG),LRFEND=$$FMTE^XLFDT(LRTEND),LRTEND=$S($E(LRTEND,6,7)="00":LRTEND+99,1:LRTEND+.24) ;,LRTEND=$E(LRTEND,1,5)_99
 S LRATS=9999999-LRTBEG,LRTSAL=9999999-LRTEND
 D ^LRMITSRS
 S LRDN=0 F  S LRDN=$O(^LAB(62.06,"AD",LRDN)) Q:LRDN<2  S LRANTIN=$O(^(LRDN,0)) I LRANTIN D
 .S LRX=$G(^LAB(62.06,LRANTIN,0)),LRANTINM=$P(LRX,U,5) Q:'$L(LRANTINM)
 .S LRANTIF=$S($P(LRX,U,4):+$P(LRX,U,4),1:"") I LRANTIF S LRANTIF=$$VALUE^LRMITSPE(LRANTIF,62.06,5) I $L(LRANTIF) S LRANTIF=$E(LRANTIF)_$$LOW^XLFSTR($E(LRANTIF,2,99))
 .S ^TMP($J,"A",LRDN)=LRANTIN_U_LRANTINM,^TMP($J,"AB",LRANTINM)=LRDN_U_LRANTIF
 .S:LRSORT ^TMP($J,"PSRT",$P(LRX,U,7))=LRANTINM
 S LRDN=2 F  S LRDN=$O(^DD(63.39,"GL",LRDN)) Q:LRDN<2  S LRANTIN=$O(^(LRDN,1,0)) I LRANTIN D
 .S LRX=$G(^DD(63.39,LRANTIN,0)),LRANTINM=$P(LRX,U) Q:'$L(LRANTINM)
 .S ^TMP($J,"T",LRDN)=LRANTINM
 K LRANTIF,LRANTIN,LRANTINM,LRDN,LRTBEG,LRTEND,LRX
 ; extract data
 D ^LRMITSPE I LREND Q
 ; count and merge data
 D ^LRMITSPC I LREND Q
 ; report data
 D ^LRMITSR
 D CLEANUP^LRMITS
 Q
