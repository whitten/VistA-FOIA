LROC1 ; IHS/DIR/FJE - TO CLEAN UP LAB ANCILLARY FILE 2/6/91 10:53 ;
 ;;5.2;LR;**1013**;JUL 15, 2002
 ;
 ;;5.2;LAB SERVICE;;Sep 27, 1994
 S X="T-14",%DT="" D ^%DT S LRKDT=Y,%H=$H-14
 S LAB="ET" D LAH
 S LAB="LOST" D LAD
 S LAB="SMAC" D LAD
 S LAB="TIME" D LAD
 K LAB,LRKDT,LRIX
 Q
LAH S LRIX="" F I=1:1 S LRIX=$O(^LA(LAB,LRIX)) Q:(LRIX<1)!(LRIX>%H)  K ^(LRIX)
 Q
LAD S LRIX="" F I=1:1 S LRIX=$O(^LA(LAB,LRIX)) Q:(LRIX<1)!(LRIX>LRKDT)  K ^(LRIX)
 Q
