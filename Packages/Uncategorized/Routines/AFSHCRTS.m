AFSHCRTS ; IHS/OIRM/DSD/JDM -PROGRAM TO SET LOCAL VARIABLES FOR CRT CHARACTERISTICS ; [ 10/27/2004   4:20 PM ]
 ;;3.0T1;AO FINANCIAL DATA MGMT SYSTEM;**13**;FEB 02, 1999
 ;;MODIFIED FOR CACHE' COMPLIANCE ;ACR*2.1*9
A0 U IO(0) W !,"NOT AN ENTRY POINT" Q
CRTSETUP ;EP
 I '$G(IOST(0)) D HOME^%ZIS K IOP
 D ^XBKVAR                            ;ACR*2.1*13.02 IM13574
 S AFSAST="********************"      ;ACR*2.1*13.02 IM13574
 S AFSZX=IOS,XY=$P(^%ZIS(2,IOST(0),1),"^",5)
 I '$D(^%ZIS(1,AFSZX,"SUBTYPE")) G BTRMDEF
 I '$D(^%ZIS(1,AFSZX,"TYPE")) G BTRMDEF
 I ^%ZIS(1,AFSZX,"TYPE")'["TRM" G BTRMDEF
 S AFSZY=^%ZIS(1,AFSZX,"SUBTYPE")
 I '$D(^%ZIS(2,AFSZY,5)) G BTRMDEF
 S AFSLE=$P(^%ZIS(2,^%ZIS(1,AFSZX,"SUBTYPE"),5),"^",6)
 S AFSLE="W "_AFSLE
 S AFSRVON=$P(^%ZIS(2,^%ZIS(1,AFSZX,"SUBTYPE"),5),"^",4)
 S AFSRVOF=$P(^%ZIS(2,^%ZIS(1,AFSZX,"SUBTYPE"),5),"^",5)
 S X=0 X ^%ZOSF("RM")
 S AFSAD="X XY W $E(AFSAST,1,%L) X XY"
 Q
 ;
BTRMDEF W !!,*7,?10,"TERMINAL CHARACTERISTICS NOT DEFINED PROPERLY",!,?15,"CONTACT YOUR SITE MANAGER FOR ASSISTANCE" H 5 Q
