LRHDR ;DALOI/CJS/RLM-HEALTH DEPARTMENT REPORT ;2/19/91 10:37 [ 04/11/2003  7:39 AM ]
 ;;5.2T9;LR;**1018*;Nov 17, 2004
 ;;5.2;LAB SERVICE;**272**;Sep 27, 1994
 ; Reference to ^%DT supported by DBIA #10003
 ; Reference to ^%ZIS supported by DBIA #10086
 ; Reference to ^%ZISC supported by DBIA #10089
 ; Reference to ^%ZTLOAD supported by DBIA #10063
 ; Reference to ADD^VADPT supported by DBIA #10061
 ; Reference to KVAR^VADPT supported by DBIA #10061
 ; Reference to $$FMTE^XLFDT supported by IA #10103
 ; Reference to $$NOW^XLFDT supported by IA #10103
 ; Reference to EN^DIQ supported by DBIA #10004
BEGIN D DATE
END D ^%ZISC K DA,IO("Q"),DFN,DR,I,LRDPF,POP,LRDT,LRIDT,ZTSK,LRDFN,LRBUG,LRACC,PNM,SSN,DOB,SEX,LRIO,LRTIME,LRPGM,ZTRTN,ZTIO,ZTDESC,ZTSAVE
 Q
DATE S %DT="AE" D ^%DT Q:Y<1  S LRDT=Y K %DT
 K DIC S %ZIS="Q" D ^%ZIS Q:POP  K %ZIS
 I $D(IO("Q")) S ZTRTN="DQ^LRHDR",ZTIO=ION,ZTSAVE("LRDT")="",ZTDESC="HEALTH DEPARTMENT REPORT" D ^%ZTLOAD W:$D(ZTSK) !,"REQUEST QUEUED" Q
 U IO D DISPLAY
 Q
DQ S:$D(ZTQUEUED) ZTREQ="@" U IO
DISPLAY S DIC="^DPT(",DR=.11 S LRBUG=0 F I=0:0 S LRBUG=$O(^LR("AD",LRDT,LRBUG)) Q:LRBUG<1  D LIST
 Q
LIST W !!!,?5,$P(^LAB(61.2,LRBUG,0),"^",1),! S LRACC="" F I=0:0 S LRACC=$O(^LR("AD",LRDT,LRBUG,LRACC)) Q:LRACC=""  S LRDFN=^(LRACC) D PATIENT
 Q
PATIENT S VA200="",LRDPF=$P(^LR(LRDFN,0),U,2),DFN=$P(^(0),U,3) D PT^LRX S:LRDPA=2 DOB=$P(VADM(3),U,2) I LRDPF'=2 S Y=DOB D DD^LRX
 ;----- BEGIN IHS MODIFICATIONS LR*5.2*1018
 ;W !,PNM,?25," ",SSN,?39," ",Y,?55," ",SEX," ",$$FMTE^XLFDT($$NOW^XLFDT,"")
 W !,PNM,?25," ",HRCN,?42," ",Y,?55," ",SEX D STAMP^LRX  ;IHS/ANMC/CLS 11/1/95
 ;----- END IHS MODIFICATIONS
 I LRDPF=2 D
 . ;N I,X,Y,N D ADD^VADPT
 .;----- BEGIN IHS MODIFICATIONS LR*5.2*1018
 . N I,X,Y,N D @$S($$ISPIMS^BLRUTIL:"ADD^VADPT",1:"ADD^BLRDPT")
 W:$L($G(VAPA(8))) !,"PHONE: ",VAPA(8) S DA=DFN D EN^DIQ
 ;D KVAR^VADPT
 D @$S($$ISPIMS^BLRUTIL:"KVAR^VADPT",1:"KVAR^BLRDPT")
 ;----- END IHS MODIFICATIONS
 Q
