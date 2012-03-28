APSPORXA ; IHS/DSD/ENM - enter outside rx ;  [ 02/20/2001  1:50 PM ]
 ;;6.0;IHS PHARMACY MODIFICATIONS;**1,3**;09/03/97
 ;
START ;EP - called from option
HDR ;write header
 W:$D(IOF) @IOF
 F J=1:1:7 S X=$P($T(TEXT+J),";;",2) W !?80-$L(X)\2,X
 K X,J
 W !!
 ;
BEGIN ;
 D INIT
 G:APSPQUIT EXIT
 D GETPAT
 G:DFN="" EXIT
 ;F  D GETDRUG Q:APSPQUIT!(APSPDIEN="")  D PROCESS
 ;F  D GETDRUG  Q:APSPQUIT!(APSPDIEN="")  D ^APSQDRG S:PSORX("DFLG") APSPQUIT=1 K PSORX("DFLG") Q:$G(APSPQUIT)  D ^APSQDRDU S:$G(PSORX("DFLG")) APSPQUIT=1 K PSORX("DFLG") Q:$G(APSPQUIT)  D PROCESS ;IHS/OKCAO/POC
 F  D GETDRUG  Q:APSPQUIT!(APSPDIEN="")  S X="APSQDRG" X ^%ZOSF("TEST") D:$T ^APSQDRG S:PSORX("DFLG") APSPQUIT=1 K PSORX("DFLG") Q:$G(APSPQUIT)  D PROCESS ;IHS/OKCAO/POC
 D EXIT
 Q
PROCESS ;
 D GETDATE
 Q:APSPDATE=""
 D GETOL
 Q:APSPOL=""
 D GENVISIT
 Q:APSPQUIT
 D GENVMED
 Q
EXIT ;cleanup and exit
 K DFN,APCDLOOK,APCDANE,APCDALVR,APCDVSIT,APCDPAT
 K APSPOIEN,APSPDRUG,APSPDIEN,APSPOL,APSPDATE,APSPQUIT ;IHS/DSD/ENM APSPDOL REMOVED
 D KILL^AUPNPAT K AUPNTALK,AUPNLK("ALL")
 K DIE,DR,DA,DIRUT,DIR,DTOUT,DUOUT,DIV,DIW,DQ,DD,DQ,DI,DIC,DIR,X,Y,I,DDH,DI,D,D0
 Q
INIT ;EP
 S APSPQUIT=0
 S APSPOIEN=$O(^PSDRUG("B","OUTSIDE DRUG",0)) I 'APSPOIEN W !!,$C(7),$C(7),"OUTSIDE DRUG not defined in DRUG file, notify supervisor." H 3 S APSPQUIT=1 Q
 ;I APCDFLG S APSPQUIT=1 Q
 I '$D(PSOPAR) D ^PSOLSET ;set up pharmacy site parameters
 ;S APSPDOL=4472
 I '$G(APSPDOL) W !!,"DEFAULT OTHER LOCATION NOT DEFINED IN PHARMACY SITE FILE!!  NOTIFY YOUR SUPERVISOR" H 3 S APSPQUIT=1 Q
 Q
GETDATE ;EP GET DATE OF ENCOUNTER
 K DIR S APSPDATE="",DIR(0)="DO^:"_DT_":EPT",DIR("A")="Enter DATE DISPENSED" D ^DIR K DIR S:$D(DUOUT) DIRUT=1
 Q:$D(DIRUT)
 S %DT="ET" D ^%DT G:Y<0 GETDATE
 I Y>DT W "  <Future dates not allowed>",$C(7),$C(7) K X G GETDATE
 S APSPDATE=Y
 Q
GETOL ;
 S APSPOL=""
 S DIR(0)="9000010,2101",DIR("A")="Enter LOCATION WHERE DISPENSED" K DA D ^DIR K DIR
 S APSPOL=Y
 Q
GETDRUG ;
 S (APSPDRUG,APSPDIEN)=""
 W !! K DD,DIR S DIR(0)="FO^1:40",DIR("A")="Enter DRUG NAME" D ^DIR K DIR S:$D(DTOUT) DIRUT=1
 Q:Y=""
 I $D(DIRUT) W !,"No drug entered!!" Q
 S (X,APSPDRUG)=Y,DIC="^PSDRUG(",DIC(0)="MQE" D ^DIC K DIC
 I Y=-1 D NODRUG Q
 S APSPDIEN=+Y,APSPDRUG="" K DIR,DA,DIRUT,DTOUT,DUOUT
 Q
 ;
NODRUG ;
 W !,"That drug cannot be found in the Drug file."
 K DIR S DIR(0)="Y",DIR("A")="Do you want to try to lookup the drug in the Drug file again",DIR("B")="Y" D ^DIR K DIR S:$D(DUOUT) DIRUT=1
 I $D(DIRUT) W !,"Exiting..." S APSPQUIT=1 Q
 I Y G GETDRUG
 S APSPDIEN=APSPOIEN
 W ! K DD,DIR S DIR(0)="FO^1:40",DIR("A")="Enter the FULL DRUG NAME...",DIR("A",1)="You must enter the drug name that will appear on the health summary.",DIR("B")=APSPDRUG D ^DIR K DIR S:$D(DTOUT) DIRUT=1
 I $D(DIRUT) S APSPQUIT=1 W !!,"Exiting..." Q
 S APSPDRUG=Y
 S APSPDRUG=$TR(APSPDRUG,"-") ;IHS/OKCAO/POC 4/22/98
 Q
GENVISIT ;
 K APCDALVR
 S APCDALVR("AUPNTALK")=""
 S APCDALVR("APCDDATE")=APSPDATE
 S APCDALVR("APCDTYPE")="O"
 S APCDALVR("APCDPAT")=DFN
 S APCDALVR("APCDLOC")=$G(APSPDOL)
 S APCDALVR("APCDCAT")="E"
 S APCDALVR("APCDAUTO")="",APCDALVR("APCDANE")=""
 S APCDALVR("APCDOLOC")=APSPOL
 D ^APCDALV
 I $D(APCDALVR("APCDAFLG")) W !!,$C(7),"Creating PCC Visit Failed....Notify Supervisor" H 3 S APSPQUIT=1 Q
 S APCDVSIT=APCDALVR("APCDVSIT"),APCDPAT=APCDALVR("APCDPAT") S Y=DFN D ^AUPNPAT
 Q
GENVMED ;
 I '$G(APSPDIEN) W !!,"Error.... no drug entry" H 2 Q
 ;D ^APCDEA3
 W !!,"Please enter all available information about this prescription.",!
 S DA=APCDVSIT,DR="[APCD ORX (ADD)]",DIE="^AUPNVSIT(" D ^DIE
 I $D(Y) W !!,"Creating V Medication entry failed!!  Notify supervisor!" H 3 Q
 Q
GETPAT ;EP
 W !
 S DFN=""
 S DIC("A")="Enter PATIENT NAME:  ",DIC="^AUPNPAT(",DIC(0)="AEMQ" D ^DIC K DIC
 Q:Y<0
 S DFN=+Y
 ;ADD THESE 2 LINES TO BUILD DRUG ARRAY IHS/OKCAO/POC
 S PSODFN=+Y
 D ^PSOBUILD
 Q
 ;
TEXT ;
 ;;
 ;;IHS PHARMACY MODULE/PCC Interface
 ;;
 ;;*******************************
 ;;*    Entry of OUTSIDE RX's    *
 ;;*******************************
 ;;
