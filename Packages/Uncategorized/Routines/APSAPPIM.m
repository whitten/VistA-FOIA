APSAPPIM ;IHS/ASDS/ENM/POC - PRINTS THE PATIENT MEDICATION SHEETS  [ 11/13/2003  4:12 PM ];09-Oct-2008 11:24;SM
 ;;7.0;IHS PHARMACY MODIFICATIONS;**1007**;Sep 23, 2004
 ; Modified - IHS/CIA/PLS - 01/21/04
 ;            IHS/MSC/PLS - 08/19/08 - Line EN1+8
EN ;EP - NORMAL ENTRY POINT FROM APSPNE4 PRINT LINE
 S PSNDFN=DFN
DEV ;
 I $G(APSPCPP)]"" D  ;
 .S APSPIEND=$O(^%ZIS(1,"B",APSPCPP,""))
 .I (APSPIEND]"")&($P(^%ZIS(1,APSPIEND,0),"^",2)=0) S IOP=APSPCPP Q  ;SLAVED
 .I (APSPIEND]"")&($P(^%ZIS(1,APSPIEND,0),"^",2)=IO(0)) S IOP=APSPCPP Q  ;HOME DEVICE
 .S IOP="Q;"_APSPCPP Q  ;ALL OTHER
 S %ZIS="Q" D ^%ZIS
 I POP W !,"BUSY...WAIT A FEW MINUTES" S %=1 D YN^DICN H 5 G:%=1 DEV G END
 I $D(IO("Q")) D  D ^%ZTLOAD D HOME^%ZIS K IO("Q") Q
 .S ZTRTN="PMI^APSAPPIM",ZTDESC="PRINT PHARMACY PMIS"
 .S ZTDTH=$H
 .F I="PPL","APSPCPP","PSNDFN" S ZTSAVE(I)=""
 D PMI
 Q
 ;
EN1 ;ENTRY FROM MENU OPTION FOR SINGLE PATIENT
 N PPL,PSOSD,DFN,APSQRXN,APSQIEN,APSQDRG,APSQ
 N PSRX,PSNDFN,NAM,APSDNAM,APSDOC,APSPNDC,CR,DFN,ED,ED1,ED2,ED3,NDC,NUM
 N P2,PG,PPI,PPL,PS,PSODFN,SSN,SEX,VA,VADM,XNDC
 N Z,PSCNT,PSOCT,PSODTCUT,PSOERR,PSOLIST,PSOOPT,PSOSTA,RXN,STP,STR
 N J,JJ,IEN,DIWI,DIW,DIWT,DN,APSPIRN,APSPZDT
 N POP,VAERR,CNT,DIR,DIWTC,DOB,DRG,IOHG,DIWX,IOBS
 D PAT^APSPNUM
 Q:'$D(PSODFN)  ;IHS/MSC/PLS - 08/19/08
 S PSONUM="LIST"
 D EN^APSPNUM
 G:'$D(PSOLIST) END
 S PPL=PSOLIST(1) ;BETTER WATCH THIS AS IT CAN BE TOO LONG
 G EN
 ;
PMI ;SET UP AND PRINT
 U IO ;POC 04/10/01
 I $E(IOST)="P" S APSQCOM=$P($G(^%ZIS(2,IOST(0),12.1)),"^"),APSQRSET=$P($G(^(6)),"^") W:APSQCOM]"" @APSQCOM ;COMPRESS IF A PRINTER TO 16 PITCH
 Q:'$D(PPL)  ;NO RXS TO PRINT SO NO MED SHEETS TO PRINT EITHER
 F I=1:1 S APSQIEN=$P(PPL,",",I) Q:'APSQIEN  D
 .S APSQDRG=$P(^PSRX(APSQIEN,0),"^",6)
 .Q:$$TEST(APSQDRG)
 .S APSQRXN=+^PSRX(APSQIEN,0)
 .S APSDOC=$P(^PSRX(APSQIEN,0),"^",4),APSDNAM=$P(^VA(200,APSDOC,0),"^",1)
 .D PICK
 .S APSQ(I)=PPI_"^"_APSQDRG_"^"_APSQRXN ;SO ONLY GET ONE OF SHEET EVEN IF TWO DRUGS POINT TO SAME PATIENT INFORMATION SHEET
 .Q
 S NUM=1
PRINT S JJ=0 F  S JJ=$O(APSQ(JJ)) Q:'JJ  S PPI=$P(APSQ(JJ),"^",1),DRG=$P(APSQ(JJ),"^",2),PSRX=$P(APSQ(JJ),"^",3) D EP2^APSAPPIP
END ;EP - POC 04/10/01
 D ^%ZISC
 S:$D(ZTQUEUED) ZTREQ="@"
 Q
TEST(N) ;TEST FOR ENTRY OF PATIENT MED SHEET IN ^PSPPI GLOBAL
 I $S('$G(^PSDRUG(+N,"I")):1,DT'>^("I"):1,1:0) Q 0
 E  Q 1 ;1=NO GO
 Q
PICK ;select a drug from file 50
 I '$D(APSQDRG) Q
 I '$D(^APSAPPI) W !,"Patient Medication Instruction Sheets data has not been installed",!! G EXIT
 S DRG=APSQDRG
 S X=$P($G(^PSDRUG(DRG,2)),U,4)
 I X="" S PPI=.5 Q
 D ^APSPMDD S NDC=X D ECK  ;GET NDC AND REMOVE DASHES
 I PPI=""!(PPI=0) S PPI=.5,P2=1
 Q
ECK ;
 S RN=0,PPI=0,XNDC=0 F  S XNDC=$O(^APSAMDF("B",XNDC)) Q:XNDC=""  I XNDC=NDC  D  Q  ;
 .S RN=$O(^APSAMDF("B",XNDC,RN))
 .S PPI=^APSAMDF(RN,3)
 .I $G(^APSAPPI(PPI,0))="" S PPI=.5
EXIT Q
