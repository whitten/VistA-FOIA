SROPCE0 ;BIR/ADM-PCE Filing Status Report ; [ 09/22/98  11:39 AM ]
 ;;3.0; Surgery ;**58,62,69,77,50**;24 Jun 93
 W @IOF,!,?26,"Report of PCE Filing Status",!!,"This report displays the filing status of completed cases performed during the",!,"selected date range.",!
 S (SRFLG,SRSOUT)=0,SRSPEC=""
ASK W ! K DIR S DIR("A",1)="Print PCE filing status of completed cases for",DIR("A",2)="",DIR("A",3)="1. O.R. Surgical Procedures",DIR("A",4)="2. Non-O.R. Procedures"
 S DIR("A",5)="3. Both O.R. Surgical Procedures and Non-O.R. Procedures (All Specialties)",DIR("A",6)="",DIR("A")="Select Number (1, 2 or 3): ",DIR("B")="1"
 S DIR(0)="NA^1:3:0" D ^DIR K DIR I $D(DTOUT)!$D(DUOUT) S SRSOUT=1 G END
 S SRFLG=Y I SRFLG=1 D SPEC G:SRSOUT END
 I SRFLG=2 D MSP G:SRSOUT END
DATE D DATE^SROUTL(.SDATE,.EDATE,.SRSOUT) G:SRSOUT END
FORM W ! K DIR S DIR("A")="Print the long form or the short form ? ",DIR("B")="SHORT",DIR(0)="SAM^L:LONG;S:SHORT" D ^DIR K DIR I $D(DTOUT)!$D(DUOUT) S SRSOUT=1 G END
 S SRFORM=Y I Y="L" W !!,"This report is designed to use a 132 column format."
 W ! K %ZIS,IOP,IO("Q"),POP S %ZIS("A")="Print the PCE Filing Status Report to which Printer ? ",%ZIS="Q" D ^%ZIS I POP S SRSOUT=1 G END
 I $D(IO("Q")) K IO("Q") S ZTDESC="PCE FILING STATUS REPORT",(ZTSAVE("EDATE"),ZTSAVE("SRFORM"),ZTSAVE("SDATE"),ZTSAVE("SRSITE*"),ZTSAVE("SRSPEC*"),ZTSAVE("SRFLG"))="",ZTRTN="EN^SROPCE0" D ^%ZTLOAD S SRSOUT=1 G END
EN U IO S SRSOUT=0,(SRHDR,SRPAGE)=1,SRSDT=SDATE-.0001,SRSEDT=EDATE+.9999,Y=SDATE X ^DD("DD") S STARTDT=Y,Y=EDATE X ^DD("DD") S ENDATE=Y F I=1:1:6 S CNT(I)=0
 S SRRPT="PCE FILING STATUS REPORT",SRTITLE="For Completed "_$S(SRFLG=1:"O.R. Surgical Procedures",SRFLG=2:"Non-O.R. Procedures",1:"O.R. Surgical and Non-O.R. Procedures"),SRFRTO="From: "_STARTDT_"  To: "_ENDATE
 S SRINST=SRSITE("SITE") D NOW^%DTC S Y=$E(%,1,12) X ^DD("DD") S SRPRINT="Report Printed: "_Y
 I SRFORM="L" D ^SROPCE0A G END
 D ^SROPCE0B
END W:$E(IOST)="P" @IOF I $D(ZTQUEUED) Q:$G(ZTSTOP)  S ZTREQ="@" Q
 I 'SRSOUT,$E(IOST)'="P" W !!,"Press RETURN to continue  " R X:DTIME
 D ^%ZISC K SRDIV,SRDX,SRFCPT,SRFICD,SRFRTO,SRINOUT,SRPARAM,SRPODX,SRQCPT,SRQICD,SRRPT,SRSCHED,SRSPS,SRSR,SRTN,SRUCPT,SRUICD D ^SRSKILL W @IOF
 Q
SPEC W @IOF,! S DIR("?",1)="Enter YES if you would like the report printed for all Surgical Specialties",DIR("?")="or enter NO to select a specific specialty."
 S DIR("A")="Do you want the report for all Surgical Specialties ? ",DIR("B")="YES",DIR(0)="YA" D ^DIR K DIR I $D(DTOUT)!$D(DUOUT) S SRSOUT=1 Q
 I 'Y W ! K DIC S DIC=137.45,DIC(0)="QEAMZ",DIC("A")="Select Surgical Specialty: ",DIC("S")="I '$P(^(0),""^"",3)" D ^DIC K DIC S:Y<0 SRSOUT=1 Q:Y<0  S SRSPEC=+Y,SRSPECN=$P(Y(0),"^")
 Q
MSP W @IOF,! S DIR("?",1)="Enter YES if you would like the report printed for all Medical Specialties",DIR("?")="or enter NO to select a specific specialty."
 S DIR("A")="Do you want the report for all Medical Specialties ? ",DIR("B")="YES",DIR(0)="YA" D ^DIR K DIR I $D(DTOUT)!$D(DUOUT) S SRSOUT=1 Q
 I 'Y W ! K DIC S DIC=723,DIC(0)="QEAMZ",DIC("A")="Select Medical Specialty: " D ^DIC K DIC S:Y<0 SRSOUT=1 Q:Y<0  S SRSPEC=+Y,SRSPECN=$P(Y(0),"^")
 Q
CHK ; set up array of fields missing data
 K SRX,DA,DIC,DIQ,DR,SRY S DIC="^SRF(",DA=SRTN,DIQ="SRY",DIQ(0)="I" D  D EN^DIQ1
 .I SRNON S DR="27;66;119;121;122;123;"_$S(SRSR'=0:"124;",1:"")_"125;"
 .I 'SRNON S DR=".02;.04;.14;"_$S(SRSR'=0:".164;",1:"")_".205;.232;27;66;"
 .I $P(^SRO(133,SRSITE,0),"^",16) S DR=DR_".0155;"
 .I SRSTATUS=5 S DR=DR_".011;"
 D CLINIC
 S SRZ=0 F  S SRZ=$O(SRY(130,SRTN,SRZ)) Q:'SRZ  I SRY(130,SRTN,SRZ,"I")="" D TR S X=$T(@SRP),SRFLD=$P(X,";;",2),SRX(SRZ)=$P(SRFLD,"^",2)
OTH I $O(^SRF(SRTN,13,0)) S SROTH=0 F  S SROTH=$O(^SRF(SRTN,13,SROTH)) Q:'SROTH  I '$P($G(^SRF(SRTN,13,SROTH,2)),"^") S SRX(.42)="OTHER PROCEDURE CPT CODE" Q
 I $O(^SRF(SRTN,15,0)) S SROTH=0 F  S SROTH=$O(^SRF(SRTN,15,SROTH)) Q:'SROTH  I '$P($G(^SRF(SRTN,15,SROTH,0)),"^",3) S SRX(.74)="OTHER POST-OP DIAGNOSIS ICD CODE" Q
 Q
CLINIC N SRCLINIC S SRCLINIC=$P(^SRF(SRTN,0),"^",21) D
 .I SRNON S:SRCLINIC="" SRCLINIC=$P(^SRF(SRTN,"NON"),"^",2) Q
 .S:SRCLINIC="" SRCLINIC=$P(^SRO(137.45,$P(^SRF(SRTN,0),"^",4),0),"^",5) I SRCLINIC="",$P(^SRF(SRTN,0),"^",2) S SRCLINIC=$P(^SRS($P(^SRF(SRTN,0),"^",2),0),"^")
 I SRCLINIC,'$$CLINIC^SROUTL(SRCLINIC,SRTN) S SRCLINIC=""
 S SRY(130,SRTN,.021,"I")=SRCLINIC
 Q
TR S SRP=SRZ,SRP=$TR(SRP,"1234567890.","ABCDEFGHIJP")
 Q
PJAA ;;.011^IN/OUT-PATIENT STATUS
PJAEE ;;.0155^CLASSIFICATION INFORMATION
PJB ;;.02^OPERATING ROOM
PJBA ;;.021^ASSOCIATED CLINIC
PJD ;;.04^SURGERY SPECIALTY
PAFD ;;.164^ATTEND SURG
PBJE ;;.205^TIME PAT IN OR
PBCB ;;.232^TIME PAT OUT OR
BG ;;27^PRINCIPAL PROCEDURE CODE
FF ;;66^PRIN DIAGNOSIS CODE
AAI ;;119^NON-OR LOCATION
ABA ;;121^TIME PROCEDURE BEGAN
ABB ;;122^TIME PROCEDURE ENDED
ABC ;;123^PROVIDER
ABD ;;124^ATTEND PROVIDER
ABE ;;125^MEDICAL SPECIALTY
