ACMRGA02 ; IHS/TUCSON/TMJ - CREATES PRE-DIABETES REGISTER AT RUN TIME ; [ 09/19/05  1:18 PM ]
 ;;2.0;ACM CASE MANAGEMENT SYSTEM;*6*;JAN 10, 1996
 ;ROUTINE TO CREATE A REGISTER CALLED FROM ACMADDP
 ;IHS PRE-DIABETES REGISTER 8/22/05
 Q
 ;
REG ;EP;CHECKS AND CREATES REGISTER
 S ACMRG=$T(NAME+1),ACMRG=$P(ACMRG,";;",3) W !,ACMRG I $L(ACMRG),$D(^ACM(41.1,"B",ACMRG)) W !!,"Register:  ",ACMRG," already exists...quiting",!! Q
 W !!,"Register:  ",ACMRG," is being added...",!!
 S X=ACMRG,DIC="^ACM(41.1,",DIC(0)="LX" K DD,DA D FILE^DICN Q:+Y<1  S ACMRG=+Y K DIC,DA,DD,DR,DINUM,D
 F I=2:1 S ACMF=$T(NAME+I) Q:ACMF["*"  S ACMF($P(ACMF,";;",2))=$P(ACMF,";;",3)
 S ACMX=0 F  S ACMX=$O(ACMF(ACMX)) Q:ACMX=""  S ACMY=0,ACMY=$O(^DD(9002241.1,"B",ACMX,ACMY)) I +ACMY S $P(ACMF(ACMX),U,2)=+ACMY
 S DR="" S ACMX="" F  S ACMX=$O(ACMF(ACMX)) Q:ACMX=""  S ACMY=ACMF(ACMX) I $L($P(ACMY,U,1))&(+$P(ACMY,U,2)) S DR=DR_$P(ACMY,U,2)_"///"_$P(ACMY,U,1)_";"
 I $L(DR) S DR=$E(DR,1,$L(DR)-1)
 S DA=ACMRG,DIE="^ACM(41.1," D ^DIE S $P(^ACM(41.1,ACMRG,0),U,12)=1 K DIC,DIE,DA,DR
ADDS ;ADDS ELEMENTS,DIAG,COMPL, DX CRITERIA, RISK FACTORS ETC TO REGISTER
 D SETE,SETD,SETC,SETR,SETDC W "...FINISHED...",!!
 K ACMX,ACMEX,ACXE,ACZ,ACMCX,ACMDX,ACMRF,ACMDC
 Q
 ;
SETE ;EP;ADDS ELEMENTS TO NEW REGISTER     
 K ACME F I=1:1 S ACME=$T(ELEM+I) Q:ACME["*"  S ACME($P(ACME,";;",3))=$P(ACME,";;",3)
 W !,"adding elements..." S ACMEX="" F  S ACMEX=$O(ACME(ACMEX)) Q:ACMEX=""  K ACMY D SETE0 I $D(ACMY) D SETE1
 Q
SETE0 S:$D(^ACM(56,"B",ACMEX)) ACMY=1 W "." Q
SETE1 S ACMY=0,ACMY=$O(^ACM(56,"B",ACMEX,ACMY)) S (DA,X,DINUM)=ACMY,DA(1)=ACMRG
 K DIC,DD S (DIE,DIC)="^ACM(41.1,"_ACMRG_",2,",DIC(0)="L"
 S:'$D(^ACM(41.1,ACMRG,2,0)) ^ACM(41.1,ACMRG,2,0)="^9002241.13P^^"
 K DD,DO D FILE^DICN K DIC,DD,DR
 Q
 ;
SETD ;EP;ADDS DIAGNOSIS TO NEW REGISTER     
 W !,"adding diagnosis...." K ACME S ACME="" F I=1:1 S ACME=$T(DIAG+I) Q:ACME["*"  S ACME($P(ACME,";;",3))=$P(ACME,";;",3)
 S ACMDX="" F  S ACMDX=$O(ACME(ACMDX)) Q:ACMDX=""  D SETD0
 Q
SETD0 I $D(^ACM(44.1,"B",ACMDX)) S ACMY=0,ACMY=$O(^ACM(44.1,"B",ACMDX,ACMY)) W "." G SETD1
 K DIC,DD,DO S (DIE,DIC)="^ACM(44.1,",DIC(0)="L",X=ACMDX D FILE^DICN S ACMY=+Y
SETD1 S X=ACMRG,DA(1)=ACMY
 K DIC,DD S (DIE,DIC)="^ACM(44.1,"_ACMY_",""RG"",",DIC(0)="L"
 S:'$D(^ACM(44.1,ACMY,"RG",0)) ^ACM(44.1,ACMY,"RG",0)="^9002244.11P^^"
 I $D(^ACM(44.1,ACMY,"RG","B",ACMRG)) Q
 K DD,DO D FILE^DICN K DIC,DD,DR
 Q
 ;
SETC ;EP;ADDS COMPLICATIONS TO NEW REGISTER     
 W !,"adding complications....." K ACME S ACME="" F I=1:1 S ACME=$T(COMP+I) Q:ACME["*"  S ACME($P(ACME,";;",3))=$P(ACME,";;",3)
 S ACMCX="" F  S ACMCX=$O(ACME(ACMCX)) Q:ACMCX=""  D SETC0
 Q
SETC0 I $D(^ACM(42.1,"B",ACMCX)) S ACMY=0,ACMY=$O(^ACM(42.1,"B",ACMCX,ACMY)) W "." G SETC1
 K DIC,DD,DO S (DIE,DIC)="^ACM(42.1,",DIC(0)="L",X=ACMCX D FILE^DICN S ACMY=+Y
SETC1 S X=ACMRG,DA(1)=ACMY
 K DIC,DD S (DIE,DIC)="^ACM(42.1,"_ACMY_",""RG"",",DIC(0)="L"
 S:'$D(^ACM(42.1,ACMY,"RG",0)) ^ACM(42.1,ACMY,"RG",0)="^9002242.11P^^"
 I $D(^ACM(42.1,ACMY,"RG","B",ACMRG)) Q
 K DD,DO D FILE^DICN K DIC,DD,DR
 Q
 ;
NAME ;;
 ;;REGISTER TYPE;;IHS PRE-DIABETES;;.01
 ;;ABBREVIATION;;DM;;.02
 ;;AUTHORIZED USER;;
 ;;DATE ESTABLISHED;;T
 ;;DATE LAST MODIFIED;;
 ;;REGISTER DEVELOPER;;POSTMASTER
 ;;DEVELOPER PHONE;;
 ;;CONFIDENTIAL REGISTER;;YES
 ;;VIEW ALL LIST ENTRIES;;NO
 ;;ALLOW LAYGO FOR LIST ENTRIES;;NO
 ;;DESCRIPTION;;
 ;;LETTER ADDRESS STARTING LINE;;
 ;;LETTER ADDRESS STARTING COLUMN;;
 ;;PHONE NUMBER;;
 ;;LETTER SIGNATURE;;
 ;;LETTER HEAD LINE 1;;
 ;;LETTER HEAD LINE 2;;
 ;;PCC PROBLEM LIST;;YES
 ;;*
 ;;BRIEF DESCRIPTION;;IHS National Pre-Diabetes Register
 ;;*
ELEM ;;
 ;;ELEMENTS;;COMPLICATIONS
 ;;ELEMENTS;;DIAGNOSES
 ;;ELEMENTS;;RISK FACTORS
 ;;ELEMENTS;;DIAGNOSTIC CRITERIA
 ;;ELEMENTS;;REGISTER DATA
 ;;ELEMENTS;;CASE REVIEW DATES
 ;;*
COMP ;;
 ;;COMPLICATIONS;;HYPERLIPIDEMIA
 ;;COMPLICATIONS;;OBESITY - NOS
 ;;COMPLICATIONS;;MORBID OBESITY
 ;;COMPLICATIONS;;HYPERTENSION
 ;;COMPLICATIONS;;POLYCYSTIC OVARIES
 ;;COMPLICATIONS;;PROTEINURIA
 ;;COMPLICATIONS;;ACQUIRED ACANTHOSIS NIGRICANS
 ;;*
DIAG ;;
 ;;DIAGNOSIS;;IMP FASTING GLUCOSE (IFG)
 ;;DIAGNOSIS;;IMP GLUCOSE TOLERANCE (IGT)
 ;;DIAGNOSIS;;OTHER ABNORMAL GLUCOSE
 ;;DIAGNOSIS;;METABOLIC SYNDROME
 ;;*
RISK ;;
 ;;RISK FACTORS;;BMI > 25
 ;;RISK FACTORS;;HX OF GESTATIONAL DIABETES
 ;;RISK FACTORS;;POLYCYSTIC OVARY DISEASE
 ;;RISK FACTORS;;FAMILY HX - TYPE 2 DIABETES
 ;;RISK FACTORS;;MOTHER - GESTATIONAL DIABETES
 ;;*
DXCRIT ;;
 ;;DIAGNOSTIC CRITERIA;;WAIST CIRCUM >40 (MEN)
 ;;DIAGNOSTIC CRITERIA;;WAIST CIRCUM >35 (WOMEN)
 ;;DIAGNOSTIC CRITERIA;;BMI > 30
 ;;DIAGNOSTIC CRITERIA;;TG > 150 mg/dl
 ;;DIAGNOSTIC CRITERIA;;HDL < 40 mg/dl (MEN)
 ;;DIAGNOSTIC CRITERIA;;HDL < 50 (WOMEN)
 ;;DIAGNOSTIC CRITERIA;;BP > 130/85 mm Hg
 ;;DIAGNOSTIC CRITERIA;;FPG > 100 mg/dl
 ;;*
GDIAG S ACMX="" F I=1:1 S ACMX=$T(DIAG+I) Q:ACXE["*"  S ACME("SET",$P(ACMX,";;",3))=$P(ACMX,";;",3)
 Q
 ;
SETR ;EP;ADDS RISK FACTORS TO NEW REGISTER
 W !,"adding Risk Factors...." K ACME S ACME="" F I=1:1 S ACME=$T(RISK+I) Q:ACME["*"  S ACME($P(ACME,";;",3))=$P(ACME,";;",3)
 S ACMRF="" F  S ACMRF=$O(ACME(ACMRF)) Q:ACMRF=""  D SETR0
 Q
 ;
SETR0 I $D(^ACM(45.1,"B",ACMRF)) S ACMY=0,ACMY=$O(^ACM(45.1,"B",ACMRF,ACMY)) W "." G SETR1
 K DIC,DD,D0 S (DIE,DIC)="^ACM(45.1,",DIC(0)="L",X=ACMRF D FILE^DICN S ACMY=+Y
SETR1 S X=ACMRG,DA(1)=ACMY
 K DIC,DD S (DIE,DIC)="^ACM(45.1,"_ACMY_",""RG"",",DIC(0)="L"
 S:'$D(^ACM(45.1,ACMY,"RG",0)) ^ACM(45.1,ACMY,"RG",0)="^9002245.11P^^"
 I $D(^ACM(45.1,ACMY,"RG","B",ACMRG)) Q
 K DD,DO D FILE^DICN K DIC,DD,DR
 Q
 ;
SETDC ;EP;ADDS DIAGNOSTIC CRITERIA TO NEW REGISTER
 W !,"adding Diagnostic Criteria...." K ACME S ACME="" F I=1:1 S ACME=$T(DXCRIT+I) Q:ACME["*"  S ACME($P(ACME,";;",3))=$P(ACME,";;",3)
 S ACMDC="" F  S ACMDC=$O(ACME(ACMDC)) Q:ACMDC=""  D SETDC0
 ;
 Q
SETDC0 I $D(^ACM(51.1,"B",ACMDC)) S ACMY=0,ACMY=$O(^ACM(51.1,"B",ACMDC,ACMY)) W "." G SETDC1
 K DIC,DD,D0 S (DIE,DIC)="^ACM(51.1,",DIC(0)="L",X=ACMDC D FILE^DICN S ACMY=+Y
 ;
SETDC1 S X=ACMRG,DA(1)=ACMY
 K DIC,DD S (DIE,DIC)="^ACM(51.1,"_ACMY_",""RG"",",DIC(0)="L"
 S:'$D(^ACM(51.1,ACMY,"RG",0)) ^ACM(51.1,ACMY,"RG",0)="^9002251.12P^^"
 I $D(^ACM(51.1,ACMY,"RG","B",ACMRG)) Q
 K DD,DO D FILE^DICN K DIC,DD,DR
 Q
 ;
