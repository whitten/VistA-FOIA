APCLP8 ; IHS/CMI/LAB - post init to patch 8 ;
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
 ;
 D ^APCLBUL
 S ATXX=$O(^ATXAX("B","APCL DIABETES REG NEW CASE",0)) I ATXX D ZTM^ATXAX
 S ATXX=$O(^ATXAX("B","APCL DIABETES REG COMPLICATION",0)) I ATXX D ZTM^ATXAX
 ;add new report to menu
 NEW X
 S X=$$ADD^XPDMENU("APCL M DX/PROC COUNT REPORTS","APCL P RX RELEASE","RXDA")
 I 'X W "Attempt to add RX Data Analysis Report option failed." H 3
 S X=$$ADD^XPDMENU("APCL M MAN QUALITY ASSURANCE","APCLBP IN/OUT CONTROL BPS","BPC")
 I 'X W "Attempt to add RX Data Analysis Report option failed." H 3
 S X=$$ADD^XPDMENU("APCL M MAIN DM MENU","APCL DM PTS NO DX PL","PLDX")
 I 'X W "Attempt to add Patients w/o DM option failed.." H 3
 S X=$$ADD^XPDMENU("APCL M MAIN DM MENU","APCL DM2000 AUDIT MENU","DM20")
 I 'X W "Attempt to add DM 2000 Audit Menu option failed.." H 3
 S X=$$ADD^XPDMENU("APCL M MAIN DM MENU","APCL TAXONOMY SETUP","TS")
 I 'X W "Attempt to add Taxonomy Setup menu option failed.." H 3
 S X=$$ADD^XPDMENU("APCL M MAIN DM MENU","APCL FLOW SHEET SETUP","FS")
 I 'X W "Attempt to add Flow Sheet Setup Menu option failed.." H 3
 S X=$$ADD^XPDMENU("APCL M MAN PATIENT LISTINGS","APCL P ELDER CARE 1","ELFA")
 I 'X W "Attempt to add Elder Care Report 1 option failed." H 3
 S X=$$ADD^XPDMENU("APCL M MAN PATIENT LISTINGS","APCL P ELDER CARE 2","ELFC")
 I 'X W "Attempt to add Elder Care Report 2 option failed." H 3
 S X=$$ADD^XPDMENU("APCL M MAN PATIENT LISTINGS","APCL P ELDER CARE 3","ELFT")
 I 'X W "Attempt to add Elder Care Report 3 option failed." H 3
 ;set up HDL taxonomy
TAXS ;
 S ATXFLG=1
 D LAB
 D DRUGS
 D DMEDUC
 K ATXFLG,ATXX,APCLX,APCLDA,APCLTX
 Q
LAB ;
 S APCLX="DM AUDIT HDL TAX" D LAB1
 Q
LAB1 ;
 W !,"Creating ",APCLX," Taxonomy..."
 S APCLDA=$O(^ATXLAB("B",APCLX,0))
 Q:APCLDA  ;taxonomy already exisits
 S X=APCLX,DIC="^ATXLAB(",DIC(0)="L",DIADD=1,DLAYGO=9002228 D ^DIC K DIC,DA,DIADD,DLAYGO,I
 I Y=-1 W !!,"ERROR IN CREATING ",APCLX," TAX" Q
 S APCLTX=+Y,$P(^ATXLAB(APCLTX,0),U,2)=APCLX,$P(^(0),U,5)=DUZ,$P(^(0),U,6)=DT,$P(^(0),U,8)="B",$P(^(0),U,9)=60,^ATXLAB(APCLTX,21,0)="^9002228.02101PA^0^0"
 S DA=APCLTX,DIK="^ATXAX(" D IX1^DIK
 Q
DRUGS ;set up drug taxonomies
 S APCLX="DM AUDIT LIPID LOWERING DRUGS" D DRUG1
 Q
DRUG1 ;
 W !,"Creating ",APCLX," Taxonomy..."
 S APCLDA=$O(^ATXAX("B",APCLX,0))
 Q:APCLDA  ;taxonomy already exisits
 S X=APCLX,DIC="^ATXAX(",DIC(0)="L",DIADD=1,DLAYGO=9002226 D ^DIC K DIC,DA,DIADD,DLAYGO,I
 I Y=-1 W !!,"ERROR IN CREATING ",APCLX," TAX" Q
 S APCLTX=+Y,$P(^ATXAX(APCLTX,0),U,2)=APCLX,$P(^(0),U,5)=DUZ,$P(^(0),U,8)=0,$P(^(0),U,9)=DT,$P(^(0),U,12)=173,$P(^(0),U,13)=0,$P(^(0),U,15)=50,^ATXAX(APCLTX,21,0)="^9002226.02101A^0^0"
 S DA=APCLTX,DIK="^ATXAX(" D IX1^DIK
 Q
DMEDUC ;
 W !,"Creating Education topics taxonomy..."
 S APCLDA=0 S APCLDA=$O(^ATXAX("B","DM AUDIT SMOKING CESS EDUC",APCLDA)) I 'APCLDA D
 .S X="DM AUDIT SMOKING CESS EDUC",DIC="^ATXAX(",DIC(0)="L",DIADD=1,DLAYGO=9002226 D ^DIC K DIC,DA,DIADD,DLAYGO,I
 .I Y=-1 W !!,"ERROR IN CREATING DM AUDIT SMOKING CESS EDUC TAX" Q
 .S APCLTX=+Y,$P(^ATXAX(APCLTX,0),U,2)="DM AUDIT SMOKING CESS EDUC",$P(^(0),U,5)=DUZ,$P(^(0),U,8)=0,$P(^(0),U,9)=DT,$P(^(0),U,12)=280,$P(^(0),U,13)=0,$P(^(0),U,15)=9999999.09,^ATXAX(APCLTX,21,0)="^9002226.02101A^0^0"
 .S ^ATXAX(APCLTX,21,APCLX,0)=+Y,$P(^ATXAX(APCLTX,21,0),U,3)=APCLX,$P(^(0),U,4)=APCLX,^ATXAX(APCLTX,21,"AA",+Y,+Y)=""
 .S DA=APCLTX,DIK="^ATXAX(" D IX1^DIK
 Q
