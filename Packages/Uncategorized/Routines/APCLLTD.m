APCLLTD ; IHS/CMI/LAB - FILEMAN INTERFACE UTILITY ;
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
 ;;UTILITY ROUTINE USED TO MANAGE ALL INTERFACE TO FILEMAN
 ;;WITH FEW EXCEPTIONS, THIS IS THE ONLY PLACE IN ARMS FROM WHICH
 ;;FILEMAN ENTRY POINTS ARE CALLED
DIC ;EP;
 Q:$D(APCLLTOT)
 K DTOUT,DUOUT,APCLLTQT,APCLLTOT
 D ^DIC
 S:$D(DUOUT) APCLLTQT=""
 S:$D(DTOUT)!(X="^^") (APCLLTQT,APCLLTOT)=""
 K DIC,DA,DD,DR,DINUM,D,DLAYGO,DIADD ;IHS/CMI/THL PATCH 4 ADD 'DIADD' TO KILL LIST
 Q
MIX ;EP;
 Q:$D(APCLLTOT)
 K DTOUT,DUOUT,APCLLTQT,APCLLTOT
 D MIX^DIC1
 S:$D(DTOUT)!$D(DUOUT)!(X="^^") (APCLLTQT,APCLLTOT)=""
 K DIC,DA,DD,DR,DINUM,D,DLAYGO
 Q
IX ;EP;
 Q:$D(APCLLTOT)
 K DTOUT,DUOUT,APCLLTQT,APCLLTOT
 D IX^DIC
 S:$D(DTOUT)!$D(DUOUT)!(X="^^") (APCLLTQT,APCLLTOT)=""
 K DIC,DA,DD,DR,DINUM,D,DLAYGO
 Q
DIE ;EP;
 Q:$D(APCLLTOT)
 Q:'DA
 K DTOUT,DUOUT,APCLLTQT,APCLLTOT
 L +@(DIE_DA_")"):4 I '$T S APCLLTQT="" Q:'$D(IOST)#2  W:$E(IOST,1,2)="C-" !!,"Entry being edited by another user.  Please try Later.",! H 3 Q
 S APCLDIDA=DA
 D ^DIE
 L -@(DIE_APCLDIDA_")"):0
 I $D(DTOUT)!$D(DUOUT) S (APCLLTQT,APCLLTOT)=""
 K DIE,DA,DR,APCLDIDA
 Q
DDS ;EP;FOR SCREENMAN CALLS
 S DDSFILE=DIE
 D ^DDS
 K DDSFILE,APCLCREN
 Q
FILE ;EP;
 K DD,DO,DTOUT,DUOUT,APCLLTQT,APCLLTOT
 D FILE^DICN
 S:$D(DTOUT)!$D(DUOUT)!(X="^^") (APCLLTQT,APCLLTOT)=""
 K DIC,DA,DD,DR,DINUM,DLAYGO
 Q
DIR ;EP;
 I $D(APCLLTOT) K DIR S Y="" Q
 K DTOUT,DUOUT,APCLLTQT,APCLLTOT
 D ^DIR
 S APCLLTY=Y
 S:$D(DIRUT)!$D(DIROUT)!$D(DUOUT) APCLLTQT=""
 S:X="^^"!$D(DTOUT) (APCLLTQT,APCLLTOT)=""
 K DIR,DIRUT,DIROUT,DUOUT,DTOUT
 Q
DIK ;EP;
 K DTOUT,DUOUT,APCLLTQT,APCLLTOT
 D ^DIK
 K DIK
 Q
IX1 ;EP;
 Q:$D(APCLLTOT)
 K DTOUT,DUOUT,APCLLTQT,APCLLTOT
 D IX1^DIK
 K DIK,DA
 Q
DIQ1 ;EP;
 D EN^DIQ1
 K DIC,DA,DR,DIQ
 Q
WP ;EP;TO BROWSE WORD PROCESSING FIELD
 Q:'$D(APCLFILE)#2!'$D(APCLIEN)#2!'$D(APCLFIEL)
 K ACMSCREN
 D WP^DDBR(APCLFILE,APCLIEN,APCLFIEL,"N",$G(APCLTITL))
 Q
