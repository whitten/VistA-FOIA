AMH40ENV ; IHS/CMI/LAB - POST INIT BH 16 Apr 2009 7:37 AM 01 Aug 2009 5:37 AM ; 13 Apr 2010  3:54 PM
 ;;4.0;IHS BEHAVIORAL HEALTH;;MAY 14, 2010
 ;re-index all cross references on Designated provider fields
 ;
ENV ;EP 
 F X="XPO1","XPZ1","XPZ2","XPI1" S XPDDIQ(X)=0
 I +$$VERSION^XPDUTL("XU")<8 D MES^XPDUTL($$CJ^XLFSTR("Version 8.0 of KERNEL is required.  Not installed",80)) D SORRY(2) I 1
 E  D MES^XPDUTL($$CJ^XLFSTR("Requires Kernel Version 8.0....Present.",80))
 I +$$VERSION^XPDUTL("DI")<22 D MES^XPDUTL($$CJ^XLFSTR("Version 22.0 of FILEMAN is required.  Not installed.",80)) D SORRY(2) I 1
 E  D MES^XPDUTL($$CJ^XLFSTR("Requires Fileman v22....Present.",80))
 I '$$INSTALLD("AMH*3.0*10") D SORRY(2)
 I $$VERSION^XPDUTL("AUM")<10.1 D MES^XPDUTL($$CJ^XLFSTR("2010 ICD Updates are required.  Not installed.",80)) D SORRY(2) I 1
 E  D MES^XPDUTL($$CJ^XLFSTR("Requires 2010 ICD updates...aum v10.1...Present.",80))
 I $$VERSION^XPDUTL("BJPC")'="2.0" D MES^XPDUTL($$CJ^XLFSTR("Version 2.0 of the IHS PCC Suite (BJPC) is required.  Not installed.",80)) D SORRY(2) I 1
 E  D MES^XPDUTL($$CJ^XLFSTR("Requires IHS PCC Suite v2.0...Present.",80))
 I $E($$VERSION^XPDUTL("BMX"),1,3)'="4.0" D MES^XPDUTL($$CJ^XLFSTR("Version 4.0 of BMX is required.  Not installed.",80)) D SORRY(2) I 1
 E  D MES^XPDUTL($$CJ^XLFSTR("Requires BMX v4.0....Present.",80))
 Q
 ;
PRE ;
 S DA=$O(^DIC(9.4,"C","AMH",0))
 I DA S DIE="^DIC(9.4,",DR=".01///IHS BEHAVIORAL HEALTH" D ^DIE
 S DA=0 F  S DA=$O(^AMHSORT(DA)) Q:DA'=+DA  S DIK="^AMHSORT(" D ^DIK
 S DA=0 F  S DA=$O(^AMHRECD(DA)) Q:DA'=+DA  S DIK="^AMHRECD(" D ^DIK
 S DA=0 F  S DA=$O(^AMHBHPC(DA)) Q:DA'=+DA  S DIK="^AMHBHPC(" D ^DIK
 K DIK,DA
KEYSAVE ;
 K AMHASK
 S DA=$O(^DIC(19.1,"B","AMHZ DELETE VISIT",0))
 I DA D
 .;STORE CURRENT USERS
 .K AMHHASK
 .S X=0 F  S X=$O(^XUSEC("AMHZ DELETE VISIT",X)) Q:X'=+X  S AMHHASK(X)=""
 .S DIE="^DIC(19.1,",DR=".01///AMHZ DELETE RECORD",DITC=1 D ^DIE K DIE,DITC,DA,DR
 .S X=0 F  S X=$O(AMHHASK(X)) Q:X'=+X  S ^XUSEC("AMHZ DELETE RECORD",X)=""
C316 ;
 S DA=$O(^AMHPROB("B","316.",0))
 I DA S DIE="^AMHPROB(",DR=".02///PSYCHOLOGICAL FACTOR AFFECTING..(INDICATE MEDICAL CONDITION)" D ^DIE K DIE,DA,DR
 S DA=$O(^DIC(19,"B","AMHGRPC",0))
 I DA S DIE="^DIC(19,",DR="1///RPMS Behavioral Health GUI" D ^DIE K DA,DIE,DR
 ;
PA ;
 S DA=$O(^AMHTPA("B","DUI/DWI session",0))
 Q:'DA
 K ^AMHTPA("B","DUI/DWI session",DA)
 S ^AMHTPA("B","DUI/DWI SESSION",DA)=""
 S $P(^AMHTPA(DA,0),U)="DUI/DWI SESSION"
 Q
 ;
POST ;EP
 ;move INTAKE documents to new format and flag as moved.
 D MES^XPDUTL("Moving and converting Intake Documents to Visit Based documents")
 S AMHX=0 F  S AMHX=$O(^AMHPINTK(AMHX)) Q:AMHX'=+AMHX  D
 .Q:$P($G(^AMHPINTK(AMHX,9999)),U)  ;already converted
 .S X=$P(^AMHPINTK(AMHX,0),U,6) I X="" S X=$P(^AMHPINTK(AMHX,0),U,2)
 .I X="" D MES^XPDUTL("ERROR:  could not move intake document "_AMHX_" no dates available.") Q
 .S AMHY=^AMHPINTK(AMHX,0)
 .S DIC("DR")=".02////"_$P(AMHY,U,1)_";.04////"_$S($P(AMHY,U,8):$P(AMHY,U,8),1:$P(AMHY,U,3))_";.06////"_$P(AMHY,U,3)_";.07////"_$P(AMHY,U,2)_";.09///I"
 .S DIC="^AMHRINTK(",DIC(0)="L",DIADD=1,DLAYGO="9002011.13"
 .D FILE^DICN K DIADD,DLAYGO,DIC
 .I Y=-1 D MES^XPDUTL("ERROR:  could not create new intake document for "_AMHX_".") Q
 .S AMHDA=+Y
 .M ^AMHRINTK(AMHDA,41)=^AMHPINTK(AMHX,41)
 .;now attempt to find an initial visit to point this intake to, if none found create one
 .S $P(^AMHPINTK(AMHX,9999),U,1)=1
 ;REINDEX XREF ON TP
 K ^AMHPTXP("AA")
 S DIK="^AMHPTXP(",DIK(1)=".02^AATOO" D ENALL^DIK
 K DIK
 S DIK="^AMHGROUP(",DIK(1)=".01^AINV" D ENALL^DIK
 K DIK
 S DIK="^AMHPROB(",DIK(1)=".01^BA" D ENALL^DIK
 K DIK
 S DIK="^AMHPROB(",DIK(1)=".01^BAA" D ENALL^DIK
 K DIK
 S DIK="^AMHPSUIC(",DIK(1)=".06^AA" D ENALL^DIK
 K DIK
 S DIK="^AMHTACT(",DIK(1)=".01^AC" D ENALL^DIK
 K DIK
 S DIK="^AMHREC(",DIK(1)=".01^AB" D ENALL^DIK
 K DIK
 S AMHX=0,AMHNMM="" F  S AMHX=$O(^AMHSITE(AMHX)) Q:AMHX'=+AMHX  D
 .Q:$P(^AMHSITE(AMHX,0),U,12)'=3
 .S $P(^AMHSITE(AMHX,0),U,12)=5,AMHNMM=AMHNMM_$S(AMHNMM]"":"; ",1:"")_$P($G(^DIC(4,AMHX,0)),U)
 I AMHNMM]"" D MM3
KEY ;delete keys and remove them from any options
 F AMHKEY="AMHZ CDMIS BACKLOAD","AMHZ DELETE SIGNED VISIT" D  D DELKEY
 .S AMHX=0 F  S AMHX=$O(^DIC(19,AMHX)) Q:AMHX'=+AMHX  D
 .Q:$E($P(^DIC(19,AMHX,0),U),1,3)'="AMH"
 .Q:$P(^DIC(19,AMHX,0),U,6)'=AMHKEY
 .S DA=AMHX,DIE="^DIC(19,",DR="3///@" D ^DIE K DA,DR,DIE
DELVKEY ;
 ;
ICD ;
 D ICDUPD
 ;
BPCKEY ;remove all BPCKEYS
 S AMHX="BPC" F  S AMHX=$O(^DIC(19.1,"B",AMHX)) Q:AMHX]"BPCZZZZZZZZ"  D
 .S AMHY=0 F  S AMHY=$O(^DIC(19.1,"B",AMHX,AMHY)) Q:AMHY'=+AMHY  D
 ..S DA=AMHY,DIK="^DIC(19.1," D ^DIK
 ..Q
 .Q
DEPSCR ;
 S AMHX=0 F  S AMHX=$O(^AMHREC(AMHX)) Q:AMHX'=+AMHX  D
 .Q:'$D(^AMHREC(AMHX,14))
 .I $P(^AMHREC(AMHX,14),U,3)="PO" S DIE="^AMHREC(",DA=AMHX,DR="1403////P" D ^DIE K DIE,DA,DR
 .I $P(^AMHREC(AMHX,14),U,5)="PO" S DIE="^AMHREC(",DA=AMHX,DR="1405////P" D ^DIE K DIE,DA,DR
TIU ;MOVE TIU DOCUMENTS FROM 1108 TO MULTIPLE
 NEW AMHX,AMHFDA,AMHIENS,AMHERRR,AMHAIEN,AMHDOC
 S AMHX=0 F  S AMHX=$O(^AMHREC(AMHX)) Q:AMHX'=+AMHX  D
 .S AMHDOC=$P($G(^AMHREC(AMHX,11)),U,8)
 .I 'AMHDOC Q
 .I $D(^AMHREC(AMHX,54,"B",AMHDOC)) Q  ;already in multiple
 .S AMHIENS="+2,"_AMHX_","
 .S AMHFDA(9002011.054,AMHIENS,.01)=AMHDOC
 .D UPDATE^DIE("","AMHFDA","AMHIENS","AMHERRR(1)")
 .I $D(AMHERRR) D MES^XPDUTL("ERROR:  could not move TIU document for record "_AMHX)
 Q
 ;
DELKEY ;
 S DA=$O(^DIC(19.1,"B",AMHKEY,0))
 I DA S DIK="^DIC(19.1," D ^DIK
 K DIK,DA
 Q
 ;
INSTALLD(AMHSTAL) ;EP - Determine if patch AMHSTAL was installed, where
 ; APCLSTAL is the name of the INSTALL.  E.g "AG*6.0*11".
 ;
 NEW AMHY,DIC,X,Y
 S X=$P(AMHSTAL,"*",1)
 S DIC="^DIC(9.4,",DIC(0)="FM",D="C"
 D IX^DIC
 I Y<1 D IMES Q 0
 S DIC=DIC_+Y_",22,",X=$P(AMHSTAL,"*",2)
 D ^DIC
 I Y<1 D IMES Q 0
 S DIC=DIC_+Y_",""PAH"",",X=$P(AMHSTAL,"*",3)
 D ^DIC
 S AMHY=Y
 D IMES
 Q $S(AMHY<1:0,1:1)
IMES ;
 D MES^XPDUTL($$CJ^XLFSTR("Patch """_AMHSTAL_""" is"_$S(Y<1:" *NOT*",1:"")_" Present.",IOM))
 Q
SORRY(X) ;
 KILL DIFQ
 I X=3 S XPDQUIT=2 Q
 S XPDQUIT=X
 W *7,!,$$CJ^XLFSTR("Sorry....FIX IT!",IOM)
 Q
 ;
MM3 ;BULLETIN;
 I '$G(DUZ) W !,"DUZ UNDEFINED OR ZERO.",! Q
 D HOME^%ZIS,DT^DICRW
 ;
 NEW XMSUB,XMDUZ,XMTEXT,XMY,DIFROM
 KILL ^TMP($J,"AMHBUL")
 D WRITEMS3,GETREC3
 ;Change following lines as desired
SUBJECT3 S XMSUB="* * * IMPORTANT RPMS INFORMATION * * *"
SENDER3 S XMDUZ="IHS Behavioral Health"
 S XMTEXT="^TMP($J,""AMHBUL"",",XMY(1)="",XMY(DUZ)=""
 I $E(IOST)="C" W !,"Sending Mailman message to holders of the"_" "_AMHKEY_" "_"security key."
 D ^XMD
 KILL ^TMP($J,"AMHBUL"),AMHKEY
 Q
 ;
WRITEMS3 ;
 S AMHIEN=$O(^AMHPATCH("AA",4,99,0))
 I AMHIEN="" Q
 S AMHX=0,AMHC=0 F  S AMHX=$O(^AMHPATCH(AMHIEN,11,AMHX)) Q:AMHX'=+AMHX  S AMHC=AMHC+1,^TMP($J,"AMHBUL",AMHC)=^AMHPATCH(AMHIEN,11,AMHX,0)
 S AMHC=AMHC+1,^TMP($J,"AMHBUL",AMHC)=" "
 S AMHC=AMHC+1,^TMP($J,"AMHBUL",AMHC)=AMHNMM
 Q
GETREC3 ;
 ;* * * Define key below to identify recipients * * *
 ;
 S CTR=0,AMHKEY="AMHZMGR"
 F  S CTR=$O(^XUSEC(AMHKEY,CTR)) Q:'CTR  S Y=CTR S XMY(Y)=""
 Q
ICDUPD ;
 D MES^XPDUTL("Updating MHSS/DSM IV Codes...")
 D INACT  ;inactivate existing codes
 D NEW  ;add new codes
 D REMAP  ;remap mapping
 S AMHX=0 F  S AMHX=$O(^AMHPROB("B","780.59",AMHX)) Q:AMHX'=+AMHX  S DA=AMHX,DIE="^AMHPROB(",DR=".13///@;.14///@" D ^DIE K DA,DIE,DR
 S AMHX=0 F  S AMHX=$O(^AMHPROB("B",10,AMHX)) Q:AMHX'=+AMHX  S DA=AMHX,DIE="^AMHPROB(",DR=".13///@;.14///@" D ^DIE K DA,DIE,DR
 Q
INACT ;
 F AMHX=239.8,274.0,279.4,348.8,453.8,488,768.7,779.3,784.5,799.2,969.0,969.7,"V10.9","V53.5","V60.8","V72.6","V80.0","21.1",333.7 D
 .S DA=$O(^AMHPROB("B",AMHX,0))
 .I 'DA Q
 .S DIE="^AMHPROB(",DR=".13///1;.14////3091001" D ^DIE K DA,DIE,DR
 .I $D(Y) D MES^XPDUTL("ERROR:  COULD NOT INACTIVATE CODE "_AMHX_".")
 S DA=$O(^AMHPROBC("B","21.1",0))
 I DA S DIE="^AMHPROBC(",DR=".04///1" D ^DIE K DA,DR,DIE
 Q
NEW ;add new codes
 K DIC,DA,DIE,DR,DLAYGO,DIADD
 S DA=$O(^AMHPROBC("B",29.3,0))
 I DA G NEW1
 S X=29.3,DIC="^AMHPROBC(",DIC(0)="L",DIC("DR")=".02///SCREENING FOR TRAUMATIC BRAIN INJURY;.03///SCREENING",DIADD=1,DLAYGO=9002012.4 K DD,D0,DO D FILE^DICN
 I Y=-1 D MES^XPDUTL("ERROR:  COULD NOT ADD CODE 29.3")
 K DIC,DA,DIE,DR,DLAYGO,DIADD
NEW1 ;
 ;add new codes if they don't exist
 S AMHTEXT="ICDNEW" F AMHX=1:1 S AMHTX=$P($T(@AMHTEXT+AMHX),";;",2,3) Q:AMHTX=""  D
 .S (X,AMHCODE)=$P(AMHTX,";;",1),C=$P(AMHTX,";;",2)
 .S AMHPC=$O(^AMHPROBC("B",C,0))
 .I AMHPC="" D MES^XPDUTL("Problem code: "_$P(AMHTX,";;",2)_" does not exist")
 .S DA=$O(^AMHPROB("B",X,0)) I DA Q
 .S DIC="^AMHPROB(",DLAYGO=9001012.2,DIADD=1,DIC="^AMHPROB("
 .S DIC(0)="L"
 .K DD,D0,DO D FILE^DICN K DIADD,DLAYGO,DD,DIC,D0,DO
 .I Y=-1 D MES^XPDUTL("Code "_AMHCODE_" could not be added.") Q
 .S DA=+Y
NEWE .;
 .S DIE="^AMHPROB("
 .K AMHINA
 .S AMHINA=$$ICDD^ICDCODE(AMHCODE,"AMHINA")
 .S DR=".02////"_$E($G(AMHINA(1)),1,160)_";.03////"_AMHPC_";.05////"_AMHCODE
 .D ^DIE K DIE,DA,DR
 .I $D(Y) D MES^XPDUTL("Error updating code "_AMHCODE_".") Q
 Q
REMAP ;
 F AMHX=70,71,83,85 S DIE="^AMHPROB(",DR=".05////V60.89",DA=$O(^AMHPROB("B",AMHX,0)) D
 .I 'DA D MES^XPDUTL("Code "_AMHX_" does not exist - cannot remap") Q
 .D ^DIE K DIE,DA,DR
 Q
ICDNEW ;;
 ;;333.72;;5
 ;;333.85;;5
 ;;799.21;;3
 ;;799.22;;3
 ;;799.23;;3
 ;;799.24;;3
 ;;799.25;;3
 ;;799.29;;3
 ;;854.00;;6.1
 ;;854.01;;6.1
 ;;854.02;;6.1
 ;;854.03;;6.1
 ;;854.04;;6.1
 ;;854.05;;6.1
 ;;854.06;;6.1
 ;;854.09;;6.1
 ;;854.10;;6.1
 ;;854.11;;6.1
 ;;854.12;;6.1
 ;;854.13;;6.1
 ;;854.14;;6.1
 ;;854.15;;6.1
 ;;854.16;;6.1
 ;;854.19;;6.1
 ;;V15.52;;6.1
 ;;V60.81;;72
 ;;V60.89;;85
 ;;V61.07;;62
 ;;V61.08;;62
 ;;V61.23;;53
 ;;V61.24;;53
 ;;V61.25;;53
 ;;V61.42;;62
 ;;V80.01;;29.3
 ;;
