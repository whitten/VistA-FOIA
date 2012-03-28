ACHS31P5 ;IHS/SET/GTH - ACHS 3.1 PATCH 5 ; [ 12/06/2002  10:36 AM ]
 ;;3.1;CONTRACT HEALTH MGMT SYSTEM;**5**;JUN 11, 2001
 ;
 ; IHS/SET/GTH ACHS*3.1*5 12/06/2002
 ;
 I '$G(IOM) D HOME^%ZIS
 ;
 I '$G(DUZ) W !,"DUZ UNDEFINED OR 0." D SORRY(2) Q
 ;
 I '$L($G(DUZ(0))) W !,"DUZ(0) UNDEFINED OR NULL." D SORRY(2) Q
 ;
 I '(DUZ(0)["@") W:'$D(ZTQUEUED) !,"DUZ(0) DOES NOT CONTAIN AN '@'." D SORRY(2) Q
 ;
 S X=$$GET1^DIQ(200,DUZ,.01)
 W !!,$$CJ^XLFSTR("Hello, "_$P(X,",",2)_" "_$P(X,","),IOM)
 W !!,$$CJ^XLFSTR("Checking Environment for "_$P($T(+2),";",4)_" V "_$P($T(+2),";",3)_" Patch "_$P($T(+2),";",5)_".",IOM)
 ;
 NEW IORVON,IORVOFF
 S X="IORVON;IORVOFF"
 D ENDR^%ZISS
 ;
 I $$VCHK("ACHS","3.1",2,"'=")
 ;
 NEW DA,DIC
 S X="ACHS",DIC="^DIC(9.4,",DIC(0)="",D="C"
 D IX^DIC
 I Y<0,$D(^DIC(9.4,"C","ACHS")) D
 . W !!,*7,*7,$$CJ^XLFSTR("You Have More Than One Entry In The",IOM),!,$$CJ^XLFSTR("PACKAGE File with an ""ACHS"" prefix.",IOM)
 . W !,$$CJ^XLFSTR(IORVON_"One entry needs to be deleted."_IORVOFF,IOM)
 . D SORRY(2)
 .Q
 ;
 I $G(XPDQUIT) W !,$$CJ^XLFSTR(IORVON_"FIX IT! Before Proceeding."_IORVOFF,IOM),!!,*7,*7,*7 Q
 ;
 W !!,$$CJ^XLFSTR("ENVIRONMENT OK.",IOM)
 ;
 D HELP^XBHELP("INTROE","ACHS31P5")
 I '$$DIR^XBDIR("E","","","","","",1) D SORRY(2) Q
 ;
 I $G(XPDENV)=1 S (XPDDIQ("XPZ1"),XPDDIQ("XPZ2"))=0 D HELP^XBHELP("INTROI","ACHS31P5") I '$$DIR^XBDIR("E","","","","","",1) D SORRY(2)
 ;
 Q
 ;
SORRY(X) ;
 KILL DIFQ
 S XPDQUIT=X
 W *7,!,$$CJ^XLFSTR("Sorry....FIX IT!",IOM)
 Q
 ;
VCHK(ACHSPRE,ACHSVER,ACHSQUIT,ACHSCOMP) ; Check versions needed.
 ;  
 NEW ACHSV
 S ACHSV=$$VERSION^XPDUTL(ACHSPRE)
 W !,$$CJ^XLFSTR("Need "_$S(ACHSCOMP="<":"at least ",1:"")_ACHSPRE_" v "_ACHSVER_"....."_ACHSPRE_" v "_ACHSV_" Present",IOM)
 I @(ACHSV_ACHSCOMP_ACHSVER) D SORRY(ACHSQUIT) Q 0
 Q 1
 ;
PRE ;EP - From KIDS.
 I $$NEWCP^XPDUTL("PRE1","AUDS^ACHS31P5")
 Q
 ;
POST ;EP - From KIDS.
 ;
 ; --- Patches 3 & 4. Checks for installs are done in Install Questions.
 S %="P3^ACHS31P5"
 I $$NEWCP^XPDUTL("POS3-"_%,%)
 S %="P4^ACHS31P5"
 I $$NEWCP^XPDUTL("POS4-"_%,%)
 ;
 ; --- Atch Denial ltr edit option at 638 sites.
 S %="DENOPT^ACHS31P5"
 I $$NEWCP^XPDUTL("POS5-"_%,%)
 ;
 ; --- Delete fiels in CHS files marked for deletion.
 S %="DELFLD^ACHS31P5"
 I $$NEWCP^XPDUTL("POS6-"_%,%)
 ;
 ; --- Atch 278 menu to Doc edit option.
 S %="M278^ACHS31P5"
 I $$NEWCP^XPDUTL("POS7-"_%,%)
 ;
 ; --- Remove non-standard option auditing from CHS options. 
 S %="POS^ACHS31P0"
 I $$NEWCP^XPDUTL("POS8-"_%,%)
 ;
 ; --- Restore dd audit settings.
 S %="AUDR^ACHS31P5"
 I $$NEWCP^XPDUTL("POS9-"_%,%)
 ;
 ; --- Send mail message of install.
 S %="MAIL^ACHS31P5"
 I $$NEWCP^XPDUTL("POS10-"_%,%)
 ;
 Q
 ;
MAIL ;
 D BMES^XPDUTL("BEGIN Delivering MailMan message to select users.")
 NEW DIFROM,XMSUB,XMDUZ,XMTEXT,XMY
 KILL ^TMP("ACHS31P5",$J)
 D RSLT(" --- ACHS v 3.1 Patch 5, has been installed into this uci ---")
 F %=1:1 D RSLT($P($T(GREET+%),";",3)) Q:$P($T(GREET+%+1),";",3)="###"
 S %=0
 F  S %=$O(^XTMP("XPDI",XPDA,"BLD",XPDBLD,1,%)) Q:'%   D RSLT(^(%,0))
 S XMSUB=$P($P($T(+1),";",2)," ",3,99),XMDUZ=$S($G(DUZ):DUZ,1:.5),XMTEXT="^TMP(""ACHS31P5"",$J,",XMY(1)="",XMY(DUZ)=""
 F %="ACHSZMENU","XUMGR","XUPROG","XUPROGMODE" D SINGLE(%)
 D ^XMD
 KILL ^TMP("ACHS31P5",$J)
 D MES^XPDUTL("END Delivering MailMan message to select users.")
 Q
 ;
RSLT(%) S ^(0)=$G(^TMP("ACHS31P5",$J,0))+1,^(^(0))=%
 Q
 ;
SINGLE(K) ; Get holders of a single key K.
 NEW Y
 S Y=0
 Q:'$D(^XUSEC(K))
 F  S Y=$O(^XUSEC(K,Y)) Q:'Y  S XMY(Y)=""
 Q
 ;
INTROE ; Intro text during KIDS Environment check.
 ;;In this distribution:
 ;;(1) PO entry handles line label processing for spec. benefits;
 ;;(2) Adjust Pd doc, handle internal entry number correctly;
 ;;(3) Check printing, correctly retrieve all adjusted amounts;
 ;;(4) HQ Denial report, handle multiple reasons with same text;
 ;;(5) Denial Ltr/Face Sheet, correct Medicare dates;
 ;;(6) Formats Pt AGE in 638 Dental statistical export record;
 ;;(7) Screen export of ICD DX Etiology codes from 638 sites;
 ;;(8) Correct Vendor Usage Report for "*" on paid POs;
 ;;(9) Deploy initial X12 278 functionality;
 ;;(10)Various maintenance mods/fixes.
 ;;###
 ;
INTROI ; Intro text during KIDS Install.
 ;;A standard message will be produced by this update.
 ;;  
 ;;If you run interactively, results will be displayed on your screen,
 ;;as well as in the mail message and the entry in the INSTALL file.
 ;;If you queue to TaskMan, please read the mail message for results of
 ;;this update, and remember not to Q to the HOME device.
 ;;###
 ;
GREET ;;To add to mail message.
 ;;  
 ;;Greetings.
 ;;  
 ;;Standard data dictionaries on your RPMS system have been updated.
 ;;  
 ;;You are receiving this message because of the particular RPMS
 ;;security keys that you hold.  This is for your information, only.
 ;;You need do nothing in response to this message.
 ;;  
 ;;Questions about this patch, which is a product of the RPMS DBA
 ;;can be directed to the DIR/RPMS
 ;;Help Desk.
 ;;Please refer to patch "ACHS*3.1*5".
 ;;  
 ;;###;NOTE: This line indicates the end of text in this message.
 ;
 ; -----------------------------------------------------
 ; The global location for dictionary audit is:
 ;           ^DD(FILE,0,"DDA")
 ; If the valuey is "Y", dd audit is on.  Any other value, or the
 ; absence of the node, means dd audit is off.
 ; -----------------------------------------------------
AUDS ;EP - From KIDS.
 D BMES^XPDUTL("Saving current DD AUDIT settings for files in this patch")
 D MES^XPDUTL("and turning DD AUDIT to 'Y'.")
 S ^XTMP("ACHS31P5",0)=$$FMADD^XLFDT(DT,10)_"^"_DT_"^"_$P($P($T(+1),";",2)," ",3,99)
 NEW ACHS
 S ACHS=0
 F  S ACHS=$O(^XTMP("XPDI",XPDA,"FIA",ACHS)) Q:'ACHS  D
 . I '$D(^XTMP("ACHS31P5",ACHS,"DDA")) S ^XTMP("ACHS31P5",ACHS,"DDA")=$G(^DD(ACHS,0,"DDA"))
 . D MES^XPDUTL(" File "_$$RJ^XLFSTR(ACHS,12)_" - "_$$LJ^XLFSTR(^XTMP("XPDI",XPDA,"FIA",ACHS),30)_"- DD audit was '"_$G(^XTMP("ACHS31P5",ACHS,"DDA"))_"'"),MES^XPDUTL($$RJ^XLFSTR("Set to 'Y'",69))
 . S ^DD(ACHS,0,"DDA")="Y"
 .Q
 D MES^XPDUTL("DD AUDIT settings saved in ^XTMP(.")
 Q
 ; -----------------------------------------------------
AUDR ; Restore the file data audit values to their original values.
 D BMES^XPDUTL("Restoring DD AUDIT settings for files in this patch.")
 NEW ACHS
 S ACHS=0
 F  S ACHS=$O(^XTMP("ACHS31P5",ACHS)) Q:'ACHS  D
 . S ^DD(ACHS,0,"DDA")=^XTMP("ACHS31P5",ACHS,"DDA")
 . D MES^XPDUTL(" File "_$$RJ^XLFSTR(ACHS,12)_" - "_$$LJ^XLFSTR($$GET1^DID(ACHS,"","","NAME"),30)_"- DD AUDIT Set to '"_^DD(ACHS,0,"DDA")_"'")
 .Q
 KILL ^XTMP("ACHS31P5")
 D MES^XPDUTL("DD AUDIT settings restored.")
 Q
 ; -----------------------------------------------------
 ;
INSTALLD(ACHS) ; Determine if patch ACHS was installed, where ACHS is
 ; the name of the INSTALL.  E.g "AVA*93.2*12".
 ;
 NEW DIC,X,Y
 ;  lookup package.
 S X=$P(ACHS,"*",1)
 S DIC="^DIC(9.4,",DIC(0)="FM",D="C"
 D IX^DIC
 I Y<1 Q 0
 ;  lookup version.
 S DIC=DIC_+Y_",22,",X=$P(ACHS,"*",2)
 D ^DIC
 I Y<1 Q 0
 ;  lookup patch.
 S DIC=DIC_+Y_",""PAH"",",X=$P(ACHS,"*",3)
 D ^DIC
 Q $S(Y<1:0,1:1)
 ;
 ; -----------------------------------------------------
 ; Fields to be deleted.
 ;File#^FileName^Field#^FieldName
DELFLD ;
 ;;9002069^CHS DATA CONTROL^1^*RESERVED-1
 ;;9002069^CHS DATA CONTROL^3^*BUDGET INDIVIDUAL ACCOU
 ;;9002069.03^PIGGYBACK FACILITIES^.01^PIGGYBACK FACILITIES
 ;;9002069^CHS DATA CONTROL^9^*PIGGYBACK FACILITIES
 ;;9002071.63^* DIAGNOSIS (APC) COMMENT^.01^* DIAGNOSIS (APC) COMMENT
 ;;9002071.06^* DIAGNOSIS (APC)^.01^* DIAGNOSIS (APC)
 ;;9002071.06^* DIAGNOSIS (APC)^2^* DIAGNOSIS (APC) NARRATIVE
 ;;9002071.06^* DIAGNOSIS (APC)^3^* DIAGNOSIS (APC) COMMENT
 ;;9002071.01^DENIAL NUMBER^600^* DIAGNOSIS (APC)
 ;;9002080^CHS FACILITY^14.1^*PROCESS PAT FOR AREA NC
 ;;9002080^CHS FACILITY^14.13^*PROCESS DOCUMENT RECS F
 ;;9002080.01^DOCUMENT^13.66^* VEND AGR NUMB (NOT USE
 ;;9002080.01^DOCUMENT^77^*DENTAL SERVICES
 ;;END
 ; Note:  above line is a loop ender.
 D BMES^XPDUTL("BEGIN Removing deleted fields from CHS data dictionaries.")
 NEW DA,DIK
 F ACHS=1:1 S X=$P($T(DELFLD+ACHS),";",3) Q:X="END"  D
 . D MES^XPDUTL($J("",5)_"Deleting '"_$$LJ^XLFSTR($P(X,U,4),30,".")_"' from '"_$P(X,U,2)_"'")
 . S DIK="^DD("_$P(X,U,1)_",",DA(1)=$P(X,U,1),DA=$P(X,U,3)
 . D ^DIK
 . Q
 . ; KILL ^DD(9999999.3911) ; 4 of the 0th nodes aren't KILL'd by ^DIK.
 .Q
 D MES^XPDUTL("END Removing deleted fields from CHS data dictionaries.")
 Q
 ;
DENOPT ; check for 638 facility, and add options if 638
 D BMES^XPDUTL("BEGIN Checking for 638 facility.")
 NEW ACHS
 S ACHS=0
 F  S ACHS=$O(^ACHSF(ACHS)) Q:'ACHS   I $P(^ACHSF(ACHS,0),U,8)="Y" D  Q
 . I $$ADD^XPDMENU("ACHS DEFDEN MENU PARM","ACHSDENPARM","P638") D MES^XPDUTL($J("",5)_"Denial parameter menu option added for 638 facility")
 . I $$ADD^XPDMENU("ACHSDENPARM","ACHSDENPARMREA","AREA") D MES^XPDUTL($J("",5)_"Denial parameter menu option added for 638 facility")
 .Q
 D MES^XPDUTL("END Checking for 638 facility.")
 Q
 ;
M278 ;
 D BMES^XPDUTL("BEGIN Attaching 278 menu.")
 I $$ADD^XPDMENU("ACHSAA","ACHS 278 MENU","278") D MES^XPDUTL($J("",5)_"278 Menu added to Document Generation menu."),MES^XPDUTL($J("",5)_"Note that the security lock was *NOT* allocated.") I 1
 E  D MES^XPDUTL($J("",5)_"ERROR:   278 menu attachment FAILED.")
 D MES^XPDUTL("END Attaching 278 menu.")
 Q
 ;
P3 ;EP - from KIDS.
 Q:'$G(XPDQUES("POS3"))
 D BMES^XPDUTL("BEGIN Re-index of Patient Name in Denials.")
 NEW ACHS,DA,DIK
 S ACHS=0
 F  S ACHS=$O(^ACHSDEN(ACHS)) Q:'ACHS  D
 . KILL ^ACHSDEN(ACHS,"D","C"),^ACHSDEN(ACHS,"D","N")
 . S DIK="^ACHSDEN("_ACHS_",""D"""_",",DA(1)=ACHS
 . F DIK(1)="7^C","10^N" D ENALL^DIK
 .Q
 D MES^XPDUTL("END Re-index of Patient Name in Denials.")
 Q
 ;
P4 ;EP - from KIDS.
 Q:'$G(XPDQUES("POS4"))
 D BMES^XPDUTL("BEGIN Re-index of Patient Name in Deferred Services.")
 NEW ACHS,DA,DIK
 S ACHS=0
 F  S ACHS=$O(^ACHSDEF(ACHS)) Q:'ACHS  D
 . KILL ^ACHSDEF(ACHS,"D","C"),^ACHSDEF(ACHS,"D","N")
 . S DIK="^ACHSDEF("_ACHS_",""D"""_",",DA(1)=ACHS
 . F DIK(1)="6^C","7^N" D ENALL^DIK
 .Q
 D MES^XPDUTL("END Re-index of Patient Name in Deferred Services.")
 Q
 ;
