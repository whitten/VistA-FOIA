KMPSPST ;SF/KAK - SAGG Post Install ;20 FEB 98 2:26 pm [ 03/13/2003  1:32 PM ]
 ;;1.8;SAGG PROJECT;;May 29, 1998
 ;
 N DA,DIC,DIE,DIK,DIU,DR
PACK ;
 D BMES^XPDUTL("PART 1: Updating SAGG PROJECT data in PACKAGE file ...")
 S KMPSDA(1)=+$O(^DIC(9.4,"B","SAGG PROJECT",0)) G:'KMPSDA(1) MOV
 ; Update data in ROUTINE field
 K DA,DR S KMPSDA=0 F  S KMPSDA=$O(^DIC(9.4,KMPSDA(1),2,KMPSDA)) Q:'+KMPSDA  D
 .S KMPSIENS=$$IENS^DILF(.KMPSDA)
 .K KMPSFDA S KMPSFDA(9.42,KMPSIENS,.01)="@"
 .D FILE^DIE("","KMPSFDA","")
 S KMPSIENS="+1,"_KMPSDA(1)_","
 F I=1:1:2 S KMPSROUT=$P($T(ROUT+I),";;",2) F J=1:1 S KMPSRTN=$P(KMPSROUT,"^",J) Q:KMPSRTN=""  D
 .K KMPSFDA S KMPSFDA(9.42,KMPSIENS,.01)=KMPSRTN
 .D UPDATE^DIE("","KMPSFDA","")
 ; Update data in GLOBAL field
 K DA,DR S DA=0,DA(1)=KMPSDA(1) F  S DA=$O(^DIC(9.4,DA(1),3,DA)) Q:'+DA  D
 .S KMPSGBL=$G(^DIC(9.4,DA(1),3,DA,0)) Q:KMPSGBL=""
 .S DIE="^DIC(9.4,"_DA(1)_",3,"
 .S DR=".01///"_$S(KMPSGBL="A1B5":"KMPS(8970.1",KMPSGBL="A1B5GE":"XTMP('KMPS'",1:KMPSGBL) D ^DIE
 ; Update data in FILE field
 K DA,DR S DA=0,DA(1)=KMPSDA(1) F  S DA=$O(^DIC(9.4,DA(1),4,DA)) Q:'+DA  D
 .S KMPSFIL=$G(^DIC(9.4,DA(1),4,DA,0)) Q:KMPSFIL=""
 .S DIE="^DIC(9.4,"_DA(1)_",4,"
 .S DR=".01///"_$S(KMPSFIL=11120:"8970.1",1:KMPSFIL) D ^DIE
 ; Update data in *LOWEST FILE NUMBER, *HIGHEST FILE NUMBER and
 ; *POST-INITIALIZATION ROUTINE fields
 K DA,DR S DA=KMPSDA(1),DR="10.6///8970.1;11///8970.1;11.3///I;914///@"
 S DIE="^DIC(9.4," D ^DIE
 D MES^XPDUTL("        Done (Part 1)")
 ;
MOV ;
 D BMES^XPDUTL("PART 2: Moving data from SAGG PROJECT file #11120 to file #8970.1 ...")
 I '$D(^KMPS(8970.1)) D BMES^XPDUTL("        ERROR - Missing new file #8970.1.") G ERRM1
 I '$D(^A1B5(11120)) D BMES^XPDUTL("        INFO - No data found in old file #11120.") G ERRM2
 I '$D(^A1B5(11120,1,1,"B")) D BMES^XPDUTL("        INFO - No VOLUME SET data found in old file #11120.") G ERRM2
 S KMPSOS=$P($G(^%ZOSF("OS")),"^")
 S (DIC,DIE)="^KMPS(8970.1,",DIC(0)="LZ",DLAYGO=8970.1,X=1
 D ^DIC I Y=-1 D BMES^XPDUTL("        ERROR - Unsuccessful in adding entry 1 into file #8970.1") G ERRM2
 S ERR=0,KMPSVOL="" F  S KMPSVOL=$O(^A1B5(11120,1,1,"B",KMPSVOL)) Q:ERR!(KMPSVOL="")  D
 .S KMPSUCI=$O(^A1B5(11120,1,1,"B",KMPSVOL,0)),KMPSUCI=$P(^A1B5(11120,1,1,KMPSUCI,0),U,2)
 .S DA(1)=1,DLAYGO=8970.1,DIC="^KMPS(8970.1,1,1,",DIC(0)="LZ"
 .S DIC("P")=$P(^DD(8970.1,.03,0),U,2),X=KMPSVOL
 .D ^DIC I Y=-1 S ERR=1 D BMES^XPDUTL("        ERROR - Unsuccessful in adding VOLUME SET "_KMPSVOL) Q
 .S DIE=DIC K DIC
 .S DA=+Y,DR=".01///^S X=KMPSVOL"_$S(KMPSOS["DSM":$S(KMPSUCI'="":";.02///^S X=KMPSUCI",1:""),1:"") D ^DIE
 I KMPSOS["DSM" D
 .K DIE S DA=1,DIE="^KMPS(8970.1,"
 .S DR=".02///^S X=XPDQUES(""POSKMPS1"",""B"");.025///^S X=XPDQUES(""POSKMPS2"",""B"")"
 .D ^DIE
 G:ERR ERRM2
 D MES^XPDUTL("        Done (Part 2)")
 ;
DEL ;
 D BMES^XPDUTL("PART 3: Deleting old SAGG PROJECT (#11120) file ...")
 K DIC,DIE,DIK S DIK="^A1B5(11120,",DA=1
 D ^DIK
 S DIU=11120.01,DIU(0)="DST" D EN^DIU2
 S DIU="^A1B5(11120,",DIU(0)="DT" D EN^DIU2
 D MES^XPDUTL("        Done (Part 3)")
 ;
TSK ;
 D BMES^XPDUTL("PART 4: Rescheduling new KMPS SAGG REPORT background task ...")
 N DIFROM,ZTSK K DIFROM
 S KMPSDA=$O(^DIC(19,"B","KMPS SAGG REPORT",0)) G:'KMPSDA ERRT
 S KMPSDA=$O(^DIC(19.2,"B",KMPSDA,0)) G:'KMPSDA ERRT
 S KMPSDTH=$P($G(^DIC(19.2,KMPSDA,0)),"^",2),ZTSK=+$G(^DIC(19.2,KMPSDA,1))
 I ZTSK D KILL^%ZTLOAD K DR S DA=KMPSDA,DIE="^DIC(19.2,",DR="12///@" D ^DIE
 I KMPSDTH="" G ERRT
 K DR S DA=KMPSDA,DIE="^DIC(19.2,",DR="2///@" D ^DIE
 I KMPSDTH<DT G ERRT
 K DR S DA=KMPSDA,DIE="^DIC(19.2,",DR="2////"_KMPSDTH D ^DIE
 I '+$G(^DIC(19.2,KMPSDA,1)) G ERRT
 D MES^XPDUTL("        Done (Part 4)")
 ;
GBL ;
 D BMES^XPDUTL("PART 5: Deleting old A1B5GE temporary collection global ...")
 S KMPSSITE=$G(^DD("SITE",1)) I KMPSSITE'="" K ^A1B5GE(KMPSSITE)
 K ^A1B5GE("ERROR"),^A1B5GE("START"),^A1B5GE("STOP")
 D MES^XPDUTL("        Done (Part 5)")
 ;
 D BMES^XPDUTL("Post-install routine complete.")
EXIT ;
 K DA,DIC,DIE,DIK,DIU,DLAYGO,DR,ERR
 K KMPSDA,KMPSFDA,KMPSDTH,KMPSFIL,KMPSGBL,KMPSIENS,KMPSOS,KMPSRTN,KMPSSITE,KMPSUCI,KMPSVOL,X
 Q
ERRM1 ;
 D MES^XPDUTL("        Please correct the ERROR condition.")
 D MES^XPDUTL("        Then re-run routine MOV^KMPSPST.")
 G EXIT
ERRM2 ;
 D MES^XPDUTL("        Enter data manually with 'Edit SAGG Project File'")
 D MES^XPDUTL("        [KMPS SAGG FILE] option.")
 G DEL
ERRT ;
 D MES^XPDUTL("        ERROR - Not able to reschedule new KMPS SAGG REPORT background task.")
 D MES^XPDUTL("        Use 'Schedule/Unschedule Options' [XUTM SCHEDULE] to reschedule")
 D MES^XPDUTL("        the 'SAGG Master Background Task' [KMPS SAGG REPORT].")
 G GBL
 ;
ROUT ; Names of routines
 ;;KMPSENV^KMPSGE^KMPSLK^KMPSLOAD^KMPSPRE^KMPSPST^KMPSUTL
 ;;ZKMPSGEM^ZKMPSGEN^ZKMPSGEV^ZKMPSGSM^ZKMPSGSN^ZKMPSGSV
