AG71A5 ;IHS/SD/EFG - Patient Registration 7.1 PATCH 5 PRE/POST INSTALL ;   
 ;;7.1;PATIENT REGISTRATION;**2,3,4,5**;APR 23,2009
 ;
 Q
PRE ;EP - From KIDS.
 ;'AG PATIENT REGISTRATION ERROR CODES' FILE
 ;FILE 9009061.5 IS DINUMED AND HAS IDENTIFIERS. KIDS WILL NOT TRANSFER
 ;NEW DATA FOR THOSE FIELDS WITHOUT FIRST KILLING THE GLOBAL DATA FIRST.
 ;ONLY NEED IF CHANGING DATA IN THIS FILE
 S IEN="" F  S IEN=$O(^AGEDERRS(IEN)) Q:IEN=""  K ^AGEDERRS(IEN)
 Q
POST ;EP - From KIDS.
 D BMES^XPDUTL("Beginning post-install routine (POST^AG71A)."),TS
 ;
 ;CONVERT ENTRIES IN .04 IN AUPNPAT TO
 ;NEW MULTIPLE FIELD 3601 IN AUPNPAT
 ;DONE ONLY IF PATCH 1 NOT FOUND
 I '$$PATCH("AG*7.1*1") D BMES^XPDUTL("Converting 'Release of Information' AUPNPAT field .04 to new multiple field 3601."),TS D ^AGCNVROI
 ;
 D BMES^XPDUTL("Fixing private eligibility with missing Policy Holder .08 field. or missing insurer pointer"),TS
 D PRVT
 ;
 D BMES^XPDUTL("Collecting Medicaid eligibility entries with missing State .04 field."),TS
 D MCD
 ;
 D BMES^XPDUTL("Fixing Medicare eligibility B cross references."),TS
 D MCR
 ;
 D BMES^XPDUTL("Fixing INSURER IENs containing decimal."),TS
 D INSURER
 ;
 D BMES^XPDUTL("Fixing Medicare records with missing .01 fields"),TS
 D MCR
 ;
 D BMES^XPDUTL("Fixing Rail Road entries with missing .01 field."),TS
 D RRE
 ;
 D BMES^XPDUTL("Fixing incomplete Guarantor records."),TS
 D GUAR
 ;
 D BMES^XPDUTL("Fixing patient file with dangling D x-ref"),TS
 D PAT
 ;
 D BMES^XPDUTL("Fixing Policy Holder fields"),TS
 D POLHOLD
 ;
 D BMES^XPDUTL("Add File #2 VA PATIENT address fields as Site Mandatory field in the REGISTRATION PARAMETER file"),TS
 D ADDMAN  ;ADD MANDATORY ADDRESS FIELDS IN FILE 2 TO REGISTRATION PARAMETER FILE
 ;
 ;TAKE CURRENT ADDRESS AND PHONE FOUND IN FILE #2 AND PLACE INTO THE NEW
 ;HISTORICAL MULTIPLES
 D BMES^XPDUTL("Adding VA PATIENT address fields to PREVIOUS ADDRESS FIELD multiple of File #9000001"),TS
 ;
 S X=$$LAST^XPDUTL("IHS PATIENT REGISTRATION","7.1")
 ;
 ;AG*7.1*5
 I $P(X,U)<4 D
 .D ADDHIST  ;DO ONLY ON PATCH 4
 .D SETDEF  ;SET DEAFULTS IN NEW PARAMETERS. DO ONLY ONCE
 ;
 I $$INSTALLD^AG71ENV("AG*7.1") D
 . D TS,BMES^XPDUTL("Delivering AG*7.1 install message to select users ...")
 . D MAIL
 . D BMES^XPDUTL("Post-install routine is complete."),TS
 ;
 Q:$$INSTALLD^AG71ENV("AG*7.1")
 ;
 D TS,OPTRES("AGMENU")
 ;
 D TS,BMES^XPDUTL("Delivering AG*7.1 install message to select users...")
 ;
 D MAIL
 ;
 D BMES^XPDUTL("Post-install routine is complete."),TS
 Q
MAIL ;Send install mail message.
 N DIFROM,XMSUB,XMDUZ,XMTEXT,XMY
 K ^TMP("AG71MS",$J)
 S ^TMP("AG71MS",$J,1)=" --- AG v 7.1 Patch 4 has been installed into this uci ---"
 S ^TMP("AG71MS",$J,2)=" "
 S CNT=3
 K AGERRLST
 S %=0
 F  S %=$O(^XTMP("XPDI",XPDA,"BLD",XPDBLD,1,%)) Q:'%   S ^TMP("AG71MS",$J,(%+CNT))=" "_^(%,0)
 S XMSUB=$P($P($T(+1),";",2)," ",3,99),XMDUZ=$S($G(DUZ):DUZ,1:.5),XMTEXT="^TMP(""AG71MS"",$J,",XMY(1)="",XMY(DUZ)=""
 F %="AGZMENU","XUMGR","XUPROG","XUPROGMODE" D SINGLE(%)
 D ^XMD
 K ^TMP("AG71MS",$J)
 Q
SINGLE(K) ;EP - Get holders of a single key K.
 N Y
 S Y=0
 Q:'$D(^XUSEC(K))
 F  S Y=$O(^XUSEC(K,Y)) Q:'Y  S XMY(Y)=""
 Q
 ;
OPTRES(AGM) ;
 D BMES^XPDUTL("Restoring '"_AGM_"' option to PRE-install configuration...")
 NEW AG,AGI
 I '$D(^XTMP("AG71",7.2,"OPTSAV",AGM)) D BMES^XPDUTL("FAILED.  Option '"_AGM_"' was not previously saved.") Q
 S AG=0
 F  S AG=$O(^XTMP("AG71",7.2,"OPTSAV",AGM,AG)) Q:'AG  S AGI=^(AG) I '$$ADD^XPDMENU(AGM,$P(AGI,U),$P(AGI,U,2),$P(AGI,U,3)) D BMES^XPDUTL("....FAILED to re-attach "_$P(AGI,U)_" to "_AGM_".")
 Q
TS D MES^XPDUTL($$HTE^XLFDT($H)) Q
MCD ;CLEAR MCD RECORDS MISSING .01 FIELD
 ;MUST DO DIRECT KILL BECAUSE X-REF NOT THERE. BOMBS ON USE OF ^DIK
 K AGERRLST
 N HRN,DFN,ST,MCDNUM
 S RECNO=0
 F  S RECNO=$O(^AUPNMCD(RECNO)) Q:'RECNO  D
 .;IF THE INS. PTR IS MISSING LETS FIX IT SO TPB CLAIMS GENERATOR DOESN'T BLOW UP
 .I '$G(^AUPNMCD(RECNO,0))!('$P($G(^AUPNMCD(RECNO,0)),U)) K ^AUPNMCD(RECNO) Q
 .I $P($G(^AUPNMCD(RECNO,0)),U,4)="" S DFN=$P($G(^AUPNMCD(RECNO,0)),U),AGERRLST(RECNO)=$P($G(^DPT(DFN,0)),U)_U_$P($G(^AUPNPAT(DFN,41,DUZ(2),0)),U,2)
 Q
RRE ;FIX "B" X-REF ENTRIES WITH NO ZERO RECORD
 ;MUST DO DIRECT KILL BECAUSE RECORD NOT THERE. BOMBS ON USE OF ^DIK DOESN'T WORK
 S RECNO=""
 F  S RECNO=$O(^AUPNRRE("B",RECNO)) Q:'RECNO  D
 .I $P($G(^AUPNRRE(RECNO,0)),U)="" K ^AUPNRRE(RECNO),^AUPNRRE("B",RECNO)
 S RECNO=0
 F  S RECNO=$O(^AUPNRRE(RECNO)) Q:'RECNO  D
 .I $P($G(^AUPNRRE(RECNO,0)),U)="" K ^AUPNRRE(RECNO),^AUPNRRE("B",RECNO)
 Q
MCR ;
 S RECNO=""
 F  S RECNO=$O(^AUPNMCR("B",RECNO)) Q:'RECNO  D
 .I $P($G(^AUPNMCR(RECNO,0)),U)="" K ^AUPNMCR(RECNO),^AUPNMCR("B",RECNO)
 S RECNO=0
 F  S RECNO=$O(^AUPNMCR(RECNO)) Q:'RECNO  D
 .I $P($G(^AUPNMCR(RECNO,0)),U)="" K ^AUPNMCR(RECNO),^AUPNMCR("B",RECNO)
 Q
INSURER ;EP - CAN WE INCLUDE THIS IN AG PATCH 1??
 S RECNO=0
 F  S RECNO=$O(^AUTNINS(RECNO)) Q:'RECNO  D
 .I RECNO[(".") K ^AUTNINS(RECNO),^AUTNINS("B",RECNO)
 S RECNO=""
 F  S RECNO=$O(^AUTNINS("B",RECNO)) Q:RECNO=""  D
 .S RECIEN=""
 .F  S RECIEN=$O(^AUTNINS("B",RECNO,RECIEN)) Q:RECIEN=""  D
 ..Q:$P($G(^AUTNINS(RECIEN,0)),U)'=""
 ..K DA,DIR,DIE,DIK,DIC,DR
 ..S DA=RECIEN,DIK="^AUTNINS(" D ^DIK
 Q
PAT ;CLEAN UP D X-REF IN PATIENT FILE
 S HRN="" F  S HRN=$O(^AUPNPAT("D",HRN)) Q:HRN=""  D
 .S RECNO="" F  S RECNO=$O(^AUPNPAT("D",HRN,RECNO)) Q:RECNO=""  D
 ..I '$D(^AUPNPAT(RECNO))!('$D(^DPT(RECNO))) K ^AUPNPAT("D",HRN,RECNO)
 Q
PRVT ;CLEAR ANY PRIVATE ELIG RECORDS MISSING INSURER POINTER
 ;MUST DO DIRECT KILL BECAUSE X-REF NOT THERE. BOMBS ON USE OF ^DIK
 S RECNO=0
 F  S RECNO=$O(^AUPNPRVT(RECNO)) Q:'RECNO  D
 .S D1=0
 .F  S D1=$O(^AUPNPRVT(RECNO,11,D1)) Q:'D1  D
 ..I $P($G(^AUPNPRVT(RECNO,11,D1,0)),U)="" K ^AUPNPRVT(RECNO,11,D1) Q
 ..I $P($G(^AUPNPRVT(RECNO,11,D1,0)),U,8)="" S DA=D1,DA(1)=RECNO,DIK="^AUPNPRVT("_DA(1)_",11," D ^DIK
 ..I $O(^AUPNPRVT(RECNO,11,0)) Q
 ..S DA=RECNO,DIK="^AUPNPRVT(" D ^DIK
 S RECNO=0 F  S RECNO=$O(^AUPNPRVT("B",RECNO)) Q:RECNO=""  D
 .S IEN=0 F  S IEN=$O(^AUPNPRVT("B",RECNO,IEN)) Q:'IEN  D
 ..I $P($G(^AUPNPRVT(IEN,0)),U)="" K ^AUPNPRVT(IEN),^AUPNPRVT("B",RECNO,IEN)
 Q
GUAR ;CLEAR GUARANTOR RECORDS WITH INCOMPETE ENTRIES
 N PATPTR
 S PATPTR=0
 F  S PATPTR=$O(^AUPNGUAR(PATPTR)) Q:'PATPTR  D GUAR1(PATPTR)
 Q
GUAR1(PATPTR) ;EP - DELETE GUARANTOR ENTRIES WITH MISSING GUARANTORS OR DATES
 N SUB1,SUB11,REDO
REDO ;
 S SUB1=$O(^AUPNGUAR(PATPTR,1,0))
 I 'SUB1 D  Q  ;NO GUARANTORS FOUND AT ALL
 .K DIE,DIK,DA,DIC S DIK="^AUPNGUAR(",DA=PATPTR D ^DIK
 ;FOR EACH GUARANTOR ARE THERE EFFECTIVE DATES?
 S (SUB1,REDO)=0
 F  S SUB1=$O(^AUPNGUAR(PATPTR,1,SUB1)) Q:'SUB1  D  G REDO:REDO
 .S SUB11=$O(^AUPNGUAR(PATPTR,1,SUB1,11,0))
 .I 'SUB11 D  Q
 ..S REDO=1 K DIE,DIK,DA,DIC S DA(1)=PATPTR,DA=SUB1,DIK="^AUPNGUAR("_DA(1)_",1," D ^DIK
 Q
VAALERT ;EP - SEND VA ALERT IF WANTED
 S XQAMSG="Patient Regsistration "_$P($T(+2),";",3)_" Patch "_$P($T(+2),";",5)_" INSTALL complete."
 S XQA("AG MAIL GROUP")=""
 D SETUP^XQALERT
 Q
 ;FIX STATE,SEX AND DOB FIELDS WHICH WERE INCORRECTLY STUFFED
POLHOLD ;
 N IEN,DOB,STATE,SEX
 S IEN=0
 F  S IEN=$O(^AUPN3PPH(IEN)) Q:'IEN  D
 .S STATE=$P($G(^AUPN3PPH(IEN,0)),U,12)
 .S DOB=$P($G(^AUPN3PPH(IEN,0)),U,19)
 .S SEX=$P($G(^AUPN3PPH(IEN,0)),U,8)
 .Q:(STATE="")&(DOB="")&(SEX="")
 .I STATE'="" D
 ..Q:+STATE>0  ;DON'T DO ANYTHING IF ALREADY A POINTER
 ..W !,IEN,"*",STATE
 ..K DIC
 ..S X=STATE
 ..S DIC=5
 ..D ^DIC
 ..Q:Y<0
 ..K DIE,DR,DIC,DA
 ..S DA=IEN
 ..S DIE="^AUPN3PPH("
 ..S DR=".12///^S X=STATE"
 ..D ^DIE
 .I DOB'="" D
 ..Q:DOB'["/"
 ..W !,IEN,"*",DOB
 ..K DIE,DR,DIC,DA
 ..S DA=IEN
 ..S DIE="^AUPN3PPH("
 ..S DR=".19///^S X=DOB"
 ..D ^DIE
 .I SEX'="" D
 ..Q:$L(SEX)=1
 ..W !,IEN,"*",SEX
 ..K DIE,DR,DIC,DA
 ..S DA=IEN
 ..S DIE="^AUPN3PPH("
 ..S DR=".08///^S X=SEX"
 ..D ^DIE
 Q
PATCH(X) ;return 1 if patch X was installed, X=aaaa*nn.nn*nnnn
 Q:X'?1.4UN1"*"1.2N1"."1.2N.1(1"V",1"T").2N1"*"1.4N 0
 N %,I,J
 S I=$O(^DIC(9.4,"C",$P(X,"*"),0)) Q:'I 0
 S J=$O(^DIC(9.4,I,22,"B",$P(X,"*",2),0)),X=$P(X,"*",3) Q:'J 0
 ;check if patch is just a number
 Q:$O(^DIC(9.4,I,22,J,"PAH","B",X,0)) 1
 S %=$O(^DIC(9.4,I,22,J,"PAH","B",X_" SEQ"))
 Q (X=+%)
 ;ADD CORRECT MANDATORY ADDRESS FIELDS FOR VA PATIENT
ADDMAN ;
 K DA,DIC,DIE,DR,DO,DD,DINUM
 S DUZ2=0
 F  S DUZ2=$O(^AGFAC(DUZ2)) Q:'DUZ2  D
 .S DA(1)=$O(^AGFAC(DUZ2,11,"B",2,""))  ;JUST DO THIS FOR 'VA PATIENT' FILE
 .;S DUZ2=516,DA(1)=1
 .Q:'DA(1)
 .S DA(2)=DUZ2
 .S DIC="^AGFAC("_DA(2)_",11,"_DA(1)_",1,"
 .S X="STATE"
 .S DIC(0)="LX"
 .S DIC("DR")=".02///^S X=0"
 .D ^DIC
 .S X="ZIP CODE"
 .S DIC(0)="LX"
 .S DIC("DR")=".02///^S X=0"
 .D ^DIC
 .S X="CITY"
 .S DIC(0)="LX"
 .S DIC("DR")=".02///^S X=0"
 .D ^DIC
 .S X="STREET ADDRESS [LINE 1]"
 .D ^DIC
 Q
 ;FIX DANGLING "c" X-REF WITH NO POLICY HOLDER IN 11 NODE
POL ;
 S POLH="" F  S POLH=$O(^AUPNPRVT("C",POLH)) Q:POLH=""  D
 .S POLM="" F  S POLM=$O(^AUPNPRVT("C",POLH,POLM)) Q:POLM=""  D
 ..S REC="" F  S REC=$O(^AUPNPRVT("C",POLH,POLM,REC)) Q:REC=""  D
 ...I $P($G(^AUPNPRVT(POLM,11,REC,0)),U,8)'=POLH D
 ....W !,POLM,"**",REC,!?5,"POLH:",POLH
 ....W !?5,"PIECE 8:",$P($G(^AUPNPRVT(POLM,11,REC,0)),U,8)
 ....I $P($G(^AUPNPRVT(POLM,11,REC,0)),U,8)="" W !?5,"INSURER NODE:",$G(^AUPNPRVT(POLM,11,REC,0))
 ....I $P($G(^AUPNPRVT(POLM,11,REC,0)),U,8)="",($G(^AUPNPRVT(POLM,11,REC,0))="") K ^AUPNPRVT("C",POLH,POLM,REC)
 Q
 ;
ADDHIST ;EP - ADD FIRST HISTORICAL ENTRY
 N PADFN
 S PATDFN=0
 F  S PATDFN=$O(^DPT(PATDFN)) Q:'PATDFN  D
 .Q:'$D(^AUPNPAT(PATDFN))
 .D UPDTHADD^AGUTILS(PATDFN,"F")  ;F MEANS FORCE THE DATA IN
 Q
 ;
SETDEF ;EP - SET DEFAULTS IN NEW PARAMETERS
 K DIE,DIR,DIC,DA
 S DUZ2=0
 F  S DUZ2=$O(^AGFAC(DUZ2)) Q:'DUZ2  D
 .S DA=DUZ2
 .S DIE="^AGFAC("
 .S DR="601////1;602////0"
 .D ^DIE
 Q
