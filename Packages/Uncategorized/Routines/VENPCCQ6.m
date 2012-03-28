VENPCCQ6 ; IHS/OIT/GIS - POSTINIT KIDS SUPPORT ROUTINE ; 30 Dec 2009  6:36 PM
 ;;2.6;PCC+;**1,3**;MAR 23, 2011
 ;
 ;
POSTINIT ; POST INIT ROUTINE
 W !!!?15,"*****  PCC+ POST INITIALIZATION PROCEDURES  *****",!!
KB ; ADD ANY MISSING ENTRIES TO THE KB
 D ^VEN8INIT
 W !!,"Checking the Knowledgebase..."
 D KBM^VENPCCQ7(1,5170)
 D TOPIC^VENPCCKB ; UPDATE EDUCATION TOPIC FILE
 W "  < Validated!"
MEAS W !!,"Checking Measurements..."
 D MSR
 W "  < Validated!"
 I '$O(^DIC(19,"B","CIAV VUECENTRIC",0)) G CIABMX  ; SKIP TO BMX IF EHR NOT INSTALLED
CIAOBJ ; UPDATE THE VUECENTRIC REGISTERED OBJECTS FILE
 S OK=0
 W !!,"Checking the Vucentric Objects..."
 I '$D(^CIAVOBJ(19930.2)) G BMX
 D CKVOBJ^VENPCCQ7("PCC+ 2.6",.OK)
 I 'OK W "  < Validated!"
CIABMX ; CHECK CIABMX RPC AND ITS REGISTRATION
 S OK=0
 W !!,"Checking the CIABMX RPC..."
 D CIABMX^VENPCCQ7(.OK)
 I 'OK W "  < Validated!"
BMX ; CHECK BMX BROKER
 S OK=0 D BMX^VENPCCQ8(.OK) I OK S ERR=1
KEYS ; ASSIGN KEYS TO USERS
AKEY W !!,"Assign the key VENZMGUI to selected IT personnel and supervisors..."
 D HOLD^VENPCCQ7("VENZMGUI^VENZDESKTOP^VENZKBEDIT","M")
DESKTOP W !!,"Assign user privileges for the WCM Desktop GUI components..."
 D HOLD^VENPCCQ7("VENZDESKTOP","U")
KBP W !!,"Assign user privileges for the WCM Knowledgebase Management components..."
 D HOLD^VENPCCQ7("VENZKBEDIT","U")
MO ; INSTALL GUI MANAGEMENT OPTIONS
 W !!,"Installing the GUI management menu option (VEN_GUIMENU)..." ; ASSIGN GUI MANAGERS OPTION
 S OK=0,LOC=""
 S GMIEN=$O(^DIC(19,"B","VEN_GUIMENU",0))
 I 'GMIEN S OK=1 G MOK
 S MIEN=$O(^DIC(19,"B","VEN_MGR",0))
 I MIEN S LOC="PCC+"
 E  S MIEN=+$G(^VA(200,DUZ,201)) I MIEN S LOC="PMO"
 I LOC="" S OK=1 G MOK
 I '$O(^DIC(19,MIEN,10,"B",GMIEN,0)) D UMO^VENPCCQ7(MIEN,GMIEN) G MOK ; PLACE VEN_GUIMGR ON THE APPROPRIATE MENU FOR THIS USER
 I LOC="PCC+" G MOK
 S USER=0
 F  S USER=$O(^XUSEC("VENZMGUI",USER)) Q:'USER  D  ; ASSIGN TO PRIMARY MENU OF ALL USERS THAT HOLD THE 'VENZMGUI' KEY
 . S PMO=+$G(^VA(200,USER,201)) I 'PMO Q
 . I $O(^DIC(19,PMO,10,"B",GMIEN,0)) Q  ; ITS ALREADY IN THERE
 . D UMO^VENPCCQ7(PMO,GMIEN)
 . Q
MOK I OK S ERR=1 W !?5,"Option assignment failed!" S ERR=1 G PIX
 W !!?5,"The WCM GUI managers menu (VEN WCM_MENU = 'MPG') has been added ",!?5,"to all users that hold the VENZMGUI key"
 W !?7,"The MPG option is located on "
 I LOC="PCC+" W "the PCC+ Manager's Menu",!
 I LOC="PMO" W "your Primary Menu",!
SUCCESS S X="Congratulations!  The Well Child GUI module is successfully installed"
EXITMSG D BOX^VENPCCQ8(X)
PIX I $$STOP
 Q
 ;
STOP() W !,"<>" ; EP - WAIT
 R %:DTIME
 I %?1."^" Q 1
 W $C(13),"             ",$C(13)
 Q 0
 ; 
MGRKEY ; EP - OPTION: VEN GUI VALIDATOR
 W !!,"Assign the key VENZMGUI to selected IT personnel and supervisors..."
 D HOLD^VENPCCQ7("VENZMGUI^VENZDESKTOP^VENZKBEDIT","M")
 I $$STOP
 Q
 ; 
DESKKEY ; EP - OPTION: VEN GUI KB MANAGER
 W !!,"Assign user privileges for the WCM Desktop GUI components..."
 D HOLD^VENPCCQ7("VENZDESKTOP","U")
 I $$STOP
 Q
 ; 
KBKEY ; EP - OPTION: VEN GUI DESKTOP KEY
 W !!,"Assign user privileges for the WCM Knowledgebase Management components..."
 D HOLD^VENPCCQ7("VENZKBEDIT","U")
 I $$STOP
 Q
 ;
MSR ; EP - CHECK MEASUREMENTS
 ; FIX ISSUE WITH CIHA CORRUPT DEFINITION OF ASQP
 N %
 N DIC,DIE,DA,DR,X,Y,Z,DLAYGO,TYPE,IEN,CODE
 S (DIC,DIE,DLAYGO)=9999999.07,DIC(0)="LO",DA="",IEN=""
 F  S DA=$O(^AUTTMSR("B","ASQP",DA)) Q:'DA  D
 . S TYPE=$P($G(^AUTTMSR(DA,0)),U,2) I TYPE="" Q
 . I TYPE="ASQ - PROBLEM SOLVING" S IEN=DA Q
 . I TYPE="ASQ PROBLEM SOLVING" D  Q
 .. S TYPE="ZSQ",CODE="00"
 .. S DR=".01////^S X=TYPE;.02////^S X=TYPE;.03////^S X=CODE;.04////^S X=1"
 .. L +^AUTTMSR(DA):1 I  D ^DIE L -^AUTTMSR(DA)
 .. Q
 . Q
 I IEN D ^XBFMK Q
 S X="ASQP",TYPE="ASQ - PROBLEM SOLVING",CODE=64
 D ^DIC I Y=-1 Q
 S DA=+Y,DR=".02////^S X=TYPE;.03////^S X=CODE"
 L +^AUTTMSR(DA):1 I  D ^DIE L -^AUTTMSR(DA)
 D ^XBFMK
 Q
 ;
PRE ; PRE INSTALL
 N DIK,DA S DA=0,DIK="^VEN(7.26," F  S DA=$O(^VEN(7.26,DA)) Q:'DA  D ^DIK ; CLEAN OUT KIDS SUPPORT FILE
 S DA=$O(^BMXADO("B","VEN ASQ TX",0)) I DA S DIK="^BMXADO(" D ^DIK ; FIX BMX FILE
 ; Check if alpha site installed as version 2.65 and fix
 I $$VERSION^XPDUTL("VEN")=2.65 D
 . NEW I
 . S I=$O(^DIC(9.4,"C","VEN",0)) S:I'>0 I=$O(^DIC(9.4,"B","VEN",0))
 . S VENUPD(9.4,I_",",13)=2.6
 . D FILE^DIE("","VENUPD")
 . K VENUPD
 ;
 ; For alpha sites that might have mispellings
 NEW TEXT,IEN
 F TEXT="IHS.WCM.EHR.ASQ.ASQCOMPONENT","IHS.WCM.EHR.PATIENTED.PATIENTEDCOMPONENT" D
 . S IEN=$$FIND1^DIC(19930.2,"","B",TEXT,"","","ERROR")
 . I IEN=0 Q
 . S VENUPD(19930.2,IEN_",",1)="@"
 I $D(VENUPD) D FILE^DIE("","VENUPD")
 K VENUPD
 Q
 ; 
