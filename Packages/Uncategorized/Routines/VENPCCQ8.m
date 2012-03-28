VENPCCQ8 ; IHS/OIT/GIS - BUILD VALIDATION ROUTINE ;  
 ;;2.6;PCC+;**1,3**;MAR 23, 2011
 ;
 ;
 ; VALIDATE PCC+ GUI INSTALLATION ; CAN ONLY BE RUN AFTER FULL KIDS INSTALL HAS BEEN COMPLETED ; 
 ;
 ; 
CSC261 ; EP - VALIDATE PCC+ 2.6 PATCH 1: WCM GUI
 N %
 S %="PCC+*2.6*1" D VALIDATE(%)
 Q
 ; 
VALIDATE(BUILD) ; EP - OPTION: VEN GUI VALIDATOR
 ; GENERIC VALIDATOR FOR VALIDATING AN INSTALL ; BASED ON BUILD NAME
 I $L($G(BUILD))
 E  Q
 N X,Y,Z,%,BIEN,RTN,FIEN,FILE,KIEN,OPTIEN,OK,ERR,OPT,RPC,KEY,GBL,USER,PMO,IEN,GMIEN,MIEN,LOC,BMXVB
 W !!!,"Validating the Well Child Module GUI installation...",!!
 W !!?2,"When you see the <> prompt, press <Enter> to continue or '^' to stop",!!!
 S OK=0,ERR=0
BUILD W !,"Checking KIDS build..."
 S BIEN=$O(^XPD(9.6,"B",BUILD,0))
 I 'BIEN W !,"Unable to locate the KIDS build for this package!  Validation terminated..." Q
 W "   < Validated"
 I $$STOP S OK=2 G CSCX
RTN W !,"Checking required ROUTINES..."
 S RTN="",OK=0
 F  S RTN=$O(^XPD(9.6,BIEN,"KRN",9.8,"NM","B",RTN)) Q:RTN=""  D
 . X ("I $L($T(^"_RTN_"))")
 . E  W !?5,U,RTN," is missing!" S OK=1,ERR=1
 . Q
 I 'OK W "   < All installed"
 I $$STOP S OK=2 G CSCX
DERTN ; DATA ENTRY ROUTINES
 S %=$T(ASQIEN+5^VENPCCQD) I %["^VEN(7.14,"!(%="") G FILE
 S Z=$C(90)
 S X=Z_"L VENPCCQB "_Z_"R VENPCCQB+1 "_Z_"R VENPCCQB "_Z_"S VENPCCQB "_Z_"L VENPCCQ8"
 X X
 S X=Z_"L VENPCCQC "_Z_"R VENPCCQC+1 "_Z_"R VENPCCQC "_Z_"S VENPCCQC "_Z_"L VENPCCQ8"
 X X
 S X=Z_"L VENPCCQD "_Z_"R VENPCCQD+1 "_Z_"R VENPCCQD "_Z_"S VENPCCQD "_Z_"L VENPCCQ8"
 X X
 W !!,"Routines for Data entry mnemonic WCE have been updated",!!
FILE W !,"Checking required FILES..."
 S FIEN=0,OK=0
 F  S FIEN=$O(^XPD(9.6,BIEN,4,"B",FIEN)) Q:'FIEN  D
 . I $D(^DD(FIEN,0)) Q
 . W !?5,"File ",FIEN," is missing!"
 . S OK=1,ERR=1
 . Q
 I 'OK W "   < All files present"
 I $$STOP S OK=2 G CSCX
RPC W !,"Checking REMOTE PROCEDURE CALLS..."
 S RPC="",OK=0
 F  S RPC=$O(^XPD(9.6,BIEN,"KRN",8994,"NM","B",RPC)) Q:RPC=""  D
 . I $O(^XWB(8994,"B",RPC,0)) Q
 . W !?5,"The RPC ",RPC," is missing!"
 . S OK=1,ERR=1
 . Q
 I 'OK W "   < All RPCs present"
 I $$STOP S OK=2 G CSCX
MEAS W !,"Checking MEASUREMENT TYPES..."
 S RPC="",OK=0
 S %="AFGLMPS"
 F I=1:1:$L(%) S X=$E(%,I) S Y=$O(^AUTTMSR("B",("ASQ"_X),0)) I 'Y S OK=1 W !?5,"Measurement type ASQ"_X_" is missing!"
 I 'OK W "   < All MEASUREMENT TYPES present"
 I $$STOP S OK=2 G CSCX
OPT W !,"Checking OPTIONS..."
 S OPT="",OK=0
 F  S OPT=$O(^XPD(9.6,BIEN,"KRN",19,"NM","B",OPT)) Q:OPT=""  D
 . I $O(^DIC(19,"B",OPT,0)) Q
 . W !?5,"The option ",OPT," is missing!"
 . S OK=1,ERR=1
 . Q
 ; I '$O(^DIC(19,"B","BMX RPC",0)) S OK=1,ERR=1 W !?5,"The option BMX RPC is missing!"
 I 'OK W "   < All OPTIONS present"
 I OK G BMX
 I $$STOP S OK=2 G CSCX
KEY W !,"Checking SECURITY KEYS..."
 S KEY="",OK=0
 F  S KEY=$O(^XPD(9.6,BIEN,"KRN",19.1,"NM","B",KEY)) Q:KEY=""  D
 . I $O(^DIC(19.1,"B",KEY,0)) Q
 . W !?5,"The key ",KEY," is missing!"
 . S OK=1,ERR=1
 . Q
 I 'OK W "   < All KEYS present"
 I $$STOP S OK=2 G CSCX
 S OK=0 D BMX(.OK) I OK S ERR=1
 I $$STOP S OK=2 G CSCX
DOM W ! D ACT^VENPCCQZ ; MAKE SURE THAT THE CURRENT SET OF KB DOMAINS IS ACTIVE
CSCX ; FINISH UP
 I $G(ERR) W !!,"The Well Child GUI validation process detected at least one problem",! S X="Please take corrective action and reinstall this patch." G EXITMSG
 I $G(OK)=2 W !!,"The validation process was terminated prematurely!",!,"Please complete the validation at a later time"
 I '$G(OK),'$G(ERR)
 E  I $$STOP Q
SUCCESS S X="Congratulations!  The Well Child GUI module has been validated"
EXITMSG D BOX(X)
 I $$STOP
 Q
 ;
BOX(X) ; EP - HIGHLIGHT TEXT INSIDE A * BOX
 I $G(X)="" Q
 I $L(X)>73 Q
 N Y,Z,%
 S %=$L(X)+1
 S Y="",$P(Y," ",%)="",Z="",$P(Z,"*",%+6)=""
 S X="*  "_X_"  *",Y="*  "_Y_"  *"
 W !!!,Z,!,Y,!,X,!,Y,!,Z,!!
 Q
 ; 
BMX(OK) W !!,"Checking BMX.NET BROKER..."
 I '$L($T(^BMXEHR)) D  G DOM
 . W !!,"Uh oh!..."
 . W !,"The BMX.NET Broker package (Ver 3.0 or higher) has not been installed."
 . W !,"Well Child Module GUI installation aborted!!!"
 . W !,"Install the BMX.NET Broker package now, and then rerun this KIDS build"
 . S OK=1
 . Q
 I 'OK W "  < Broker installed"
BMXSCH W !!,"Checking BMX SCHEMAS..."
 I '$O(^BMXADO("B","VEN CF VISIT LIST",0)) S OK=1 W !?5,"The schema 'VEN CF VISIT LIST' is missing!!"
 E  W "   < All BMX schemas are present"
BMXSTART W !!,"Checking BMX BROKER STATUS...",!!
 D STRT^BMXMON(5200)
 W !!,"The PCC+ GUI manager's menu has an option to START and STOP the BMX Broker"
 W !,"The BMX Broker must be running continuously to enable the PCC+ GUI"
 W !,"You must start the BMX Broker each time the RPMS server is re-booted"
 Q
 ; 
STOP() W !,"<>" ; EP - WAIT
 R %:DTIME
 I %?1."^" Q 1
 W $C(13),"             ",$C(13)
 Q 0
 ; 
SBMX ; EP - OPTION: VEN WCM START OR STOP BROKER
 N %,%Y,X,Y,Z,DIR,PORT
 S DIR(0)="NO^5000:99999:",DIR("A")="Enter BMX port",DIR("B")="9200"
 D ^DIR I Y'>1 Q
 S PORT=+Y
BSTART I $$SEMAPHOR^BMXMON(PORT,"LOCK") D  S %=$$STOP Q  ; START THE BROKER
 . S %=1
 . W !!,"Want to start the BMX broker listener now"
 . D YN^DICN I %'=1 Q
 . W !!
 . D STRT^BMXMON(PORT)
 . Q
BSTOP ; STOP THE BROKER
 W !!,"The BMX broker listener is currently running on port "_PORT,!," Want to stop it" ; STOP THE BROKER
 S %=1
 D YN^DICN I %'=1 Q
 W !!
 D STOP^BMXMON(PORT)
 I $$STOP
 Q
 ;
HDR ; EP - OPTION HEADER
 W !?10,"PCC+ GUI Management"
 W !,"----------------------------------------------------------",!!
 Q
 ;
