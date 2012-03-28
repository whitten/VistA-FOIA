AMERREP3  ;IHS/OIT/SCR - CANNED REPORT SUB-ROUTINE FOR ER AUDIT REPORTS
 ;;3.0;ER VISIT SYSTEM;;FEB 23, 2009
 ;
ERVSTLOG ; EP AMER REPORT MENU
 N AMERQUIT,AMERSEL,DIR,Y
 S AMERQUIT=0
 F  D  Q:AMERQUIT
 .S DIR(0)="SO^1:DAILY ER AUDIT LOG;"
 .S DIR(0)=DIR(0)_"2:SINGLE ER VISIT ER AUDIT LOG;"
 .S DIR(0)=DIR(0)_"3:DATA ENTERER ER AUDIT LOG;"
 .S DIR(0)=DIR(0)_"4:VISIT FIELD ER AUDIT LOG;"
 .S DIR(0)=DIR(0)_"5:EDIT REASON ER AUDIT LOG;"
 .S DIR(0)=DIR(0)_"6:ER AUDIT LOG BY VISIT DATE;"
 .S DIR("A")="Select Report",DIR("?")="Select one choice or '^' to leave."
 .D ^DIR
 .I Y=""!(Y="^") S AMERQUIT=1 Q
 .S AMERSEL=Y
 .S AMERQUIT=$$REPORT(AMERSEL)
 .Q
 K AMERQUIT,AMERSEL,DIR,Y
 Q
 ; 
REPORT(AMERSEL) ;
 N AMERLINE,AMERBANN,AMERQUIT
 S AMERQUIT=0
 S %="",$P(%,"~",80)="",AMERLINE=% K %
 D EN^DDIOL(AMERLINE,"","!!")
 I AMERSEL=1 D
 .I '$$ERAUDDAY^AMERREP3() S AMERQUIT=1 Q  ; ER AUDIT BY DAY
 .D EN^DDIOL(AMERLINE,"","!!")
 .D EN^DDIOL("","","!")
 .Q
 I AMERSEL=2 D
 .I '$$ERAUDVST^AMERREP3() S AMERQUIT=1 Q  ; ER AUDIT BY ER VISIT
 .D EN^DDIOL(AMERLINE,"","!!")
 .D EN^DDIOL("","","!")
 .Q
 I AMERSEL=3 D
 .I '$$ERAUDUSR^AMERREP3() S AMERQUIT=1 Q  ; ER AUDIT BY DATA ENTERER
 .D EN^DDIOL(AMERLINE,"","!!")
 .D EN^DDIOL("","","!")
 .Q
 I AMERSEL=4 D
 .I '$$ERAUDFLD^AMERREP3() S AMERQUIT=1 Q  ; ER AUDIT BY ER VISIT FIELD
 .D EN^DDIOL(AMERLINE,"","!!")
 .D EN^DDIOL("","","!")
 .Q
 I AMERSEL=5 D
 .I '$$ERAUDRSN^AMERREP3() S AMERQUIT=1 Q  ; EDIT REASON ER AUDIT LOG
 .D EN^DDIOL(AMERLINE,"","!!")
 .D EN^DDIOL("","","!")
 .Q
 I AMERSEL=6 D
 .I '$$ERAUDLOG^AMERREP3() S AMERQUIT=1 Q  ; ER AUDIT LOG
 .D EN^DDIOL(AMERLINE,"","!!")
 .D EN^DDIOL("","","!")
 .Q
 I AMERQUIT Q 1
 I $D(DUOUT)!$D(DTOUT) K DUOUT,DTOUT Q 1
 K AMERLINE,AMERBANN,AMERQUIT
 Q 0
 ;
ERAUDDAY() ; EP FROM REPORT
 ; DAILY ER AUDIT LOG
 N AMERD1,AMERHDR,FR,TO,BY,DIC,DIR,FLDS,DHS,%DT,X
 S (AMERD1,AMERHDR)=""
 I '$$TIME^AMERREPT(.AMERD1) D EN^DDIOL("Invalid Time","","!!,?20,!!") Q 0
 D EN^DDIOL("Log Date: "_AMERD1,"","?10,!!")
 S AMERHDR="ER VISIT DAILY EDIT LOG REPORT"
 S BY=".01"
 ; First change the nice looking date to a file man date for addition
 S X=AMERD1,%DT=""
 D ^%DT
 S FR=Y+.0001,TO=Y+.2359
 S DHD=$$AMERDHD^AMERREPT(AMERHDR,AMERD1,"")
 S FLDS="[AMER AUDIT DAILY PRINT]"
 S L=0
 S DIC="^AMERAUDT("
 I '$D(POP) S POP=0
 D EN1^DIP
 Q 1
 ;
ERAUDVST() ; EP FROM REPORT
 ; ER AUDIT LOG BY VISIT
 N AMERHDR,FR,TO,BY,DIC,DIR,FLDS,DHS,%DT,X
 S AMERHDR=""
 S DIC="^AMERVSIT(",DIC(0)="AZEMQ",DIC("A")="Enter ER VISIT: "
 D ^DIC
 I Y<0 Q 1
 K DIC
 D EN^DDIOL("ER EDIT LOG FOR VISIT: "_$P(Y,U,2),"","?10,!!")
 S AMERHDR="ER VISIT LOG ENTRY"
 ; First change the nice looking date to a file man date for addition
 S BY=".02"
 S FR=$P(Y,U,2)
 S TO=$P(Y,U,2)
 S DHD=$$AMERDHD^AMERREPT(AMERHDR,"","")
 S FLDS="[CAPTION]"
 S L=0
 S DIC="^AMERAUDT("
 I '$D(POP) S POP=0
 D EN1^DIP
 I $G(IOST)["C-" S DIR(0)="E" D ^DIR
 Q 1
 ; 
ERAUDUSR() ; EP FROM REPORT
 ; ER AUDIT LOG BY USER
 ;IHS/OIT/SCR 10/01/08 - changed next line to remove unused variables
 ;N AMERD1,AMERD2,AMERD1X,AMERD2X,AMERHDR,FR,TO,BY,DIC,DIR
 N AMERD1,AMERD2,AMERHDR,FR,TO,BY,DIC,DIR
 S (AMERD1,AMERD2,AMERHDR)=""
 I '$$TIME^AMERREPT(.AMERD1,.AMERD2) D EN^DDIOL("Invalid Time","","!!,?20,!!") Q 1
 D EN^DDIOL("Start Date: "_AMERD1,"","?10,!!")
 D EN^DDIOL("End Date: "_AMERD2,"","?40")
 S AMERHDR="ER VISIT DATA ENTERER AUDIT LOG REPORT"
 S FLDS=".03;C3;L34,.01;C38,.02;C62"
 S BY=".03,.01"
 ;IHS/OIT/SCR 10/01/08 - changed next two lines to avoid error
 ;S FR="A,"_AMERD1X_""""
 ;S TO="Zz,"_AMERD2X_""""
 S FR="A,"_AMERD1_""
 S TO="Zz,"_AMERD2_""
 S DHD=$$AMERDHD^AMERREPT(AMERHDR,AMERD1,AMERD2)
 S L=0
 S DIC="^AMERAUDT("
 I '$D(POP) S POP=0
 D EN1^DIP
 I $G(IOST)["C-" S DIR(0)="E" D ^DIR
 Q 1
 ; 
ERAUDFLD() ; EP FROM REPORT
 ; ER AUDIT BY ER VISIT FIELD
 ;IHS/OIT/SCR 10/01/08 - changed next line to remove unused variables
 ; N AMERD1,AMERD2,AMERD1X,AMERD2X,AMERHDR,FR,TO,BY,DIC,DIR
 N AMERD1,AMERD2,AMERHDR,FR,TO,BY,DIC,DIR
 S (AMERD1,AMERD2,AMERHDR)=""
 I '$$TIME^AMERREPT(.AMERD1,.AMERD2) D EN^DDIOL("Invalid Time","","!!,?20,!!") Q 1
 D EN^DDIOL("Start Date: "_AMERD1,"","?10,!!")
 D EN^DDIOL("End Date: "_AMERD2,"","?40")
 S AMERHDR="ER VISIT FIELD AUDIT LOG REPORT"
 S FLDS="[AMER AUDIT FIELD PRINT]"
 S BY=".03,.01"
  ;IHS/OIT/SCR 10/01/08 - changed next two lines to avoid error
 ;S FR="A,"_AMERD1X_""""
 ;S TO="Zz,"_AMERD2X_""""
 S FR="A,"_AMERD1_""
 S TO="Zz,"_AMERD2_""
 S DHD=$$AMERDHD^AMERREPT(AMERHDR,AMERD1,AMERD2)
 S L=0
 S DIC="^AMERAUDT("
 I '$D(POP) S POP=0
 D EN1^DIP
 I $G(IOST)["C-" S DIR(0)="E" D ^DIR
 Q 1
 ; 
ERAUDLOG() ; EP FROM REPORT
 ; ER AUDIT LOG
 N AMERD1,AMERD2,AMERD1X,AMERD2X,AMERHDR,FR,TO,BY,DIC,DIR
 S (AMERD1,AMERD2,AMERHDR,AMERCLMS,AMERCLM2)=""
 I '$$TIME^AMERREPT(.AMERD1,.AMERD2) D EN^DDIOL("Invalid Time","","!!,?20,!!") Q 0
 D EN^DDIOL("Start Date: "_AMERD1,"","?10,!!")
 D EN^DDIOL("End Date: "_AMERD2,"","?40")
 S AMERHDR="ER VISIT EDIT LOG REPORT BY ER VISIT"
 S FLDS="[CAPTIONED]"
 S BY=".02"
 S FR=AMERD1
 S TO=AMERD2
 S DHD=$$AMERDHD^AMERREPT(AMERHDR,AMERD1,AMERD2)
 S L=0
 S DIC="^AMERAUDT("
 I '$D(POP) S POP=0
 D EN1^DIP
 I $G(IOST)["C-" S DIR(0)="E" D ^DIR
 Q 1
 ; 
ERAUDRSN() ; EP FROM REPORT
 ; AUDIT LOG BY EDIT REASON
 ;  ;IHS/OIT/SCR 10/01/08 - changed next line to remove unused variables
 ; N AMERD1,AMERD2,AMERD1X,AMERD2X,AMERHDR,FR,TO,BY,DIC,DIR,AMERMODE
 N AMERD1,AMERD2,AMERHDR,FR,TO,BY,DIC,DIR,AMERMODE
 S (AMERD1,AMERD2,AMERHDR,AMERCLMS,AMERCLM2)=""
 I '$$TIME^AMERREPT(.AMERD1,.AMERD2) D EN^DDIOL("Invalid Time","","!!,?20,!!") Q 0
 D EN^DDIOL("Start Date: "_AMERD1,"","?10,!!")
 D EN^DDIOL("End Date: "_AMERD2,"","?40")
 S AMERHDR="EDIT REASON AUDIT LOG REPORT"
 S FLDS=".01;C1;L15,.02;C22;L15,.03;C42;L30"
 S BY="2,.05;S1,.03,.01"
  ;IHS/OIT/SCR 10/01/08 - changed next two lines to avoid error
 ;S FR="A,A,"_AMERD1X_""""
 ;S TO="Zz,Zz,"_AMERD2X_""""
 S FR="A,A,"_AMERD1_""
 S TO="Zz,Zz,"_AMERD2_""
 S DHD=$$AMERDHD^AMERREPT(AMERHDR,AMERD1,AMERD2)
 S L=0
 S DIC="^AMERAUDT("
 I '$D(POP) S POP=0
 D EN1^DIP
 I $G(IOST)["C-" S DIR(0)="E" D ^DIR
 Q 1
