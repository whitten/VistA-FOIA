AGED1 ; IHS/ASDS/EFG - EDIT PG 1 - ELIG/IDENTIFIERS ; MAR 19, 2010  
 ;;7.1;PATIENT REGISTRATION;**2,4,5,7,8**;AUG 25, 2005
 ;
 ;AG*7.1*7 - Modified code to allow the new page 10 to be called
 ;
 I "YC"[AGOPT(14) S AG("SVELIG")=""
 S AG("SVELIG")=$P($G(^AUPNPAT(DFN,11)),U,12)
 N OLDST,NEWST,OLDADDR2,NEWADDR2,OLDADDR3,NEWADDR3,OLDCITY,NEWCITY,OLDSTATE  ;AG*7.1*4
 N NEWSTATE,OLDZIP,NEWZIP,OLDHPH,NEWHPH,TEMPY  ;AG*7.1*4
 ;
VAR ;EP
 Q:'$D(^DPT(DFN,0))
 S AG("PG")=1
 S ROUTID=$P($T(+1)," ")  ;SET ROUTINE ID FOR PROGRAMMER VIEW
 S AGPAT=$P($G(^DPT(DFN,0)),U)
 S AGCHRT=$S($D(^AUPNPAT(DFN,41,DUZ(2),0)):$P($G(^AUPNPAT(DFN,41,DUZ(2),0)),U,2),1:"xxxxx")
 S AG("AUPN")=""
 S:$D(^AUPNPAT(DFN,0)) AG("AUPN")=^(0)
 W ?36,$$DTEST^AGUTILS(DFN)
 S AGLINE("-")=$TR($J(" ",78)," ","-")
 S AGLINE("EQ")=$TR($J(" ",78)," ","=")
 S $P(AGLINE("PGLN"),"=",81)=""
VAR2 ;
 D DRAW
 W:'$D(AGSEENLY) !,AGLINE("PGLN")
 Q:$D(AGSEENLY)
SSNA ;Ver=add ask if patient has verified SSN
 I $P($G(^AUPNPAT(DFN,0)),U,23)]"",$D(^AUTTSSN($P(^(0),U,23),0)),"A"[$P($G(^(0)),U) D
 . W !!,*7
 . K DIR
 . S DIR(0)="Y"
 . S DIR("A")="Has the patient verified the SSN added by SSA  "
 . S DIR("B")="NO"
 . D ^DIR
 . K DIR
 . Q:Y=0
 . K DR
 . S DIE="^AUPNPAT("
 . S DA=DFN
 . S DR=".23///V"
 . D ^DIE
 . K DR
ESSNA . D DRAW
 D CKELIG:"YC"[AGOPT(14)
 K DIR
 S DIR("?")="Enter your choice now."
 S DIR("?",1)="You may enter the item number of the field you wish to edit,"
 S DIR("?",2)="OR you can enter 'P#' where P stands for 'page' and '#' stands for"
 S DIR("?",3)="the page you wish to jump to, OR enter '^' to go back one page"
 S DIR("?",4)="OR, enter '^^' to exit the edit screens, OR RETURN to go to the next screen."
 S DIR("A")="CHANGE which item? (1-"_AG("N")_") NONE//" D READ
 I $D(MYERRS("C","E")),(Y'?1N.N),(Y'=AGOPT("ESCAPE")) W !,"ERRORS ON THIS PAGE. PLEASE FIX BEFORE EXITING!!" H 3 G VAR
 S TEMPY=Y I +Y'=Y D ASKADD^AG3A S Y=TEMPY K TEMPY  ;AG*7.1*4
 Q:Y=AGOPT("ESCAPE")
 Q:$D(DUOUT)!$D(DTOUT)!$D(DFOUT)
 G:$D(AG("ED"))&'$D(AGXTERN) @("^AGED"_AG("ED"))
 G END:$D(DLOUT)!(Y["N"),VAR:$D(AG("ERR"))
 I $D(DQOUT)!(+Y<1)!(+Y>AG("N"))&($P($G(^AUPNPAT(DFN,11)),U,12)'="") W !!,"You must enter a number from 1 to ",AG("N") H 2 G VAR
 ;S AG("C")="ELIG^AG2A,DOB^AG2A,COB^AG3A,SOB^AG3A,SEX^AG2A,SSN^AG3A,MSTAT,EDCOM^AG2B,ST^AG3A,ADDR2,ADDR3,CITY^AG3A,STATE^AG3A,ZIP^AG3A,LOC,HPH^AG3A,OPH^AG3A,MSGPH"
 ;S AG("C")="ELIG^AG2A,DOB^AG2A,COB^AG3A,SOB^AG3A,SEX^AG2A,SSN^AG3A,MSTAT,EDCOM^AG2B,ST^AG3A,ADDR2,ADDR3,CITY^AG3A,STATE^AG3A,ZIP^AG3A,LOC,HPH^AG3A,OPH^AG3A,MSGPH,WEB"  ;AG*7.1*2 ITEM 5 PAGE 11
 ;S AG("C")="ELIG^AG2A,DOB^AG2A,COB^AG3A,SOB^AG3A,SEX^AG2A,SSN^AG3A,MSTAT,EDCOM^AG2B,ALLADDR^AG3A,ALLADDR^AG3A,ALLADDR^AG3A,ALLADDR^AG3A,ALLADDR^AG3A,ALLADDR^AG3A,LOC,ALLADDR^AG3A,OPH^AG3A,MSGPH,WEB,EDEMAIL"  ;AG*7.1*4
 ;S AG("C")="ELIG^AG2A,DOB^AG2A,COB^AG3A,SOB^AG3A,SEX^AG2A,SSN^AG3A,MSTAT,EDCOM^AG2B,ST^AG3A,ADDR2,ADDR3,CITY^AG3A,STATE^AG3A,ZIP^AG3A,LOC,HPH^AG3A,OPH^AG3A,MSGPH,EDEMAIL"  ;AG*7.1*7
 S AG("C")="ELIG^AG2A,DOB^AG2A,COB^AG3A,SOB^AG3A,SEX^AG2A,SSN^AG3A,MSTAT,EDCOM^AG2B,ST^AG3A,ADDR2,ADDR3,CITY^AG3A,STATE^AG3A,ZIP^AG3A,LOC,HPH^AG3A,OPH^AG3A,MSGPH"  ;AG*7.1*8
 I '$D(AGSEENLY) D C
 G VAR
END ;
 K AG,DA,DIC,DR,DUOUT,DLOUT,DQOUT,DTOUT,DFOUT,AGSCRN,Y,DIR,ROUTID
 Q:$D(AGXTERN)
 I $P($G(^AUPNPAT(DFN,11)),U,12)=""  W *7,!!,"Eligibility Status must be entered." H 2 G VAR
 G ^AGED2
READ ;EP
 K DFOUT,DTOUT,DUOUT,DQOUT,DLOUT,AG("ED"),AG("ERR"),DIROUT
 S DIR(0)="FO"
 D ^DIR
 Q:$D(DTOUT)
 S:Y="/.,"!(Y="^^") DFOUT=""
 S:Y="" DLOUT=""
 S:Y="^" (DUOUT,Y)=""
 S:Y?1"?".E!(Y["^") (DQOUT,Y)=""
 S X=Y,Y=$$UP^XLFSTR(X)
 Q:Y="P"
 I $E(Y,1)="p" S $E(Y,1)="P"
 I $E(Y,1)="P"&($P($G(^AUPNPAT(DFN,11)),U,12)'="") D
 . S AG("ED")=+$P($E(Y,2,99),".")
 . I AG("ED")<1!(AG("ED")>10) D  ;AG*7.1*7
 .. W *7,!!,"Use only pages 1 through 10."  ;AG*7.1*7
 .. H 2
 .. K AG("ED")
 .. S AG("ERR")=""
 . I $D(AG("ED"))  D
 .. I AG("ED")>0&(AG("ED")<11)  D  ;AG*7.1*7
 ... I AG("ED")=4 S AG("ED")="4A"
 ... I AG("ED")=5 S AG("ED")="BEA"  ;REPLACE OLD PG 5A WITH BENEFITS COORD SCREEN
 ... I AG("ED")=6 S AG("ED")=13
 ... I AG("ED")=8 S AG("ED")=11
 ... I AG("ED")=7 S AG("ED")=8
 ... I AG("ED")=9 S AG("ED")="11A"
 ... I AG("ED")=10 S AG("ED")="10A"  ;AG*7.1*7
 I $E(Y,1)="P"&($P($G(^AUPNPAT(DFN,11)),U,12)="") D
 . W *7,!!,"Eligibility Status must be entered." H 2
 Q
C ;EP - Edit multiple fields on a Reg edit page.
 S AGY=Y
 F AGI=1:1 S AG("SEL")=+$P(AGY,",",AGI) Q:AG("SEL")<1!(AG("SEL")>AG("N"))  D @($P(AG("C"),",",AG("SEL")))
 D UPDATE1^AGED(DUZ(2),DFN,1,"")
 K AGI,AGY
EC ;
 Q
MSTAT ;GET MARITAL STATUS FROM VA PATIENT FILE
 K DUOUT
 S DIE="^DPT("
 S DA=DFN
 S DR=.05
 D ^DIE
 S:$D(Y) DUOUT=""
 K DIC("S"),DIC("W"),DIC("A"),DIC("B")
 Q
ADDR2 ;GET ADDRESS LINE 2 FROM VA PATIENT FILE
 S OLDADDR2=$$GET1^DIQ(2,DFN_",",.112)    ;AG*7.1*4
 K DUOUT
 S DIE="^DPT("
 S DA=DFN
 S DR=.112
 D ^DIE
 S:$D(Y) DUOUT=""
 K DIC("S"),DIC("W"),DIC("A"),DIC("B")
 S NEWADDR2=$$GET1^DIQ(2,DFN_",",.112)  ;AG*7.1*4
 Q
ADDR3 ;GET ADDRESS LINE 3 FROM VA PATIENT FILE
 S OLDADDR3=$$GET1^DIQ(2,DFN_",",.113)  ;AG*7.1*4
 K DUOUT
 S DIE="^DPT("
 S DA=DFN
 S DR=.113
 D ^DIE
 S:$D(Y) DUOUT=""
 K DIC("S"),DIC("W"),DIC("A"),DIC("B")
 S NEWADDR3=$$GET1^DIQ(2,DFN_",",.113)  ;AG*7.1*4
 Q
LOC ;GET LOCATION OF HOME FROM PATIENT FILE
 K DUOUT
 S DIE="^AUPNPAT("
 S DA=DFN
 S DR=1201
 D ^DIE
 S:$D(Y) DUOUT=""
 K DIC("S"),DIC("W"),DIC("A"),DIC("B")
 Q
MSGPH ;GET MESSAGE PHONE FROM PATIENT FILE
 K DUOUT
 S DIE="^AUPNPAT("
 S DA=DFN
 S DR=1801
 D ^DIE
 S:$D(Y) DUOUT=""
 K DIC
 Q
 ;AG*7.1*8 - Entire tag reworked to handle new multiple field
WEB ;EP - INTERNET ACCESS QUESTION
 N DIC,DA,DIC,X,Y,OIEN,NIEN
 ;
 ;Get latest entry ien
 S OIEN=$O(^AUPNPAT(DFN,81,"B"),-1)
 ;
 ;Define new entry and save
 S DIC="^AUPNPAT("_DFN_",81,",DA(1)=DFN
 S DIC(0)="L"
 S X=DT
 S DLAYGO="9000001.81",DIC("P")=DLAYGO
 I '$D(^AUPNPAT(DFN,81,0)) S ^AUPNPAT(DFN,81,0)="^9000001.81D^^"
 K DO,DD D FILE^DICN
 K DIC,DA,DIC,X
 Q:+Y<1
 ;
 ;Copy existing entry into current one
 I OIEN]"" M ^AUPNPAT(DFN,81,+Y)=^AUPNPAT(DFN,81,OIEN)
 ;
 N DIE,DIR,DIC,DA,DR
 S (NIEN,DA)=+Y
 S DA(1)=DFN
 S DIE="^AUPNPAT("_DA(1)_",81,"
 S DEF=$$GET1^DIQ(9000001.81,NIEN_","_DFN_",",.02,"Y")
 S DR=.02 S:DEF]"" DR=DR_"//"_DEF
 D ^DIE
 K DIE,DIR,DIC,DA,DR
 ;
 ;If No Internet Access, Remove the Where value
 I '$P($G(^AUPNPAT(DFN,81,NIEN,0)),U,2) D  Q
 . N AGVAR,ERROR,WIEN
 . S AGVAR(9000001.81,NIEN_","_DFN_",",".03")="@"
 . S WIEN=0 F  S WIEN=$O(^AUPNPAT(DFN,81,NIEN,1,WIEN)) Q:'WIEN  D
 .. S AGVAR(9000001.811,WIEN_","_NIEN_","_DFN_",",.01)="@"
 . D FILE^DIE("","AGVAR","ERROR")
 ;
 N DIE,DIR,DIC,DR,DA
 S DA(1)=DFN,DA=NIEN
 S DIE="^AUPNPAT("_DA(1)_",81,"
 S DR=".04"
 D ^DIE
 ;
 Q
 ;
 ;NEW CODE AG*7.1*4
EDEMAIL ;EP - EDIT CURRENT EMAIL ADDRESS
 W !!
 N OLDEMAIL
 S OLDEMAIL=$$GET1^DIQ(9000001,DFN_",",1802)
 K DIE,DIC,DR,DA,DIR
 S DIE="^AUPNPAT("
 S DA=DFN
 S DR=1802
 D ^DIE
 Q:$D(Y)
 Q:$$GET1^DIQ(9000001,DFN_",",1802)=""
 Q:OLDEMAIL=$$GET1^DIQ(9000001,DFN_",",1802)  ;NO CHANGE
 K DIR
 S DIR(0)="Y"
 S DIR("A")="Should this new email address be added to the historical addresses"
 S DIR("B")="Y"
 D ^DIR
 Q:'Y!$D(DTOUT)!$D(DUOUT)
 W !!,"Adding to PREVIOUS EMAIL FIELD...." H 2
 D UPDTEMAL^AGUTILS(DFN)
 ;END NEW CODE
 Q
 ;
QUES ;EP
 W !!,"To change an item, enter a number from 1 to ",AG("N")
 W ". (Press RETURN for ""NO-CHANGE"".)"
 D READ
 Q
CKELIG ;EP
 I $D(^AUPNPAT(DFN,11)),$P($G(^(11)),U,12)'=AG("SVELIG") D  Q
 . S AG("SVELIG")=$P($G(^AUPNPAT(DFN,11)),U,12)
 . D CALCELIG^AGBIC2
 . W *7,!,"This patient's Eligibility has been changed to "
 . W $P(AG("NARR1"),":",2)
 Q
DRAW ;DRAW PAGE 1
 S AG("PG")=1
 S AG("N")=18   ;AG*7.1*8
 S DA=DFN
 S ROUTID=$P($T(+1)," ")  ;SET ROUTINE ID FOR PROGRAMMER VIEW
 D ^AGED
 K ^UTILITY("DIQ1",$J)
 ;OUTPUT OPTION NUMBER,FIELD NAME, AND DATA
 F AG=1:1:AG("N") D
 . S AGSCRN=$P($T(@1+AG),";;",2,19)
 . S DIC=$P(AGSCRN,U,3)         ;FILE NUMBER
 . S DR=$P(AGSCRN,U,4)          ;FIELD NUMBER
 . I AG'=4&(AG'=13)&(AG'=17)&(AG'=14) W ?1,AG,".",?(27-$L($P($G(^DD(DIC,DR,0)),U))),$P($G(^DD(DIC,DR,0)),U)," : "  ;AG*7.1*4
 . I AG=17 W ?45,AG,".",$P(AGSCRN,U)," : "  ;AG*7.1*2 ITEM 5 PAGE 11
 . I AG=4 W "      ",AG,".",$P(AGSCRN,U)," : "
 . I AG=13 W "      ",AG,".",$P(AGSCRN,U)," : "
 . ;I AG=14 W ?54,AG,". ZIP CODE : "  ;AG*7.1*4
 . I AG=14 W ?54," ",AG,". ZIP CODE : "  ;AG*7.1*5 H4532
 . ;DISPLAY MSG BELOW IF THERE ARE DIRECTIONS TO PATIENT'S HOME
 . I AG=15&($D(^AUPNPAT(DFN,12,1,0))) D
 .. I '$D(AGSEENLY) W "LOCATION OF HOME CONTAINS DATA"
 .. I $D(AGSEENLY)  D
 ... S HOME=$P($G(^AUPNPAT(DFN,12,1,0)),U)
 ... S AG("Y")=$L(HOME),LNCNT=0
 ... F  S AG("K")=$E(HOME,1,49) Q:$L(AG("K"))=0  D
 .... S HOME=$E(HOME,50,AG("Y"))
 .... I LNCNT>0 W !
 .... W ?30,AG("K")
 .... S LNCNT=LNCNT+1
 ... K HOME,AG("Y"),AG("K"),LNCNT
 . I AG'=15&(AG'=4)&(AG'=13) D
 .. K AGRES
 .. S TEMPDIC=DIC
 .. S DIQ="AGRES",DIQ(0)="E" D EN^DIQ1
 .. S DIC=TEMPDIC
 .. ;W $G(AGRES(DIC,DFN,DR,"E"))
 .. ;BEGIN NEW CODE FOR ABOVE LINE IHS/SD/TPF AG*7.1*4
 .. N HIT
 .. D EN^AGSECCHK("AGZVIEWSSN",.HIT)
 .. I DIC=2,(DR=.09) D
 ... I HIT="Y" W $$GET1^DIQ(2,DFN_",",.09) Q
 ... I ($$GET1^DIQ(9000001,DFN_",",.23,"E")="V") W $$GET1^DIQ(9000001,DFN_",",1107.3)
 ... E  W $$GET1^DIQ(2,DFN_",",.09)
 .. E  W $G(AGRES(DIC,DFN,DR,"E"))
 .. ;END NEW CODE
 .. K AGRES,TEMPDIC,AGRES
 . ;I AG=19 W !?1,AG,"." W ?15,"EMAIL ADDRESS: ",$$GET1^DIQ(9000001,DFN_",",1802)
 . I AG=4 D
 .. K AGRES
 .. S TEMPDIC=DIC
 .. S DIQ="AGRES",DIQ(0)="I" D EN^DIQ1
 .. S DIC=TEMPDIC
 .. I $G(AGRES(DIC,DFN,DR,"I"))'="" W $P($G(^DIC(5,$G(AGRES(DIC,DFN,DR,"I")),0)),U,2)
 .. K AGRES,TEMPDIC,AGRES
 . I AG=13 D
 .. K AGRES
 .. S TEMPDIC=DIC
 .. S DIQ="AGRES",DIQ(0)="I" D EN^DIQ1
 .. S DIC=TEMPDIC
 .. I $G(AGRES(DIC,DFN,DR,"I"))'="" W $P($G(^DIC(5,$G(AGRES(DIC,DFN,DR,"I")),0)),U,2)
 .. K AGRES,TEMPDIC,AGRES
 . ;SHOW SSN VERIFICATION STATUS NEXT TO THE SSN FIELD
 . I AG=6 D
 .. I $P($G(^DPT(DFN,0)),U,9)="" D
 ... S AGSSNCHK=$P($G(^AUPNPAT(DFN,0)),U,24)
 ... I AGSSNCHK=1 W "Not Available"
 ... I AGSSNCHK=2 W "Patient refused"
 ... I AGSSNCHK=3 W "Patient will submit"
 ... I AGSSNCHK="" W "Reason for no SSN not yet entered"
 .. I $P($G(^AUPNPAT(DFN,0)),U,23)'="",$D(^AUTTSSN($P($G(^(0)),U,23),0)) W "(",$P($G(^(0)),U,2),")"
 .. I $P($G(^DPT(DFN,0)),U,9)'=""&($P($G(^AUPNPAT(DFN,0)),U,23)="") W "(Not yet verified by the SSA)"
 . I AG'=3&(AG'=12)&(AG'=18)&(AG'=16)&(AG'=13) W !  ;AG*7.1*4/AG*7.1*8
 . I AG=8!(AG=15) D
 .. W AGLINE("-"),!
 S AG("N")=18  ;AG*7.1*8
 W !,AGLINE("-")
 K MYERRS,MYVARS
 D FETCHERR^AGEDERR(AG("PG"),.MYERRS)
 S MYVARS("DFN")=DFN,MYVARS("FINDCALL")="",MYVARS("SITE")=DUZ(2)
 D EDITCHEK^AGEDERR(.MYERRS,.MYVARS,1)
 Q
 ;***************************************************************
 ; ON LINES BELOW:
 ; PIECE 1= FLD LBL
 ; PIECE 2= POSITION ON LINE TO DISP FLD
 ; PIECE 3= FILE #
 ; PIECE 4= FLD #
1 ;
 ;;ELIGIBILITY STATUS^10^9000001^1112
 ;;DOB^25^2^.03
 ;;CITY OF BIRTH^15^2^.092
 ;;ST^62^2^.093
 ;;SEX^25^2^.02
 ;;SSN^25^2^.09
 ;;MARITAL STATUS^14^2^.05
 ;;CURRENT COMMUNITY^11^9000001^1118
 ;;MAILING ADDRESS-STREET^6^2^.111
 ;;STREET ADDRESS [LINE 2]^5^2^.112
 ;;STREET ADDRESS [LINE 3]^5^2^.113
 ;;MAILING ADDRESS-CITY^8^2^.114
 ;;ST^62^2^.115
 ;;MAILING ADDRESS-ZIP^9^2^.116
 ;;LOCATION OF HOME^12^9000001^1201
 ;;HOME PHONE^16^2^.131
 ;;WORK PHONE^17^2^.132
 ;;MESSAGE PHONE^18^9000001^1801
