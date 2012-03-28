LRPXAPI2 ;VA/SLC/STAFF - Lab Extract API code ;2/26/04  15:15
 ;;5.2;LAB SERVICE;**1030**;NOV 01, 1997
 ;;5.2;LAB SERVICE;**295**;Sep 27, 1994;Build 5
 ;
VERIFIED(LRDFN,LRIDT) ; $$(lrdfn,lridt) -> 1 if verified, else 0
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.  Return 0.
 Q 0
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 ; checks for date report completed
 I +$P($G(^LR(LRDFN,"CH",LRIDT,0)),U,3) Q 1
 Q 0
 ;
MIVERIFY(LRDFN,LRIDT,SUB) ; $$(lrdfn,lridt,sub) -> 1 if verified, else 0
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.  Return 0.
 Q 0
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 ; checks for report date approved on subscript
 S SUB=+$G(SUB)
 I SUB>0,SUB<17,$G(^LR(LRDFN,"MI",LRIDT,SUB)) Q 1
 Q 0
 ;
APVERIFY(LRDFN,LRIDT,APSUB) ; $$(lrdfn,lridt,ap subscrpt) -> 1 if verified
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.  Return 0.
 Q 0
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 ; autopsy checks for:
 ;   date of death, 
 ;   date autopsy report completed,
 ;   autopsy release date/time
 ; otherwise, checks for date report completed and report release date
 N OK
 S OK=0
 I APSUB="CY"!(APSUB="EM")!(APSUB="SP") D  Q OK
 . I $P($G(^LR(LRDFN,APSUB,LRIDT,0)),U,3),$P(^(0),U,11) S OK=1
 I APSUB="AU" D  Q OK
 . I '$$DOD^LRPXAPIU($$DFN^LRPXAPIU(LRDFN)) Q
 . I '$P($G(^LR(LRDFN,"AU")),U,3) Q
 . I '$P(^LR(LRDFN,"AU"),U,15) Q
 . S OK=1
 Q OK
 ;
VAL(LRDFN,LRIDT,LRDN) ; from LRPXAPI
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.  Return 0.
 Q 0
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 ; $$(lrdfn,lridt,lrdn) -> result node
 Q $G(^LR(LRDFN,"CH",LRIDT,LRDN))
 ;
REFVAL(REF) ; from LRPXAPI
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.  Return 0.
 Q 0
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 ; $$(reference location in ^LR) -> data node
 N SUB
 I REF'[";" Q ""
 S SUB=$P(REF,";",2)
 S SUB=""""_SUB_""""
 S $P(REF,";",2)=SUB
 S REF=$TR(REF,";",",")
 S REF="^LR("_REF_")"
 Q $G(@REF)
 ;
LRPXRM(RESULT,REF,ITEM,TYPES) ; from LRPXAPI
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.
 Q
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 ; returns result node from index subscript as RESULT
 N FILE,IEN,SECTION,TEST,VALUES
 S RESULT=""
 S VALUES=$$REFVAL(REF)
 I '$L(VALUES) Q
 I ITEM>0 D  Q
 . S $P(VALUES,U)=$$VRESULT^LRPXAPIU(ITEM,$P(VALUES,U))
 . S RESULT=+ITEM_U_$$TESTNM^LRPXAPIU(+ITEM)_U_VALUES
 . D SC(.RESULT,REF,TYPES)
 I '$L(ITEM) D  Q
 . I $P(REF,";",2)'="CH" Q
 . S TEST=$$TEST^LRPXAPIU(+$P(REF,";",4))
 . I 'TEST Q
 . S RESULT=TEST_U_$$TESTNM^LRPXAPIU(TEST)_U_VALUES
 . D SC(.RESULT,REF,TYPES)
 S SECTION=$P(ITEM,";") I $L(SECTION)'=1 Q
 S FILE=$P(ITEM,";",2) I $L(FILE)'=1 Q
 S IEN=+$P(ITEM,";",3) I 'IEN Q
 I SECTION="M" D  Q
 . I FILE="S" S RESULT=IEN_U_$$SPECNM^LRPXAPIU(IEN)_U_VALUES Q
 . I FILE="T" S RESULT=IEN_U_$$TESTNM^LRPXAPIU(IEN)_U_VALUES Q
 . I FILE="O" S RESULT=IEN_U_$$BUGNM^LRPXAPIU(IEN)_U_VALUES Q
 . I FILE="A" S RESULT=IEN_U_$$ABNM^LRPXAPIU(IEN)_U_VALUES Q
 . I FILE="M" S RESULT=IEN_U_$$TBNM^LRPXAPIU(IEN)_U_VALUES Q
 I SECTION="A" D  Q
 . I FILE="S" S RESULT=U_$$UP^XLFSTR(VALUES)_U_VALUES Q
 . I FILE="T" S RESULT=IEN_U_$$TESTNM^LRPXAPIU(IEN)_U_VALUES Q
 . I FILE="O" S RESULT=IEN_U_$$ORGNM^LRPXAPIU(IEN)_U_VALUES Q
 . I FILE="D" S RESULT=IEN_U_$$DISNM^LRPXAPIU(IEN)_U_VALUES Q
 . I FILE="M" S RESULT=IEN_U_$$MORPHNM^LRPXAPIU(IEN)_U_VALUES Q
 . I FILE="E" S RESULT=IEN_U_$$ETINM^LRPXAPIU(IEN)_U_VALUES Q
 . I FILE="F" S RESULT=IEN_U_$$FUNNM^LRPXAPIU(IEN)_U_VALUES Q
 . I FILE="P" S RESULT=IEN_U_$$PROCNM^LRPXAPIU(IEN)_U_VALUES Q
 . I FILE="I" S RESULT=IEN_U_$$ICD9^LRPXAPIU(IEN)_U_VALUES Q
 Q
 ;
SC(RESULT,REF,TYPES) ;
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.
 Q
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 N CNT,LINE,LRDFN,LRIDT,SPEC
 I TYPES["S" D
 . S $P(REF,";",4)=0
 . S SPEC=+$P($$REFVAL(REF),U,5)
 . S RESULT("SPECIMEN")=SPEC_U_$$SPECNM^LRPXAPIU(SPEC)
 I TYPES["C" D
 . S CNT=0,LRDFN=+$P(REF,";"),LRIDT=+$P(REF,";",3)
 . S LINE=0
 . F  S LINE=$O(^LR(LRDFN,"CH",LRIDT,1,LINE)) Q:LINE<1  D
 .. S CNT=CNT+1
 .. S RESULT("COMMENTS",CNT)=$G(^LR(LRDFN,"CH",LRIDT,1,LINE,0))
 . S RESULT("COMMENTS")=CNT
 Q
 ;
SPEC(DATA,DFN,DATE,STYPE,ERR) ; from LRPXAPI
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.
 Q
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 ; returns specimen node, comment, values in array DATA
 N LRDFN,LRIDT K DATA
 S ERR=0
 S LRDFN=$$LRDFN^LRPXAPIU(DFN)
 I 'LRDFN S ERR=1 Q
 I 'DATE S ERR=1 Q
 S LRIDT=$$LRIDT^LRPXAPIU(DATE)
 D LRSPEC(.DATA,LRDFN,LRIDT,STYPE,.ERR)
 Q
 ;
LRSPEC(DATA,LRDFN,LRIDT,STYPE,ERR) ; from LRPXAPI
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.
 Q
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 ; returns specimen node, comment, values in array DATA
 K DATA
 S ERR=0
 I '$O(^LR(LRDFN,"CH",LRIDT,0)) S ERR=1 Q
 I '$L(STYPE) S STYPE="A"
 I STYPE="S" D SSPEC(.DATA,LRDFN,LRIDT) Q
 I STYPE="C" D CSPEC(.DATA,LRDFN,LRIDT) Q
 I STYPE="V" D VSPEC(.DATA,LRDFN,LRIDT) Q
 I STYPE="A" D
 . N ALL K ALL
 . D SSPEC(.DATA,LRDFN,LRIDT) M ALL=DATA
 . D CSPEC(.DATA,LRDFN,LRIDT) M ALL=DATA
 . D VSPEC(.DATA,LRDFN,LRIDT) M ALL=DATA
 . K DATA M DATA=ALL
 Q
 ;
SSPEC(DATA,LRDFN,LRIDT) ; specimen node values
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.
 Q
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 K DATA
 S DATA("S")=$G(^LR(LRDFN,"CH",LRIDT,0))
 Q
 ;
CSPEC(DATA,LRDFN,LRIDT) ; specimen comments
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.
 Q
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 N CMT,CNT K DATA
 I '$D(^LR(LRDFN,"CH",LRIDT,1,0)) Q
 S CNT=0
 S CMT=0
 F  S CMT=$O(^LR(LRDFN,"CH",LRIDT,1,CMT)) Q:CMT<1  D
 . I '$D(^LR(LRDFN,"CH",LRIDT,1,CMT,0)) Q
 . S CNT=CNT+1
 . S DATA("C",CNT)=^LR(LRDFN,"CH",LRIDT,1,CMT,0)
 Q
 ;
VSPEC(DATA,LRDFN,LRIDT) ; test nodes for collected specimen
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.
 Q
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 N CNT,LRDN,VALUE K DATA
 S CNT=0
 S LRDN=1
 F  S LRDN=$O(^LR(LRDFN,"CH",LRIDT,LRDN)) Q:LRDN<2  S VALUE=^(LRDN) D
 . S CNT=CNT+1
 . S DATA("V",CNT)=LRDN_U_VALUE
 Q
 ;
COMMENT(LRDFN,LRIDT) ; $$(lrdfn,lridt) --> 1 if comment exists, else 0
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.  Return 0.
 Q 0
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 I +$O(^LR(LRDFN,"CH",LRIDT,1,0)) Q 1
 Q 0
 ;
VALUE(RESULT,DFN,DATE,TEST,COND,ERR) ; from LRPXAPI, LRPXAPI1
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.
 Q
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 ; returns result node that has met conditions as RESULT
 N LRDFN,LRIDT,LRDN
 I $L(COND),'$$CONDOK^LRPXAPIU(COND,"C") S ERR=1 Q
 I $L(COND) S COND=$$REPLACE("I "_COND)
 S RESULT=""
 S ERR=0
 S LRDFN=$$LRDFN^LRPXAPIU(DFN)
 I 'LRDFN S ERR=1 Q
 I 'DATE S ERR=1 Q
 S LRIDT=$$LRIDT^LRPXAPIU(DATE)
 S LRDN=$$LRDN^LRPXAPIU(TEST)
 I 'LRDN S ERR=1 Q
 D LRVAL(.RESULT,LRDFN,LRIDT,LRDN,COND,.ERR)
 Q
 ;
LRVALUE(RESULT,LRDFN,LRIDT,LRDN,COND,ERR) ; from LRPXAPI, LRPXAPI1
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.
 Q
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 ; returns result node that has met conditions as RESULT
 I $L(COND),'$$CONDOK^LRPXAPIU(COND,"C") S ERR=1 Q
 I $L(COND) S COND=$$REPLACE("I "_COND)
 D LRVAL(.RESULT,LRDFN,LRIDT,LRDN,COND,.ERR)
 Q
 ;
LRVAL(RESULT,LRDFN,LRIDT,LRDN,COND,ERR) ;
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.
 Q
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 N F,S,V,VALUE
 S RESULT=""
 S ERR=0
 S VALUE=$G(^LR(LRDFN,"CH",LRIDT,LRDN))
 I '$L(VALUE) S ERR=1 Q
 I $L(COND) D  I ERR Q
 . S V=$P(VALUE,U)
 . S F=$P(VALUE,U,2)
 . S S=$P($P(VALUE,U,5),"!")
 . I 'S S S=$P($G(^LR(LRDFN,"CH",LRIDT,0)),U,5)
 . X COND I '$T S ERR=1
 S RESULT=VALUE
 Q
 ;
CHNODE(ARRAY,NODE) ; from LRPXAPI
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.
 Q
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 N NAME,NAME3,NAME5,NODE3,NODE5,PIECE,PIECE3,PIECE5,SUB K ARRAY
 I '$L(NODE) Q
 S NAME="RESULT^FLAG^CODES^VERIFIER^NORMALS^DATE-R^DATE-T^^INSTITUTION^LEDI^INSTRUMENT^TYPE"
 S NAME3="NLT-O!NLT-R!LOINC!METHOD!MAP!TEST"
 S NAME5="SPEC!LOW!HIGH!LOW-C!HIGH-C!!UNITS!DELTA-T!DELTA-V!DEF!LOW-T!HIGH-T"
 F PIECE=1:1:12 D
 . I PIECE=8 Q
 . S SUB=$P(NAME,U,PIECE)
 . I PIECE=8 Q
 . I PIECE=3 D  Q
 .. S NODE3=$P(NODE,U,3)
 .. F PIECE3=1:1:6 S ARRAY($P(NAME3,"!",PIECE3))=$P(NODE3,"!",PIECE3)
 . I PIECE=5 D  Q
 .. S NODE5=$P(NODE,U,5)
 .. F PIECE5=1:1:12 D
 ... I PIECE5=6 Q
 ... S ARRAY($P(NAME5,"!",PIECE5))=$P(NODE5,"!",PIECE5)
 . S ARRAY(SUB)=$P(NODE,U,PIECE)
 Q
 ;
ACCY(TESTS,ACC,BDN) ; from LRPXAPI
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.
 Q
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 ; returns TESTS from yearly accession, ACC, BDN required
 ; BDN is beginning date number
 ; TESTS is array of file 60 iens
 N DIC,LRAA,LRAAB,LRAD,LRAN,TEST,X,Y K DIC,TESTS
 I '$L($G(ACC)) Q
 S LRAAB=$P(ACC," ")
 I LRAAB="" Q
 S BDN=$E($G(BDN))
 I BDN'>1 Q
 S LRAN=+$P(ACC," ",3)
 I 'LRAN Q
 S LRAA=+$O(^LRO(68,"B",LRAAB,0))
 I 'LRAA D
 . S DIC=68,DIC(0)="M"
 . S X=LRAAB
 . D ^DIC K DIC
 . S LRAA=+Y
 I LRAA'>0 Q
 S LRAD=BDN_$P(ACC," ",2)_"0000" ; yearly acc areas are assumed
 S TEST=0
 F  S TEST=$O(^LRO(68,LRAA,1,LRAD,1,LRAN,4,TEST)) Q:TEST<1  D
 . S TESTS(TEST)=TEST_U_$$TESTNM^LRPXAPIU(TEST)
 Q
 ;
CONDOK(CONDO,TYPE) ; $$ from LRPXAPIU
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.
 Q
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 N DEL,NUM,OK,OPER,PIECE,PIECES,VALID,VALIDOP,VALUE,VAR K PIECES
 I '(TYPE="C"!(TYPE="M")!(TYPE="A")) Q 0
 S COND=CONDO
 I $E(COND)="|" S COND=$E(COND,2,245)
 I $E(COND)="~" S COND=$E(COND,2,245)
 I $L(COND)'>2 Q 0
 I $E(COND,1,2)'?1U1P Q 0
 I COND[U Q 0
 I CONDO[" " Q 0
 I CONDO["|" S DEL="|"
 E  S DEL="~"
 I '$$SYNTAX($$REPLACE(COND)) Q 0
 S PIECE=COND
 D
 . I TYPE="C" S VALID="FSV" Q
 . I TYPE="A" S VALID="CDEFIMOPST" Q
 . I TYPE="M" S VALID="ACIMORST" Q
 F NUM=1:1 Q:'$L($P(PIECE,DEL,NUM))  S PIECES(NUM)=$P(PIECE,DEL,NUM)
 S OK=1
 S NUM=0
 F  S NUM=$O(PIECES(NUM)) Q:NUM<1  D  Q:'OK
 . S PIECE=PIECES(NUM)
 . I $L(PIECE)<3 S OK=0 Q
 . S VAR=$E(PIECE)
 . I VALID'[VAR S OK=0 Q
 . D
 .. I VAR="V" S VALIDOP="=<>[]" Q
 .. I VAR="F" S VALIDOP="=[]" Q
 .. I VAR="I" S VALIDOP="=[]" Q
 .. I VAR="R" S VALIDOP="=[]" Q
 .. I VAR="S",TYPE="A" S VALIDOP="=[]" Q
 .. S VALIDOP="="
 . I $E(PIECE,3)="'" S OK=0 Q
 . I $E(PIECE,2)="'" S OPER=$E(PIECE,3)
 . E  S OPER=$E(PIECE,2)
 . I VALIDOP'[OPER S OK=0 Q
 . S VALUE=$P(PIECE,OPER,2,999)
 . I $E(VALUE)="""",$E(VALUE,$L(VALUE))'="""" S OK=0 Q
 . I VAR="C" D  Q:'OK
 .. I VALUE'?1""""1U1"""" S OK=0 Q
 .. I $$CATSUB^LRPXAPIU($E(VALUE,2),TYPE)=-1 S OK=0 Q
 . I VALUE,VALUE'=+VALUE S OK=0 Q
 . I $L($P(VALUE,"""",3)) S OK=0 Q
 . I '$$SYNTAX(PIECE) S OK=0 Q
 . I $E(PIECE,2)="=",COND[(VAR_"'=") S OK=0 Q
 I 'OK Q 0
 Q 1
 ;
REPLACE(COND) ; $$(condition) -> condition replacing | or ~ with commas
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.  Return " ".
 Q " "
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 Q $TR(COND,"~|",",,")
 ;
SYNTAX(X) ; $$(condition) -> 1 if correct, else 0
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.  Return 0.
 Q 0
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 ; check syntax when condition applies to an if statement
 S X="I "_X
 D ^DIM
 I '$D(X) Q 0
 Q 1
 ;
NORMALS(LOW,HIGH,TEST,SPEC) ; from LRPXAPIU
 ; ----- BEGIN IHS/OIT/MKK - LR*5.2*1030
 ;       RPMS Lab does not use Clinical Reminders.
 ;       None of the following code will be used.
 Q
 ; ----- END IHS/OIT/MKK - LR*5.2*1030
 N NODE
 S (LOW,HIGH)=""
 S TEST=+$G(TEST)
 I 'TEST Q
 S SPEC=+$G(SPEC)
 I 'SPEC Q
 S NODE=$G(^LAB(60,TEST,1,SPEC,0))
 S LOW=$P(NODE,U,2)
 S HIGH=$P(NODE,U,3)
 Q
 ;
