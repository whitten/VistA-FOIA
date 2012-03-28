LRPXAPP ;VA/SLC/STAFF - Test Lab APIs ;11/12/03  15:44
 ;;5.2;LAB SERVICE;**1030**;NOV 01, 1997
 ;;5.2;LAB SERVICE;**295**;Sep 27, 1994;Build 5
 ;
 ; - This routine shows examples of calling APIs in LRPXAPI.
 ; - This routine is provided for documentation and testing.
 ; - The temp global ^TMP("LRPXAPI",$J, is used as an example.
 ; - You should use a TMP global with your package's namespace.
 ;
TESTS ; sample application to test TESTS API
 ; gets the lab tests (without results) on a patient (in date range)
 N COND,DFN,ERR,FROM,ITEMS,MORE,TO,TYPE K ITEMS
 K ^TMP("LRPXAPP",$J)
 D GETTYPE^LRPXAPPU(.TYPE,.ERR) I ERR Q
 D GETPT^LRPXAPPU(.DFN,.ERR) I ERR Q
 D GETDATE^LRPXAPPU(.FROM,.TO,.ERR) I ERR Q
 D GETCOND^LRPXAPPU(.COND,TYPE,.ERR) I ERR Q
 F  D  Q:'MORE
 . D TESTS^LRPXAPI(.ITEMS,DFN,TYPE,,.MORE,COND,FROM,TO)
 . M ^TMP("LRPXAPP",$J)=ITEMS
 D DISPLAY^LRPXAPPU
 K ^TMP("LRPXAPP",$J)
 Q
 ;
ARESULTS ; sample application to test RESULTS API for all results
 ; gets all lab results on a patient (in date range) 
 N COND,DFN,ERR,FROM,TO,TYPE
 K ^TMP("LRPXAPP",$J)
 D GETTYPE^LRPXAPPU(.TYPE,.ERR) I ERR Q
 D GETPT^LRPXAPPU(.DFN,.ERR) I ERR Q
 D GETDATE^LRPXAPPU(.FROM,.TO,.ERR) I ERR Q
 D GETCOND^LRPXAPPU(.COND,TYPE,.ERR) I ERR Q
 D RESULTS^LRPXAPI("LRPXAPP",DFN,TYPE,1000000,,COND,FROM,TO)
 D DISPLAY^LRPXAPPU
 K ^TMP("LRPXAPP",$J)
 Q
 ;
RESULTS ; sample application to test RESULTS API
 ; gets patient's lab test results (in date range)
 K ^TMP("LRPXAPP",$J)
 N COND,DFN,ERR,FROM,ITEM,MORE,RESULTS,TO,TYPE K RESULTS
 D GETTYPE^LRPXAPPU(.TYPE,.ERR) I ERR Q
 D GETPT^LRPXAPPU(.DFN,.ERR) I ERR Q
 D GETDATE^LRPXAPPU(.FROM,.TO,.ERR) I ERR Q
 I TYPE="C" D GETTEST^LRPXAPPU(.ITEM,TYPE,.ERR) I ERR Q
 I TYPE="A" D GETAP^LRPXAPPU(.ITEM,.ERR) I ERR Q
 I TYPE="M" D GETMICRO^LRPXAPPU(.ITEM,.ERR) I ERR Q
 D GETCOND^LRPXAPPU(.COND,TYPE,.ERR) I ERR Q
 F  D  Q:'MORE
 . D RESULTS^LRPXAPI(.RESULTS,DFN,ITEM,,.MORE,COND,FROM,TO)
 . M ^TMP("LRPXAPP",$J)=RESULTS
 D DISPLAY^LRPXAPPU
 K ^TMP("LRPXAPP",$J)
 Q
 ;
PATIENTS ; sample application to test PATIENTS API
 ; gets all patients that have had a specific lab test (in date range)
 N ERR,COND,FROM,ITEM,MORE,PATIENTS,SUB,TO,TYPE K PATIENTS
 D GETTYPE^LRPXAPPU(.TYPE,.ERR) I ERR Q
 I TYPE="C" D GETTEST^LRPXAPPU(.ITEM,TYPE,.ERR) I ERR Q
 I TYPE="A" D GETAP^LRPXAPPU(.ITEM,.ERR) I ERR Q
 I TYPE="M" D GETMICRO^LRPXAPPU(.ITEM,.ERR) I ERR Q
 D GETCOND^LRPXAPPU(.COND,TYPE,.ERR) I ERR Q
 D GETDATE^LRPXAPPU(.FROM,.TO,.ERR) I ERR Q
 F  D  Q:'MORE
 . D PATIENTS^LRPXAPI(.PATIENTS,ITEM,,10,.MORE,COND,FROM,TO)
 . S SUB=""
 . F  S SUB=$O(PATIENTS(SUB)) Q:SUB=""  W !,PATIENTS(SUB)
 Q
 ;
DATES ; sample application to test DATES API
 ; gets the dates of labs (without results) on a patient (in date range)
 N DFN,ERR,FROM,ITEMS,MORE,TO,TYPE K ITEMS
 K ^TMP("LRPXAPP",$J)
 D GETPT^LRPXAPPU(.DFN,.ERR) I ERR Q
 D GETTYPE^LRPXAPPU(.TYPE,.ERR) I ERR Q
 D GETDATE^LRPXAPPU(.FROM,.TO,.ERR) I ERR Q
 F  D  Q:'MORE
 . D DATES^LRPXAPI(.ITEMS,DFN,TYPE,,.MORE,FROM,TO)
 . M ^TMP("LRPXAPP",$J)=ITEMS
 D DISPLAY^LRPXAPPU
 K ^TMP("LRPXAPP",$J)
 Q
 ;
TESTLOOK ; test a lookup that screens for only tests done on patient
 N DIC,DFN,ERR,X,Y K DIC
 D GETPT^LRPXAPPU(.DFN,.ERR) I ERR Q
 S DIC=60,DIC(0)="AEMOQ"
 S DIC("S")="I $P(^(0),U,4)=""CH"",$$HASITEM^LRPXAPI(DFN,+Y)"
 D ^DIC I Y<1 Q
 W !,Y
 Q
 ;
TESTVAL ; test conditions and values for a patient's test
 N COL,COND,DFN,ERR,LRDFN,LRDN,LRIDT,RESULT,TEST
 D GETPT^LRPXAPPU(.DFN,.ERR) I ERR Q
 S LRDFN=$$LRDFN^LRPXAPIU(DFN)
 D GETTEST^LRPXAPPU(.TEST,"C",.ERR) I ERR Q
 S LRDN=$$LRDN^LRPXAPIU(TEST)
 D GETCOND^LRPXAPPU(.COND,TYPE,.ERR) I ERR Q
 S COL=0
 F  S COL=$O(^PXRMINDX(63,"IP",TEST,DFN,COL)) Q:COL<1  D
 . S LRIDT=$$LRIDT^LRPXAPIU(COL)
 . W !!,$$VAL^LRPXAPI(LRDFN,LRIDT,LRDN)
 . D VALUE^LRPXAPI(.RESULT,DFN,COL,TEST,COND,.ERR)
 . W !,RESULT
 . D LRVALUE^LRPXAPI(.RESULT,LRDFN,LRIDT,LRDN,COND,.ERR)
 . W !,RESULT
 Q
 ;
VALUES ; test to get patient's values from PXRMINDX index
 N COL,DFN,ERR,ITEM,NODE,RESULT,STOP,TYPE
 D GETTYPE^LRPXAPPU(.TYPE,.ERR) I ERR Q
 D GETPT^LRPXAPPU(.DFN,.ERR) I ERR Q
 I TYPE="C" S ITEM=0,STOP="@"
 I TYPE="A" S ITEM="A",STOP="AZ"
 I TYPE="M" S ITEM="M",STOP="MZ"
 F  S ITEM=$O(^PXRMINDX(63,"PI",DFN,ITEM)) Q:ITEM=""  Q:ITEM]STOP  D
 . S COL=0
 . F  S COL=$O(^PXRMINDX(63,"PI",DFN,ITEM,COL)) Q:COL<1  D
 .. S NODE=""
 .. F  S NODE=$O(^PXRMINDX(63,"PI",DFN,ITEM,COL,NODE)) Q:NODE=""  D
 ... D LRPXRM^LRPXAPI(.RESULT,NODE,ITEM)
 ... W !,ITEM,!,NODE,!,RESULT
 Q
 ;
SPEC ; test of specimen APIs
 ; displays specimen node, comments, results
 ; restricted to MAX number of collections
 N COL,COLCNT,CNT,DATA,DFN,ERR,ITEM,MAX,RESULTS K COLCNT,RESULTS
 S MAX=10,CNT=0
 D GETPT^LRPXAPPU(.DFN,.ERR) I ERR Q
 S ITEM=0
 F  S ITEM=$O(^PXRMINDX(63,"PI",DFN,ITEM)) Q:ITEM<1  D  Q:CNT>MAX
 . S COL=0
 . F  S COL=$O(^PXRMINDX(63,"PI",DFN,ITEM,COL)) Q:COL<1  D  Q:CNT>MAX
 .. I $D(COLCNT(COL)) Q
 .. S COLCNT(COL)="",CNT=CNT+1
 .. ; use "A", "C", "S", "V" to test
 .. D SPEC^LRPXAPI(.RESULTS,DFN,COL,"A",.ERR)
 .. W !
 .. W !,$$COMMENT^LRPXAPI($$LRDFN^LRPXAPIU(DFN),$$LRIDT^LRPXAPIU(COL))
 .. S DATA="RESULTS(0)"
 .. F  S DATA=$Q(@DATA) Q:DATA=""  D
 ... W !,DATA_"="_@DATA
 .. K RESULTS
 Q
 ;
CHNODE ; test CH data - some not defined
 N CNT,ERR,DEF,DFN,LRDFN,LRDN,LRIDT,NODE,RESULTS K RESULTS
 D GETPT^LRPXAPPU(.DFN,.ERR) I ERR Q
 S LRDFN=$$LRDFN^LRPXAPIU(DFN)
 S CNT=0
 S LRIDT=0
 F  S LRIDT=$O(^LR(LRDFN,"CH",LRIDT)) Q:LRIDT<1  D  Q:CNT>3
 . S CNT=CNT+1
 . W !!!,LRDFN,"     ",LRIDT
 . S LRDN=1
 . F  S LRDN=$O(^LR(LRDFN,"CH",LRIDT,LRDN)) Q:LRDN<1  D
 .. S NODE=$G(^LR(LRDFN,"CH",LRIDT,LRDN))
 .. W !!,LRDN,!,NODE,!
 .. D CHNODE^LRPXAPI(.RESULTS,NODE)
 .. S DEF=""
 .. F  S DEF=$O(RESULTS(DEF)) Q:DEF=""  D
 ... W !,DEF," = ",RESULTS(DEF)
 Q
 ;
CONDOK ; sample application to test if condition is valid
 N COND,ERR,TYPE
 D GETTYPE^LRPXAPPU(.TYPE,.ERR) I ERR Q
 F  D GETCOND^LRPXAPPU(.COND,TYPE,.ERR) Q:ERR  Q:'$L(COND)  D
 . I $$CONDOK^LRPXAPIU(COND) W !,COND,!,"condition is ok" Q
 . W !,COND,!,"condition is NOT ok" Q
 Q
 ;
