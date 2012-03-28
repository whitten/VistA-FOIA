BAREDP02 ; IHS/SD/LSL - PARSE SEGMENTS INTO ELEMENTS AND CONVERT ; 
 ;;1.8;IHS ACCOUNTS RECEIVABLE;**1,5,20**;OCT 26,2005
 ;
 ; IHS/SD/LSL - 08/19/02 - V1.7 Patch 4 - HIPAA
 ;     Modified EN to use EDI Standard Data Types File if data types
 ;     are not specified in A/R Transport File
 ;
 ; IHS/SD/LSL - 02/10/04 - V1.7 Patch 5  - Remark Codes
 ;       Add HRMKCD line tag to look up Remark Codes in
 ;       A/R REMARK CODE File
 ;       Add LQCD line tag for NCPDP reject/payment codes and
 ;       remark codes.
 ;
 ; IHS/SD/LSL - 2/9/04 - V1.7 Patch 5 - IM12514
 ;      Denial codes for proprietary ERA not working correctly
 ;
 ; IHS/SD/LSL - 06/23/04 V1.8 Patch 1 - IM13518
 ;      "+" standard code before lookup table in case leading zero sent
 ;
 ; IHS/SD/SDR - bar*1.8*20 - ERA REQ1 - Added CHKS tag to populate Check/EFT multiple
 ;      
 ; *******************************************************************
 ;;
EN(TRDA,IMPDA)     ;EP
 S TABID=$$GET1^DIQ(90056.01,TRDA,.03)      ; Table data type code
 ;
 ; gather data types & conversions
 K DAT
 D ENPM^XBDIQ1(90056.0103,"TRDA,0",".01;.02;.03","DAT(")
 I '$D(DAT) D
 . S BARI=0
 . F  S BARI=$O(^BAREDT(BARI)) Q:'+BARI  D
 . . F BARJ=".01",".02",".03" S DAT(BARI,BARJ)=$$GET1^DIQ(90056.05,BARI,BARJ)
 K BARI,BARJ
 K DAT("ID") ;node generated by XBDIQ1
 ; build index of types with conversion code  DAT("ID")="S X=F(X)"
 S DATDA=0
 F  S DATDA=$O(DAT(DATDA)) Q:DATDA'>0  D
 . ; replace | with ^ for routine tag^rou references
 . S DAT(DAT(DATDA,.01))=$TR(DAT(DATDA,.03),"|","^")
 ;
 ;pull import record list into ^TMP($J,"SE")
 K ^TMP($J,"SE")
 W !,"Processing Segment Elements into Values",!
 D ENPM^XBDIQ1(90056.0202,"IMPDA,0",".01","^TMP($J,""SE"",")
 S RECM="^TMP($J,""SE"")"
 ; scan import records
 S COUNT=1
 S RECDA=0
 F  S RECDA=$O(@RECM@(RECDA)) Q:RECDA'>0  D
 . W:'(COUNT#10) "."
 . W:'(COUNT#500) "  ",COUNT,!
 . S COUNT=COUNT+1
 . K REC
 . D ENP^XBDIQ1(90056.0202,"IMPDA,RECDA",".02;.03;.04;1.01","REC(")
 . S PATHS=REC(.04)
 . D LOADTR
 W " ",COUNT,!
 Q
 ; ********************************************************************
 ;
LOADTR ;
 ; load element parsing specifications
 S:'$D(E) E="|"
 K ^TMP($J,"ELM"),ARRAY
 S SEGXDA=$P(PATHS,",",2)
 ; pull records id, path, and value
 D ENPM^XBDIQ1(90056.0102,"TRDA,SEGXDA,0",".01","^TMP($J,""ELM"",")
 S ELMDA=0
 F  S ELMDA=$O(^TMP($J,"ELM",ELMDA)) Q:ELMDA'>0  D
 . K ELM
 . D ENP^XBDIQ1(90056.0102,"TRDA,SEGXDA,ELMDA",".02;.03;.04;.07;.09","ELM(","I")
 . S SEQ=ELM(.03)
 . S X=$P(REC(1.01),E,SEQ+1)
 . D CONVERT
 . ; store converted value in array
 . S ARRAY(SEQ)=X
 ; set array elements into WP field of record
 K ^BAREDI("I",DUZ(2),IMPDA,20,RECDA,10)
 S I=$O(ARRAY(""),-1)
 S ^BAREDI("I",DUZ(2),IMPDA,20,RECDA,10,0)="^^"_I_"^"_I_"^"_DT
 F ELMDA=1:1:I S ^BAREDI("I",DUZ(2),IMPDA,20,RECDA,10,ELMDA,0)=ARRAY(ELMDA)
 Q
 ; ********************************************************************
 ;
CONVERT ;
 ; Convert incoming data according to data type(s)
 I $L(ELM(.07)) D TABLE Q
 I $L(ELM(.09,"I")) D STNDTBL Q
 I $D(DAT(ELM(.04))) S XX=DAT(ELM(.04)),XX=$TR(XX,"|","^") X XX Q
 S X=X
 Q
 ; ********************************************************************
 ;
TABLE ;
 ; perform lookup on table ; ELM(.07) contains path to table   
 ;S X=X_"-TABLE-"_ELM(.07)
 S MCODE=$$GET1^DIQ(90056.0105,ELM(.07),.03)
 S MCODE=$TR(MCODE,"|","^")
 I $L(MCODE) X MCODE Q  ;table is using a routine to resolve X
 S X=$$FIND(90056.0106,ELM(.07),X) ; regular table entry
 Q
 ; *******************************************************************
 ;
STNDTBL ; EP - Pull value from A/R EDI TABLES File
 S MCODE=$$GET1^DIQ(90056.04,ELM(.09,"I"),.03)    ; Processing code
 S MCODE=$TR(MCODE,"|","^")
 I $L(MCODE) X MCODE Q
 ; Find standard tables entry
 S DA(1)=ELM(.09,"I")
 S DIC="^BARETBL("_DA(1)_",1,"
 S DIC(0)="XZ"
 D ^DIC
 I +Y<0 S X=X_" | No Match" Q
 S X=X_" | "_$P(Y(0),U,2)
 Q
 ; *******************************************************************
 ;
DT ;
 ; Convert date to readable format for HIPAA 835
 ; ISA09 comes as YYMMDD all other elements are YYYYMMDD
 S X=$S($L(X)=6:($E(X,3,4)_"/"_$E(X,5,6)_"/"_$E(X,1,2)),1:$E(X,5,6)_"/"_$E(X,7,8)_"/"_$E(X,1,4))
 S %DT="X"
 D ^%DT,DD^%DT
 S X=Y
 Q
 ; ********************************************************************
 ;
CLMCODE ; EP
 ; .03 (processing routine) for this data type of this element
 ; find claim reason code 
 Q:X=""
 K DIC,DA
 S DIC=$$DIC^XBDIQ1(90056.0107)
 S DA(1)=TRDA
 S DIC(0)="X"
 D ^DIC
 I Y'>0 S X=X_" | No Match" Q
 S XDA=+Y
 S X=X_" | "_$$VAL^XBDIQ1(90056.0107,"TRDA,XDA",.02)
 Q
 ; *******************************************************************
 ;
CLMADJCD ; EP
 ; .03 (processing routine) for this data type of this element
 ; Find standard claim level adjustment code
 S X=$TR(X," ")  ;BAR*1.8*5 IM26444
 Q:X=""
 K DIC,DA
 I +X>0 S X=+X               ;IM13518
 S DIC="^BARADJ("
 S DIC(0)="ZX"
 K DD,DO
 D ^DIC
 I +Y<0 S X=X_" | No Match" Q
 S XDA=+Y
 S X=X_" | "_$$GET1^DIQ(90056.06,XDA,.02)
 Q
 ; *******************************************************************
 ;
RMKCODE ;
 ; find remark code
 Q:X=""
 S X=X_" | Remark Code"
 Q
 ; ********************************************************************
 ;
HRMKCD ; EP
 ; This line tag called by .03 field in A/R EDI TABLES File
 ; Processing routine for Remark Code elements to look up in RPMS File
 Q:X=""
 K DIC,DA
 S DIC="^BARMKCD("
 S DIC(0)="ZX"
 K DD,DO
 D ^DIC
 I +Y<0 S X=X_" | No Match" Q
 S XDA=+Y
 S X=X_" | "_$$GET1^DIQ(90056.23,XDA,.02)
 Q
 ; ********************************************************************
 ;
LQCD ; EP
 ; This line tag called by .03 field in A/R EDI TABLES File
 ; Processing routine for Remark or NCPDP Reject Payment Codes.
 ; Whichever is passed in on LQ02
 ;
 ; BARCTYP = HE = Remark Code
 ; BARCTYP = RX = NCPDP Reject/Payment Code
 Q:X=""
 S BARCTYP=$P(REC(1.01),E,2)    ; Type of Code
 I BARCTYP="HE" D HRMKCD Q
 I BARCTYP'="RX" D  Q
 . S X=X_" | No Match"
 K DIC,DA
 S DIC="^ABSPF(9002313.93,"
 S DIC(0)="ZX"
 K DD,DO
 D ^DIC
 I +Y<0 S X=X_" | No Match" Q
 S XDA=+Y
 S X=X_" | "_$$GET1^DIQ(9002313.93,XDA,.02)
 Q
 ; ********************************************************************
 ;
PLVCODE ;
 ; find provider level reason code
 ; Not called for HIPAA - regular table entry.
 Q:X=""
 K DIC,DA
 S DIC=$$DIC^XBDIQ1(90056.0108)
 S DA(1)=TRDA
 S DIC(0)="X"
 D ^DIC
 I Y'>0 S X=X_" | No PLV match" Q
 S XDA=+Y,X=X_" | "_$$VAL^XBDIQ1(90056.0108,"TRDA,XDA",.02)
 Q
 ; ********************************************************************
 ;
FIND(FILE,PATH,X)       ;
 ;find X in file and return X_VALUE
 K DIC,DA
 I X="" Q X
 ;
 S DIC=$$DIC^XBDIQ1(FILE)
 S DIC(0)="XZ"
 F I=1:1 S DAZ(I)=$P(PATH,",",I) Q:DAZ(I)'>0
 F C=1:1:I-1 S DA(C)=DAZ(I-C)
 D ^DIC
 I Y'>0 S X=X_" | NO MATCH" Q X
 S X=X_" | "_$P(Y(0),U,2)
 Q X
 ;start new code bar*1.8*20 REQ1
CHKS(IMPDA) ;
 N BARCTMP
 S I=0,BARCCNT=1
 F  S I=$O(^BAREDI("I",DUZ(2),IMPDA,15,I)) Q:'I  D
 .S IREC=$G(^BAREDI("I",DUZ(2),IMPDA,15,I,0))
 .Q:$P(IREC,"*")'="ST"
 .S $P(BARCTMP(BARCCNT),U,2)=$P(IREC,"*",3)  ;ST control#
 .S I=I+1
 .S $P(BARCTMP(BARCCNT),U,4)=$P(^BAREDI("I",DUZ(2),IMPDA,15,I,0),"*",2)  ;BPR01
 .S $P(BARCTMP(BARCCNT),U,3)=$P(^BAREDI("I",DUZ(2),IMPDA,15,I,0),"*",3)  ;BPR02
 .S $P(BARCTMP(BARCCNT),U,5)=($P(^BAREDI("I",DUZ(2),IMPDA,15,I,0),"*",17)-17000000)  ;BPR16
 .S I=I+1
 .I $P(^BAREDI("I",DUZ(2),IMPDA,15,I,0),"*")="NTE" S I=I+1  ;check for NTE that PNC includes here
 .S $P(BARCTMP(BARCCNT),U)=$P(^BAREDI("I",DUZ(2),IMPDA,15,I,0),"*",3)  ;TRN02
 .S I=I+2
 .S $P(BARCTMP(BARCCNT),U,6)=$P(^BAREDI("I",DUZ(2),IMPDA,15,I,0),"*",3)  ;N102
 .S $P(BARCTMP(BARCCNT),U,11)="UNF"
 .S BARCCNT=+$G(BARCCNT)+1
 S BARCCNT=0
 D PLB
 F  S BARCCNT=$O(BARCTMP(BARCCNT)) Q:'BARCCNT  D
 .K DIC,DIE,DIR,X,Y,DA
 .S DA(1)=IMPDA
 .S DIC="^BAREDI(""I"","_DUZ(2)_","_DA(1)_",5,"
 .S DIC("P")=$P(^DD(90056.02,5,0),U,2)
 .S DIC(0)=""
 .S X=$P(BARCTMP(BARCCNT),U)
 .S DIC("DR")=".02////"_$P(BARCTMP(BARCCNT),U,2)
 .S DIC("DR")=DIC("DR")_";.03////"_$P(BARCTMP(BARCCNT),U,3)
 .S DIC("DR")=DIC("DR")_";.04////"_$P(BARCTMP(BARCCNT),U,4)
 .S DIC("DR")=DIC("DR")_";.05////"_$P(BARCTMP(BARCCNT),U,5)
 .S DIC("DR")=DIC("DR")_";.06////"_$P(BARCTMP(BARCCNT),U,6)
 .S DIC("DR")=DIC("DR")_";.09////"_$P(BARCTMP(BARCCNT),U,9)
 .S DIC("DR")=DIC("DR")_";.11////"_$P(BARCTMP(BARCCNT),U,11)
 .K DD,DO
 .D FILE^DICN
 Q
PLB ; EP
 N BARCDA,CNT,BARVCK,BARSEG,BARSCK
 S BARCDA=0
 S (BARVCK,BARSCK)=""
 F CNT=1:1 S BARCDA=$O(^BAREDI("I",DUZ(2),IMPDA,15,BARCDA)) Q:'BARCDA  D
 .W:'(CNT#1000) "."
 .S BAR15=^BAREDI("I",DUZ(2),IMPDA,15,BARCDA,0)
 .S BARSEG=$P(BAR15,"*")
 .S:BARSEG="TRN" BARVCK=$P(BAR15,"*",3) ;Check Number
 .Q:BARSEG'="PLB"                     ;Only want PLB
 .S BARCCNT=0
 .F  S BARCCNT=$O(BARCTMP(BARCCNT)) Q:'BARCCNT  D
 ..Q:($P($G(BARCTMP(BARCCNT)),U)'=BARVCK)  ;find check number in temp array
 ..S $P(BARCTMP(BARCCNT),U,9)=+$P(BARCTMP(BARCCNT),U,9)+$P(BAR15,"*",5)  ;PLB amount PLB04
 ..S $P(BARCTMP(BARCCNT),U,9)=+$P(BARCTMP(BARCCNT),U,9)+$P(BAR15,"*",7)  ;PLB amount PLB06
 ..S $P(BARCTMP(BARCCNT),U,9)=+$P(BARCTMP(BARCCNT),U,9)+$P(BAR15,"*",9)  ;PLB amount PLB08
 ..S $P(BARCTMP(BARCCNT),U,9)=+$P(BARCTMP(BARCCNT),U,9)+$P(BAR15,"*",11)  ;PLB amount PLB10
 ..S $P(BARCTMP(BARCCNT),U,9)=+$P(BARCTMP(BARCCNT),U,9)+$P(BAR15,"*",13)  ;PLB amount PLB12
 ..S $P(BARCTMP(BARCCNT),U,9)=+$P(BARCTMP(BARCCNT),U,9)+$P(BAR15,"*",15)  ;PLB amount PLB14
 Q
 ;end new code REQ1
