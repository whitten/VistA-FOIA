AUMDO1B ; IHS/OIRM/DSD/JCM,AEF - UPDATE ICD GLOBAL ;  [ 12/03/1998   2:35 PM ]
 ;;99.1;ICD UPDATE;;DEC 03, 1998
 W !!,"ENTRY NOT PERMITTED HERE (^AUMDO1B)",! Q
SPECNOTE ; SPECIAL NOTE FOR PROGRAMMERS
 ; ***NOTE - ALL VARIABLES ARE IN THE AUMDO("variable name') ARRAY
 ;
 ; PARTS OF THE ORIGINAL AUMDO1B ROUTINE HAVE BEEN EXTRACTED
 ; AND PLACED IN AUMDO1BS TO REDUCE THE SIZE OF THIS PUPPY
 ; THE SUBROUTINES ARE 'COMMON' AND 'DSPSTAT' 12/2/92
 ;
EN ; ENTRY POINT FROM EN+1^AUMDO1A
 ; ADD NEW ENTRY OR CHANGE THE OLD ENTRY
 ; NOTE - ICD DESCRIPTION FIELDS CONTAIN ';' MUST DO A HARD SET
 S DR="" ; CLEAR DR STRING
 D @$S(AUMDO("ICD0"):"OPONLY",AUMDO("ICD9"):"DXONLY")
 D COMMON^AUMDO1BS ; CALL SUB-ROUTINE IN AUMDO1BS TO SET REMAINDER OF DR STRING
 D CALLFM ; CALL FILEMANAGER DIE, AUMDO1C AND DIK TO UPDATE THIS ICD CODE
 D DSPSTAT^AUMDO1BS ; CALL SUB-ROUTINE IN AUMDO1BS TO DISPLAY STATUS OF UPDATED RECORD
 Q  ;RETURN TO ^AUMDO1A
OPONLY ; ICD0 ONLY FIELDS
 S:$P(@(AUMDO("UPD GL REF")_AUMDO("UPD DFN")_",0)"),U,4)]"" $P(^ICD0(AUMDO("ICD DFN"),0),U,4)=$P(^(0),U,4)
 Q
DXONLY ; ICD9 ONLY FIELDS
 S:$P(@(AUMDO("UPD GL REF")_AUMDO("UPD DFN")_",0)"),U,3)]"" $P(^ICD9(AUMDO("ICD DFN"),0),U,3)=$P(^(0),U,3)
 S:$P(@(AUMDO("UPD GL REF")_AUMDO("UPD DFN")_",0)"),U,4)]"" DR=DR_"101////"_$P(^(0),U,4)_";"
 S:$P(@(AUMDO("UPD GL REF")_AUMDO("UPD DFN")_",0)"),U,7)]"" DR=DR_"70////"_$P(^(0),U,7)_";"
 I $P(@(AUMDO("UPD GL REF")_AUMDO("UPD DFN")_",0)"),U,5)]"" S DR=DR_"5////"_$P(^(0),U,5)_";" D
DXMDC . ; FOR ICD9 MDC FIELD, PRINT UPDATE MESSAGE
 . D:$Y>55 HDR^AUMDO
 . I AUMDO("ADD/REPLACE"),$P(^ICD9(AUMDO("ICD DFN"),0),U,5)]"",$P(@(AUMDO("UPD GL REF")_AUMDO("UPD DFN")_",0)"),U,5)="" W !,?27,"Removing old MDC entry",!,?27,"Adding new MDC entry",!,?30,"Update value is NULL" Q
 . I AUMDO("CHANGE")!(AUMDO("ADD/REPLACE")) W !,?27,"Removing old MDC entry"
 . W !,?27,"Adding new MDC entry",!,?30,"MDC "_$P(@(AUMDO("UPD GL REF")_AUMDO("UPD DFN")_",0)"),U,5)_" added"
 S:$P(@(AUMDO("UPD GL REF")_AUMDO("UPD DFN")_",0)"),U,6)]"" DR=DR_"5.5////"_$P(^(0),U,6)_";"
 S:$P(@(AUMDO("UPD GL REF")_AUMDO("UPD DFN")_",0)"),U,12)]"" DR=DR_"5.7////"_$P(^(0),U,6)_";"
 S:$P(@(AUMDO("UPD GL REF")_AUMDO("UPD DFN")_",0)"),U,13)]"" DR=DR_"5.9////"_$P(^(0),U,6)_";"
 I $D(^AUMDDTMP(AUMDO("UPD DFN"),"DRG")) D
 . F AUMDO("DRG FLD")=60:1:65 D
 .. S DR=DR_AUMDO("DRG FLD")
 .. I $P(^AUMDDTMP(AUMDO("UPD DFN"),"DRG"),U,AUMDO("DRG FLD")-59)="" D
 ... S DR=DR_"///@;"
 .. E  S DR=DR_"////"_$P(^AUMDDTMP(AUMDO("UPD DFN"),"DRG"),U,AUMDO("DRG FLD")-59)_";"
 . D:$Y>55 HDR^AUMDO
 . I AUMDO("ADD/REPLACE"),$G(^ICD9(AUMDO("ICD DFN"),"DRG"))]"",^AUMDDTMP(AUMDO("UPD DFN"),"DRG")="" D  Q
 .. W !,?27,"Removing old DRG entries",!,?27,"Adding new DRG entry",!,?30,"Update value is NULL"
 . I AUMDO("CHANGE")!(AUMDO("ADD/REPLACE")) W !,?27,"Removing old DRG entries"
 . W !,?27,"Adding new DRG entries"
 . F AUMDO("DRG FLD")=60:1:65 D
 .. Q:$P(^AUMDDTMP(AUMDO("UPD DFN"),"DRG"),U,AUMDO("DRG FLD")-59)=""
 .. W !,?30,"DRG "_$P(^AUMDDTMP(AUMDO("UPD DFN"),"DRG"),U,AUMDO("DRG FLD")-59)_" added"
 Q
CALLFM ; CALL FILEMANAGER TO UPDATE ENTRY
CALLDIE ; CALL DIE
 S DIE=AUMDO("ICD GL REF"),DA=AUMDO("ICD DFN") D ^DIE I $D(Y) S AUMDO("QUIT")=1 D:$Y>55 HDR^AUMDO W !,?27,"Update operation failed. (Call to ^DIE)" Q
 X $S(AUMDO("CHANGE"):"S AUMDO(""TOTAL CHANGES"")=AUMDO(""TOTAL CHANGES"")+1",AUMDO("ADD"):"S AUMDO(""TOTAL ADDS"")=AUMDO(""TOTAL ADDS"")+1",AUMDO("ADD/REPLACE"):"S AUMDO(""TOTAL ADD/REPLACE"")=AUMDO(""TOTAL ADD/REPLACE"")+1")
CALLSUB ; CALL SUBROUTINES TO PROCESS SUB-FILE MULTIPLE
 D:$D(@(AUMDO("UPD GL REF")_AUMDO("UPD DFN")_",""DR"")"))!($D(^("MDC")))!($D(^(9999999.21))) EN^AUMDO1C ; Call subroutine to process DRG or MDC or Keyword multiple
CALLDIK ; SET CROSS-REFERENCES FOR DESCRIPTION FIELDS
 S DIK=AUMDO("ICD GL REF"),DA=AUMDO("ICD DFN") D IX^DIK
 Q
