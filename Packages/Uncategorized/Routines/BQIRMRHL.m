BQIRMRHL ;PRXM/HC/ALA-Register Reminders Help ; 01 Nov 2007  5:45 PM
 ;;2.1;ICARE MANAGEMENT SYSTEM;;Feb 07, 2011
 ;
 Q
 ;
EN(DATA,REG) ;EP -- BQI REG REMIND HELP
 ;Description - Entry point for the panel
 NEW UID,II,X,BQIRM,VAL,DFN,HIEN,E,J,K,L,MAX,MIN,NAFLG,STVWCD,RGIEN
 NEW CODE,NCODE,RMIEN
 S UID=$S($G(ZTSK):"Z"_ZTSK,1:$J)
 S DATA=$NA(^TMP("BQIRMRHL",UID))
 K @DATA
 ;
 S II=0
 NEW $ESTACK,$ETRAP S $ETRAP="D ERR^BQIRMRHL D UNWIND^%ZTER" ; SAC 2006 2.2.3.3.2
 ;
 S REG=$G(REG,"")
 S @DATA@(II)="I00010MEAS_IEN^T00050REM_NAME^T00015CODE^T00030CATEGORY^T00030CLIN_GROUP^T01024REPORT_TEXT"_$C(30)
 ;
 I $G(REG)'="" D  Q:$G(BMXSEC)'=""
 . S RGIEN=$O(^BQI(90507,"B",REG,""))
 . I RGIEN="" S BMXSEC=REG_" not a valid register name" Q
 . D RGRM(RGIEN)
 ;
 I $G(REG)="" D
 . S RGIEN=0
 . F  S RGIEN=$O(^BQI(90507,RGIEN)) Q:'RGIEN  D
 .. I $P(^BQI(90507,RGIEN,0),U,8)=1 Q
 .. D RGRM(RGIEN)
 ;
 S RGIEN=""
 F  S RGIEN=$O(BQISORT(RGIEN)) Q:RGIEN=""  D
 . S REMNM=""
 . F  S REMNM=$O(BQISORT(RGIEN,REMNM)) Q:REMNM=""  D
 .. S IEN=""
 .. F  S IEN=$O(BQISORT(RGIEN,REMNM,IEN)) Q:IEN=""  D
 ... S RMIEN=BQISORT(RGIEN,REMNM,IEN)
 ... D GETDATA(RGIEN,RMIEN,IEN)
 ;
DONE ;
 S II=II+1,@DATA@(II)=$C(31)
 Q
 ;
RGRM(RGIEN) ;
 S NCODE="REG_"_RGIEN
 S RMIEN=0
 F  S RMIEN=$O(^BQI(90507,RGIEN,15,RMIEN)) Q:'RMIEN  D
 . S CODE=NCODE_"_"_RMIEN
 . S IEN=""
 . F  S IEN=$O(^BQI(90506.1,"B",CODE,IEN)) Q:IEN=""  D
 .. S REMNM=$P(^BQI(90506.1,IEN,0),U,3)
 .. S BQISORT(RGIEN,REMNM,IEN)=RMIEN
 Q
 ;
ERR ;
 D ^%ZTER
 NEW Y,ERRDTM
 S Y=$$NOW^XLFDT() X ^DD("DD") S ERRDTM=Y
 S BMXSEC="Recording that an error occurred at "_ERRDTM
 I $D(II),$D(DATA) S II=II+1,@DATA@(II)=$C(31)
 Q
 ;
GETDATA(RGIEN,RMIEN,RIEN) ;EP - Get the reminder help text
 NEW NAME,CODE,HIEN,IEN,CAT,CLIN,STATUS
 S NAME=$P(^BQI(90506.1,RIEN,0),U,3),CODE=$P(^(0),U,1)
 ;S CAT=$$GET1^DIQ(90506.1,RIEN_",",2.03,"E")
 ;S CLIN=$$GET1^DIQ(90506.1,RIEN_",",2.05,"E")
 S CAT=$$GET1^DIQ(90506.1,RIEN_",",3.03,"E")
 S CLIN=$$GET1^DIQ(90506.1,RIEN_",",3.02,"E")
 S TXIEN=0,TEXT=""
 F  S TXIEN=$O(^BQI(90507,RGIEN,15,RMIEN,13,TXIEN)) Q:'TXIEN  D
 . S TEXT=TEXT_^BQI(90507,RGIEN,15,RMIEN,13,TXIEN,0)_$C(10)
 S TEXT=$$TKO^BQIUL1(TEXT,$C(10))
 S II=II+1,@DATA@(II)=RIEN_"^"_$P(^BQI(90506.1,RIEN,0),U,3)_"^"_CODE_"^"_CAT_"^"_CLIN_"^"_TEXT_$C(30)
 Q
