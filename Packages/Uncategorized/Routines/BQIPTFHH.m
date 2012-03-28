BQIPTFHH ;APTIV/HC/ALA-Family and Household Demographics ; 19 Feb 2008  4:51 PM
 ;;2.1;ICARE MANAGEMENT SYSTEM;;Feb 07, 2011
 ;
EN(DATA,BQIDFN) ;EP -- BQI PATIENT HOUSEHOLD
 ;Input
 ;  BQIDFN - Patient internal entry number
 ;
 NEW UID,II,EMER,NOK,PAR,NHH,MBPL,FBPL
 S UID=$S($G(ZTSK):"Z"_ZTSK,1:$J)
 S DATA=$NA(^TMP("BQIPTFHH",UID))
 K @DATA
 ;
 S II=0
 NEW $ESTACK,$ETRAP S $ETRAP="D ERR^BQIPTFHH D UNWIND^%ZTER" ; SAC 2006 2.2.3.3.2
 ;
 S @DATA@(II)="N00002NUM_HOUSE^T00035EMER_NAME^T00030EMER_RELAT^T00020EMER_PHONE^"
 S @DATA@(II)=@DATA@(II)_"T00035NOK_NAME^T00030NOK_RELAT^T00020NOK_PHONE^"
 S @DATA@(II)=@DATA@(II)_"T00035MOTH_MAID_NAME^T00046MOTH_BIRTHPLACE^T00035FATH_NAME^T00046FATH_BIRTHPLACE"_$C(30)
 ;
 S EMER=$G(^DPT(BQIDFN,.33))
 S NOK=$G(^DPT(BQIDFN,.21))
 S PAR=$G(^DPT(BQIDFN,.24))
 S NHH=$P(^AUPNPAT(BQIDFN,0),U,35) S:NHH="" NHH=-1
 S MBPL=$P($G(^AUPNPAT(BQIDFN,26)),U,5)_" "_$$GET1^DIQ(5,$$GET1^DIQ(9000001,BQIDFN_",",2606,"I"),1,"E")
 S FBPL=$P($G(^AUPNPAT(BQIDFN,26)),U,2)_" "_$$GET1^DIQ(5,$$GET1^DIQ(9000001,BQIDFN_",",2603,"I"),1,"E")
 ;
 S II=II+1,@DATA@(II)=NHH_U_$P(EMER,U,1)_U_$P(EMER,U,2)_U_$P(EMER,U,9)_U_$P(NOK,U,1)_U_$P(NOK,U,2)_U
 S @DATA@(II)=@DATA@(II)_$P(NOK,U,9)_U_$P(PAR,U,3)_U_MBPL_U_$P(PAR,U,1)_U_FBPL_$C(30)
 ;
 S II=II+1,@DATA@(II)=$C(31)
 Q
 ;
ERR ;
 D ^%ZTER
 NEW Y,ERRDTM
 S Y=$$NOW^XLFDT() X ^DD("DD") S ERRDTM=Y
 S BMXSEC="Recording that an error occurred at "_ERRDTM
 I $D(II),$D(DATA) S II=II+1,@DATA@(II)=$C(31)
 Q
