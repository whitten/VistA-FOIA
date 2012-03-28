BQIRGHOF ;VNGT/HS/ALA - Set HMS Turn off Date ; 16 Jul 2008  5:41 PM
 ;;2.1;ICARE MANAGEMENT SYSTEM;;Feb 07, 2011
 ;
GET(DATA,FAKE) ; EP - BQI GET HMS TURN OFF DATE
 ;
 NEW UID,II,DATE,TDATE,BKMHIV,FLAG
 S UID=$S($G(ZTSK):"Z"_ZTSK,1:$J)
 S DATA=$NA(^TMP("BQIRGHOF",UID))
 K @DATA
 ;
 S II=0
 NEW $ESTACK,$ETRAP S $ETRAP="D ERR^BQIRGHOF D UNWIND^%ZTER" ; SAC 2006 2.2.3.3.2
 ;
 S BKMHIV=$$HIVIEN^BKMIXX3()
 S TDATE=$$GET1^DIQ(90450,BKMHIV_",",.08,"I")\1
 S FLAG=$$GET1^DIQ(90450,BKMHIV_",",30,"I")
 I FLAG S BMXSEC="HMS has already been turned off." Q
 ;I TDATE'="",DT>TDATE S BMXSEC="HMS has already been turned off." Q
 S DATE=$$FMTE^BQIUL1(TDATE)
 S @DATA@(II)="D00030TURN_OFF_DATE"_$C(30)
 S II=II+1,@DATA@(II)=DATE_$C(30)
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
 ;
UPD(DATA,DATE) ; EP - BQI SET HMS TURN OFF DATE
 NEW UID,II,INIEN,INDT,ERROR,RESULT,BKMHIV,EXDT,ERROR,BKMUPD
 S UID=$S($G(ZTSK):"Z"_ZTSK,1:$J)
 S DATA=$NA(^TMP("BQIRGHOF",UID))
 K @DATA
 ;
 S II=0
 NEW $ESTACK,$ETRAP S $ETRAP="D ERR^BQIRGHOF D UNWIND^%ZTER" ; SAC 2006 2.2.3.3.2
 ;
 S @DATA@(II)="I00010RESULT^T01024MSG"_$C(30)
 S DATE=$$DATE^BQIUL1(DATE)
 S BKMHIV=$$HIVIEN^BKMIXX3()
 S EXDT=$$GET1^DIQ(90450,BKMHIV_",",.08,"I")\1
 ;
 S INIEN=$O(^XPD(9.7,"B","HIV MANAGEMENT SYSTEM 2.0",""),-1)
 S INDT=$P(^XPD(9.7,INIEN,0),U,3)\1
 S INDT=$$FMADD^XLFDT(INDT,95) ; approximately 3 months from date of install
 I DATE>INDT S RESULT="-1^Date cannot be greater than "_$$FMTE^BQIUL1(INDT) G DONE
 I DATE'>DT S RESULT="-1^Date must be a future date" G DONE
 S BKMUPD(90450,BKMHIV_",",.08)=DATE
 D FILE^DIE("","BKMUPD","ERROR")
 I '$D(ERROR) S RESULT="1^"
 I $D(ERROR) S RESULT=-1_U_$G(ERROR("DIERR",1,"TEXT",1))
 ;
DONE ;
 S II=II+1,@DATA@(II)=RESULT_$C(30)
 S II=II+1,@DATA@(II)=$C(31)
 Q
