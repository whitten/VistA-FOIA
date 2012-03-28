AGGPTHRN ;VNGT/HS/ALA-Patient HRN ; 24 May 2010  9:29 AM
 ;;1.0;PATIENT REGISTRATION GUI;;Nov 15, 2010
 ;
 ;
GTMP(DATA,DFN) ; EP -- AGG GET TEMP PAT HRN
 ;
 NEW UID,II,THRN
 S UID=$S($G(ZTSK):"Z"_ZTSK,1:$J)
 S DATA=$NA(^TMP("AGGPTHRN",UID))
 K @DATA
 S II=0
 NEW $ESTACK,$ETRAP S $ETRAP="D ERR^AGGWDISP D UNWIND^%ZTER" ; SAC 2006 2.2.3.3.2
 S @DATA@(II)="T00010AGGPTHRN"_$C(30)
 S THRN=$$THRN()
 S ^AUPNPAT("D",THRN,DFN,DUZ(2))=""
 S II=II+1,@DATA@(II)=THRN_$C(30)
 S II=II+1,@DATA@(II)=$C(31)
 Q
 ;
THRN() ;EP - Get a temporary HRN number
 NEW AGCH,AGTCH
 S AGCH=999999
 S AGTCH=0,QFL=0
 F I=0:0 S AGCH=$O(^AUPNPAT("D",AGCH)) Q:AGCH=""  D  Q:QFL
 . I $D(^AUPNPAT("D",AGCH,DFN)) S AGTCH=AGCH,QFL=1
 . S AGTCH=AGCH
 ;
 I 'QFL D
 . S:AGTCH=0 AGTCH="T00000"
 . S AGTCH=$E(AGTCH,2,6)
 . S AGTCH=AGTCH+1
 . S AGTCH="T"_$E(100000+AGTCH,2,6)
 Q AGTCH
