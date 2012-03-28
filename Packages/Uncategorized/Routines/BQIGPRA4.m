BQIGPRA4 ;PRXM/HC/ALA - Calculate GPRA for single patient ; 26 Jul 2006  10:05 AM
 ;;2.1;ICARE MANAGEMENT SYSTEM;;Feb 07, 2011
 ;
 Q
 ;
SNG(DATA,DFN) ;EP -- BQI GPRA POPULATE BY PATIENT
 ;Description
 ;  Get GPRA for a single patient
 ;Input
 ;  DFN - Patient internal entry number
 NEW UID,II,BQIGREF,BQIDATA,BQIROU,BGPIND,BGPBD,BGPED,BGPBBD,BGPBED,X
 NEW BGPPBD,BGPPED,BGPPER,BGPQTR,BGPRTYPE,BGPRPT,BGP3YE,BGPP3YE,BGPB3YE
 S UID=$S($G(ZTSK):"Z"_ZTSK,1:$J)
 S DATA=$NA(^TMP("BQIGPSNG",UID))
 K @DATA
 ;
 S II=0
 NEW $ESTACK,$ETRAP S $ETRAP="D ERR^BQIGPRA4 D UNWIND^%ZTER" ; SAC 2006 2.2.3.3.2
 ;
 S BQIGREF=$NA(^TMP("BQIGPRA",UID))
 S BQIDATA=$NA(^BQIPAT)
 K @BQIGREF,@BQIDATA@(DFN,30)
 ;
 S @DATA@(II)="I00010RESULT"_$C(30)
 ;
 ; If patient is deceased, don't calculate
 I $P($G(^DPT(DFN,.35)),U,1)'="" G DONE
 ; If patient has no active HRNs, quit
 I '$$HRN^BQIUL1(DFN) G DONE
 ; If patient has no visit in last 3 years, quit
 I '$$VTHR^BQIUL1(DFN) G DONE
 ;
 D INP^BQINIGHT
 I $G(BQIROU)="" Q
 ;
 I $T(@("BQI^"_BQIROU))="" Q
 ;
 NEW VER,BQX,XN
 S VER=$$VERSION^XPDUTL("BGP")
 I VER<8.0 D
 . S X=0 F  S X=$O(@BQIINDG@("GPRA",1,X)) Q:X'=+X  S BGPIND(X)=""
 ;
 I VER>7.0 D
 . S BQX=""
 . F  S BQX=$O(^BQI(90506.1,"AC","G",BQX)) Q:BQX=""  D
 .. I $P(^BQI(90506.1,BQX,0),U,10)=1 Q
 .. S X=$P(^BQI(90506.1,BQX,0),U,1),XN=$P(X,"_",2)
 .. S X=$P(@BQIMEASG@(XN,0),U,1),BGPIND(X)=""
 ;
 ; Define the time frame for the patient
 S BGPBD=$$DATE^BQIUL1("T-12M"),BGPED=DT
 S BGPBBD="300"_$E(BGPBD,4,7),BGPBED="300"_$E(BGPED,4,7)
 S BGPPBD=$$DATE^BQIUL1("T-24M"),BGPPED=$$DATE^BQIUL1("T-12M")
 S BGPPER=$E($$DT^XLFDT(),1,3)_"0000"
 S BGPQTR=$S(BGPBD>($E(BGPBD,1,3)_"0101")&(BGPBD<($E(BGPBD,1,3)_"0331")):1,BGPBD>($E(BGPBD,1,3)_"0401")&(BGPBD<($E(BGPBD,1,3)_"0630")):2,BGPBD>($E(BGPBD,1,3)_"0701")&(BGPBD<($E(BGPBD,1,3)_"0930")):3,1:4)
 S BGPRTYPE=4,BGPRPT=4
 S BGP3YE=$$FMADD^XLFDT(BGPED,-1096)
 S BGPP3YE=$$FMADD^XLFDT(BGPPED,-1096)
 S BGPB3YE=$$FMADD^XLFDT(BGPBED,-1096)
 ;
 S BQIPUP(90507.5,DFN_",",.02)=BQIYR
 S BQIPUP(90507.5,DFN_",",.03)=BGPBD
 S BQIPUP(90507.5,DFN_",",.04)=BGPED
 S BQIPUP(90507.5,DFN_",",.05)=$$NOW^XLFDT()
 D FILE^DIE("","BQIPUP","ERROR")
 K BQIPUP
 D @("BQI^"_BQIROU_"(DFN,.BQIGREF)")
 ;
 ;  if the patient doesn't already exist in the iCare Patient file, add them
 I $G(^BQIPAT(DFN,0))="" D
 . NEW DIC,X,DINUM,DLAYGO
 . S (X,DINUM)=DFN,DLAYGO=90507.5,DIC="^BQIPAT(",DIC(0)="L"
 . K DO,DD D FILE^DICN
 ;
 S @BQIDATA@(DFN,30,0)="^90507.53^^"
 ;
 ; if the patient doesn't meet any GPRA logic, quit
 I '$D(@BQIGREF@(DFN)) Q
 ;
 NEW SIND,IND,MEAS,MCT,CT,GPMEAS
 S SIND="",CT=0
 F  S SIND=$O(^BQI(90506.1,"AC","G",SIND)) Q:SIND=""  D
 . S CT=CT+1
 . I $P(^BQI(90506.1,SIND,0),U,10)=1 Q
 . S @BQIDATA@(DFN,30,CT,0)=$P(^BQI(90506.1,SIND,0),U,1)
 . S @BQIDATA@(DFN,30,"B",$P(^BQI(90506.1,SIND,0),U,1),CT)=""
 ;
 S IND=0
 F  S IND=$O(@BQIGREF@(DFN,IND)) Q:IND=""  D
 . S MEAS=0
 . F  S MEAS=$O(@BQIGREF@(DFN,IND,MEAS)) Q:MEAS=""  D
 .. ;Q:'$$SUM^BQIGPUTL(BQIYR,MEAS)
 .. S GPMEAS=BQIYR_"_"_MEAS
 .. S MCT=$O(^BQIPAT(DFN,30,"B",GPMEAS,"")) I MCT="" Q
 .. S $P(@BQIDATA@(DFN,30,MCT,0),U,2)=$P(@BQIGREF@(DFN,IND),U,2)
 .. S $P(@BQIDATA@(DFN,30,MCT,0),U,3)=$P(@BQIGREF@(DFN,IND,MEAS),U,2)
 .. S $P(@BQIDATA@(DFN,30,MCT,0),U,4)=$P(@BQIGREF@(DFN,IND,MEAS),U,3)
 ;
 ;  Create cross-references
 K @BQIGREF
 ;NEW DA,DIK
 ;S DA=DFN,DIK="^BQIPAT(" D IX1^DIK
 ;
DONE ;
 K BGPNUM,BGPDEN
 S II=II+1,@DATA@(II)="1"_$C(30)
 S II=II+1,@DATA@(II)=$C(31)
 Q
 ;
ERR ;
 D ^%ZTER
 NEW Y,ERRDTM
 S Y=$$NOW^XLFDT() X ^DD("DD") S ERRDTM=Y
 S BMXSEC="Recording that an error occurred at "_ERRDTM
 I $D(II),$D(DATA) S II=II+1,@DATA@(II)="-1"_$C(30)
 I $D(II),$D(DATA) S II=II+1,@DATA@(II)=$C(31)
 Q
