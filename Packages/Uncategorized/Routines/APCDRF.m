APCDRF ; IHS/CMI/LAB - LIST MANAGER API'S FOR FAMILY HISTORY AND API FOR REP FACTORS 19 Jun 2008 2:14 PM ; 28 Jan 2009  11:59 AM 
 ;;2.0;IHS PCC SUITE;**2**;MAY 14, 2009
 ;
 ;
 ;
RFADD(P) ;PEP - called to add a patient to the Reproductive Factors file
 ;output:  DFN (ien of entry, file is dinum)
 ;         0^error message if add failed
 I '$G(P) Q 0_"^patient DFN invalid"
 I '$D(^DPT(P)) Q 0_"^patient DFN invalid"
 I $P(^DPT(P,0),U,2)'="F" Q 0_"^patient not FEMALE"
 I $D(^AUPNREP(P,0)) Q P
 NEW X,DIC,DD,D0,DO,Y
 S X=P,DIC="^AUPNREP(",DIC(0)="L"
 K DD,D0,DO,DINUM
 S DINUM=X
 D FILE^DICN
 I Y=-1 Q 0_"^fileman failed adding patient"
 Q 1
 ;
RHEDIT(APCDIE,APCDPT,APCDDATA,RETVAL) ;PEP - called to edit reproductive hx data fields
 ;input - APCDPT is DFN of patient
 ;        APCDDATA - passed in by reference array containing fields and values to be updated and put in the FDA array.
 ;                  should be in format APCDDATA(field #)=value (either internal or external)
 ;        either internal or external values can be passed, values will be checked for validity
 ;        PLEASE NOTE:  if you send a field in the array and the value is blank the field value will be deleted, you can also send an "@" to delete a field value
 ;output - ien^error message 1 if error occured|error message 2 if error occured|error message 3|etc until all errors passed back to caller
 ;         if patient could not be added to the file the return value will be 0^error message
 ;note:  each individual field value passed is checked for validity, those that pass will be filed, those that don't will be passed back
 ;with the VAL^DIE message
 I '$G(APCDPT) S RETVAL="0^patient DFN invalid" Q
 I '$D(^DPT(APCDPT)) S RETVAL="0^patient DFN invalid" Q
 I $P(^DPT(APCDPT,0),"^",2)'="F" S RETVAL="0^patient not FEMALE" Q
 I $G(APCDVAL)="" S APCDVAL="I"
 I $G(APCDIE)="" S APCDIE="I"  ;default to internal values if not passed in
 NEW V,APCDIENS,APCDFDA,E,APCDC
 S APCDC=0
 I '$D(^AUPNREP(APCDPT,0)) S V=$$RFADD(APCDPT) I 'V S RETVAL=V Q
 ;I $G(APCDDATA)="" S RETVAL="0^no fields to edit" Q  ;Q APCDPT  ;no fields to edit??
 I '$D(APCDDATA) S RETVAL="0^no fields to edit" Q  ;Q APCDPT  ;no fields to edit??
 ;M APCDDATA=@APCDDATA
 I '$O(APCDDATA("")) S RETVAL="0^no fields in the data array" Q  ;Q APCDPT  ;no fields in the data array
 S APCDIENS=APCDPT_","
 S APCDIENS(1)=APCDPT
 ;let's check the values being passed in with VAL^DIE, if any are in error set error and don't set into FDA array
 ;guess you never know what people will try to pass
 NEW APCDF,APCDV,APCDE,APCDI
 I $G(APCDIE)="E" D  ;if external check the validity of data
 .S APCDF="" F  S APCDF=$O(APCDDATA(APCDF)) Q:APCDF=""  D
 ..I APCDF=".01" K APCDDATA(APCDF) Q  ;you can't edit the .01, it's dinum'ed
 ..I '$D(^DD(9000017,APCDF,0)) K APCDDATA(APCDF) D E("field number not valid") Q
 ..S APCDV=APCDDATA(APCDF)
 ..Q:APCDV=""
 ..K APCDE,APCDI
 ..S APCDI=""
 ..D VAL^DIE(9000017,APCDIENS,APCDF,"E",APCDV,.APCDI,,"APCDE")
 ..I $D(APCDE("DIERR",1,"TEXT",1)) D E(APCDE("DIERR",1,"TEXT",1)) K APCDDATA(APCDF) Q
 ..S APCDDATA(APCDF)=APCDI
 ;now set FDA array with values left that are valid and call FILE^DIE
 K APCDFDA
 S APCDF="" F  S APCDF=$O(APCDDATA(APCDF)) Q:APCDF=""  D
 .S APCDFDA(9000017,APCDIENS,APCDF)=APCDDATA(APCDF)
 ;CALL FILE^DIE
 K APCDE
 D FILE^DIE("K","APCDFDA","APCDE(0)")
 S APCDI=0 F  S APCDI=$O(APCDE(0,"DIERR",APCDI)) Q:APCDI'=+APCDI  D
 .Q:'$D(APCDE(0,"DIERR",APCDI,"TEXT"))
 .D E(APCDE(0,"DIERR",APCDI,"TEXT"))
 S RETVAL=APCDPT_"^"_RETVAL
 Q
E(V) ;
 S APCDC=APCDC+1,$P(RETVAL,"|",APCDC)=V
 Q
 ;
TEST ;
 S P=478
 K APCDLORI,LORIERR
 S APCDLORI(2)=3090101
 S APCDLORI(1103)="A"
 ;S APCDLORI(3)="O"
 S APCDLORI(3.05)=3090101
 S APCDLORI(4)=3100405
 S APCDLORI(4.05)="D"
 S APCDLORI(1103)=5
 S APCDLORI(1105)=0
 S APCDLORI(1107)=5
 S APCDLORI(1109)=0
 S APCDLORI(1111)=""
 S APCDLORI(1113)=5
 S APCDLORI(1131)="XX"
 S APCDLORI(1133)=""
 D RHEDIT("E",P,.APCDLORI,.LORIERR)
 ;ZW LORIERR
 Q
FP ;EP - called from d/e input template APCD FP (FP)
 S APCDREPI=DA
 D EN^XBNEW("FP1^APCDFH","APCDREPI;APCDDATE")
 K Y
 Q
FP1 ;EP
 ;
RF ;EP - called from XBNEW call
 S APCDREPI=DA
 D EN^XBNEW("RF1^APCDFH","APCDREPI;APCDDATE")
 K Y
 Q
RF1 ;EP - called from xbnew
 S APCDRFS="",APCDPARS=""
 I '$D(^AUPNREP(APCDREPI)) S X=$$RFADD(APCDREPI) I 'X W $P(X,"^",2) Q
RF11 ;
 S DIE="^AUPNREP(",DA=APCDREPI,DR="[APCD RF EDIT]" D ^DIE
 K DIE,DA,DR
 K Y
 Q
 ;
PAUSE ;EP
 S DIR(0)="EO",DIR("A")="Press enter to continue...." D ^DIR K DIR S:$D(DUOUT) DIRUT=1
 Q
