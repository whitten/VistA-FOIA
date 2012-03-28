RAUTL8 ;HISC/CAH-Utility routines ;10/3/97  16:02
 ;;5.0;Radiology/Nuclear Medicine;;Mar 16, 1998
 ;
 ;Called by Rad Pt File 70, Exam subfile, Procedure Fld 2 Input transform
PRC G PRC1:'$D(^RADPT(DA(2),"DT","AP",X)) ; check for C.M. reaction
 N RADUP S RADUP=+$$DPDT^RAUTL8(X,.DA)
 I RADUP D ASK Q:'$D(X)
PRC1 ; Check for C.M. reaction
 Q:$$ORCHK^GMRAOR(DA(2),"CM")'=1
 F RAI=4,10:1:12,14:1:22 G MES:$D(^RAMIS(71,"AC",RAI,+X))
 K RAI
 Q
ASK ; Prompt user for yes/no response
 N RAX
ASK1 W !!?3,*7,"Procedure is already entered for this date. Is it ok to continue? No// " R RAX:DTIME
 S:'$T!(RAX="")!(RAX["^")!("Nn"[$E(RAX)) RAX="N"
 K:RAX="N" X Q:'$D(X)
 I "Yy"'[$E(RAX) W !!?3,"Enter 'YES' to register patient for this procedure, or 'NO' to edit the",!?3,"above procedure." G ASK1
 Q
 ;
MES W !!,*7,"Patient has had a reaction to contrast medium!" W:$D(^RA(79,+$P(^RADPT(DA(2),"DT",DA(1),0),"^",3),"CON")) !?3,"...",^("CON"),"..."
MES1 R !?5,"...Type 'OK' to acknowledge or '^' to select another procedure   ==> ",RAX:DTIME
 S RAX=$$UP^XLFSTR(RAX)
 I '$T!(RAX["^")!(RAX="OK") K:RAX'="OK" X K RAX,RAI Q
 G MES1
 ;
STATSEL ;Select one or more order statuses
 ;INPUT VARIABLES:
 ;   RANO() array contains status codes prohibited from selection
 ;OUTPUT VARIABLES:
 ;   RAST is a string of status codes selected (ex: 1^3^8)
 ;   RAORST() is an array of selected status codes and status names
 ;     (ex:   RAORST(1)="DISCONTINUED", RAORST(3)="HOLD", ... )
 K RAST,RAORST W ! S RAORSTS=$P(^DD(75.1,5,0),U,3) F I=1:1 S X=$P(RAORSTS,";",I) Q:X=""  S X1=$P(X,":",1) I '$D(RANO(X1)) S X2=$P(X,":",2),RAORST(X1)=X2
 W !!,"Select statuses to include on report.",! S X1="" F  S X1=$O(RAORST(X1)) Q:X1=""  W !?5,$J(X1,2,0)_"   "_RAORST(X1)
STAT W ! K DIR S DIR(0)="L" D ^DIR Q:'$D(Y(0))
 S RAST="" F I=1:1 S RASTX=$P(Y(0),",",I) Q:RASTX=""  I $D(RAORST(RASTX)) S RAST=RAST_"^"_RASTX
 S RAST=$E(RAST,2,99) I RAST="" W !,"  ?? Sorry, invalid status selection.  Please try again.",! G STAT
 S I="" F  S I=$O(RAORST(I)) Q:I=""  I RAST'[I K RAORST(I)
 K RASTX,I,X,X1,X2 Q
 ;
 ;INPUT TRANSFORM FOR SECONDARY INTERPRETING RESIDENT
S() ; do not enter primary OR SAME SEC in secondary interpreting resident
 I '$D(X)!('$D(DA(3))) G S2
 I '$D(^RADPT(DA(3),"DT",DA(2),"P",DA(1),0)) G S2
 I $D(^RADPT(DA(3),"DT",DA(2),"P",DA(1),"SRR","B",+Y)) Q 0 ;SAME SEC RES
 I $P(^RADPT(DA(3),"DT",DA(2),"P",DA(1),0),"^",12)=+Y Q 0
 Q 1
S2 I '$D(^RADPT(DA(2),"DT",DA(1),"P",DA,0)) Q 0
 I $D(^RADPT(DA(2),"DT",DA(1),"P",DA,"SRR","B",+Y)) Q 0 ;SAME SEC RES
 I $P(^RADPT(DA(2),"DT",DA(1),"P",DA,0),"^",12)=+Y Q 0
 Q 1
 ;INPUT TRANSFORM FOR SECONDARY INTERPRETING STAFF
SSR() ; do not enter primary OR SAME SEC in secondary interpreting staff
 I '$D(X)!('$D(DA(3))) G SSR2
 I '$D(^RADPT(DA(3),"DT",DA(2),"P",DA(1),0)) G SSR2
 I $D(^RADPT(DA(3),"DT",DA(2),"P",DA(1),"SSR","B",+Y)) Q 0 ;SAME SEC STF
 I $P(^RADPT(DA(3),"DT",DA(2),"P",DA(1),0),"^",15)=+Y Q 0
 Q 1
SSR2 I '$D(^RADPT(DA(2),"DT",DA(1),"P",DA,0)) Q 0
 I $D(^RADPT(DA(2),"DT",DA(1),"P",DA,"SSR","B",+Y)) Q 0 ;SAME SEC STF
 I $P(^RADPT(DA(2),"DT",DA(1),"P",DA,0),"^",15)=+Y Q 0
 Q 1
 ;INPUT TRANSFORM FOR PRIMARY INTERPRETING RESIDENT
 ; *** NOT USED - See EN ***
PRRS() ; do not enter secondary into primary interpreting resident screen
 ; called from input transform ^DD(70.03,12,0)
 I $D(^RADPT(DA(2),"DT",DA(1),"P",DA,"SRR","B",+Y)) Q 0
 Q 1
 ;INPUT TRANSFORM FOR PRIMARY INTERPRETING STAFF
 ; *** NOT USED - See EN ***
PSRS() ; do not enter secondary into primary interpreting staff screen
 ; called from input transform ^DD(70.03,15,0)
 I $D(^RADPT(DA(2),"DT",DA(1),"P",DA,"SSR","B",+Y)) Q 0
 Q 1
EN(X,FLD,RA) ;Input transform screen for Primary Staff, Primary Res
 ;Used by fields 70.03,12 & 70.03,15.  If 'Primary' is found in
 ; the 'Secondary' multiple then delete the 'Secondary' entry.
 ; X = 'Primary' IEN,  FLD = 'Secondary' mult. to check,  RA = DA array
 N DA,DEL,HDR,IEN,NODE,SAVEX,SUBDD,XREF
 S NODE=$S(FLD=60:"SSR",FLD=70:"SRR",1:""),SAVEX=X
 S SUBDD=$S(FLD=60:70.11,FLD=70:70.09,1:""),(IEN,DEL)=0
 I (NODE="")!(X'>0)!(FLD'>0)!(SUBDD'>0) Q
 F  S IEN=$O(^RADPT(RA(2),"DT",RA(1),"P",RA,NODE,"B",X,IEN)) Q:IEN'>0  D
 . S XREF=0
 . F  S XREF=$O(^DD(SUBDD,.01,1,XREF)) Q:XREF'>0  D
 .. S (D0,DA(3))=RA(2),(D1,DA(2))=RA(1),(D2,DA(1))=RA,(D3,DA)=IEN,X=SAVEX
 .. I $G(^DD(SUBDD,.01,1,XREF,2))]"" X ^(2)
 .. Q
 . K ^RADPT(RA(2),"DT",RA(1),"P",RA,NODE,IEN,0) S DEL=DEL+1
 . Q
 I DEL D
 . S HDR=$G(^RADPT(RA(2),"DT",RA(1),"P",RA,NODE,0)) Q:HDR=""
 . S HDR(3)=+$O(^RADPT(RA(2),"DT",RA(1),"P",RA,NODE,0))
 . S HDR(4)=$P(HDR,U,4)-DEL
 . S:HDR(3)'>0 HDR(3)="" S:HDR(4)'>0 HDR(4)=""
 . S $P(^RADPT(RA(2),"DT",RA(1),"P",RA,NODE,0),U,3,4)=HDR(3)_U_HDR(4)
 . Q
 S X=SAVEX
 Q
DPDT(RAPRC,RAY) ; Check for registration of duplicate procedures on the same
 ; date/time.  Called from PRC above.
 ; INPUT VARIABLES
 ; 'RAPRC' --> IEN of the procedure (71)
 ; 'RAY'   --> DA array i.e, DA, DA(1), & DA(2)
 ; OUTPUT VARIABLES
 ; 'RAFLG' --> RAFLG=1 procedure registered for this date/time
 ;         --> RAFLG=0 initial registration for procedure@date/time
 N RA72,RABDT,RACIEN,RAEDT,RAFLG,RAI S RAFLG=0
 S RABDT=RAY(1)\1,RAEDT=RABDT_".9999",RAI=RABDT-.0000001
 F  S RAI=$O(^RADPT(RAY(2),"DT","AP",RAPRC,RAI)) Q:RAI'>0!(RAI>RAEDT)  D  Q:RAFLG
 . Q:RAI=RAY(1)  ; At this point our exam status is 'WAITING FOR EXAM'
 . S RACIEN=$O(^RADPT(RAY(2),"DT","AP",RAPRC,RAI,0)) Q:'RACIEN
 . S RA72=+$P($G(^RADPT(RAY(2),"DT",RAI,"P",RACIEN,0)),U,3) ;xam stat
 . S RA72(3)=$P($G(^RA(72,RA72,0)),U,3)
 . I RA72(3)'=0 S RAFLG=1 ; cancelled exams are not taken into account
 . Q
 Q RAFLG
SCRN(RADA,RARS,Y,RALVL) ; check if the primary or secondary int'ng staff
 ; or resident has access to a location or locations which have
 ; an imaging type which match the imaging type of the examination.
 ; This screen will also check the classification of the individual to 
 ; ensure that they are active and valid for the field being edited.
 ;
 ; Called from DD's: ^DD(70.03,12 - ^DD(70.03,15  - ^DD(70.03,60
 ;                   ^DD(70.03,70 - ^DD(70.09,.01 - ^DD(70.11,.01
 ;
 ; Input variables:  RADA-> DA array, maps to RADFN, RADTI & RACNI
 ;                   RARS-> Classification: Resident("R") or Staff("S")
 ;                      Y-> selected resident/staff
 ;                   RALVL-> "PRI"=Primary physician, "SEC"=Secondary
 ;
 ; Output variable: $S(1:I-Types & classification match, resident/staff
 ;                      ok,0:no match re-select resident/staff)
 ;
 I $S('$D(^VA(200,+Y,"RA")):1,'$P(^("RA"),U,3):1,DT'>$P(^("RA"),U,3):1,1:0),($D(^VA(200,"ARC",RARS,+Y)))
 Q:'$T 0 ; failed the classification part of the screen
 Q:$D(^XUSEC("RA ALLOC",+Y)) 1 ; Resident/Staff has access to all loc's!
 N RA7002,RACCESS
 ; adjust RADA() due Fileman's unpredictable retention of DA() levels
 I RALVL="SEC" D
 . I '$D(RADA(3)) S RA7002=$G(^RADPT(RADA(2),"DT",RADA(1),0))
 . I $D(RADA(3)),(RADA(2)'=RADA(3)) S RA7002=$G(^RADPT(RADA(3),"DT",RADA(2),0))
 . I $D(RADA(3)),(RADA(2)=RADA(3)) S RA7002=$G(^RADPT(RADA(2),"DT",RADA(1),0))
 I RALVL="PRI" S RA7002=$G(^RADPT(RADA(2),"DT",RADA(1),0))
 D VARACC^RAUTL6(+Y) ; set-up access array for selected resident/staff
 Q:'$D(RACCESS(+Y,"IMG",+$P(RA7002,"^",2))) 0 ; no i-type match
 Q 1
