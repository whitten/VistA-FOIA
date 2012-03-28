RAORD1A ;HISC/FPT-Request an Exam ;9/29/97  10:40
 ;;5.0;Radiology/Nuclear Medicine;**1**;Mar 16, 1998
 ;
CS ; Category of exam switch. Called from [RA ORDER EXAM] input template
 ; when requesting an exam. User can change category of exam from
 ; (1) inpatient to outpatient and select a clinic patient location OR
 ; (2) outpatient to inpatient and select a ward patient location.
 ;
 N RAA,RAB,X,Y K DIR
 S RAA=$S($E(RACAT)="I":"INPATIENT",1:"OUTPATIENT")
 S RAB=$S($E(RAA)="I":"OUTPATIENT",1:"INPATIENT")
 W ! S DIR("A",1)="CATEGORY OF EXAM is currently "_RAA
 S DIR("A",2)=" "
 S DIR("A")="Want to change CATEGORY OF EXAM to "_RAB
 S DIR(0)="Y"
 D ^DIR K DIR
 I $D(DIRUT) S RALIFN("OUT")="" Q
 I Y=0 S RALIFN("NO")="" Q
REQLOC ; select patient location
 N DIC,RAHL,RAHLWD,RASCI W !
ASK S DIC("A")="Patient Location: ",DIC="^SC(",DIC(0)="AEMQ"
 I $E(RAB)="O" S DIC("S")="I $$TYPE^RAORD1A(RAB,+Y),$$SCREEN^RAORD1A" I '$D(RAOERRFG) S:$P($G(^SC(+RALIFN,0)),U,3)="C" DIC("B")=$P(^SC(+RALIFN,0),U,1)
 I $E(RAB)="I" S DIC("S")="I $$TYPE^RAORD1A(RAB,+Y),$$SCREEN^RAORD1A" I '$D(RAOERRFG) S:$P($G(^SC(+RALIFN,0)),U,3)="W" DIC("B")=$P(^SC(+RALIFN,0),U,1)
 D ^DIC K DIC
 I +Y'>0 S RALIFN("OUT")="" Q
 I $E(RAB)="I" S RAHLWD=+$G(^SC(+Y,42)) I RAHLWD S RAHL=+$G(^DIC(42,RAHLWD,44)) I RAHL,RAHL'=+Y W !!,*7,"This Hospital Location points to ",$P($G(^DIC(42,+Y,0)),U,1) G ASK
 S RALIFN=+Y,RACAT=RAB
 Q:$D(RAOERFLG)  ;quit if REQLOC was called from REQLOC1 
 K:$E(RAB)="O" RAWARD
 S:$E(RAB)="I" RAWARD=$P(^SC(+Y,0),U,1)
 Q
SCREEN() ; screen for active clinics/wards
 ; This code is also called from RAORD1 (screen for the Patient Location
 ; prompt)
 Q:$D(^SC(+Y,"OOS")) 0 ; don't want Occasion Of Service (OOS) locations
 N RA44 S RA44=$G(^SC(+Y,0))
 Q:"FI"[$P(RA44,"^",3) 0 ; File areas & Imaging Types are not selectable
 I $P(RA44,"^",3)="W" G SCREENW ; ward check
 ; check inactivation & reactivation dates of clinic/operating
 ; room in file #44
 I '$D(^SC(+Y,"I")) Q 1
 ; This Hospital Location has an "I" node.  We have to check INACTIVATE
 ; DATE & REACTIVATE DATE fields to determine if the Hosp. Location is
 ; active.
 N RASCA S RASCI=$G(^SC(+Y,"I")),RASCA=+$P(RASCI,"^",2)
 ; RASCA is the REACTIVATE DATE
 ; Not selectable if REACTIVATE DATE is beyond DT or null (0).
 S RASCA=$S(RASCA=0:0,RASCA>DT:0,1:RASCA)
 I +RASCI=0 Q 1 ; no INACTIVATE DATE
 I +RASCI>DT Q 1 ; INACTIVATE DATE exceeds today's date
 ; Check INACTIVATE DATE against REACTIVATE DATE
 ; if REACTIVATE DATE exists and is not after (or is equal to) the
 ; INACTIVATE DATE the location is not active.
 I RASCA,(+RASCI<RASCA) Q 1
 Q 0
SCREENW ; check currently out-of-service field of ward file (#42)
 N D0,DGPMOS,X
 S D0=+$G(^SC(+Y,42)) I 'D0 Q 0
 I '$D(^DIC(42,D0,0)) Q 0
 S:$D(RAWHEN) DGPMOS=$P(RAWHEN,".",1)
 D WIN^DGPMDDCF
 S X=$S(X=0:1,1:0)
 Q X
 ;
REQLOC1 ; Requesting Location does not go with Category of Exam
 ; Category of Exam = Inpatient  -> Requesting Location = Ward
 ; Category of Exam = Outpatient -> Requesting Location = Clinic
 ; Called from [RA OERR EDIT] and [RA QUICK EXAM ORDER] input templates
 W !!?5,*7,"When the CATEGORY OF EXAM is "_$S(RAX="I":"Inpatient",1:"Outpatient")_" the REQUESTING LOCATION",!?5,"must be a "_$S(RAX="I":"Ward",1:"Clinic")_" or OR.",!
 W !?5,"The current REQUESTING LOCATION is ",$S($P($G(^SC(+RALIFN,0)),U,1)]"":$P($G(^SC(+RALIFN,0)),U,1),1:"Unknown"),!
 N RAB,X,Y
 S RAX=$S(RAX="I":"INPATIENT",1:"OUTPATIENT"),RAB=RAX,RAOERRFG=""
 D REQLOC
 K RAOERRFG
 Q
TYPE(RACAT,Y) ; Indicates whether a Hospital Location is a valid selection.
 ; If the patient is an inpatient, all operating room location types &
 ; all wards are valid selections.  If the patient is an outpatient, all
 ; operating room location types & all clinics are valid selections.
 ; Input Variables: RACAT=$S(Inpatient:"I",1:"O") "O" for outpatient
 ; Input Variables: Y=IEN of entries in the Hospital Location file
 ; This fuction returns 1 if valid, 0 if not valid
 N RAX S RAX=0
 I $E(RACAT,1)="I" D
 . I $P(^SC(+Y,0),U,3)="W"!($P(^SC(+Y,0),U,3)="OR") S RAX=1
 . Q
 E  D
 . I $P(^SC(+Y,0),U,3)="C"!($P(^SC(+Y,0),U,3)="OR") S RAX=1
 . Q
 Q RAX
MATCH(RACAT,RALOC) ; Detect mismatched req loc type and cat. of exam
 ;                 and return code for correct category of exam
 ; Input Variable: 'RACAT' - the value for the 'Category Of Exam' field.
 ;                 Only passed in if either 'I' or 'O'.
 ;                 'RALOC' - The ien of the 'Requesting Location'
 ; Output: correct category (I or O)_"^"_$S(Category='I':ward,1:"")
 ;
 N RA44 S RA44=$G(^SC(+RALOC,0))
 I $E(RACAT,1)'="I",$E(RACAT,1)'="O" Q RACAT
 I $E(RACAT,1)="O",$P(RA44,U,3)'="C",($P(RA44,U,3)'="OR") S RACAT="INPATIENT"
 I $E(RACAT,1)="I",$P(RA44,U,3)'="W",($P(RA44,U,3)'="OR") S RACAT="OUTPATIENT"
 Q RACAT_"^"_$S($E(RACAT,1)="I":$P(RA44,"^"),1:"")
 ;
PREG(RADFN,RADT) ; Subroutine will display the pregnancy prompt to the
 ; user if the patient is between the ages of 12 - 55 inclusive.
 ; Called from CREATE1^RAORD1.
 ; Input : RADFN - Patient, RADT - Today's date
 ; Output: Patient Pregnant? (yes, no, unknown or no default)
 ;   Note: (may set RAOUT if the user times out or '^' out)
 Q:RASEX'="F" "" ; not a female
 S:RADT="" RADT=$$DT^XLFDT()
 N RADAYS,VADM D DEM^VADPT ; $P(VADM(3),"^") DOB of patient, internal
 S RADAYS=$$FMDIFF^XLFDT(RAWHEN,$P(VADM(3),"^"),3)
 Q:((RADAYS\365.25)<12) "" ; too young
 Q:((RADAYS\365.25)>55) "" ; too old
 N DIR,DIROUT,DIRUT,DUOUT,DTOUT S DIR(0)="75.1,13" D ^DIR
 S:$D(DIRUT) RAOUT="^" Q:$D(RAOUT) ""
 Q $P(Y,"^")
 ;
INIMOD(Y) ; check if the user has selected the same
 ; modifier more than once when the order is requested.
 ; The 'Request an Exam' option.  Called from MODS^RAORD1
 ; Input: 'Y' the name of the procedure modifier
 ; Output: 'X' if the user has not entered this modifier in
 ;             the past return one (1).  Else return zero (0).
 Q:'$D(RAMOD) 1 ; must allow the selection of the first modifier
 ; after this, it is assumed that the RAMOD array is defined.
 N RACNT,X S X=1,RACNT=99999
 F  S RACNT=$O(RAMOD(RACNT),-1) Q:RACNT=""!(X=0)  S:RAMOD(RACNT)=Y X=0
 Q X
