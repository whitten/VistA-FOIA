RAUTL5 ;HISC/CAH,FPT,GJC-Utility Routine ;3/12/98  13:27
 ;;5.0;Radiology/Nuclear Medicine;**8,26**;Mar 16, 1998
CH ; Populate the 'CLINICAL HISTORY' field (400) in file 74 (^RADPT)
 ; Called from 'CREATE1^RAORD1'.
 N WPFLG
CH1 I $D(RAVSTFLG),$D(RAVLEDTI),$D(RAVLECNI),$D(^RADPT(RADFN,"DT",RAVLEDTI,"P",RAVLECNI,"H")) S:$D(^("H",0)) ^TMP($J,"RAWP",0)=^(0) F RAI=1:1 Q:'$D(^RADPT(RADFN,"DT",RAVLEDTI,"P",RAVLECNI,"H",RAI,0))  S ^TMP($J,"RAWP",RAI,0)=^(0)
 I $L($G(^RA(79,+RADIV,"HIS"))) W !!?3,*7,^("HIS"),! K DIR S DIR(0)="E" D ^DIR I $D(DTOUT)!($D(DUOUT)) S RAOUT=1 Q
 S DIC="^TMP("_$J_",""RAWP"",",DWPK=1,DIWESUB="Clin Hist/Reason" W !,"CLINICAL HISTORY FOR EXAM"
 D EN^DIWE K DIWESUB I '$O(^TMP($J,"RAWP",0)) W !!,*7,"A clinical history corresponding to this request is required.",! D  Q:$D(RAOUT)  G CH1
 .S DIR(0)="Y",DIR("A")="Do you want to exit processing request",DIR("B")="Yes" D ^DIR K DIR S:Y!($D(DIRUT)) RAOUT=1
 K DIC S DIC="^TMP("_$J_",""RAWP"",",DWPK=1
 S WPFLG=$$VALWP("^TMP("_$J_",""RAWP"",")
 I 'WPFLG D  Q:$D(RAOUT)  G CH
 . W !!,$C(7),"A clinical history corresponding to this request is required.",!
 . K DIR S DIR(0)="Y",DIR("B")="Yes"
 . S DIR("A")="Do you want to exit processing this request"
 . S DIR("?")="Enter 'Y' for yes, 'N' for no." D ^DIR K DIR
 . S:+Y!($D(DIRUT)) RAOUT=1 K DIROUT,DIRUT,DTOUT,DUOUT
 . Q
WPLEN ;Is clin hist too long to go into a local array for OE/RR HL7 msg?
 S (CNT,X)=0 F  S X=$O(^TMP($J,"RAWP",X)) S CNT=CNT+1 Q:X'>0
 I CNT>350 K CNT D  Q:$D(RAOUT)  G CH
 . W !!,$C(7),"Clinical history cannot exceed 350 lines."
 . K DIR S DIR(0)="Y",DIR("B")="Yes"
 . S DIR("A")="Do you want to exit processing this request"
 . S DIR("?")="Enter 'Y' for yes, 'N' for no." D ^DIR K DIR
 . S:+Y!($D(DIRUT)) RAOUT=1 K DIROUT,DIRUT,DTOUT,DUOUT
 . Q
 K CNT Q
 ;
VALWP(RAROOT) ; Validate word processing field.
 ; Pass back '1' if data is valid, '0' if not valid.
 ; at least 2 alphanumeric char's required
 Q:'$O(@(RAROOT_"0)")) 0
 N CHAR,CNT,WL,WPFLG,X,Y,Z
 S (WPFLG,X)=0
 F  S X=$O(@(RAROOT_X_")")) Q:X'>0  D  Q:WPFLG
 . S (CNT,WL)=0
 . S Y=$G(@(RAROOT_X_",0)")) Q:Y']""
 . S WL=$L(Y)
 . F Z=1:1:WL D  Q:WPFLG
 .. S CHAR=$E(Y,Z) S:CHAR?1AN CNT=CNT+1
 .. S:CHAR'?1AN&(CNT>0) CNT=0 S:CNT=2 WPFLG=1
 .. Q
 . Q
 Q WPFLG
RDQ(D0) ; Used by input transform on ^DD(74.31,2
 ; Checks for unprinted reports associated with REPORT
 ; DISTRIBUTION QUEUE of internal entry number of D0.
 N %,%Y,FOUND,RA744
 S (FOUND,RA744)=0
 F  S RA744=$O(^RABTCH(74.4,"C",D0,RA744)) Q:RA744'>0!FOUND  D
 . S FOUND=($P($G(^RABTCH(74.4,RA744,0)),"^",4)'>0)
 . Q
 Q:'FOUND
 W !!,"*** UNPRINTED REPORTS IN THE QUEUE ! ***"
 W !,"If this queue is inactivated before printing, these reports will be",!,"removed from the queue."
 F  D  Q:%
 . W !!,"Are you sure you want to remove these reports"
 . S %=2 D YN^DICN
 . I '% W !!?5,"Please answer Y(es) or N(o)."
 . Q
 I %'=1 W !,"Inactivation date deleted" K X
 Q
ATND(RADFN,DATE) ;Returns the external form of the ATTENDING PHYSICIAN
 ;for patient RADFN (IEN file #2) on date DATE (FM format)
 N DPT,VA200,VAIP,X
 S DFN=RADFN,VAIP("D")=DATE,VA200=1
 I DATE D IN5^VADPT
 S X=$P($G(VAIP(18)),"^",2),X=$S(X]"":X,1:"UNKNOWN")
 Q X
PRIM(RADFN,DATE) ;Returns the external form of the PRIMARY PHYSICIAN
 ;for patient RADFN (IEN file #2) on date DATE (FM format)
 N DPT,VA200,VAIP,X
 S DFN=RADFN,VAIP("D")=DATE,VA200=1
 I DATE D IN5^VADPT
 I '+$G(VAIP(7)) D
 . ; If the Primary Physician is not found (based on inpatient episode)
 . ; find the current PC Practitioner (See patch SD*5.3*30)
 . ; VAIP(7) is null at this point.  VAIP(7) will exit this DO block
 . ; set to the Primary Care Practitioner or null.
 . N X S X="SDUTL3" X ^%ZOSF("TEST")
 . S:$T VAIP(7)=$$OUTPTPR^SDUTL3(RADFN)
 . Q
 S X=$P($G(VAIP(7)),"^",2),X=$S(X]"":X,1:"UNKNOWN")
 Q X
EOS() ; 'End Of Screen' prompt for terminals only, check user response.
 Q:$E(IOST,1,2)'="C-" 0
 N RAY,X,X1,X2,X3,Y,Y0,Y1,Y2,Y3,Y4,Y5
 ;Returns 1 if user enters anything other than a carriage return
 K DIR S DIR(0)="E" D ^DIR K DIR,DIROUT,DIRUT,DTOUT,DUOUT
 S RAY='+Y
 Q RAY
XTERNAL(Y,C) ; Change internal format to external format
 ; 'Y' is the internal form of the data
 ; 'C' defines the data type of the variable 'Y'
 D:Y]"" Y^DIQ
 Q Y
PROCMSG(RAPRI) ; Print the appropriate procedure messages.  Called from
 ; DESDT^RAUTL12.  This code works under the assumption that the
 ; user has entered through OE/RR.
 ;ATTENTION: this code must be parallet to code in EN2^RAPRI
 Q:+$G(RASTOP)  ; Do not display if displayed in the past.
 I $O(^RAMIS(71,RAPRI,3,0)) D  S RASTOP=1
 . N I,RAX,X S I=0
 . W !!?5,"NOTE: The following special requirements apply to this "
 . W "procedure:",$C(7),!
 . F  S I=+$O(^RAMIS(71,RAPRI,3,I)) Q:'I  D
 .. S RAX=+$G(^RAMIS(71,RAPRI,3,I,0))
 .. I $D(^RAMIS(71.4,+RAX,0)) D
 ... I $Y>(IOSL-6) D READ^ORUTL W @IOF
 ... S X=$G(^RAMIS(71.4,+RAX,0)) W !?3,X
 ... Q
 .. Q
 . Q
 I $O(^RAMIS(71,RAPRI,"EDU",0)),($$UP^XLFSTR($P($G(^RAMIS(71,RAPRI,0)),"^",17))="Y") D
 . W:+$O(^RAMIS(71,+RAPRI,3,0))>0 !!
 . N DIW,DIWF,DIWL,DIWR,RAX,X
 . K ^UTILITY($J,"W") S DIWF="W",DIWL=1,DIWR=75,RAX=0
 . F  S RAX=$O(^RAMIS(71,RAPRI,"EDU",RAX)) Q:RAX'>0  D
 .. I $Y>(IOSL-4) D READ^ORUTL W @IOF
 .. S X=$G(^RAMIS(71,RAPRI,"EDU",RAX,0)) D ^DIWP
 .. Q
 . I $Y>(IOSL-4) D READ^ORUTL W @IOF
 . D ^DIWW
 . W !
 . Q
 Q
MIDNGHT(X) ; Check if the date passed in is midnight.  If it is, add one
 ; minute to the date/time.  Fixes infinite loop problem in FM when
 ; midnight.
 ; Input: X-Current system date/time (derived from $$NOW^XLFDT)
 S:X["." X=$E(X,1,($F(X,".")+3)) ; chop off seconds IF there's decimal
 S:+$P(X,".",2)=24!(+$P(X,".",2)=0) X=$$FMADD^XLFDT(X,0,0,1,0) ; add a minute to midnight
 Q X
