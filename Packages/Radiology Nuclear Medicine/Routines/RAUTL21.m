RAUTL21 ;HOIFO/SWM,CRT;list & delete unneeded ^RARPT("ASTF" & "ARES" ;2/12/99  16:01
 ;;5.0;Radiology/Nuclear Medicine;**26**;Mar 16, 1998
 ;
EN1 N RA1,RA2,RACNT,RAKILREF,RALL,RATOT
 S U="^"
 S $P(RADL,"=",32)=""
 S $P(RASL,"-",26)=""
 S RATOT=0 ; total # of superfluous x-refs
 S RAKILREF=0 ; flag to control kill of x-refs and display
 ;
 D EN^DDIOL("RAD/NUC MED UTILITY TO LIST/DELETE LEFT-OVER REPORT X-REFS",,"!?3")
 D EN^DDIOL(" ",,"!!")
 ;
 S DIR(0)="Y"
 S DIR("B")="YES"
 S DIR("A")="  Do you want to print a list of left-over x-refs?"
 D ^DIR K DIR
 I $D(DTOUT)!$D(DUOUT) Q
 I Y=1 D
 .N %ZIS
 .S %ZIS("A")="Select Device: "
 .D ^%ZIS I POP K STOUT,DUOUT,POP Q
 .U IO
 .F RAXREF="ARES","ASTF" D L1
 .S:RATOT=0 RATOT=-1
 .D ^%ZISC,HOME^%ZIS
 ;
 Q:RATOT<0
 D EN^DDIOL(" ",,"!!")
 S DIR(0)="Y"
 S DIR("A")="  Do you want to clean up the"_$S(RATOT:"se "_RATOT,1:"")_" left-over x-refs?"
 S DIR("B")="NO"
 D ^DIR K DIR
 I $D(DTOUT)!$D(DUOUT) Q
 I Y=1 D
 .S RAKILREF=1
 .F RAXREF="ARES","ASTF" D L1
 Q
 ;
L1 ; Loop through left-over x-refs
 ;
 N WAIT
 ;
 I 'RAKILREF D HEAD
 ;
 S WAIT=""
 S RA1=0 F  S RA1=$O(^RARPT(RAXREF,RA1)) Q:'RA1  D  Q:WAIT="^"
 . S RACNT=0
 . S RA2=0 F  S RA2=$O(^RARPT(RAXREF,RA1,RA2)) Q:'RA2  D  Q:WAIT="^"
 .. I $D(^RARPT(RA2,0)),$P(^RARPT(RA2,0),U,5)'="V" Q
 .. S RACNT=RACNT+1 ; Total for this physician
 .. S RATOT=RATOT+1
 .. I 'RAKILREF D  Q:WAIT="^"
 ... I $Y>(IOSL-3) D WAIT Q:WAIT="^"  S WAIT="" W @IOF D HEAD S RACNT=1
 ... D EN^DDIOL($S(RACNT=1:$E($P($G(^VA(200,RA1,0)),U),1,30),1:"  "),,"!?3")
 ... D EN^DDIOL($S($D(^RARPT(RA2,0)):$P(^(0),U),1:"Unknown report #"_RA2),,"?40")
 .. I RAKILREF D
 ... D EN^DDIOL("^RARPT("""_RAXREF_""","_RA1_","_RA2_") deleted","","!?3")
 ... K ^RARPT(RAXREF,RA1,RA2)
 Q:WAIT="^"
 I RATOT=0 D EN^DDIOL("< There are no left-over """_RAXREF_""" x-refs found. >","","!?10")
 Q
 ;
HEAD ;
 D EN^DDIOL("LEFT-OVER ^RARPT("""_RAXREF_""") X-REFS",,"!!?20")
 D EN^DDIOL(RADL,,"!?20")
 D EN^DDIOL($S(RAXREF="ARES":"RESIDENT",1:"STAFF")_" PHYSICIAN",,"!!?3")
 D EN^DDIOL("CASE # OF LEFT-OVER X-REF",,"?40")
 D EN^DDIOL($S(RAXREF="ARES":$E(RASL,1,18),1:$E(RASL,1,15)),,"!?3")
 D EN^DDIOL(RASL,,"?40")
 D EN^DDIOL(" ",,"!")
 Q
 ;
WAIT ;
 I $E(IOST,1,2)'="C-" S WAIT="" Q  ;Don't prompt if report not to screen
 ;
 N DIR
 S DIR(0)="E"
 S (DIR("?"),DIR("??"))=""
 D ^DIR K DIR
 I Y=""!(Y=0) S WAIT="^"
 Q
