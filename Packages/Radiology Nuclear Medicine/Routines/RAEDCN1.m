RAEDCN1 ;HISC/GJC-Utility routine for RAEDCN ;9/18/97  13:49
 ;;5.0;Radiology/Nuclear Medicine;**18**;Mar 16, 1998
 ; last modif by SS for P18
UNDEF ; Message for undefined imaging types
 I '+$G(RAMLC) D  Q
 . W !?5,"Imaging Location data is not defined, "
 . W "contact IRM.",$C(7)
 . Q
 W !?5,"An Imaging Type was not defined for the following Imaging"
 W !?5,"Location: "_$P(^SC($P($G(^RA(79.1,+RAMLC,0)),U),0),U)_"."
 Q
STUB(RARPT) ; Determine if this is an imaging stub report.
 ; Input: RARPT-ien of the report record
 ; Output: 1 if an imaging stub rpt, else 0
 N RA0 S RA0=$O(^RARPT(RARPT,"L",""),-1) ; most recent activity on rpt
 I RA0>0,$P($G(^RARPT(RARPT,"L",RA0,0)),U,2)="C",$P(^RARPT(RARPT,0),U,5)="",$O(^RARPT(RARPT,2005,0)),'$D(^RARPT(RARPT,"I")),'$D(^("P")),'$D(^("R")) Q 1 ; rpt is an image stub
 Q 0 ; (non-stub rpt record)
 ;
PSET(RADFN,RADTI,RACNI) ; Determine if this exam is part of a printset.
 ; Input: RADFN-patient dfn <-> RADTI-exam timestamp <-> RACNI-exam ien
 ; Output: 1 if part of a printset, else 0
 Q $S($P($G(^RADPT(RADFN,"DT",RADTI,"P",RACNI,0)),"^",25)=2:1,1:0)
 ;
CKREASON(X) ;check file 75.2 ; P18 moved it from RAEDCN because the routine's length exceeded limit
 ; 0=OKAY, 1=BAD
 ; don't check for var RAOREA, because it's not set this early
 I X="C",$O(^RA(75.2,"B","EXAM CANCELLED",0)) Q 0
 I X="D",$O(^RA(75.2,"B","EXAM DELETED",0)) Q 0
 W !!?5,$S(X="C":"Cancellation",1:"Deletion")," cannot be done, because your file #75.2,"
 W !?5,"RAD/NUC MED REASON, does not have ""EXAM ",$S(X="C":"CANCELLED",1:"DELETED"),"""","."
 W !!?5,"Please notify your ADPAC.",!
 K DIR S DIR(0)="E",DIR("A")="Press RETURN for menu options" D ^DIR K DIR,DIROUT,DIRUT,DTOUT,DUOUT
 Q 1
