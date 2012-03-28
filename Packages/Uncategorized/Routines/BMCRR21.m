BMCRR21 ; IHS/PHXAO/TMJ - PROCESS REFERRAL LIST ;   
 ;;4.0;REFERRED CARE INFO SYSTEM;;JAN 09, 2006
 ;IHS/ITSC/FCJ ADDED TEST FOR SR
 ;
START ;
 S (BMCBT,BMCBTH)=$H,BMCJOB=$J,BMCRCNT=0
 D PROCESS,END
 Q
 ;
PROCESS ;
 S BMCREF=0 F  S BMCREF=$O(^BMCREF(BMCREF)) Q:BMCREF'=+BMCREF  D PROC
 Q
 ;
END ;
 S BMCET=$H
 Q
PROC ;
 S BMCRREC=^BMCREF(BMCREF,0),DFN=$P(BMCRREC,U,3)
 Q:$P(BMCRREC,U,4)="N"
 Q:$P(BMCRREC,U,14)'="I"
 Q:$P(BMCRREC,U,15)'="A"  ;QUIT IF NOT ACTIVE
 Q:$$AVDOS^BMCRLU(BMCREF,"I")=""  ;QUIT IF NO EST OR ACTUAL BEG DOS
 Q:$$AVDOS^BMCRLU(BMCREF,"I")>DT  ;QUIT IF DOS IS AFTER TODAY
 I $P($G(^BMCREF(BMCREF,11)),U,8)]"",$P(^BMCREF(BMCREF,11),U,8)<DT Q  ;if actual ending date of service is present and before today - quit
 ;get sort value
 S BMCSORT=""
 D @BMCSTYPE
 S:BMCSORT="" BMCSORT="??"
 S ^XTMP("BMCRR2",BMCJOB,BMCBTH,"DATA HITS",BMCSORT,BMCREF)="",BMCRCNT=BMCRCNT+1
 Q
F ;sort by facility
 S BMCSORT=$$FACREF^BMCRLU(BMCREF)
 Q
P ;sort by patient name
 S BMCSORT=$P(^DPT(DFN,0),U)
 Q
C ;sort by case manager
 S BMCSORT=$$CASEMAN^BMCRLU(BMCREF)
 Q
