SDAMO ;ALB/MJK - AM Mgt Reports ;  [ 07/18/2002  3:44 PM ]
 ;;5.3;Scheduling;**132,1007**;Aug 13, 1993
 ;
EN ; main entry point
 N DIC,SDBEG,SDEND,SDSEL,VAUTD,VAUTC,SDSORT,SDAMLIST
EN1 I '$$INIT G ENQ
 I '$$RANGE G ENQ
 I '$$SELECT G ENQ
 S SDSEL=SDSEL+4 ; for backwards compatibility
 G STATS^SDAMOS
ENQ Q
 ;
INIT() ; -- init vars
 Q 1
 ;
RANGE() ; select date range
 ;  input: none
 ; output: SDBEG := begin date
 ;         SDEND := end date
 ; return: was selection made [ 1|yes   0|no]
 ;
 W !!,$$LINE("Date Range Selection")
 Q $$RANGE^SDAMQ(.SDBEG,.SDEND)
 ;
SELECT() ; -- get selection criteria
 ;  input: none
 ; output: SDSEL := criteria selected
 ; return: was selection made [ 1|yes   0|no]
 ;
 W !!,$$LINE("Statistics Criteria")  ;cmi/anch/maw 8/14/2007 patch 1007 mispelled
 S X="S^"
 S X=X_"1:Statistics;"
 S X=X_"2:Division(s) Only Statistics"
 S DIR(0)=X,DIR("A")="Which Visits",DIR("B")="Statistics"
 D ^DIR K DIR S SDSEL=$S($D(DIRUT):0,1:+Y)
 Q SDSEL>0
 ;
DIV() ;EP -- get division data
 ;  input: none
 ; output: VAUTD := divs selected (VAUTD=1 for all)
 ; return: was selection made [ 1|yes   0|no]
 ;
 W:$P($G(^DG(43,1,"GL")),U,2) !!,$$LINE("Division Selection")
 D ASK2^SDDIV I Y<0 K VAUTD
 Q $D(VAUTD)>0
 ;
CLINIC() ;EP -- get clinic data
 ;  input: VAUTD  := divisions selected
 ; output: VAUTC := clinic selected (VAUTC=1 for all)
 ; return: was selection made [ 1|yes   0|no]
 ;
 W !!,$$LINE("Clinic Selection")
 ;
 ;7/18/02 WAR - REMd next line and changed code per LJF16
 ;D CLINIC^SDAMO0
 D CLINIC^BSDU(2)   ;IHS/ANMC/LJF 7/5/2002
 I Y<0 K VAUTC
CLINICQ Q $D(VAUTC)>0
 ;
STOP() ; -- get stop code data
 ; output: VAUTC := stop codes selected (VAUTC=1 for all)
 ; return: was selection made [ 1|yes   0|no]
 ;
 W !!,$$LINE("Stop Codes Selection")
 S DIC="^DIC(40.7,",VAUTSTR="stop code",VAUTVB="VAUTC",VAUTNI=2
 D FIRST^VAUTOMA
 I Y<0 K VAUTC
STOPQ Q $D(VAUTC)>0
 ;
 ;
LINE(STR) ;EP -- print line
 ;  input: STR := text to insert
 ; output: none
 ; return: text to use
 ;
 N X
 S:STR]"" STR=" "_STR_" "
 S $P(X,"_",(IOM/2)-($L(STR)/2))=""
 Q X_STR_X
 ;
