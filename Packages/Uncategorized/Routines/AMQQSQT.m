AMQQSQT ; IHS/CMI/THL - TRAVERSES SUBQUERY TREE ;
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
 ;-----
TREE ; NAVIGATES THRU SUBQUERY TREES ; ENTRY POINT FROM NUMEROUS ROUTINES ; SENDS AMQQSQN1 (AMQQUSQN) AND AMQQSQN2 (AMQQSQSQ) TO THE SUBROUTINE IN @AMQQJOB
 N AMQQUATN,AMQQUSQN,AMQQX,AMQQY,AMQQSQSQ,AMQQTLVL
 S AMQQX="^UTILITY(""AMQQ"",$J,""SQXQ"")"
 S AMQQY="^UTILITY(""AMQQ"",$J,""SQXS"")"
 F AMQQUATN=0:0 S AMQQUATN=$O(@AMQQX@(AMQQUATN)) Q:'AMQQUATN  F AMQQUSQN=0:0 S AMQQUSQN=$O(@AMQQX@(AMQQUATN,AMQQUSQN)) Q:'AMQQUSQN  D TOP
 Q
 ;
TREE1 ; ATTRIBUTE SPECIFIC SUBQUERY TREE NAVIGATOR
 N AMQQY,AMQQUSQN,AMQQSQSQ,AMQQTLVL
 S AMQQY="^UTILITY(""AMQQ"",$J,""SQXS"")"
 F AMQQUSQN=0:0 S AMQQUSQN=$O(^UTILITY("AMQQ",$J,"SQXQ",AMQQUATN,AMQQUSQN)) Q:'AMQQUSQN  D TOP
 Q
 ;
TREE2 ; ENTRY POINT FROM AMQQSQ
 N AMQQY,AMQQSQSQ,AMQQTLVL
 S AMQQY="^UTILITY(""AMQQ"",$J,""SQXS"")"
 D TOP
 F AMQQY=1:1:9 K AMQQUSQN(AMQQY)
 Q
 ;
TOP S AMQQSQN1=AMQQUATN
 S AMQQSQN2=AMQQUSQN
 S AMQQTLVL=1
 D @AMQQSQJ1
 K AMQQSQN1,AMQQSQN2
 I '$D(@AMQQY@(AMQQUSQN)) Q
 S AMQQUSQN(1)=AMQQUSQN
 S AMQQSQSQ(1)=0
INC S AMQQSQSQ(AMQQTLVL)=$O(@AMQQY@(AMQQUSQN(AMQQTLVL),AMQQSQSQ(AMQQTLVL)))
 I 'AMQQSQSQ(AMQQTLVL) Q:AMQQTLVL=1  S AMQQTLVL=AMQQTLVL-1 G INC
 I $D(@AMQQY@(AMQQSQSQ(AMQQTLVL))) S AMQQUSQN(AMQQTLVL+1)=AMQQSQSQ(AMQQTLVL),AMQQTLVL=AMQQTLVL+1,AMQQSQSQ(AMQQTLVL)=0 G INC
 S AMQQSQN1=AMQQUSQN(AMQQTLVL)
 S AMQQSQN2=AMQQSQSQ(AMQQTLVL)
 D @AMQQSQJ2
 K AMQQSQN1,AMQQSQN2
 G INC
 ;
