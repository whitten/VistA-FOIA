APCDR13 ; IHS/CMI/LAB - V EXAM REVIEW ;
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
EXAM ; Exam Code -- Char Pos 49-50
 S APCDXAM=$P(^AUPNVXAM(APCDEDFN,0),U) I APCDXAM="" S APCDE="E012" D ERR G XIT
 I '$D(^AUTTEXAM(APCDXAM,0)) S APCDE="E012" D ERR G XIT
 I $P(^AUTTEXAM(APCDXAM,0),U,2)="" S APCDE="E012" D ERR G XIT
 I $P(^AUTTEXAM(APCDXAM,0),U,2)=15,AUPNSEX="M" S APCDE="E049" D ERR G XIT
 ;
XIT ;
 K APCDXAM,APCDE
 Q
ERR ;
 S APCDE("FILE")=9000010.13,APCDE("ENTRY")=APCDEDFN
 D ERR^APCDRV
 Q
