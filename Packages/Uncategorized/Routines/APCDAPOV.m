APCDAPOV ; IHS/CMI/LAB - POV LOOKUP ;
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
 ;
START ;
 S APCDTPCC=""
 X:$D(^DD(9000010.07,.01,12.1)) ^DD(9000010.07,.01,12.1) S DIC="^ICD9(",DIC(0)="AEMQ",DIC("A")="Enter PURPOSE of VISIT: " D ^DIC K DIC
 G:Y="" XIT
 I Y=-1,X=""!(X="^") S APCDTSKI=1,APCDLOOK="" G XIT
 I Y=-1 S APCDTERR=1,APCDLOOK="" G XIT
 S APCDLOOK="`"_+Y,APCDTNQP=X
XIT K Y,X,DO,D,DD,DIPGM,APCDTPCC
 Q
