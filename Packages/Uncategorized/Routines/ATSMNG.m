ATSMNG ;TUCSON/DG;CREATE, MODIFY SEARCH TEMPLATES  [ 10/25/91  1:47 PM ]
 ;;2.5;SEARCH TEMPLATE COMPARISON;;OCT 25, 1991
 ;
CREATE ; - EP - SELECT FILE FROM WHICH TO GENERATE SEARCH TEMPLATE
 S ATSNOASK="" ;SETS FLAG SO THAT ADD DOESN`T ASK SEARCH TEMPLATE NAME
 S DIC="^DIC(",DIC(0)="AEMQ",DIC("A")="Select File: " D ^DIC K DIC
 I Y<0 D EOJ Q
 E  S ATSFLNUM=+Y
 D NAME D:'$D(ATSTP) ADD D EOJ
 Q
 ;
NAME ;CREATE SEARCH TEMPLATE
 R !,"Enter Name Of New Search Template: ",X:300 S:'$T X="^"
 I "^"[X S ATSTP="" Q
 I $E(X)="?" G NAME
 S DIC="^DIBT(",DIC(0)="EQL",DIC("DR")="[ATSCREATE]",DLAYGO=.401,DIADD=1 D ^DIC K DIC,DLAYGO,DR,DIE,DIADD
 I Y<0 S ATSTP=""
 E  S ATSTMP=+Y
 Q
 ;
ADD ; - EP - ADD ENTRY TO A SEARCH TEMPLATE
 I '$D(ATSNOASK) S DIC="^DIBT(",DIC(0)="AEMQ",DIC("S")="I $P(^(0),""^"",5)=DUZ,($D(^(1))!('$D(^(2))))",DIC("A")="Select SEARCH TEMPLATE: " D ^DIC K DIC Q:Y<0  S ATSTMP=+Y
 F ATSL=0:0 S DIC=$P(^DIBT(ATSTMP,0),"^",4),DIC(0)="AEMQ" D ^DIC K DIC Q:Y<0  W @$S($D(^DIBT(ATSTMP,1,+Y)):"!,*7,""Entry already in search template!"",!",1:"!,""Entered!"",!") S ^DIBT(ATSTMP,1,+Y)=""
 D:'$D(ATSNOASK) EOJ
 Q
 ;
DELETE ; - EP - DELETE ENTRY FROM A SEARCH TEMPLATE
 S DIC="^DIBT(",DIC(0)="AEMQ",DIC("S")="I $P(^(0),""^"",5)=DUZ,($D(^(1))!('$D(^(2))))",DIC("A")="Select SEARCH TEMPLATE: " D ^DIC K DIC Q:Y<0  S ATSTMP=+Y
 F ATSL=0:0 S DIC=$P(^DIBT(ATSTMP,0),"^",4),DIC(0)="AEMQ" D ^DIC K DIC Q:Y<0  W @$S('$D(^DIBT(ATSTMP,1,+Y)):"!,*7,""Entry does not exist in search template!"",!",1:"!,""Deleted!"",!") K ^DIBT(ATSTMP,1,+Y)
 D EOJ
 Q
 ;
EOJ ;
 K ATSTMP,ATSNOASK,ATSTP,ATSFLNUM,ATSL
 Q
 ;
