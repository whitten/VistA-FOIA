AMQQ1 ; IHS/CMI/THL - AMQQ SUBROUTINE...GETS GOAL OF QUERY; ; 26 Oct 2010  9:07 AM
 ;;2.0;IHS PCC SUITE;**5**;MAY 14, 2009
 ;IHS/CMI/LAB - added ability to choose a CMS register
 ;-----
GOAL I '$D(AMQQOPT) S AMQQOPT="SEARCH"
 I $D(AMQQEN31),AMQQEN31=+AMQQEN31 D SWAP Q
G1 W !,$S(AMQQOPT="FAST":"Tell me what you want: ",1:"What is the subject of your search?  LIVING PATIENTS // ")
 R X:DTIME E  S AMQQQUIT=1 Q
 I X["  " W "  ??",*7 G GOAL
 I $L(X," ")>4 S AMQQQSTG=X D ^AMQQN S AMQQQUIT="" K AMQQXX G EXIT
 I $G(AMQQOPT)="FAST",$E(X)'="?" S:"^"[X AMQQQUIT=1 G:"^"[X EXIT S AMQQQSTG=X D ^AMQQN S AMQQQUIT="" K AMQQXX G EXIT
 I X="",AMQQOPT="QUICK" S X=U
 I X="HELP" S X="?"
 I X="??" D LISTG^AMQQHELP G GOAL
 I $E(X)=U S AMQQQUIT=1 G EXIT
 I X?1."?" N %A,%B S XQH=$O(^DIC(9.2,"B","AMQQSUBJECT","")) D EN1^XQH G GOAL
 I X="" S X="LIVING PATIENTS"
 I $E(X)'?1UN W "  ??",*7 G GOAL
 ;I X="LIVING PATIENTS"!(X="REGISTER") D QAPT^AMQQAPT W !! S X="LIVING PATIENTS"
 D AUTO
 I Y'=-1 D  Q
 .NEW AMQQLLLL S AMQQLLLL=""
 .I $P(^AMQQ(5,+Y,0),U,9)="P" S AMQQLLLL=Y D QAPT^AMQQAPT W !! S Y=AMQQLLLL
 .D NEW Q
 D ^AMQQ2
AUTO1 ; ENTRY POINT FOR DFN SUBJECT
 N X
 I $D(AMQQFAIL) K AMQQFAIL G GOAL
 D PERSON
 Q
 ;
EXIT K X,%,I
 Q
 ;
AUTO ; ENTRY POINT FROM AMQQQ
 S DIC(0)="E"
 S DIC="^AMQQ(5,"
 S DIC("S")="I $P(^(0),U,9)'="""""
 S D="C"
 I $D(AMQQNECO) S DIC(0)=""
 E  I $D(AMQQXX) S DIC(0)="ES"
 D IX^DIC
 K DIC
 Q
 ;
LISTG S DIC="^AMQQ(5,"
 S DIC(0)="E"
 S D="GOAL"
 S DZ="??"
 D DQ^DICQ
 K DIC,DZ,D,DIX,DIY,DD,%H,%,DO,X,Y
 Q
 ;
PERSON ; ENTRY POINT FROM AMQQN1 THE NATURAL LANGUAGE ROUTINE
 S X=$P(Y,U,3)
 S Y=$P(Y,U,4)
 S Y=$P(Y,",",2)_" "_$P(Y,",")
 S AMQQQ="8^NAME^L^^9^1^EQUAL TO^=^"_X_"^^100^W ?6,""NAME = "","""_Y_"""^1^0^=;"_X_";"
 S ^UTILITY("AMQQ",$J,"Q",1)=AMQQQ
 S AMQQUATN=2
 S AMQQUNBC=1
 I '$D(AMQQXX) S ^UTILITY("AMQQ",$J,"LIST",2)="W ?6,""NAME = "_Y_""""
 S ^UTILITY("AMQQ",$J,"WEIGHT",-99,1)=""
 S AMQQONE=Y
 S Y="1^PATIENT"
 D NEW
 I $D(AMQQXX) Q
 S AMQQILIN=2
 D LIST^AMQQ
 Q
 ;
NEW ; ENTRY POINT FROM AMQQN1
 I $D(^AMQQ(5,+Y,2)) S AMQQATN=+Y,AMQQCCLS=$P(^AMQQ(5,+Y,0),U,9) D SCRIPT Q
N1 S AMQQCNAM=$P(Y,U,2)
 S (X,AMQQCCLS)=$P(^AMQQ(5,+Y,0),U,9)
 I AMQQCNAM["RANDOM" S AMQQRSAF=""
 I $D(AMQQXX) Q
 I AMQQCNAM="REGISTER" D ^AMQQREG Q
 S AMQQILIN=1
 S X=$S(X="P":"PATIENTS",X="H":"PROVIDER",X="V":"VISIT",1:"CLINICAL DATA")
 I $D(AMQQONE),AMQQONE'="" S X=AMQQONE
 S %="W ?3"
 S %=%_",@AMQQRV,""Subject of search: "_X_""",@AMQQNV" G SETNG
 S %=%_","""_X_""""
SETNG S ^UTILITY("AMQQ",$J,"LIST",.1)=%
 Q
 ;
SCRIPT ; ENTRY POINT FROM AMQQATA
 S Z=0
 I ^AMQQ(5,AMQQATN,2,1,0)?1U S X=^(0),Z=1 D N1
SCR1 ; ENTRY POINT FROM AMQQATA
 S AMQQI=Z
 F  S AMQQI=$O(^AMQQ(5,AMQQATN,2,AMQQI)) Q:'AMQQI  S AMQQQ=^(AMQQI,0) D:$P(AMQQQ,U,3)="D" SCRDT D ^AMQQATR,^AMQQATL,^AMQQATS S AMQQUATN=AMQQUATN+1,AMQQUNBC=AMQQUNBC+1
 K AMQQATN,Z,AMQQI
 D LIST^AMQQ
 Q
 ;
SCRDT N %,X,Y,Z,A,B S %=$P(AMQQQ,U,9)
 I %["NULL"!(%["ANY")!(%["ALL")!(%["EXIST") Q
 S Y=$P(%,";"),Z=$P(%,";",2)
 D SCRDT1
 I B'="" S A=A_";"_B
 S $P(AMQQQ,U,9)=A
 Q
 ;
SCRDT1 S A=Y
 S B=Z
 I 'Y S X=Y D ^%DT S A=Y
 I Z=""!(+Z) Q
 S X=Z
 D ^%DT
 S B=Y
 Q
 ;
SWAP S AMQQCCLS="P"
 S AMQQCNAM="PATIENTS"
 S AMQQUATN=2
 S AMQQILIN=0
 S ^UTILITY("AMQQ",$J,"LIST",.1)="W ?3,@AMQQRV,""Subject of search: PATIENTS in the COHORT"",@AMQQNV"
 S ^UTILITY("AMQQ",$J,"WEIGHT",-99,1)=""
 S %=AMQQEN31
 S ^UTILITY("AMQQ",$J,"Q",1)="40^COHORT^C^0^238^1^^^"_%_"^^99^^^0^"_%_";;^0"
 W !!,"You will now enter criteria for conducting a search on a preexisting cohort"
 W !,"of patients."
 W !!
 Q
 ;
