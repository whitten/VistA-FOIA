AMQQCMP1 ; IHS/CMI/THL - PRELIMINARY QUERY COMPILE ;
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
 ;-----
 I $D(AMQQMULX) D ^AMQQCMPM I $D(AMQQQUIT) G EXIT
VAR S AMQQ="^UTILITY(""AMQQ"",$J,""WEIGHT"")"
 K AMQQRED
 I AMQQOPT="FAST",'$D(^UTILITY("AMQQ",$J)) S AMQQFAIL=4 D FAIL^AMQQN S AMQQQUIT=1 Q
 S AMQQLINO=1
 S AMQQVAR=9
 S (%,AMQQSER)=$O(@AMQQ@(-9999))
 S AMQQUATN=$O(@AMQQ@(+%,""))
 S AMQQTURB=^(AMQQUATN)
 S Q=^UTILITY("AMQQ",$J,"Q",AMQQUATN)
 I $P(Q,U,17)!($P(Q,U,3)="I") S %=$P(Q,U,9),%=$P(%,";",5) I %="NULL"!(%="INVERSE")!(%="ANY") D @("START"_AMQQCCLS) G EXIT
 I Q[";ALL^",$P(Q,U,3)="L" D @("START"_AMQQCCLS) G EXIT
 I $D(AMQQRAND) D @("RAND"_AMQQCCLS) K AMQQRAND G EXIT
 I $D(AMQQCHRT) D @("COH"_AMQQCCLS) K AMQQCHRT G EXIT
 S %=$P(Q,U,9)
 I %[";NULL"!(%[";ANY") D @("START"_AMQQCCLS) G EXIT
 I $D(^UTILITY("AMQQ",$J,"Q",AMQQUATN,1)),$P(^(1),U,2)="NULL" D @("START"_AMQQCCLS) G EXIT
 I %'["EXIST",'$P(Q,U,4),(($P(Q,U,8)["'><")!($P(Q,U,8)["'=")) D @("START"_AMQQCCLS) G EXIT
 I AMQQSER>1 G EXIT
 S %=$P(Q,U,15)
 S %=$P(%,";",4,5)
 I +%>$P(%,";",2) G EXIT
GT I AMQQTURB["AQ" D @(AMQQTURB_"^AMQQCMPT") G EXIT
 I AMQQTURB S %=$P(Q,U,15),%=$P(%,";",4) I %'["*" D @("TURB"_AMQQTURB_U_$S(AMQQTURB<5:"AMQQCMPT",1:"AMQQCMPZ"))
EXIT S AMQQSER=-9999
 K X,AMQQTURB,Q
 Q
 ;
STARTH S ^UTILITY("AMQQ",$J,"Q",.1)="211^NAME (PROVIDER)^F^^^^^^^^^^^^'=;|||||;;;"
 G ST1
STARTP S ^UTILITY("AMQQ",$J,"Q",.1)="3^NAME^F^^^^^^^^^^^^'=;|||||;;;"
 G ST1
NOALPHA S ^UTILITY("AMQQ",$J,"Q",.1)="164^POV NUMBER^N^^^^^^^^^^^^0;999999999;"
 G ST1
STARTV S ^UTILITY("AMQQ",$J,"Q",.1)="133^DATE OF VISIT^D^^^^^^^^^^^^0;99999999;"
 G ST1
ST1 S ^UTILITY("AMQQ",$J,"WEIGHT",-99,.1)=""
 Q
 ;
RANDP S ^UTILITY("AMQQ",$J,"Q",.1)="37^RANDOM^R^^^^^^^^^^^1^"_AMQQRAND
 G RA1
RANDV S ^UTILITY("AMQQ",$J,"Q",.1)="140^RANDOM^R^^^^^^^^^^^1^"_AMQQRAND
 G RA1
RA1 S ^UTILITY("AMQQ",$J,"WEIGHT",-99,.1)=""
 Q
 ;
COHP S ^UTILITY("AMQQ",$J,"Q",.1)="40^COHORT^C^^^^^^^^^^^1^"_AMQQCHRT
 G CO1
COHV S ^UTILITY("AMQQ",$J,"Q",.1)="141^COHORT^C^^^^^^^^^^^1^"_AMQQCHRT
 G CO1
CO1 S ^UTILITY("AMQQ",$J,"WEIGHT",-99,.1)=""
 Q
 ;
