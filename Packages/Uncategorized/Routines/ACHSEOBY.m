ACHSEOBY ; IHS/ITSC/PMF - CHECK STATUS OF BCBS EOBR REPORTS ;  [ 10/16/2001   8:16 AM ]
 ;;3.1;CONTRACT HEALTH MGMT SYSTEM;;JUN 11, 2001
 ;
 I '$L($$AOP^ACHS(2,1)) D NODIR Q
 K ACHSUNMS,ACHSUFLS
 S ACHSLCTR=0
 I $$LIST^%ZISH($$AOP^ACHS(2,1),"bcbseob*",.ACHSUFLS) S ACHSEMSG="M10" D ERROR^ACHSTCK1 G ABEND
 S ACHSI=0
C1 ;
 S ACHSI=$O(ACHSUFLS(ACHSI))
 G C1A:+ACHSI=0
 S ACHSXX=ACHSUFLS(ACHSI),ACHSUFLS(ACHSI)=ACHSUFLS(ACHSI)_"^"
 D TESTEX,^%ZISC
 G C1
 ;
C1A ;
 G FILDEL
 ;
TESTEX ;
 I $$OPEN^%ZISH($$AOP^ACHS(2,1),ACHSXX,"R") S ACHSEMSG="M10" D ERROR^ACHSTCK1 G ABEND
 S ACHSLMT=100,ACHSRCT=0
TESTRD ;
 U IO
 R ACHSXX:1
 Q:'$T
 G EOF:$$STATUS^%ZISH
 I ACHSISAO S ACHSXX=$E(ACHSXX,3,85)
 S ACHSRCT=ACHSRCT+1
 I ACHSRCT>ACHSLMT G EOF
 I $E(ACHSXX,1,2)'="$$" G TESTRD
 S ACHSERDT=$E(ACHSXX,3,8),ACHSERRC=+$E(ACHSXX,10,18)
 S $P(ACHSUFLS(ACHSI),U,3)=$E(ACHSXX,3,8),$P(ACHSUFLS(ACHSI),U,4)=+$E(ACHSXX,10,18),$P(ACHSUFLS(ACHSI),U,5)=+$E(ACHSXX,20,22)
 S $P(ACHSUFLS(ACHSI),U,2)="OK"
 Q
 ;
EOF ;
 S $P(ACHSUFLS(ACHSI),U,2)=""
 Q
 ;
FILDEL ; Delete HFS files.
 S ACHSI=""
FILDELA ;
 S ACHSI=$O(ACHSUFLS(ACHSI))
 G FILDELC:+ACHSI=0
 S ACHSDATE=$P(ACHSUFLS(ACHSI),U,3)
 G FILDELA:$L(ACHSDATE)=0
 S X=ACHSDATE,X=$E(X,5,6)_$E(X,1,4),X=$S($E(X,1,2)>50:2,1:3)_X
 S ACHSDATE=X,ACHSRDAT=9999999-ACHSDATE,ACHSUFLS("C",ACHSRDAT,ACHSI)=""
 G FILDELA
 ;
FILDELC ;
 S (ACHSR,ACHSRR,ACHSDELD,ACHSCNT,ACHSDSAV)=0
FILDELC1 ;
 S ACHSR=$O(ACHSUFLS("C",ACHSR))
 G FILDELF:+ACHSR=0
FILDELC2 ;
 S ACHSRR=$O(ACHSUFLS("C",ACHSR,ACHSRR))
 G FILDELC1:+ACHSRR=0
 S ACHSCNT=ACHSCNT+1
 I ACHSCNT=6 S ACHSDELD=9999999-ACHSR
 G FILDELC2
 ;
FILDELF ;
 G FILDEND:+ACHSDELD=0
 S Y=$$FMTE^XLFDT(ACHSDELD)
 U IO(0)
 S Y=$$DIR^XBDIR("Y","Delete ALL FI EOBR FILES With Process Date BEFORE "_Y,"Y","","","",2)
 I Y=1 G FILDELK
 G FILDEND
 ;
FILDELK ;
 S ACHSR=9999999-ACHSDELD
FILDELK1 ;
 S ACHSR=$O(ACHSUFLS("C",ACHSR))
 G FILDEND:+ACHSR=0
 S ACHSRR="",ACHSRR=$O(ACHSUFLS("C",ACHSR,ACHSRR))
 G FILDELK1:+ACHSRR=0
 I $$AOP^ACHS(2,9)<10,$P(ACHSUFLS(ACHSRR),U,5)>990 G FILDELK2
 G FILDELK1:$P(ACHSUFLS(ACHSRR),U,5)'<$$AOP^ACHS(2,9)
FILDELK2 ;
 G FILDELK1:$P(ACHSUFLS(ACHSRR),U,5)=$$AOP^ACHS(2,9)
FILDELK3 ;
 S ACHSZFN=$P(ACHSUFLS(ACHSRR),U,1)
 I '$$DEL^%ZISH($$AOP^ACHS(2,1),ACHSZFN) U IO(0) W !!?10,ACHSZFN,"  has been DELETED" K ACHSUFLS("C",ACHSR,ACHSRR),ACHSUFLS(ACHSRR)
 G FILDELK1
 ;
FILDEND ;
 Q
 ;
ABEND ;
 U IO(0)
 W *7,!!,$$C^XBFUNC("JOB ENDED WITH ERROR(S) - NOTIFY SUPERVISOR"),!
 D CLOSEALL^ACHS
 I $$DIR^XBDIR("E","Enter <RETURN> to CONTINUE")
 Q
 ;
NODIR ;
 U IO(0)
 W *7,!,$$C^XBFUNC("Your EOBR IMPORT DIRECTORY is not defined in your")
 W !,$$C^XBFUNC("CHS AREA OFFICE PARAMETERS file.")
 W !,$$C^XBFUNC("The directory is usually"),!,$$C^XBFUNC("/usr/ihs/reports/"),!,$$C^XBFUNC("for unix systems, and"),!,$$C^XBFUNC("C:\IMPORT\"),!,$$C^XBFUNC("for DOS systems.")
 G ABEND
 ;
