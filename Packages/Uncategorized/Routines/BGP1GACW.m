BGP1GACW ; IHS/CMI/LAB - AREA NATIONAL GPRA REPORT 01 Jul 2010 7:57 PM ;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;;JUN 27, 2011;Build 33
 ;
 ;
TESTCHW ;
 S ERR=""
 S BGPSUL(1)=""
 S BGPSUL(2)=""
 F X=1:1:11 S BGPSUL(X)=""
 D EP(.ERR,1,2522,"BGP 11 AREA GPRA","A",.BGPSUL,$$NOW^XLFDT)
 W !,ERR
 Q
EP(BGPRET,BGPUSER,BGPDUZ2,BGPOPTN,BGPRPTT,BGPSUL,BGPRTIME,BGPONEF,BGPFILE) ;EP - called from GUI to produce national gpra report (AO-AGP)
 ;SEE ROUTINE BGP1DAR for more info
 ;  BGPUSER - DUZ
 ;  BGPDUZ2 - DUZ(2)
 ;  BGPOPTN - OPTION NAME
 ;  BGPRPTT - A or F depending on whether site wants area or facility report, either way you need to display the entries to the user
 ;            from BGP 11 DATA CURRENT that match the following:
 ;            (SEE BGP1ASL or CALL ME)
 ;  BGPSUL - ARRAY OF IENS FROM THE D GET^BGP1ASL(.BGPSUL,.BGPFILE,1,....... call that the user selected
 ;  BGPROT - type of output  P for printed, D For Delimited, B for both
 ;  BGPRTIME - report will be queued automatically, this variable
 ;             contains the time it will run, internal fileman format
 ;             must be date and time
 ;
 ;create entry in gui output file
 ;queue report to run with/GUIR
 D EP1
 S Y=BGPRET
 ;D EN^XBVK("BGP") S:$D(ZTQUEUED) ZTREQ="@"
 S BGPRET=Y
 Q
EP1 ;
 S U="^"
 I $G(BGPUSER)="" S BGPRET=0_"^USER NOT PASSED" Q
 I $G(BGPDUZ2)="" S BGPRET=0_"^DUZ(2) NOT PASSED" Q
 I $G(BGPOPTN)="" S BGPRET=0_"^OPTION NAME NOT PASSED" Q
 I $G(BGPRPTT)="" S BGPRET=0_"^AREA OR FACILITY VARIABLE NOT PASSED" Q
 I "AF"'[BGPRPTT S BGPRET=0_"^AREA OR FACILTY VARIABLE INVALID" Q
 I '$D(BGPSUL) S BGPRET=0_"^FACILITY ARRAY NOT PASSED" Q
 S X=0,C=0 F  S X=$O(BGPSUL(X)) Q:X'=+X  S C=C+1
 I 0 S BGPRET=0_"^FACILITY ARRAY NOT PASSED" Q
 S BGPSUCNT=0
 S BGPSUCNT=C
 S BGPRTIME=$G(BGPRTIME)
 ;S DUZ=BGPUSER
 S DUZ(2)=BGPDUZ2
 S:'$D(DT) DT=$$DT^XLFDT
 D ^XBKVAR
 S BGPGUI=1
 S IOM=80,BGPIOSL=55
 S BGPAREAA=1
 S BGPEXCEL=$G(BGPEXCEL)
 S BGPRTYPE=1,BGPBEN=1,BGP1RPTH="",BGPCHWE=1
 S X=$O(^BGPCTRL("B",2011,0))
 S Y=^BGPCTRL(X,0)
 S BGPBD=$P(Y,U,8),BGPED=$P(Y,U,9)
 S BGPPBD=$P(Y,U,10),BGPPED=$P(Y,U,11)
 S BGPBBD=$P(Y,U,12),BGPBED=$P(Y,U,13)
 S BGPPER=$P(Y,U,14),BGPQTR=3
 G NT
 S BGPBD=3030101,BGPED=3031231
 S BGPBBD=3000101,BGPBED=3001231
 S BGPPBD=3020111,BGPPED=3021231
 S BGPPER=3030000,BGPQTR=3,BGPBEN=1
NT ;
 S BGPUF=$$GETDIR^BGP1UTL2()
 ;I ^%ZOSF("OS")["PC"!(^%ZOSF("OS")["NT")!($P($G(^AUTTSITE(1,0)),U,21)=2) S BGPUF=$S($P($G(^AUTTSITE(1,1)),U,2)]"":$P(^AUTTSITE(1,1),U,2),1:"C:\EXPORT")
 ;I $P(^AUTTSITE(1,0),U,21)=1 S BGPUF="/usr/spool/uucppublic/"
 S BGPASUF=$P(^AUTTLOC(DUZ(2),0),U,10)
 S BGPNOW=$$NOW^XLFDT() S BGPNOW=$$NOW^XLFDT() S BGPNOW=$P(BGPNOW,".")_"."_$$RZERO^BGP1UTL($P(BGPNOW,".",2),6)
 S BGPC=0,X=0 F  S X=$O(BGPSUL(X)) Q:X'=+X  S BGPC=BGPC+1
 S BGPFN="CRSHW"_$P(^AUTTLOC(DUZ(2),0),U,10)_$$D^BGP1DCHA(BGPBBD)_$$D^BGP1DCHA(BGPED)_BGPNOW_".TXT"
 S BGPASUF=$P(^AUTTLOC(DUZ(2),0),U,10)
 S BGPDELT=""
 ;create entry in GUI file
 D ^XBFMK
 ;S X=BGPUSER_$$NOW^XLFDT
 S X=BGPFILE
 S DIC="^BGPGUIB(",DIC(0)="L",DIADD=1,DLAYGO=90546.08,DIC("DR")=".02////"_BGPUSER_";.03////"_$S(BGPRTIME]"":BGPRTIME,1:$$NOW^XLFDT)_";.05///"_BGPOPTN_";.06///R;.07///"_$G(BGPROT)_";1///"_$G(BGPFN)
 K DD,D0,DO D FILE^DICN K DLAYGO,DIADD,DD,D0,DO
 I Y=-1 S BGPRET=0_"^UNABLE TO CREATE ENTRY IN GUI OUTPUT FILE" Q
 S BGPGIEN=+Y
 ;SEND THE REPORT PROCESS OFF TO THE BACKGROUND USING TASKMAN CALL
 K ^TMP($J,"BGPGUI")
 ;D GS^BGP1DCHA
 D TSKMN
 ;S BGPRET=BGPGIEN
 ;S BGPRET=1
 S X=0 F  S X=$O(BGPFNX(X)) Q:X'=+X  S BGPRET=BGPRET_BGPFNX(X)_"-"
 S $P(BGPRET,"~",3)=BGPRET_"- in directory "_BGPUF
 K ^TMP($J) D ^XBFMK
 ;D XIT
 Q
 ;
TSKMN ;
 ;S ZTIO=""
 ;K ZTSAVE S ZTSAVE("*")=""
 ;S ZTCPU=$G(IOCPU),ZTRTN="AOCHW^BGP1GACW",ZTDTH=$S(BGPRTIME]"":BGPRTIME,1:$$NOW^XLFDT),ZTDESC="GUI CHILDHOOD HT/WT EXPORT" D ^%ZTLOAD Q
 D AOCHW
 Q
AOCHW ;
 K ^TMP($J,"BGPGUI")
 D GS^BGP1DCHA
 D ENDLOG
 ;D XIT
 Q
 ;
XIT ;
 NEW BGPRET
 K ^TMP($J)
 D EN^XBVK("BGP") S:$D(ZTQUEUED) ZTREQ="@"
 K DIRUT,DUOUT,DIR,DOD
 K DIADD,DLAYGO
 D KILL^AUPNPAT
 K X,X1,X2,X3,X4,X5,X6
 K A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,V,W,X,Y,Z
 K N,N1,N2,N3,N4,N5,N6
 K BD,ED
 D KILL^AUPNPAT
 D ^XBFMK
 L -^BGPDATA
 Q
 ;
ENDLOG ;-- UPDATE LOG AT END
 S DIE="^BGPGUIB(",DA=BGPGIEN,DR=".04////"_$$NOW^XLFDT_";.06///C"
 D ^DIE
 K DIE,DR,DA
 Q
