APCDCHKJ ; IHS/CMI/LAB - I-LINKER ;
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
 ;IHS/CMI/LAB - patch 4 added check for MFI created visits
 ;
EN ;PEP - can be called by Billing, etc.
INIT ;
 S:$D(ZTQUEUED) ZTREQ="@"
 K ^XTMP("APCDCHKJ",$J)
 S U="^",APCDIDFN=""
DRIVER F  S APCDIDFN=$O(^AUPNVSIT("AI",APCDIDFN)) Q:APCDIDFN'=+APCDIDFN  D PROCESS
 D GENMSG
 D EOJ
 Q
 ;
PROCESS ; Process each "In hospital" record
 ; Set variables
 I '$D(^AUPNVSIT(APCDIDFN,0)) K ^AUPNVSIT("AI",APCDIDFN) Q
 S APCDIVR=^AUPNVSIT(APCDIDFN,0)
 I $P(APCDIVR,U,11) K ^AUPNVSIT("AI",APCDIDFN) Q
 I $P(APCDIVR,U,23)=.5 K ^AUPNVSIT("AI",APCDIDFN) Q
 I $P($G(^AUPNVSIT(APCDIDFN,11)),U,13) K ^AUPNVSIT("AI",APCDIDFN) Q
 I $P(APCDIVR,U,12) K ^AUPNVSIT("AI",APCDIDFN) Q
 S APCDIDAT=+$P(APCDIVR,U),DFN=$P(APCDIVR,U,5),APCDITYP=$P(APCDIVR,U,3),APCDILOC=$P(APCDIVR,U,6),(APCDFND,APCDOLD,APCDHDFN)=0,APCDVD=$P(APCDIDAT,".") K APCDHOSP
 ; Check for hospitalization prior to (or on same day) as the "I" visit
 S APCDVDH=(9999999-APCDVD),APCDSVD=(APCDVDH-1)_".9999",APCDHDFN=""
 F  S APCDSVD=$O(^AUPNVSIT("AAH",DFN,APCDSVD)) Q:APCDSVD'=+APCDSVD!($P(APCDSVD,".")<APCDVDH)  D PROC2
 I APCDFND>1 S APCDHDFN=0 F APCDL=0:0 S APCDHDFN=$O(APCDHOSP(APCDHDFN)) Q:APCDHDFN'=+APCDHDFN  S ^XTMP("APCDCHKJ",$J,"TWOHITS",APCDIDFN,APCDHDFN)=""
 I 'APCDFND D CHKYR
 ;
 I APCDFND=1 S APCDHDFN=$O(APCDHOSP("")),DIE="^AUPNVSIT(",DA=APCDIDFN,DR=".12///`"_APCDHDFN D
 .D ^DIE K DA,DR,DIE
 .S ^XTMP("APCDCHKJ",$J,"HIT",APCDHDFN,APCDIDFN)=APCDDCD
 .D CHKPROC
 .S AUPNVSIT=APCDHDFN D MOD^AUPNVSIT
 . ; -- IHS/DAOU/EJN for HL7
 . Q:'$G(APCDIDFN)
 . I $T(A08^BTSEVENT)]"" S APCDHLER=$$A08^BTSEVENT(APCDIDFN) K APCDHLER
 . ; -- END HL7 mods
 I 'APCDFND,'APCDOLD S ^XTMP("APCDCHKJ",$J,"NOHIT",APCDIDFN)=""
 Q
 ;
PROC2 ;
 S APCDHDFN=0 F  S APCDHDFN=$O(^AUPNVSIT("AAH",DFN,APCDSVD,APCDHDFN)) Q:APCDHDFN'=+APCDHDFN  I APCDHDFN]"",$D(^AUPNVSIT(APCDHDFN,0)),'$P(^(0),U,11),$P(^(0),U,9) D CHECK
 Q
CHECK ;
 S APCDHVR=^AUPNVSIT(APCDHDFN,0)
 S APCDHDAT=+$P(APCDHVR,U),APCDHTYP=$P(APCDHVR,U,3),APCDHLOC=$P(APCDHVR,U,6)
CHKHOSP ; Check corresponding V Hospitalization for discharge date
 S APCDINPD="",APCDINPD=$S(APCDITYP="C":$O(^AUPNVCHS("AD",APCDHDFN,"")),1:$O(^AUPNVINP("AD",APCDHDFN,"")))
 Q:APCDINPD=""
 S:APCDITYP="C" APCDDCD=$P(^AUPNVCHS(APCDINPD,0),U,7)
 S:APCDITYP'="C" APCDDCD=$P(^AUPNVINP(APCDINPD,0),U)
 I APCDDCD'<APCDVD S APCDFND=APCDFND+1,APCDHOSP(APCDHDFN)=""
 Q
CHKYR ;
 S %=DT-APCDVD I %>10000 I 'APCDFND S ^XTMP("APCDCHKJ",$J,"ONEYR",APCDIDFN)="",APCDOLD=1 K ^AUPNVSIT("AI",APCDIDFN)
 Q
CHKPROC ;
 S APCDVPRC=0 F  S APCDVPRC=$O(^AUPNVPRC("AD",APCDHDFN,APCDVPRC)) Q:APCDVPRC'=+APCDVPRC  I $D(^AUPNVPRC(APCDVPRC,0)) S APCDHPRC($P(^AUPNVPRC(APCDVPRC,0),U))=APCDVPRC
 S APCDVPRC=0 F  S APCDVPRC=$O(^AUPNVPRC("AD",APCDIDFN,APCDVPRC)) Q:APCDVPRC'=+APCDVPRC  I $D(^AUPNVPRC(APCDVPRC,0)) S APCDICDP=$P(^AUPNVPRC(APCDVPRC,0),U) I $D(APCDHPRC(APCDICDP)) D DELPRC
 Q
DELPRC ;
 I $P(^AUPNVPRC(APCDHPRC(APCDICDP),0),U,4)'=$P(^AUPNVPRC(APCDVPRC,0),U,4) Q
 ;quit if provider narratives are NOT the same - per Diana 7/12/90
 S ^XTMP("APCDCHKJ",$J,"PROC ERROR",APCDVPRC)=^AUPNVPRC(APCDVPRC,0)
 S DA=APCDVPRC,DIE="^AUPNVPRC(",DR=".01///@" D ^DIE K DA,DR,DIE
 I $D(Y) K ^XTMP("APCDCHKJ",$J,"PROC ERROR",APCDVPRC) Q
 Q
GENMSG ;
 K ^XTMP("APCDCHKJ",$J,"MESSAGE")
 Q:$P($G(^APCDSITE(DUZ(2),0)),U,23)=""  ;no one to send message to
 I '$D(^XTMP("APCDCHKJ",$J,"TWOHITS"))&('$D(^XTMP("APCDCHKJ",$J,"PROC ERROR"))) Q
 I $D(^XTMP("APCDCHKJ",$J,"TWOHITS")) D
 .S X=1,^XTMP("APCDCHKJ",$J,"MESSAGE",X,0)="The following In-Hospital/Daily Hospitalization Visits could be linked"
 .S X=2,^XTMP("APCDCHKJ",$J,"MESSAGE",X,0)="to two or more Hospitalizations.  They must be linked manually."
 .S Y=0 F  S Y=$O(^XTMP("APCDCHKJ",$J,"TWOHITS",Y)) Q:Y'=+Y  D
 ..S X=X+1,^XTMP("APCDCHKJ",$J,"MESSAGE",X,0)="IN-HOSP:  "_$$VAL^XBDIQ1(9000010,Y,.01)_"  "_$$VAL^XBDIQ1(9000010,Y,.05)
 ..S X=X+1,^XTMP("APCDCHKJ",$J,"MESSAGE",X,0)="          TYPE: "_$$VAL^XBDIQ1(9000010,Y,.03)_"  LOCATION: "_$$VAL^XBDIQ1(9000010,Y,.06)
 ..S X=X+1,^XTMP("APCDCHKJ",$J,"MESSAGE",X,0)="HOSPITALIZATIONS:"
 ..S Z=0 F  S Z=$O(^XTMP("APCDCHKJ",$J,"TWOHITS",Y,Z)) Q:Z'=+Z  D
 ...S X=X+1,^XTMP("APCDCHKJ",$J,"MESSAGE",X,0)="          DATE: "_$$VAL^XBDIQ1(9000010,Z,.01)_"   NAME: "_$$VAL^XBDIQ1(9000010,Z,.05)
 ...I $$VALI^XBDIQ1(9000010,Z,.03)="C" S APCDINPD=$O(^AUPNVCHS("AD",Z,0)) I APCDINPD]"" S APCDDCD=$P(^AUPNVCHS(APCDINPD,0),U,7)
 ...I $$VALI^XBDIQ1(9000010,Z,.03)'="C" S APCDINPD=$O(^AUPNVINP("AD",Z,0)) I APCDINPD]"" S APCDDCD=$P(^AUPNVINP(APCDINPD,0),U)
 ...S X=X+1,^XTMP("APCDCHKJ",$J,"MESSAGE",X,0)="          LOCATION: "_$$VAL^XBDIQ1(9000010,Z,.06)_"  TYPE: "_$$VAL^XBDIQ1(9000010,Z,.03)_"  D/C: "_$$FMTE^XLFDT(APCDDCD,"1P")
 ...Q
 ..Q
 .Q
 ;SEND MESSAGE
 S XMDUZ=.5
 S XMTEXT="^XTMP(""APCDCHKJ"",$J,""MESSAGE"","
 S XMSUB="IN-HOSPITAL LINK REPORT - ERRORS"
 S X=$$VAL^XBDIQ1(9001001.2,DUZ(2),.23)
 S XMY(X)=""
 D ^XMD K XMY
 Q
 ;
EOJ ; Clean up and XIT
 K ^XTMP("APCDCHKJ",$J)
 K APCDIDFN,APCDFND,APCDOLD,APCDHDFN,DFN,APCDINPD,APCDVD,APCDIDAT,APCDSVD,APCDVDH,APCDDCD,APCDDT,APCDHDFN,APCDHOSP,APCDHV,APCDHDAT,APCDHLOC,APCDHTYP,APCDHVR,APCDILOC,APCDIVR,APCDTYPE,APCDITYP,IO("Q"),APCDPG,APCDT
 K APCDHPRC,APCDICDP,APCDL,APCDVPRC
 Q
