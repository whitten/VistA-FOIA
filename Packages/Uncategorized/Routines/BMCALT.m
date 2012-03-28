BMCALT ; IHS/PHXAO/TMJ - LIST ALTERNATE RESOURCES ;
 ;;4.0;REFERRED CARE INFO SYSTEM;;JAN 09, 2006
 ;
 ; This routine lists alternate resources for the selected patient
 ;
START ;
 F  D MAIN Q:BMCQ  D HDR^BMC
 D EOJ
 Q
 ;
MAIN ;
 S BMCQ=0
 D PATIENT ;              get patient being referred
 Q:BMCQ
 D LIST ;                 list alternate resourece
 Q
 ;
PATIENT ; GET PATIENT
 F  D PATIENT2 I BMCQ!($G(BMCDFN)) Q
 Q
 ;
PATIENT2 ; ASK FOR PATIENT UNTIL USER SELECTS OR QUITS
 S BMCQ=1
 S DIC="^AUPNPAT(",DIC(0)="AEMQ" D DIC^BMCFMC
 Q:Y<1
 S BMCDFN=+Y,BMCREC("PAT NAME")=$P(^DPT(+Y,0),U)
 S BMCQ=0
 I $$DOD^AUPNPAT(BMCDFN) D  I 'Y K BMCDFN,BMCREC("PAT NAME") Q
 . W !!,"This patient is deceased."
 . S DIR(0)="YO",DIR("A")="Are you sure you want this patient",DIR("B")="NO" K DA D ^DIR K DIR
 . W !
 . Q
 Q
 ;
LIST ; LIST ALTERNATE RESOURCES
 Q:'$G(BMCDFN)
 S BMCRDATE=DT
 NEW BMCMSG,BMCI,BMCX
 S BMCI=1
 S BMCX=$$BEN^AUPNPAT(BMCDFN,"E")
 S:BMCX="" BMCX="UNKNOWN"
 S BMCMSG(BMCI)="CLASSIFICATION/BENEFICIARY IS: "_BMCX,BMCI=+BMCI+1
 S BMCX=$$ELIGSTAT^AUPNPAT(BMCDFN,"E")
 S:BMCX="" BMCX="UNKNOWN"
 S BMCMSG(BMCI)="ELIGIBILITY STATUS IS: "_BMCX,BMCI=+BMCI+1
 NEW BMCELG
 S BMCELG=BMCI
 I $$MCR^AUPNPAT(BMCDFN,BMCRDATE) S BMCMSG(BMCI)="PATIENT HAS MEDICARE",BMCI=BMCI+1
 ;I $$MCD^AUPNPAT(BMCDFN,BMCRDATE) S BMCMSG(BMCI)="PATIENT HAS MEDICAID--",BMCI=BMCI+1
 S BMCX=$$MCDPN^AUPNPAT(BMCDFN,BMCRDATE,"E")
 S:BMCX="" BMCX="UNKNOWN"
 I $$MCD^AUPNPAT(BMCDFN,BMCRDATE) S BMCMSG(BMCI)="PATIENT HAS MEDICAID-PLAN NAME:  "_BMCX,BMCI=+BMCI+1
 ;I $$PI^AUPNPAT(BMCDFN,BMCRDATE) S BMCMSG(BMCI)="PATIENT HAS PRIVATE INSURANCE--",BMCI=BMCI+1
 S BMCX=$$PIN^AUPNPAT(BMCDFN,BMCRDATE,"E")
 S:BMCX="" BMCX="UNKNOWN"
 I $$PI^AUPNPAT(BMCDFN,BMCRDATE) S BMCMSG(BMCI)="PATIENT HAS INSURANCE-INSURER:  "_BMCX,BMCI=BMCI+1
 I BMCELG=BMCI S BMCMSG(BMCI)="NO THIRD PARTY COVERAGE RECORDED",BMCI=BMCI+1
 I $D(^AUPNPAT(BMCDFN,13)) D
 .S BMCMSG(BMCI)="",BMCI=BMCI+1,BMCMSG(BMCI)="ADDITIONAL REGISTRATION INFORMATION:",BMCI=BMCI+1
 .K BMCAR D ENP^XBDIQ1(9000001,BMCDFN,1301,"BMCAR(","E")
 .S I=0 F  S I=$O(BMCAR(1301,I)) Q:I'=+I  S BMCMSG(BMCI)=BMCAR(1301,I),BMCI=BMCI+1
 W:BMCI !!
 S BMCI=0
 F  S BMCI=$O(BMCMSG(BMCI)) Q:'BMCI  W BMCMSG(BMCI),!
 D PAUSE^BMC
 Q
 ;
EOJ ; END OF JOB
 D ^BMCKILL
 Q
