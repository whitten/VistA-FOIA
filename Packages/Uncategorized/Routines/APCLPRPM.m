APCLPRPM ; IHS/CMI/LAB - driver for primary care provider report ;
 ;;2.0;IHS PCC SUITE;**2**;MAY 14, 2009
 ;
 W:$D(IOF) @IOF
 W !,"This report will generate a list of patients for a specific Designated Primary Care"
 W !,"Provider or a list of patients for all Primary Care Providers at this facility."
 I '$G(DUZ(2)) W !!!,$C(7),$C(7),"SITE NOT SET IN YOUR USER PROFILE!  Please notify your Site Manager!" Q
ASK ;
 S APCLPROV=""
 S DIR(0)="S^1:ONE PROVIDER;2:ALL PROVIDERS",DIR("A")="Run the report for",DIR("B")=1 D ^DIR K DIR S:$D(DUOUT) DIRUT=1
 G:$D(DIRUT) EOJ
 G:Y=2 DIP
PROV ;
 ;
 S DIC=$S($P(^DD(9000010.06,.01,0),U,2)[200:200,1:6),DIC("A")="Enter PROVIDER: ",DIC(0)="AEMQ" D ^DIC K DIC
 I Y=-1 G ASK
 S APCLPROV=+Y
 S APCLPRV=$S($P(^DD(9000010.06,.01,0),U,2)[200:$P(^VA(200,+Y,0),U),1:$P(^DIC(16,+Y,0),U))
DIP ;
 S FLDS="[APCL PRIM PROV LISTING]",BY=$S(APCLPROV="":"#.14",1:"@INTERNAL(#.14)"),DIC="^AUPNPAT(",L=0 I APCLPROV S DHD="[APCL PRIM PROV HEADING]"
 S FR=$S(APCLPROV="":"",1:APCLPROV),TO=$S(APCLPROV="":"",1:APCLPROV)
 K DHIT,DIOEND,DIOBEG
 D EN1^DIP
DONE ;
 S DIR(0)="EO",DIR("A")="End of report.  Hit return" D ^DIR K DIR S:$D(DUOUT) DIRUT=1 I $D(IOF) W @IOF
EOJ ;clean up
 K DIRUT,DUOUT,X,Y,DIR,FLDS,DIP,BY,TO,FR,DIC,DHD
 K APCLPROV,APCLPRV,APCLDT,APCLAST,APCLVDFN,AUPNPAT
 Q
LVST ;ENTRY POINT from [APCL PRIM PROV LISTING print template
 S AUPNPAT=D0
 S APCLAST=""
 S APCLVDFN=""
 S APCLAST=$O(^AUPNVSIT("AA",AUPNPAT,""))
 I APCLAST="" S APCLAST="NONE FOUND" Q
 S APCLVDFN=$O(^AUPNVSIT("AA",AUPNPAT,APCLAST,""))
 S Y=$P(^AUPNVSIT(APCLVDFN,0),U)
 D DD^%DT S APCLDT=$E(Y,1,12)
 Q
