APCPSRP4 ; IHS/TUCSON/LAB - PRINT . SECTION AUGUST 14, 1992 ; [ 08/17/03  8:20 PM ]
 ;;2.0;IHS PCC DATA EXTRACTION SYSTEM;**1,6**;APR 03, 1998
 ;IHS/CMI/LAB - XTMP
 ;
 I $Y>(IOSL-10) D HEAD^APCPSRP Q:$D(APCPSR("QUIT"))
 I '$P(^APCPLOG(APCPSR("LOG"),0),U,14) W !,"There were NO Community Health Nursing Activity (CHA) transactions generated.",!! G ERROR
VD ;
 ;
 S APCPSR("PTR")=0,APCPSR("T")="By Visit Date:",APCPSR("1")="V DATE CHA",APCPSR("2")="V DATE CHA",APCPSR("WC")=0
 D PROC Q:$D(APCPSR("QUIT"))
TYPE ;
 S APCPSR("PTR")=0,APCPSR("T")="By Type:",APCPSR("1")="TYPE CHA",APCPSR("2")="TYPE CHA CC",APCPSR("WC")=0
 D PROC Q:$D(APCPSR("QUIT"))
LOC ;
 S APCPSR("PTR")=1,APCPSR("T")="By Location:",APCPSR("1")="LOC CHA",APCPSR("2")="LOC CHA CC",APCPSR("WC")=0,APCPSR("GLOBAL")="^DIC(4,",APCPSR("PIECE")=1
 D PROC Q:$D(APCPSR("QUIT"))
SC ;
 S APCPSR("PTR")=0,APCPSR("T")="By Service Category:",APCPSR("1")="SC CHA",APCPSR("2")="SC CHA CC",APCPSR("WC")=0
 D PROC Q:$D(APCPSR("QUIT"))
CLINIC ;
 S APCPSR("PTR")=0,APCPSR("T")="By Clinic Type:",APCPSR("1")="CLINIC CHA",APCPSR("2")="CLINIC CHA CC",APCPSR("WC")=0
 D PROC Q:$D(APCPSR("QUIT"))
PROVDISC ;
 S APCPSR("PTR")=0,APCPSR("T")="By Provider Type (Primary Provider only):",APCPSR("1")="PROV CHA",APCPSR("2")="PROV CHA CC",APCPSR("WC")=0
 D PROC Q:$D(APCPSR("QUIT"))
ERROR ;
 G:'$D(^XTMP("APCPSR",APCPJOB,APCPBTH,"SKIPPED","CHAACTERR")) EOJ
 W !!,"There were ",^XTMP("APCPSR",APCPJOB,APCPBTH,"SKIPPED","CHAACTERR")," visits on which a Community Health Nurse was a provider.",!,"These visits DID NOT have an ACTIVITY TIME recorded, therefore they DID NOT"
 W !,"generate a transaction record to be passed to the CHA System.",!,"This should be reviewed.",!
EOJ ;
 Q
PROC ;
 I $Y>(IOSL-9) D HEAD^APCPSRP Q:$D(APCPSR("QUIT"))
 W !!?10,APCPSR("T")
 S APCPSR("N")=0 F  S APCPSR("N")=$O(^XTMP("APCPSR",APCPJOB,APCPBTH,"GEN",APCPSR("2"),APCPSR("N"))) Q:APCPSR("N")=""!($D(APCPSR("QUIT")))  D PROC1
 Q
PROC1 ;
 I APCPSR("2")["DATE" D PRNT Q
 S APCPSR("D")=0 F  S APCPSR("D")=$O(^XTMP("APCPSR",APCPJOB,APCPBTH,"GEN",APCPSR("2"),APCPSR("N"),APCPSR("D"))) Q:APCPSR("D")=""  D PRNT
 Q
PRNTDATE ;
 S Y=APCPSR("N") D DD^%DT W !?13,Y,?45,$J(^XTMP("APCPSR",APCPJOB,APCPBTH,"GEN",APCPSR("2"),APCPSR("N")),7) S APCPSR("WC")=APCPSR("WC")+1
 Q
PRNT ;
 I $Y>(IOSL-5) D HEAD^APCPSRP Q:$D(APCPSR("QUIT"))  W !!?10,APCPSR("T") W:APCPSR("WC")>0 " (cont.)"
 I APCPSR("1")="V DATE CHA" D PRNTDATE Q
 S X=^XTMP("APCPSR",APCPJOB,APCPBTH,"GEN",APCPSR("2"),APCPSR("N"),APCPSR("D"))
 I APCPSR("PTR")=1 D PRNTPTR Q
 W !?13,APCPSR("D"),?45,$J(X,7) S APCPSR("WC")=APCPSR("WC")+1
 Q
PRNTPTR ;
 S G=APCPSR("GLOBAL")_APCPSR("D")_")"
 W !?13,$P(@G@(0),U,APCPSR("PIECE")),?45,$J(X,7) S APCPSR("WC")=APCPSR("WC")+1
 I APCPSR("1")="LOC" W ?55,"(IHS CODE: ",$P(^AUTTLOC(APCPSR("D"),0),U,10),")"
 K G
 Q
