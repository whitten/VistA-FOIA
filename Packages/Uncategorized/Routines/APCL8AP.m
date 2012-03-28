APCL8AP ; IHS/CMI/LAB - print apc report 1A ;
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
START ;
 S APCL80="-----------------------------------------------------------------------------"
 S Y=$E(APCLFYE,1,3)_"0000" D DD^%DT S APCLFYD=Y S Y=DT D DD^%DT S APCLDT=Y
 S APCLLOCC=$P(^AUTTLOC(APCLLOC,0),U,10),APCLLOCP=$P(^DIC(4,APCLLOC,0),U)
 S APCLAREA=$P(^AUTTLOC(APCLLOC,0),U,4) I APCLAREA="" S (APCLAREA,APCLAREC)="???" G SU
 I '$D(^AUTTAREA(APCLAREA,0)) S (APCLAREA,APCLAREC)="???" G SU
 S APCLAREC=$P(^AUTTAREA(APCLAREA,0),U,2),APCLAREA=$P(^AUTTAREA(APCLAREA,0),U)
SU ;
 S APCLSU=$P(^AUTTLOC(APCLLOC,0),U,5) I APCLSU="" S (APCLSU,APCLSUC)="???" G START2
 I '$D(^AUTTSU(APCLSU,0)) S (APCLSU,APCLSUC)="999" G START2
 S APCLSUC=$P(^AUTTSU(APCLSU,0),U,3),APCLSU=$P(^AUTTSU(APCLSU,0),U)
START2 S (APCLPG,APCLVDFN,APCLPRIT)=0 D HEAD
 K APCLQUIT
 F  S APCLVDFN=$O(^XTMP("APCL8A",APCLJOB,APCLBT,"VISITS",APCLVDFN)) Q:APCLVDFN=""!($D(APCLQUIT))  D P
 ;I $D(^XTMP("APCL8A",APCLJOB,APCLBT,"NO EXPORT")) D  I 1
 W !
DONE D DONE^APCLOSUT
 K ^XTMP("APCL8A",APCLJOB,APCLBT)
 Q
FIRSTPG ;
 W !,"Total Number of APC visits counted:  ",APCLGRAN
 W !,"Total Number of those APC Visits NOT Exported:  ",$S($D(^XTMP("APCL8A",APCLJOB,APCLBT,"NO EXPORT")):^("NO EXPORT"),1:0)
 W !!,"Of the total number of visits counted in the 1A, but NOT exported to the",!,"National Data Warehouse, ",$S($D(^XTMP("APCL8A",APCLJOB,APCLBT,"IN XREF")):^("IN XREF"),1:0)
 W " were not exported because they were posted ",!,"or modified after the last NDW export tape was generated.",!
 I '$D(^XTMP("APCL8A",APCLJOB,APCLBT,"VISITS")) S APCLQUIT=1 Q
 W !,"The remaining ",($S($D(^XTMP("APCL8A",APCLJOB,APCLBT,"NO EXPORT")):^("NO EXPORT"),1:0)-$S($D(^XTMP("APCL8A",APCLJOB,APCLBT,"IN XREF")):^("IN XREF"),1:0))," visits are listed below.",!
 Q
P ;
 ;S DA=APCLVDFN,DIE="^AUPNVSIT(",DR=".13///^S X=DT" D ^DIE D ^XBFMK
 I $Y>(IOSL-5) D HEAD Q:$D(APCLQUIT)
 S APCLVREC=^AUPNVSIT(APCLVDFN,0),APCLVLOC=$P(APCLVREC,U,6),APCLTYPE=$P(APCLVREC,U,3),APCLSC=$P(APCLVREC,U,7)
 S DFN=$P(APCLVREC,U,5),APCLHRN="" S:$D(^AUPNPAT(DFN,41,APCLVLOC,0)) APCLHRN=$P(^AUPNPAT(DFN,41,APCLVLOC,0),U,2)
 S:APCLHRN="" APCLHRN=$P($G(^AUPNPAT(DFN,41,DUZ(2),0)),U,2)
CLINIC ;
 S APCLCLN=$P(^AUPNVSIT(APCLVDFN,0),U,8) I APCLCLN="" S APCLCLN="--" G VD
 S APCLCLN=$P(^DIC(40.7,APCLCLN,0),U,2)
VD ;
 S Y=+APCLVREC X ^DD("DD") S APCLRD=Y
PRN ;
 W !,APCLHRN,?8,APCLRD,?28,$E($P(^DIC(4,APCLVLOC,0),U),1,11),?40,APCLTYPE,?44,$E(APCLSC,1,15),?47,$E(APCLCLN,1,10),?52,^XTMP("APCL8A",APCLJOB,APCLBT,"VISITS",APCLVDFN)
 Q
 ;
HEAD I 'APCLPG G HEAD1
 I $E(IOST)="C",IO=IO(0) W ! S DIR(0)="EO" D ^DIR K DIR I Y=0!(Y="^")!($D(DTOUT)) S APCLQUIT="" Q
HEAD1 ;
 W:$D(IOF) @IOF S APCLPG=APCLPG+1
 W APCL80
 W !,"AREA:  ",APCLAREC,"      ",APCLAREA,?47,"PCC-APC REPORT 1A",?68,"Page ",APCLPG
 W !,"S.U.:  ",APCLSUC,"      ",APCLSU,?47,"FISCAL YEAR ",APCLFYD
 W !,"FAC.:  ",APCLLOCC,"  ",APCLLOCP,?49,APCLDT
 W !?46,"VISITS NOT EXPORTED",!
 W APCL80,!
 I APCLPG=1 D FIRSTPG
 Q:$D(APCLQUIT)
 W APCL80,!
 W " HRN ",?8,"VISIT DATE/TIME",?28,"LOCATION",?39,"TYPE",?44,"SC",?45," CLIN",?55,"REASON",!,APCL80,!
 Q
 ;
