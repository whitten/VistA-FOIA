BUD8RPL3 ; IHS/CMI/LAB - UDS print lists ;
 ;;5.0;IHS/RPMS UNIFORM DATA SYSTEM;;JAN 18, 2011;Build 12
 ;
T53 ;EP
 S BUDP=0
 D T53H
 S BUD8L=35,BUD8L2=0,BUDY=$O(^BUDGTFIV("B",BUD8L,0)),BUDY=$P(^BUDGTFIV(BUDY,0),U,2)_" "_$P(^BUDGTFIV(BUDY,0),U,3)_" "_$P(^BUDGTFIV(BUDY,0),U,4)
 S BUDCOM="" F  S BUDCOM=$O(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM)) Q:BUDCOM=""!(BUDQUIT)  D
 .S BUDAGE="" F  S BUDAGE=$O(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE)) Q:BUDAGE=""!(BUDQUIT)  D
 ..S BUDSEX="" F  S BUDSEX=$O(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX)) Q:BUDSEX=""!(BUDQUIT)  D
 ...S DFN=0 F  S DFN=$O(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN)) Q:DFN'=+DFN!(BUDQUIT)  D T5W
 ...Q
 ..Q
 .Q
 W !
 Q
T5W ;
 I '$$DUP(DFN),'$$DUPOE(DFN,"OTH SERV"),'$$DUPOE(DFN,"ENA SERV") Q   ;NO DUPES
 W !!,$E($P(^DPT(DFN,0),U,1),1,22),?24,$S($$HRN^AUPNPAT(DFN,BUDSITE)]"":$$HRN^AUPNPAT(DFN,BUDSITE,2),1:$$HRN^AUPNPAT(DFN,DUZ(2),2)),?36,$E(BUDCOM,1,12)
 W ?51,$P(^DPT(DFN,0),U,2),?55,$$AGE^AUPNPAT(DFN,BUDCAD),?60,$E($P($$RACE^BUD8RPTC(DFN),U,3)_"-"_$P($$RACE^BUD8RPTC(DFN),U,4),1,19)
 D MEDSERV
 Q:BUDQUIT
 D DENTSERV
 Q:BUDQUIT
 D MENTSERV
 Q:BUDQUIT
 D SUBSERV
 Q:BUDQUIT
 D OTHSERV
 Q:BUDQUIT
 D ENASERV
 Q:BUDQUIT
 Q
 ;
TW ;
 W !?5,$$FMTE^XLFDT($P($P(^AUPNVSIT(BUDV,0),U),".")),?18,$E($$PRIMPROV^APCLV(BUDV,"E"),1,17),?36,$$PRIMPROV^APCLV(BUDV,"T"),?42,$$PRIMPROV^APCLV(BUDV,"D")
 W ?48,$$PRIMPOV^APCLV(BUDV,"C"),?56,$P(^AUPNVSIT(BUDV,0),U,7),?59,$E($$CLINIC^APCLV(BUDV,"E"),1,9),?70,$E($$LOCENC^APCLV(BUDV,"E"),1,9)
 Q
MEDSERV ;
 Q:'$$DUP(DFN,"MED SERV")
 W !!,"Line 15 Total Medical Care"
 I $Y>(IOSL-4) D T53H Q:BUDQUIT
 S BUDD=0 F  S BUDD=$O(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,"MED SERV",BUDD)) Q:BUDD'=+BUDD!(BUDQUIT)  D
 .Q:$P(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,"MED SERV",BUDD),U,2)=""
 .F BUDPIEC=1:1 S BUDV=$P(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,"MED SERV",BUDD),U,BUDPIEC) Q:BUDV=""!(BUDQUIT)  D
 ..I $Y>(IOSL-3) D T53H Q:BUDQUIT
 ..D TW
 ..Q
 Q
DENTSERV ;
 Q:'$$DUP(DFN,"DENT SERV")
 W !!,"Line 19 Total Dental Services"
 I $Y>(IOSL-4) D T53H Q:BUDQUIT
 S BUDD=0 F  S BUDD=$O(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,"DENT SERV",BUDD)) Q:BUDD'=+BUDD!(BUDQUIT)  D
 .Q:$P(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,"DENT SERV",BUDD),U,2)=""
 .F BUDPIEC=1:1 S BUDV=$P(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,"DENT SERV",BUDD),U,BUDPIEC) Q:BUDV=""!(BUDQUIT)  D
 ..I $Y>(IOSL-3) D T53H Q:BUDQUIT
 ..D TW
 ..Q
 Q
MENTSERV ;
 Q:'$$DUP(DFN,"MENT SERV")
 W !!,"Line 20 Mental Health"
 I $Y>(IOSL-4) D T53H Q:BUDQUIT
 S BUDD=0 F  S BUDD=$O(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,"MENT SERV",BUDD)) Q:BUDD'=+BUDD!(BUDQUIT)  D
 .Q:$P(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,"MENT SERV",BUDD),U,2)=""
 .F BUDPIEC=1:1 S BUDV=$P(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,"MENT SERV",BUDD),U,BUDPIEC) Q:BUDV=""!(BUDQUIT)  D
 ..I $Y>(IOSL-3) D T53H Q:BUDQUIT
 ..D TW
 ..Q
 Q
SUBSERV ;
 Q:'$$DUP(DFN,"SUB SERV")
 W !!,"Line 21 Substance Abuse Services"
 I $Y>(IOSL-4) D T53H Q:BUDQUIT
 S BUDD=0 F  S BUDD=$O(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,"SUB SERV",BUDD)) Q:BUDD'=+BUDD!(BUDQUIT)  D
 .Q:$P(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,"SUB SERV",BUDD),U,2)=""
 .F BUDPIEC=1:1 S BUDV=$P(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,"SUB SERV",BUDD),U,BUDPIEC) Q:BUDV=""!(BUDQUIT)  D
 ..I $Y>(IOSL-3) D T53H Q:BUDQUIT
 ..D TW
 ..Q
 Q
OTHSERV ;
 Q:'$$DUPOE(DFN,"OTH SERV")
 W !!,"Line 22 Other Professional Services"
 I $Y>(IOSL-4) D T53H Q:BUDQUIT
 S BUDDIS="" F  S BUDDIS=$O(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,"OTH SERV",BUDDIS)) Q:BUDDIS=""!(BUDQUIT)  D OTHSERV1
 Q
OTHSERV1 ;
 S BUDD=0 F  S BUDD=$O(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,"OTH SERV",BUDDIS,BUDD)) Q:BUDD'=+BUDD!(BUDQUIT)  D
 .Q:$P(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,"OTH SERV",BUDDIS,BUDD),U,2)=""
 .F BUDPIEC=1:1 S BUDV=$P(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,"OTH SERV",BUDDIS,BUDD),U,BUDPIEC) Q:BUDV=""!(BUDQUIT)  D
 ..I $Y>(IOSL-3) D T53H Q:BUDQUIT
 ..D TW
 ..Q
 Q
ENASERV ;
 Q:'$$DUPOE(DFN,"ENA SERV")
 W !!,"Line 29 Total Enabling Services"
 I $Y>(IOSL-4) D T53H Q:BUDQUIT
 S BUDDIS="" F  S BUDDIS=$O(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,"ENA SERV",BUDDIS)) Q:BUDDIS=""!(BUDQUIT)  D ENASERV1
 Q
ENASERV1 ;
 S BUDD=0 F  S BUDD=$O(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,"ENA SERV",BUDDIS,BUDD)) Q:BUDD'=+BUDD!(BUDQUIT)  D
 .Q:$P(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,"ENA SERV",BUDDIS,BUDD),U,2)=""
 .F BUDPIEC=1:1 S BUDV=$P(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,"ENA SERV",BUDDIS,BUDD),U,BUDPIEC) Q:BUDV=""!(BUDQUIT)  D
 ..I $Y>(IOSL-3) D T53H Q:BUDQUIT
 ..D TW
 ..Q
 Q
DUP(DFN,T) ;
 NEW X,Y,G
 S G=""
 S T=$G(T)
 I T="" D  Q G
 .F X="MED SERV","DENT SERV","MENT SERV","SUB SERV" D
 ..S Y=0 S Y=$O(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,X,Y)) I Y D
 ...I $P(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,X,Y),U,2)]"" S G=1
 ...Q
 S Y=0 F  S Y=$O(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,T,Y)) Q:Y'=+Y  D
 .I $P(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,T,Y),U,2)]"" S G=1
 .Q
 Q G
 ;
DUPOE(DFN,T) ;
 NEW X,Y,G
 S G=""
 S T=$G(T)
 I T="" Q ""
 S Y="" F  S Y=$O(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,T,Y)) Q:Y'=+Y  D
 .S X=0 F  S X=$O(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,T,Y,X)) Q:X'=+X  D
 ..I $P(^XTMP("BUD8RPT1",BUDJ,BUDH,"T53",BUDCOM,BUDAGE,BUDSEX,DFN,T,Y,X),U,2)]"" S G=1
 .Q
 Q G
T53H ;
 G:'BUDGPG T5H2
 K DIR I $E(IOST)="C",IO=IO(0),'$D(ZTQUEUED) W ! S DIR(0)="EO" D ^DIR K DIR I Y=0!(Y="^")!($D(DTOUT)) S BUDQUIT=1 Q
T5H2 ;
 W:$D(IOF) @IOF S BUDGPG=BUDGPG+1
 W !,"***** CONFIDENTIAL PATIENT INFORMATION, COVERED BY THE PRIVACY ACT *****"
 W !?3,$P(^VA(200,DUZ,0),U,2),?35,$$FMTE^XLFDT(DT),?70,"Page ",BUDGPG,!
 W !,$$CTR("***  BPHC Uniform Data System (UDS)  ***",80)
 W !,$$CTR("Patient List for Table 5 Columns B & C, Patients w/Multiple Visits on Same",80)
 W !,$$CTR("Day in Same Service Categories",80)
 W !,$$CTR($P(^DIC(4,BUDSITE,0),U),80)
 S X="Reporting Period: "_$$FMTE^XLFDT(BUDBD)_" to "_$$FMTE^XLFDT(BUDED) W !,$$CTR(X,80)
 W !,$TR($J("",80)," ","-")
 I BUDP=0 W !,"List of all patients with multiple visits on the same day for the same",!,"BPHC service category.",!,"Age is calculated as of June 30.",! D
 .W !,"  R- denotes the value was obtained from the Race field"
 .W !,"  C- denotes the value was obtained from the Classification/Beneficiary field"
 .W !
 W !,"PATIENT NAME",?24,"HRN",?36,"COMMUNITY",?50,"SEX",?55,"AGE",?60,"RACE*"
 W !?5,"VISIT DATE",?18,"PROV TYPE",?36,"INI",?40,"PROV CD",?48,"PRI DX",?55,"SRV",?59,"CLINIC",?70,"LOCATION"
 W !,$TR($J("",80)," ","-")
 ; !!,BUDSUBT,!
 S BUDP=1
 Q
CTR(X,Y) ;EP - Center X in a field Y wide.
 Q $J("",$S($D(Y):Y,1:IOM)-$L(X)\2)_X
 ;----------
USR() ;EP - Return name of current user from ^VA(200.
 Q $S($G(DUZ):$S($D(^VA(200,DUZ,0)):$P(^(0),U),1:"UNKNOWN"),1:"DUZ UNDEFINED OR 0")
 ;----------
LOC() ;EP - Return location name from file 4 based on DUZ(2).
 Q $S($G(DUZ(2)):$S($D(^DIC(4,DUZ(2),0)):$P(^(0),U),1:"UNKNOWN"),1:"DUZ(2) UNDEFINED OR 0")
 ;----------
