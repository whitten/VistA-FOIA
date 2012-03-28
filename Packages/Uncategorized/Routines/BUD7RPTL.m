BUD7RPTL ; IHS/CMI/LAB - UDS print lists 12 Dec 2007 12:12 PM ;
 ;;5.0;IHS/RPMS UNIFORM DATA SYSTEM;;JAN 18, 2011;Build 12
 ;
START ;
 S BUDQUIT="",BUDGPG=0
 I $G(BUDTZL) D TZ
 Q:BUDQUIT
 I $G(BUDT3AL) D T3A
 Q:BUDQUIT
 I $G(BUDT3BL),'$G(BUDT3AL) D T3A
 Q:BUDQUIT
 I $G(BUDT4L) D T4
 Q:BUDQUIT
 I $G(BUDT5L) D T5
 Q:BUDQUIT
 I $G(BUDT5L1) D T51
 Q:BUDQUIT
 I $G(BUDT5L2) D T52
 Q:BUDQUIT
 I $G(BUDT5L3) D T53
 Q:BUDQUIT
 I $G(BUDT6L) D T6
 Q:BUDQUIT
 Q
T4 ;
 Q
T3A ;
 S BUDP=0
 D T3H Q:BUDQUIT
 S BUDAGE="" F  S BUDAGE=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"3A",BUDAGE)) Q:BUDAGE'=+BUDAGE!(BUDQUIT)  D
 .S BUDSEX="" F  S BUDSEX=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"3A",BUDAGE,BUDSEX)) Q:BUDSEX=""!(BUDQUIT)  D
 ..S BUDCOM="" F  S BUDCOM=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"3A",BUDAGE,BUDSEX,BUDCOM)) Q:BUDCOM=""!(BUDQUIT)  D
 ...S DFN="" F  S DFN=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"3A",BUDAGE,BUDSEX,BUDCOM,DFN)) Q:DFN'=+DFN!(BUDQUIT)  D
 ....I $Y>(IOSL-3) D T3H Q:BUDQUIT
 ....W !,$E($P(^DPT(DFN,0),U,1),1,22),?24,$S($$HRN^AUPNPAT(DFN,BUDSITE)]"":$$HRN^AUPNPAT(DFN,BUDSITE,2),1:$$HRN^AUPNPAT(DFN,DUZ(2),2)),?36,$E(BUDCOM,1,12),?51,$P(^DPT(DFN,0),U,2),?55,$$AGE^AUPNPAT(DFN,BUDCAD),?60,$P($$RACE^BUD7RPTC(DFN),U,2)
 ....S BUDV=0 F  S BUDV=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"3A",BUDAGE,BUDSEX,BUDCOM,DFN,BUDV)) Q:BUDV'=+BUDV!(BUDQUIT)  D
 .....I $Y>(IOSL-3) D T3H Q:BUDQUIT
 .....W !?5,$$FMTE^XLFDT($P($P(^AUPNVSIT(BUDV,0),U),".")),?25,$E($$PRIMPROV^APCLV(BUDV,"E"),1,14),?42,$P(^AUPNVSIT(BUDV,0),U,7),?45,$E($$CLINIC^APCLV(BUDV,"E"),1,14),?62,$E($$LOCENC^APCLV(BUDV,"E"),1,14)
 W !
 Q
T3H ;
 G:'BUDGPG T3H1
 K DIR I $E(IOST)="C",IO=IO(0),'$D(ZTQUEUED) W ! S DIR(0)="EO" D ^DIR K DIR I Y=0!(Y="^")!($D(DTOUT)) S BUDQUIT=1 Q
T3H1 ;
 W:$D(IOF) @IOF S BUDGPG=BUDGPG+1
 W !,"***** CONFIDENTIAL PATIENT INFORMATION, COVERED BY THE PRIVACY ACT *****"
 W !?3,$P(^VA(200,DUZ,0),U,2),?35,$$FMTE^XLFDT(DT),?70,"Page ",BUDGPG,!
 W !,$$CTR("***  BPHC Uniform Data System (UDS)  ***",80)
 W !,$$CTR("Patient List for Tables 3A, 3B",80),!
 W $$CTR($P(^DIC(4,BUDSITE,0),U),80),!
 S X="Reporting Period: "_$$FMTE^XLFDT(BUDBD)_" to "_$$FMTE^XLFDT(BUDED) W $$CTR(X,80),!
 W $TR($J("",80)," ","-")
 I BUDP=0 W !,"List of all patients with one or more visits during the calendar year,",!,"with gender, age, race or ethnicity, and visit information.",!,"Age is calculated as of June 30.",!
 W !,"PATIENT NAME",?24,"HRN",?36,"COMMUNITY",?50,"SEX",?55,"AGE",?60,"RACE/ETHN"
 W !?5,"VISIT DATE",?25,"PROV TYPE",?41,"SRV",?45,"CLINIC",?62,"LOCATION"
 S BUDP=1
 W !,$TR($J("",80)," ","-")
 Q
T52 ;
 D T52^BUD7RPL2
 Q
T53 ;
 D T53^BUD7RPL3
 Q
T51 ;
 S BUDNEWR=1
 S BUD7L="" F  S BUD7L=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"T51",BUD7L)) Q:BUD7L=""!(BUDQUIT)  D
 .S BUD7L2="" F  S BUD7L2=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"T51",BUD7L,BUD7L2)) Q:BUD7L2=""!(BUDQUIT)  D
 ..S BUD7LL=BUD7L_$S(BUD7L2=0:"",1:BUD7L2)
 ..S BUDY=$O(^BUDETFIV("B",BUD7LL,0)),BUDY=$P(^BUDETFIV(BUDY,0),U,2)
 ..S BUDSUBT="Line "_BUD7LL_"   "_BUDY
 ..I $Y>(IOSL-3)!$G(BUDNEWR) D T51H Q:BUDQUIT  K BUDNEWR
 ..W !!,"Line ",BUD7LL,"   ",BUDY
 ..S BUDPROV="" F  S BUDPROV=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"T51",BUD7L,BUD7L2,BUDPROV)) Q:BUDPROV=""!(BUDQUIT)  D
 ...I $Y>(IOSL-3) D T51H Q:BUDQUIT  W !!,"Line ",BUD7LL,"   ",BUDY
 ...W !,BUDPROV,?35,^XTMP("BUD7RPT1",BUDJ,BUDH,"T51",BUD7L,BUD7L2,BUDPROV)
 .Q
 W !
 Q
T51H ;
 G:'BUDGPG T51H1
 K DIR I $E(IOST)="C",IO=IO(0),'$D(ZTQUEUED) W ! S DIR(0)="EO" D ^DIR K DIR I Y=0!(Y="^")!($D(DTOUT)) S BUDQUIT=1 Q
T51H1 ;
 W:$D(IOF) @IOF S BUDGPG=BUDGPG+1
 W !,"***** CONFIDENTIAL PATIENT INFORMATION, COVERED BY THE PRIVACY ACT *****"
 W !?3,$P(^VA(200,DUZ,0),U,2),?35,$$FMTE^XLFDT(DT),?70,"Page ",BUDGPG,!
 W !,$$CTR("***  BPHC Uniform Data System (UDS)  ***",80)
 W !,$$CTR("Personnel List for Table 5 Column A, By Service Category",80),!
 W $$CTR($P(^DIC(4,BUDSITE,0),U),80),!
 S X="Reporting Period: "_$$FMTE^XLFDT(BUDBD)_" to "_$$FMTE^XLFDT(BUDED) W $$CTR(X,80),!
 W $TR($J("",80)," ","-")
 W !,"List of all Active Provider Personnel sorted by Major Service Category.",!
 W !,"PROVIDER NAME",?35,"PROVIDER CODE",?70,"FTE"
 W !,$TR($J("",80)," ","-")
 ;W !!,BUDSUBT
 Q
T5 ;
 S BUDP=0
 S BUD7L="" F   S BUD7L=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"T5",BUD7L)) Q:BUD7L=""!(BUDQUIT)  D
 .S BUD7L2="" F  S BUD7L2=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"T5",BUD7L,BUD7L2)) Q:BUD7L2=""!(BUDQUIT)  D
 ..S BUD7LL=BUD7L_$S(BUD7L2=0:"",1:BUD7L2)
 ..S BUDY=$O(^BUDETFIV("B",BUD7LL,0)),BUDY=$P(^BUDETFIV(BUDY,0),U,2)
 ..S BUDSUBT="Line "_BUD7LL_"   "_BUDY
 ..D T5H Q:BUDQUIT
 ..S BUDCOM="" F  S BUDCOM=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"T5",BUD7L,BUD7L2,BUDCOM)) Q:BUDCOM=""!(BUDQUIT)  D
 ...S BUDAGE="" F  S BUDAGE=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"T5",BUD7L,BUD7L2,BUDCOM,BUDAGE)) Q:BUDAGE=""!(BUDQUIT)  D
 ....S BUDSEX="" F  S BUDSEX=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"T5",BUD7L,BUD7L2,BUDCOM,BUDAGE,BUDSEX)) Q:BUDSEX=""!(BUDQUIT)  D
 .....S DFN=0 F  S DFN=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"T5",BUD7L,BUD7L2,BUDCOM,BUDAGE,BUDSEX,DFN)) Q:DFN'=+DFN!(BUDQUIT)  D T5W
 .....Q
 ...Q
 ..Q
 .Q
 W !
 Q
T5W W !,$E($P(^DPT(DFN,0),U,1),1,22),?24,$S($$HRN^AUPNPAT(DFN,BUDSITE)]"":$$HRN^AUPNPAT(DFN,BUDSITE,2),1:$$HRN^AUPNPAT(DFN,DUZ(2),2)),?36,$E(BUDCOM,1,12),?51,$P(^DPT(DFN,0),U,2),?55,$$AGE^AUPNPAT(DFN,BUDCAD),?60,$P($$RACE^BUD7RPTC(DFN),U,2)
 K BUDVLST S BUDV=0 F  S BUDV=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"T5",BUD7L,BUD7L2,BUDCOM,BUDAGE,BUDSEX,DFN,BUDV)) Q:BUDV'=+BUDV!(BUDQUIT)  D
 .S BUDVLST($P(^AUPNVSIT(BUDV,0),U),BUDV)=""
 S BUDDD=0 F  S BUDDD=$O(BUDVLST(BUDDD)) Q:BUDDD=""!(BUDQUIT)  D
 .S BUDV=0 F  S BUDV=$O(BUDVLST(BUDDD,BUDV)) Q:BUDV'=+BUDV!(BUDQUIT)  D
 ..I $Y>(IOSL-3) D T5H Q:BUDQUIT
 ..W !?5,$$FMTE^XLFDT($P($P(^AUPNVSIT(BUDV,0),U),".")),?25,$E($$PRIMPROV^APCLV(BUDV,"E"),1,14),?42,$E($$PRIMPROV^APCLV(BUDV,"D"),1,8),?50,$P(^AUPNVSIT(BUDV,0),U,7),?55,$E($$CLINIC^APCLV(BUDV,"E"),1,14),?70,$E($$LOCENC^APCLV(BUDV,"E"),1,9)
 ..Q
 Q
T5H ;
 G:'BUDGPG T5H1
 K DIR I $E(IOST)="C",IO=IO(0),'$D(ZTQUEUED) W ! S DIR(0)="EO" D ^DIR K DIR I Y=0!(Y="^")!($D(DTOUT)) S BUDQUIT=1 Q
T5H1 ;
 W:$D(IOF) @IOF S BUDGPG=BUDGPG+1
 W !,"***** CONFIDENTIAL PATIENT INFORMATION, COVERED BY THE PRIVACY ACT *****"
 W !?3,$P(^VA(200,DUZ,0),U,2),?35,$$FMTE^XLFDT(DT),?70,"Page ",BUDGPG,!
 W !,$$CTR("***  BPHC Uniform Data System (UDS)  ***",80)
 W !,$$CTR("Patient List for Table 5 Columns B & C, By Service Category",80),!
 W $$CTR($P(^DIC(4,BUDSITE,0),U),80),!
 S X="Reporting Period: "_$$FMTE^XLFDT(BUDBD)_" to "_$$FMTE^XLFDT(BUDED) W $$CTR(X,80),!
 W $TR($J("",80)," ","-")
 I BUDP=0 W !,"List of all patients, sorted by defined Service Categories.  Displays",!,"community, gender, age and visit data, including Provider codes."
 W !,"PATIENT NAME",?24,"HRN",?36,"COMMUNITY",?50,"SEX",?55,"AGE",?60,"RACE/ETHN"
 W !?5,"VISIT DATE",?25,"PROV TYPE",?41,"PROV CD",?50,"SRV",?55,"CLINIC",?62,"LOCATION"
 W !,$TR($J("",80)," ","-")
 W !!,BUDSUBT,!
 S BUDP=1
 Q
TZ ;
 S BUDP=0
 D TZH Q:BUDQUIT
 S BUDZIP="" F  S BUDZIP=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"Z",BUDZIP)) Q:BUDZIP=""!(BUDQUIT)  D
 .S BUDCOM="" F  S BUDCOM=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"Z",BUDZIP,BUDCOM)) Q:BUDCOM=""!(BUDQUIT)  D
 ..S BUDSEX="" F  S BUDSEX=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"Z",BUDZIP,BUDCOM,BUDSEX)) Q:BUDSEX=""!(BUDQUIT)  D
 ...S BUDNAME="" F  S BUDNAME=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"Z",BUDZIP,BUDCOM,BUDSEX,BUDNAME)) Q:BUDNAME=""!(BUDQUIT)  D
 ....S DFN="" F  S DFN=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"Z",BUDZIP,BUDCOM,BUDSEX,BUDNAME,DFN)) Q:DFN'=+DFN!(BUDQUIT)  D
 .....I $Y>(IOSL-3) D TZH Q:BUDQUIT
 .....W !,$E($P(^DPT(DFN,0),U,1),1,22),?24,$S($$HRN^AUPNPAT(DFN,BUDSITE)]"":$$HRN^AUPNPAT(DFN,BUDSITE,2),1:$$HRN^AUPNPAT(DFN,DUZ(2),2)),?36,$E(BUDCOM,1,12),?51,$P(^DPT(DFN,0),U,2),?55,BUDZIP
 .....S BUDV=0 F  S BUDV=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"Z",BUDZIP,BUDCOM,BUDSEX,BUDNAME,DFN,BUDV)) Q:BUDV'=+BUDV!(BUDQUIT)  D
 ......I $Y>(IOSL-3) D TZH Q:BUDQUIT
 ......W !?5,$$FMTE^XLFDT($P($P(^AUPNVSIT(BUDV,0),U),".")),?25,$E($$PRIMPROV^APCLV(BUDV,"E"),1,14),?42,$P(^AUPNVSIT(BUDV,0),U,7),?45,$E($$CLINIC^APCLV(BUDV,"E"),1,14),?62,$E($$LOCENC^APCLV(BUDV,"E"),1,14)
 W !
 Q
TZH ;
 G:'BUDGPG TZH1
 K DIR I $E(IOST)="C",IO=IO(0),'$D(ZTQUEUED) W ! S DIR(0)="EO" D ^DIR K DIR I Y=0!(Y="^")!($D(DTOUT)) S BUDQUIT=1 Q
TZH1 ;
 W:$D(IOF) @IOF S BUDGPG=BUDGPG+1
 W !,"***** CONFIDENTIAL PATIENT INFORMATION, COVERED BY THE PRIVACY ACT *****"
 W !?3,$P(^VA(200,DUZ,0),U,2),?35,$$FMTE^XLFDT(DT),?70,"Page ",BUDGPG,!
 W !,$$CTR("***  BPHC Uniform Data System (UDS)  ***",80)
 W !,$$CTR("Patient List Center/Grantee Profile: Patient by Zip Code",80),!
 W $$CTR($P(^DIC(4,BUDSITE,0),U),80),!
 S X="Reporting Period: "_$$FMTE^XLFDT(BUDBD)_" to "_$$FMTE^XLFDT(BUDED) W $$CTR(X,80),!
 W $TR($J("",80)," ","-")
 I BUDP=0 W !,"List of all patients with one or more visits during the calendar year."
 I BUDP=0 W !,"Zip code is from patient registration."
 I BUDP=0 W !!,"NOTE:  Patients with a zip code included in the Other Zip Codes category"
 I BUDP=0 W !,"have their zip code value followed by a ""","*",""" (e.g. 87015*)."
 W !!,"PATIENT NAME",?24,"HRN",?36,"COMMUNITY",?50,"SEX",?55,"ZIP CODE"
 W !?5,"VISIT DATE",?25,"PROV TYPE",?41,"SRV",?45,"CLINIC",?62,"LOCATION"
 S BUDP=1
 W !,$TR($J("",80)," ","-")
 Q
T6 ;
 S BUDP=0
 ;D T6H Q:BUDQUIT
 ;S BUD7L="" F   S BUD7L=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"T6",BUD7L)) Q:BUD7L=""!(BUDQUIT)  D
 F BUD7L=1:1:19,40:1:43,21:1:34 D
 .Q:'$D(^XTMP("BUD7RPT1",BUDJ,BUDH,"T6",BUD7L))
 .D T6H Q:BUDQUIT
 .W !!,"Line ",$S($P($T(@BUD7L),";;",3)]"":$P($T(@BUD7L),";;",3),1:BUD7L),"   ",$P($T(@BUD7L),";;",2)
 .S BUDCOM="" F  S BUDCOM=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"T6",BUD7L,BUDCOM)) Q:BUDCOM=""!(BUDQUIT)  D
 ..S BUDAGE="" F  S BUDAGE=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"T6",BUD7L,BUDCOM,BUDAGE)) Q:BUDAGE=""!(BUDQUIT)  D
 ...S BUDSEX="" F  S BUDSEX=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"T6",BUD7L,BUDCOM,BUDAGE,BUDSEX)) Q:BUDSEX=""!(BUDQUIT)  D
 ....S DFN=0 F  S DFN=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"T6",BUD7L,BUDCOM,BUDAGE,BUDSEX,DFN)) Q:DFN'=+DFN!(BUDQUIT)  D T6W
 ....Q
 ...Q
 ..Q
 .Q
 W !
 Q
T6W ;
 I $Y>(IOSL-3) D T6H Q:BUDQUIT  W !!,"Line ",$S($P($T(@BUD7L),";;",3)]"":$P($T(@BUD7L),";;",3),1:BUD7L),"   ",$P($T(@BUD7L),";;",2)
 W !,$E($P(^DPT(DFN,0),U,1),1,22),?24,$S($$HRN^AUPNPAT(DFN,BUDSITE)]"":$$HRN^AUPNPAT(DFN,BUDSITE,2),1:$$HRN^AUPNPAT(DFN,DUZ(2),2)),?36,$E(BUDCOM,1,12),?51,$P(^DPT(DFN,0),U,2),?55,$$AGE^AUPNPAT(DFN,BUDCAD),?60,$P($$RACE^BUD7RPTC(DFN),U,2)
 K BUDVRR S BUDV=0,BUDVC=0 F  S BUDV=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"T6",BUD7L,BUDCOM,BUDAGE,BUDSEX,DFN,BUDV)) Q:BUDV'=+BUDV!(BUDQUIT)  S BUDVC=BUDVC+1,BUDVRR($$VD^APCLV(BUDV),BUDVC)=BUDV
 S BUDVD=0 F  S BUDVD=$O(BUDVRR(BUDVD)) Q:BUDVD=""  S BUDVC=0 F  S BUDVC=$O(BUDVRR(BUDVD,BUDVC)) Q:BUDVC=""  D
 .I $Y>(IOSL-3) D T6H Q:BUDQUIT  W !!,"Line ",$S($P($T(@BUD7L),";;",3)]"":$P($T(@BUD7L),";;",3),1:BUD7L),"   ",$P($T(@BUD7L),";;",2)
 .S BUDV=BUDVRR(BUDVD,BUDVC)
 .W !?5,$$FMTE^XLFDT($P($P(^AUPNVSIT(BUDV,0),U),".")),?25,^XTMP("BUD7RPT1",BUDJ,BUDH,"T6",BUD7L,BUDCOM,BUDAGE,BUDSEX,DFN,BUDV),?40,$P(^AUPNVSIT(BUDV,0),U,7),?45,$E($$CLINIC^APCLV(BUDV,"E"),1,15),?62,$E($$LOCENC^APCLV(BUDV,"E"),1,15)
 .Q
 I BUD7L=22 D
 .S BUDW=0 F  S BUDW=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"T6",BUD7L,BUDCOM,BUDAGE,BUDSEX,DFN,"WH","MAM",BUDW)) Q:BUDW'=+BUDW!(BUDQUIT)  D
 ..W !?5,$P(^XTMP("BUD7RPT1",BUDJ,BUDH,"T6",BUD7L,BUDCOM,BUDAGE,BUDSEX,DFN,"WH","MAM",BUDW),U,2),?25,$P(^(BUDW),U,1)
 I BUD7L=23 D
 .S BUDW=0 F  S BUDW=$O(^XTMP("BUD7RPT1",BUDJ,BUDH,"T6",BUD7L,BUDCOM,BUDAGE,BUDSEX,DFN,"WH","PAP",BUDW)) Q:BUDW'=+BUDW!(BUDQUIT)  D
 ..W !?5,$P(^XTMP("BUD7RPT1",BUDJ,BUDH,"T6",BUD7L,BUDCOM,BUDAGE,BUDSEX,DFN,"WH","PAP",BUDW),U,2),?25,$P(^(BUDW),U,1)
 Q
T6H ;
 G:'BUDGPG T6H1
 K DIR I $E(IOST)="C",IO=IO(0),'$D(ZTQUEUED) W ! S DIR(0)="EO" D ^DIR K DIR I Y=0!(Y="^")!($D(DTOUT)) S BUDQUIT=1 Q
T6H1 ;
 W:$D(IOF) @IOF S BUDGPG=BUDGPG+1
 W !,"***** CONFIDENTIAL PATIENT INFORMATION, COVERED BY THE PRIVACY ACT *****"
 W !?3,$P(^VA(200,DUZ,0),U,2),?35,$$FMTE^XLFDT(DT),?70,"Page ",BUDGPG,!
 W !,$$CTR("***  BPHC Uniform Data System (UDS)  ***",80)
 W !,$$CTR("Patient List for Table 6, By Diagnosis Category",80),!
 W $$CTR($P(^DIC(4,BUDSITE,0),U),80),!
 S X="Reporting Period: "_$$FMTE^XLFDT(BUDBD)_" to "_$$FMTE^XLFDT(BUDED) W $$CTR(X,80),!
 W $TR($J("",80)," ","-")
 I BUDP=0 W !,"List of all patients, sorted by primary diagnosis and tests/screening",!,"categories.  Displays community, gender, age and visit data, and codes."
 W !,"PATIENT NAME",?24,"HRN",?36,"COMMUNITY",?50,"SEX",?55,"AGE",?60,"RACE/ETHN"
 W !?5,"VISIT DATE",?25,"VALUE",?41,"SRV",?45,"CLINIC",?63,"LOCATION"
 W !,$TR($J("",80)," ","-")
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
1 ;;Symptomatic HIV
2 ;;Asymptomatic HIV
3 ;;Tuberculosis
4 ;;Syphilis and other sexually transmitted diseases
5 ;;Asthma
6 ;;Chronic bronchitis and emphysema
7 ;;Abnormal breast findings, female
8 ;;Abnormal cervical findings
9 ;;Diabetes mellitus
10 ;;Heart disease (selected)
11 ;;Hypertension
12 ;;Contact dermatitis and other eczema
13 ;;Dehydration
14 ;;Exposure to heat or cold
15 ;;Otitis media and eustachian tube disorders
16 ;;Selected perinatal medical conditions
17 ;;Lack of expected normal physiological development
18 ;;Alcohol related disorders
19 ;;Other substance related disorders (excluding tobacco use disorders)
20 ;;Other Mental disorders, excluding drug or alcohol dependence
40 ;;Depression and Other Mood Disorders;;20a
41 ;;Anxiety disorders including PTSD;;20b
42 ;;Attention Deficit and disruptive behavior disorders;;20c
43 ;;Other Mental disorders, excl drug or alcohol dependence (incl mental retardation);;20d
21 ;;HIV Test
22 ;;Mammogram
23 ;;Pap Smear
24 ;;Selected Immunizations
25 ;;Contraceptive Management
26 ;;Health supervision of infant or child (ages 0 through 11)
27 ;;I.  Emergency Services
28 ;;II. Oral Exams
29 ;;Prophylaxis - adult or child
30 ;;Sealants
31 ;;Fluoride treatment
32 ;;III. Restorative Services
33 ;;IV. Oral Surgery (extractions only)
34 ;;V. Rehabilitative Services (Endo, Perio, Prostho, Ortho)
