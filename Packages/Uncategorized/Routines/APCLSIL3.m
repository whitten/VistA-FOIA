APCLSIL3 ; IHS/CMI/LAB - ili surveillance ;
 ;;3.0;IHS PCC REPORTS;**24,26,27**;FEB 05, 1997
 ;
 ;
INFORM ;
 W:$D(IOF) @IOF
 W !,$$CTR($$LOC)
 W !,$$CTR($$USR)
 W !!,"This report will create a search template of visits that meet the "
 W !,"Surveillance ILI criteria.  You will be asked the provide the date"
 W !,"range of visits, a name for the visit search template to be created, and"
 W !,"the device to which the cover page/summary will be printed.",!
 W !,"The visits must meet the following criteria:"
 W !?5," - must be in the date range selected by the user"
 W !?5," - must have a service category of H OR A, O, R or S (outpatient)"
 W !?5," - must have at least one diagnosis that is contained in the "
 W !?8,"SURVEILLANCE ILI taxonomy"
 W !?5," - if ambulatory, must be to a clinic in the SURVEILLANCE ILI CLINICS taxonomy"
 W !?5," - the patient's name must not contain 'DEMO,PATIENT' (demo patients"
 W !?8,"skipped)"
 W !
 D EXIT
 S APCLCTAX=$O(^ATXAX("B","SURVEILLANCE ILI CLINICS",0))  ;clinic taxonomy
 S APCLDTAX=$O(^ATXAX("B","SURVEILLANCE ILI",0))  ;dx taxonomy
 I 'APCLDTAX W !!,"SURVEILLANCE ILI icd-9 taxonomy missing...cannot continue." D EXIT Q
 I 'APCLCTAX W !!,"SURVEILLANCE ILI CLINICS taxonomy missing...cannot continue." D EXIT Q
 ;
DATES K APCLED,APCLBD
 K DIR W ! S DIR(0)="DO^::EXP",DIR("A")="Enter Beginning Visit Date"
 D ^DIR G:Y<1 EXIT S APCLBD=Y
 K DIR S DIR(0)="DO^:DT:EXP",DIR("A")="Enter Ending Visit Date"
 D ^DIR G:Y<1 EXIT S APCLED=Y
 ;
 I APCLED<APCLBD D  G DATES
 . W !!,$C(7),"Sorry, Ending Date MUST not be earlier than Beginning Date."
 S APCLSD=$$FMADD^XLFDT(APCLBD,-1)_".9999"
 ;
STMP ;
 S APCLSTMP=""
 D ^APCLSTMV
 I APCLSTMP="" G DATES
 ;
ZIS ;call to XBDBQUE
 S XBRP="PRINT^APCLSIL3",XBRC="PROC1^APCLSIL3",XBRX="EXIT^APCLSIL3",XBNS="APCL"
 D ^XBDBQUE
 D EXIT
 Q
 ;
EXIT ;clean up and exit
 D EN^XBVK("APCL")
 D ^XBFMK
 Q
PROC1 ;
 S APCLJ=$J,APCLH=$H
 S APCLCTAX=$O(^ATXAX("B","SURVEILLANCE ILI CLINICS",0))  ;clinic taxonomy
 S APCLDTAX=$O(^ATXAX("B","SURVEILLANCE ILI",0))  ;dx taxonomy
 I 'APCLCTAX D EXIT Q
 I 'APCLDTAX D EXIT Q
 ;
 S APCLVTOT=0,APCLPTOT=0  ;visit counter
 F  S APCLSD=$O(^AUPNVSIT("B",APCLSD)) Q:APCLSD'=+APCLSD!($P(APCLSD,".")>APCLED)  D
 .S APCLV=0 F  S APCLV=$O(^AUPNVSIT("B",APCLSD,APCLV)) Q:APCLV'=+APCLV  D
 ..Q:'$D(^AUPNVSIT(APCLV,0))  ;no zero node
 ..Q:$P(^AUPNVSIT(APCLV,0),U,11)  ;deleted visit
 ..Q:"AORHS"'[$P(^AUPNVSIT(APCLV,0),U,7)  ;just want outpatient
 ..S APCLCLIN=$$CLINIC^APCLV(APCLV,"I")  ;get clinic code
 ..I $P(^AUPNVSIT(APCLV,0),U,7)'="H" Q:APCLCLIN=""
 ..I $P(^AUPNVSIT(APCLV,0),U,7)'="H" Q:'$D(^ATXAX(APCLCTAX,21,"B",APCLCLIN))  ;not in clinic taxonomy
 ..S APCLLOC=$P(^AUPNVSIT(APCLV,0),U,6)  Q:APCLLOC=""  ;no location ???
 ..S APCLDATE=$P($P(^AUPNVSIT(APCLV,0),U),".")
 ..S DFN=$P(^AUPNVSIT(APCLV,0),U,5)
 ..Q:DFN=""
 ..Q:'$D(^DPT(DFN,0))
 ..Q:$P(^DPT(DFN,0),U)["DEMO,PATIENT"
 ..Q:$$DEMO^APCLUTL(DFN,"E")  ;exclude demo patients
 ..S APCLASUF=$P($G(^AUTTLOC(APCLLOC,0)),U,10)
 ..I APCLASUF="" Q  ;no ASUFAC????
 ..S APCLLOCT(APCLASUF,$$JDATE^APCLSILI(APCLDATE))=$G(APCLLOCT(APCLASUF,$$JDATE^APCLSILI(APCLDATE)))+1   ;total number of visits on this date/location
 ..S G=0
 ..S X=0 F  S X=$O(^AUPNVPOV("AD",APCLV,X)) Q:X'=+X  S T=$P(^AUPNVPOV(X,0),U) I $$ICD^ATXCHK(T,APCLDTAX,9) S G=1
 ..Q:'G  ;no diagnosis
 ..;
 ..D SET
 ..Q
 .Q
 K ^XTMP("APCLSILI",APCLJ,APCLH)
 Q
PRINT ;EP - called from xbdbque
 S APCLPG=0
 D HEADER
 W !!,"Search Template Created: ",$P(^DIBT(APCLSTMP,0),U)
 W !!,"Total # of visits meeting criteria and placed in the template:  ",APCLVTOT
 W !!,"Total # of patients for these visits:  ",APCLPTOT,!
 D EOP
 Q
SET ;
 S APCLVTOT=APCLVTOT+1
 S ^DIBT(APCLSTMP,1,APCLV)=""
 Q:$D(^XTMP("APCLSILI",APCLJ,APCLH,"PATS",DFN))
 S APCLPTOT=APCLPTOT+1
 S ^XTMP("APCLSILI",APCLJ,APCLH,"PATS",DFN)=""
 Q
HEADER ;
 I 'APCLPG G HEAD1
 I $E(IOST)="C",IO=IO(0) W ! S DIR(0)="EO" D ^DIR K DIR I Y=0!(Y="^")!($D(DTOUT)) S APCLQ="" Q
HEAD1 ;
 W:$D(IOF) @IOF S APCLPG=APCLPG+1
 W $P(^VA(200,DUZ,0),U,2),?72,"Page ",APCLPG,!
 W ?(80-$L($P(^DIC(4,DUZ(2),0),U))/2),$P(^DIC(4,DUZ(2),0),U),!
 W !,$$CTR("SURVEILLANCE ILI VISIT SEARCH"),!
 W !,$$CTR("DATE RANGE: "_$$FMTE^XLFDT(APCLBD)_"-"_$$FMTE^XLFDT(APCLED),80),!
 W !,$$REPEAT^XLFSTR("-",79)
 Q
CTR(X,Y) ;EP - Center X in a field Y wide.
 Q $J("",$S($D(Y):Y,1:IOM)-$L(X)\2)_X
 ;----------
EOP ;EP - End of page.
 Q:$E(IOST)'="C"
 Q:$D(ZTQUEUED)!'(IOT["TRM")!$D(IO("S"))
 NEW DIR
 K DIRUT,DFOUT,DLOUT,DTOUT,DUOUT
 S DIR(0)="E" D ^DIR
 Q
 ;----------
USR() ;EP - Return name of current user from ^VA(200.
 Q $S($G(DUZ):$S($D(^VA(200,DUZ,0)):$P(^(0),U),1:"UNKNOWN"),1:"DUZ UNDEFINED OR 0")
 ;----------
LOC() ;EP - Return location name from file 4 based on DUZ(2).
 Q $S($G(DUZ(2)):$S($D(^DIC(4,DUZ(2),0)):$P(^(0),U),1:"UNKNOWN"),1:"DUZ(2) UNDEFINED OR 0")
 ;----------
