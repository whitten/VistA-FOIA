APCLSILR ;IHS/CMI/LAB - AGGREGATE ILI REPORT;
 ;;3.0;IHS PCC REPORTS;**24,26,27**;FEB 05, 1997
 ;
START ;
 W:$D(IOF) @IOF
 W !,"**********  AGGREGATE ILI/H1N1/Adverse Event Surveillance Report  **********",!
 D EN^XBVK("APCL")
BD ;get beginning date
 W ! S DIR(0)="D^:DT:EP",DIR("A")="Enter beginning date for search" D ^DIR K DIR S:$D(DUOUT) DIRUT=1
 I $D(DIRUT) D EOJ Q
 S APCLBD=Y
ED ;get ending date
 W ! S DIR(0)="DA^"_APCLBD_":DT:EP",DIR("A")="Enter ending date for search:  " D ^DIR K DIR S:$D(DUOUT) DIRUT=1
 I $D(DIRUT) G BD
 S APCLED=Y
FAC ;
 K APCLQUIT
 S APCLLOCT=""
 K DIR S DIR(0)="S^O:ONE Facility;A:All Facilities;S:Selected set of Facilities or Taxonomy of Faclities"
 S DIR("A")="Enter a code indicating what FACILITIES/LOCATIONS are of interest",DIR("B")="O" K DA D ^DIR K DIR,DA
 G:$D(DIRUT) BD
 S APCLLOCT=Y
 K APCLLOCS
 D @APCLLOCT
 G:$D(APCLQUIT) FAC
 ;
ZIS ;
DEMO ;
 D DEMOCHK^APCLUTL(.APCLDEMO)
 I APCLDEMO=-1 G FAC
 S XBRP="PRINT^APCLSILR",XBRC="PROC^APCLSILR",XBRX="EOJ^APCLSILR",XBNS="APCL"
 D ^XBDBQUE
 ;
EOJ ;ENTRY POINT
 D EN^XBVK("APCL")
 Q
O ;
 W ! S DIC("A")="Which Facility: ",DIC="^AUTTLOC(",DIC(0)="AEMQ" D ^DIC K DIC,DA I Y<0 S APCLQUIT=1 Q
 S APCLLOCS(+Y)=""
 Q
A ;
 K APCLLOCS
 Q
S ;taxonomy - call qman interface
 K APCLLOCS
 S X="ENCOUNTER LOCATION",DIC="^AMQQ(5,",DIC(0)="FM",DIC("S")="I $P(^(0),U,14)" D ^DIC K DIC,DA I Y=-1 W "OOPS - QMAN NOT CURRENT - QUITTING" S APCLQUIT=1 Q
 D PEP^AMQQGTX0(+Y,"APCLLOCS(")
 I '$D(APCLLOCS) S APCLQUIT=1 Q
 I $D(APCLLOCS("*")) K APCLLOC,APCLLOCS W !!,$C(7),$C(7),"ALL locations is NOT an option with this report",! G S
 Q
PROC ;EP - called from xbdbque
 S APCLJ=$J,APCLH=$H
 D XTMP^APCLOSUT("APCLSILR","ILI/H1N1 SURV REPORT")
 K APCLVTOT,APCLSRDH,APCLSRDP,APCLILIS,APCLILIP,APCLMEDS,APCLMEDP,APCLAGEG,APCLIMMG,APCLSEXL,APCLIMML,APCLTAB5,APCLTAB8,APCLPTOT,APCLALLF,APCLTAB7
 K ^XTMP("APCLSILR",APCLJ,APCLH)
V ; Run by visit date
 S APCLSD=$$FMADD^XLFDT(APCLBD,-1)
 K APCLVTOT
 S APCLVTOT=0,APCLPTOT=0
 S APCLODAT=APCLSD_".9999" F  S APCLODAT=$O(^AUPNVSIT("B",APCLODAT)) Q:APCLODAT=""!((APCLODAT\1)>APCLED)  D V1
 ;
END ;
 Q
V1 ;
 ;
 S APCLVDFN="" F  S APCLVDFN=$O(^AUPNVSIT("B",APCLODAT,APCLVDFN)) Q:APCLVDFN'=+APCLVDFN  I $D(^AUPNVSIT(APCLVDFN,0)),$P(^(0),U,9),'$P(^(0),U,11) S APCLVREC=^(0) D PROC1
 Q
PROC1 ;
 Q:'$D(^AUPNVSIT(APCLVDFN,0))
 Q:$$DEMO^APCLUTL($P(APCLVREC,U,5),$G(APCLDEMO))
 S DFN=$P(APCLVREC,U,5)
 Q:'$D(^AUPNPAT(DFN,0))
 Q:'$D(^DPT(DFN,0))
 S APCLVLOC=$P(APCLVREC,U,6) Q:APCLVLOC=""
 I $D(APCLLOCS) Q:'$D(APCLLOCS(APCLVLOC))  ;not a location they want
 S APCLLOCN=$P(^DIC(4,APCLVLOC,0),U)
 I "AORSIH"[$P(APCLVREC,U,7) D
 .Q:$D(^XTMP("APCLSILR",APCLJ,APCLH,"TOTPAT",DFN))
 .S APCLPTOT=APCLPTOT+1
 .S ^XTMP("APCLSILR",APCLJ,APCLH,"TOTPAT",DFN)=""
 I "AORSIH"[$P(APCLVREC,U,7) D
 .Q:$D(^XTMP("APCLSILR",APCLJ,APCLH,"TOTPATLOC",DFN,APCLVLOC))
 .S APCLPTOT(APCLVLOC)=$G(APCLPTOT(APCLVLOC))+1
 .S ^XTMP("APCLSILR",APCLJ,APCLH,"TOTPATLOC",DFN,APCLVLOC)=""
 S APCLCLIN=$P(APCLVREC,U,8)
 S APCLILIV=$$ILIV(APCLVDFN)  ;if this is an ILI visit:  1^A,C,H^term to use in facilty visit count^HAD ILI OR H1N1 DX
 I APCLILIV D
 .;set total # of visits and # by loc
 .S $P(APCLVTOT,U,1)=$P(APCLVTOT,U,1)+1
 .S $P(APCLVTOT(APCLLOCN),U,1)=$P($G(APCLVTOT(APCLLOCN)),U,1)+1
 .S $P(APCLVTOT(APCLLOCN,$P(APCLILIV,U,2),$P(APCLILIV,U,3)),U,1)=$P($G(APCLVTOT(APCLLOCN,$P(APCLILIV,U,2),$P(APCLILIV,U,3))),U,1)+1
 .S $P(APCLALLF($P(APCLILIV,U,2),$P(APCLILIV,U,3)),U,1)=$P($G(APCLALLF($P(APCLILIV,U,2),$P(APCLILIV,U,3))),U,1)+1
 .Q:'$P(APCLILIV,U,4)
 .S $P(APCLVTOT,U,2)=$P(APCLVTOT,U,2)+1
 .S $P(APCLVTOT(APCLLOCN),U,2)=$P($G(APCLVTOT(APCLLOCN)),U,2)+1
 .S $P(APCLVTOT(APCLLOCN,$P(APCLILIV,U,2),$P(APCLILIV,U,3)),U,2)=$P($G(APCLVTOT(APCLLOCN,$P(APCLILIV,U,2),$P(APCLILIV,U,3))),U,2)+1
 .S $P(APCLALLF($P(APCLILIV,U,2),$P(APCLILIV,U,3)),U,2)=$P($G(APCLALLF($P(APCLILIV,U,2),$P(APCLILIV,U,3))),U,2)+1
 .D ILIAGE
 .;W !,APCLVDFN,"   ",$P(APCLVREC,U,5)
 .S S=$$VAL^XBDIQ1(2,$P(APCLVREC,U,5),.02)
 .S $P(APCLILIS(S),U,1)=$P($G(APCLILIS(S)),U,1)+1
 .S $P(APCLSEXL(APCLLOCN,S),U,1)=$P($G(APCLSEXL(APCLLOCN,S)),U,1)+1
 .;Q:$D(APCLILIP($P(APCLVREC,U,5)))
 .I '$D(^XTMP("APCLSILR",APCLJ,APCLH,"APCLILIP",$P(APCLVREC,U,5))) D
 ..S $P(APCLILIS(S),U,2)=$P($G(APCLILIS(S)),U,2)+1
 ..;S APCLILIP($P(APCLVREC,U,5))=""
 ..S ^XTMP("APCLSILR",APCLJ,APCLH,"APCLILIP",$P(APCLVREC,U,5))=""
 .I '$D(^XTMP("APCLSILR",APCLJ,APCLH,"APCLSEXL",APCLLOCN,$P(APCLVREC,U,5))) D
 ..S $P(APCLSEXL(APCLLOCN,S),U,2)=$P($G(APCLSEXL(APCLLOCN,S)),U,2)+1
 ..S ^XTMP("APCLSILR",APCLJ,APCLH,"APCLSEXL",APCLLOCN,$P(APCLVREC,U,5))=""
 I $P(^AUPNVSIT(APCLVDFN,0),U,7)="H" D RESDIS
 D MEDS^APCLSILA
 D VACAGE^APCLSILA
 D TAB727^APCLSILA
 D TAB827^APCLSILA
 ;I APCLILIV D TAB9^APCLSILA  TABLE 9 TAKEN OUT IN PATCH 27
 Q
 ;
ILIAGE ;
 NEW H,I,S1,S2,X,T,APCLA,A
 S (H,I,S1,S2)=""
 S X=0 F  S X=$O(^AUPNVPOV("AD",APCLVDFN,X)) Q:X'=+X  S T=$P(^AUPNVPOV(X,0),U) D
 .I $$ICD^ATXCHK(T,$O(^ATXAX("B","SURVEILLANCE ILI",0)),9) S I=1
 .I $$ICD^ATXCHK(T,$O(^ATXAX("B","SURVEILLANCE H1N1 DX",0)),9) S H=1
 I $P(^AUPNVSIT(APCLVDFN,0),U,7)="H" S S2="H"
 I $P(^AUPNVSIT(APCLVDFN,0),U,7)'="H" S S2="A"
 S APCLAY=$$AGE^APCLSILU($P(APCLVREC,U,5),1,$$VD^APCLV(APCLVDFN))
 I APCLAY["<"!(APCLAY<5) D  Q
 .S APCLAY=$$AGE^APCLSILU($P(APCLVREC,U,5),2,$$VD^APCLV(APCLVDFN))
 .I APCLAY<6 Q
 .S A=$$AGEGM(APCLAY)
 .I I S APCLAGEG("I",S2,A)=$G(APCLAGEG("I",S2,A))+1,APCLAGEG("I",S2,"TOTAL")=$G(APCLAGEG("I",S2,"TOTAL"))+1
 .I H S APCLAGEG("H",S2,A)=$G(APCLAGEG("H",S2,A))+1,APCLAGEG("H",S2,"TOTAL")=$G(APCLAGEG("H",S2,"TOTAL"))+1
 S A=$$AGEG(APCLAY)
 I I S APCLAGEG("I",S2,A)=$G(APCLAGEG("I",S2,A))+1,APCLAGEG("I",S2,"TOTAL")=$G(APCLAGEG("I",S2,"TOTAL"))+1
 I H S APCLAGEG("H",S2,A)=$G(APCLAGEG("H",S2,A))+1,APCLAGEG("H",S2,"TOTAL")=$G(APCLAGEG("H",S2,"TOTAL"))+1
 Q
AGEGM(APCLA) ;EP - age months
 I APCLA<24 Q "6-23m"
 I APCLA>23,APCLA<60 Q "24-59m"
 Q ""
AGEG(APCLA) ;EP 0 age years
 I APCLA>4,APCLA<19 Q "60m-18y"
 I APCLA>18,APCLA<25 Q "19-24y"
 I APCLA>24,APCLA<50 Q "25-49y"
 I APCLA>49,APCLA<65 Q "50-64y"
 I APCLA>64 Q "65+y"
 Q ""
 ;
RESDIS ;does this H visit have severe resp diagnosis, if yes set counter
 NEW X,Y,D,I
 S X=0 F  S X=$O(^AUPNVPOV("AD",APCLVDFN,X)) Q:X'=+X  D
 .S D=$P($G(^AUPNVPOV(X,0)),U,1)
 .I D="" Q
 .I '$$ICD^ATXCHK(D,$O(^ATXAX("B","SURVEILLANCE SEV RESP DIS DXS",0)),9) Q
 .S I=$$ICDDX^ICDCODE(D,$$VD^APCLV($P(^AUPNVPOV(X,0),U,3))),I=$P(I,U,4)
 .S $P(APCLSRDH(APCLVLOC,I),U,1)=$P($G(APCLSRDH(APCLVLOC,I)),U,1)+1
 .I $D(APCLSRDP(APCLVLOC,I,$P(^AUPNVPOV(X,0),U,2))) Q
 .S $P(APCLSRDH(APCLVLOC,I),U,2)=$P($G(APCLSRDH(APCLVLOC,I)),U,2)+1
 .S APCLSRDP(APCLVLOC,I,$P(^AUPNVPOV(X,0),U,2))=""
 .Q
 Q
ILIV(V) ;
 NEW C,P,APCLCLIN,X,Z,G,Y,VAL,T,APCLCTAX
 S APCLCTAX=$O(^ATXAX("B","SURVEILLANCE ILI CLINICS",0))
 I '$G(V) Q ""
 I '$D(^AUPNVSIT(V)) Q ""
 I "AORSH"'[$P(^AUPNVSIT(V,0),U,7) Q ""
 S APCLCLIN=$$CLINIC^APCLV(V,"I")  ;get clinic code
 ;is there a PHN
 S X=0,P=0 F  S X=$O(^AUPNVPRV("AD",V,X)) Q:X'=+X!(P)  D
 .Q:'$D(^AUPNVPRV(X,0))
 .S Y=$P(^AUPNVPRV(X,0),U)
 .S Z=$$VALI^XBDIQ1(200,Y,53.5)
 .Q:'Z
 .I $P($G(^DIC(7,Z,9999999)),U,1)=13 S P=1
 I P G ILIDX1
 I $P(^AUPNVSIT(V,0),U,7)'="H" I APCLCLIN="" Q ""
 I $P(^AUPNVSIT(V,0),U,7)'="H" I '$D(^ATXAX(APCLCTAX,21,"B",APCLCLIN)) Q ""   ;not in clinic taxonomy
ILIDX1 ;
 S C=0
 K G,Y S G=""
 S X=0 F  S X=$O(^AUPNVPOV("AD",V,X)) Q:X'=+X  S T=$P(^AUPNVPOV(X,0),U) D
 .I $$ICD^ATXCHK(T,$O(^ATXAX("B","SURVEILLANCE ILI",0)),9) S C=C+1,Y(C)=$$VAL^XBDIQ1(9000010.07,X,.01) Q
 .I $$ICD^ATXCHK(T,$O(^ATXAX("B","SURVEILLANCE H1N1 DX",0)),9) S C=C+1,Y(C)=$$VAL^XBDIQ1(9000010.07,X,.01)
 S VAL=""
 I $P(^AUPNVSIT(V,0),U,7)="H" S VAL="H^Hospitalizations"
 I P S VAL="C^Provider Code: 13 PHN"
 I VAL="" S VAL="A^"_$$VAL^XBDIQ1(9000010,V,.08)
 Q 1_U_VAL_U_$S($D(Y):1,1:"")
 ;
PER(N,D) ;return % of n/d
 I 'D Q "0%"
 NEW Z
 S Z=N/D,Z=Z*100,Z=$J(Z,3,0)
 Q $$STRIP^XLFSTR(Z," ")_"%"
 ;----------
C(X,X2,X3) ;
 D COMMA^%DTC
 Q X
PAD(D,L) ; -- SUBRTN to pad length of data
 ; -- D=data L=length
 S L=L-$L(D)
 Q $E($$REPEAT^XLFSTR(" ",L),1,L)_D
 ;
PRINT ;
 S APCLPG=0
 D HEADER
 ;PATIENT COUNT
 W "TOTAL PATIENTS"
 W !,"These counts represent the total number of patients seen for any ambulatory "
 W !,"visit or hospital stay during the report period.  A count by facility is "
 W !,"also provided."
 W !!,"Total Number of Patients seen at any facility:  ",?65,$$C(APCLPTOT,0,7),!
 S APCLL=0 F  S APCLL=$O(APCLPTOT(APCLL)) Q:APCLL'=+APCLL!($D(APCLQUIT))  D
 .I $Y>(IOSL-3) D HEADER Q:$D(APCLQUIT)
 .W !,"Total Number of Patients seen at ",$P(^DIC(4,APCLL,0),U),":  ",?65,$$C(APCLPTOT(APCLL),0,7)
 .Q
 Q:$D(APCLQUIT)
 I $Y>(IOSL-15) D HEADER Q:$D(APCLQUIT)
 W !!,"TOTAL FACILITY VISITS"
 W !,"These counts represent the total number of visits defined as 'surveillance' "
 W !,"visits.  The definition of these visits is the following:"
 W !,"   - a Hospitalization"
 W !,"   - a visit to a PHN"
 W !,"   - an Ambulatory visit (service categories A, O, R, S) to one of the following"
 W !,"     clinics:  01 GENERAL, 06 DIABETIC, 10 GYN, 12 IMMUNIZATION,"
 W !,"               13 INTERNAL MEDICINE, 20 PEDIATRICS, 24 WELL CHILD CARE, "
 W !,"               28 FAMILY PRACTICE, 30 EMERGENCY ROOM, 57 EPSDT, "
 W !,"               70 WOMEN'S HEALTH, 80 URGENT CARE, 89 EVENING"
 W !!,"Table 1:  ILI / H1N1 Visits"
 W !,"This table displays the total number of visits defined above and displays the"
 W !,"total count of those visits on which there was an ILI or H1N1 diagnosis."
 I $Y>(IOSL-3) D HEADER Q:$D(APCLQUIT)
 W !,"The data is broken down by location of encounter and clinic.",!
 W "Note that some patients may have been seen in multiple clinics",!
 W !?40,"Total # Visits",?56,"Total # Visits",?72,"% w/ILI"
 W !?40,"w/ILI/H1N1",?72,"/H1N1 "
 W !,"FACILITY",?40,"Diagnosis",?71,"Diagnosis"
 I APCLLOCT="O" G LOCV
 W !,"ALL FACILITITES COMBINED"
 S APCLCLNT="" F  S APCLCLNT=$O(APCLALLF(APCLCLNT)) Q:APCLCLNT=""!($D(APCLQUIT))  D
 .I $Y>(IOSL-4) D HEADER Q:$D(APCLQUIT)  D SUBHEAD1
 .I APCLCLNT="A" W !?2,"Ambulatory Clinics",!
 .I APCLCLNT'="A" W !
 .S APCLCLN="" F  S APCLCLN=$O(APCLALLF(APCLCLNT,APCLCLN)) Q:APCLCLN=""!($D(APCLQUIT))  D
 ..I $Y>(IOSL-4) D HEADER Q:$D(APCLQUIT)  D SUBHEAD1
 ..W ?3,APCLCLN,?40,$$C($P(APCLALLF(APCLCLNT,APCLCLN),U,2),0,7)
 ..W ?56,$$C($P(APCLALLF(APCLCLNT,APCLCLN),U,1),0,7)
 ..W ?72,$$PER($P(APCLALLF(APCLCLNT,APCLCLN),U,2),$P(APCLALLF(APCLCLNT,APCLCLN),U,1)),!
 .Q
 Q:$D(APCLQUIT)
 W $$REPEAT^XLFSTR("-",79),!
LOCV S APCLLOC="" F  S APCLLOC=$O(APCLVTOT(APCLLOC)) Q:APCLLOC=""!($D(APCLQUIT))  D
 .I $Y>(IOSL-3) D HEADER Q:$D(APCLQUIT)  D SUBHEAD1
 .W !,APCLLOC,?40,$$C($P(APCLVTOT(APCLLOC),U,2),0,7),?56,$$C($P(APCLVTOT(APCLLOC),U,1),0,7),?72,$$PER($P(APCLVTOT(APCLLOC),U,2),$P(APCLVTOT(APCLLOC),U,1)),!
 .S APCLCLNT="" F  S APCLCLNT=$O(APCLVTOT(APCLLOC,APCLCLNT)) Q:APCLCLNT=""!($D(APCLQUIT))  D
 ..I $Y>(IOSL-3) D HEADER Q:$D(APCLQUIT)  D SUBHEAD1
 ..I APCLCLNT="A" W !?2,"Ambulatory Clinics",!
 ..I APCLCLNT'="A" W !
 ..S APCLCLN="" F  S APCLCLN=$O(APCLVTOT(APCLLOC,APCLCLNT,APCLCLN)) Q:APCLCLN=""!($D(APCLQUIT))  D
 ...I $Y>(IOSL-3) D HEADER Q:$D(APCLQUIT)  D SUBHEAD1
 ...W ?3,APCLCLN,?40,$$C($P(APCLVTOT(APCLLOC,APCLCLNT,APCLCLN),U,2),0,7)
 ...W ?56,$$C($P(APCLVTOT(APCLLOC,APCLCLNT,APCLCLN),U,1),0,7)
 ...W ?72,$$PER($P(APCLVTOT(APCLLOC,APCLCLNT,APCLCLN),U,2),$P(APCLVTOT(APCLLOC,APCLCLNT,APCLCLN),U,1)),!
 .W $$REPEAT^XLFSTR("-",79),!
 .Q
 Q:$D(APCLQUIT)
 ;W !,"TOTAL",?40,$$C($P(APCLVTOT,U,2),0,7),?56,$$C($P(APCLVTOT,U,1),0,7),?72,$$PER($P(APCLVTOT,U,2),$P(APCLVTOT,U,1)),!
SRVD ;
 D HEADER Q:$D(APCLQUIT)
 W !,"TABLE 2:  Hospitalizations for Severe Respiratory Disease"
 W !,"This table provides a total number of patients and hospitalization visits "
 W !,"with a documented Severe Respiratory Disease diagnosis.  The definition of"
 W !,"Severe Respiratory Disease include the following ICD9 diagnoses:"
 W !?5," - 481, 482.*, 487.*, 488.1, 518.81, 518.82, 518.84",!
 S APCLTOT="0^0"
 W !?40,"# of Hospitalizations",?68,"# patients"
 W !,?40,"w/Severe Respiratory",!?40,"Disease Diagnosis",!
 S APCLLOC="" F  S APCLLOC=$O(APCLSRDH(APCLLOC)) Q:APCLLOC=""!($D(APCLQUIT))  D
 .I $Y>(IOSL-3) D HEADER Q:$D(APCLQUIT)  D SUBHEAD2
 .W !!,$P(^DIC(4,APCLLOC,0),U)
 .S APCLD="" F  S APCLD=$O(APCLSRDH(APCLLOC,APCLD)) Q:APCLD=""!($D(APCLQUIT))  D
 ..I $Y>(IOSL-3) D HEADER Q:$D(APCLQUIT)  D SUBHEAD2
 ..W !?2,APCLD,?40,$$C($P(APCLSRDH(APCLLOC,APCLD),U,1),0,7),?68,$$C($P(APCLSRDH(APCLLOC,APCLD),U,2),0,7)
 ..S $P(APCLTOT,U,1)=$P($G(APCLTOT),U,1)+$P(APCLSRDH(APCLLOC,APCLD),U,1)
 ..S $P(APCLTOT,U,2)=$P($G(APCLTOT),U,2)+$P(APCLSRDH(APCLLOC,APCLD),U,2)
 Q:$D(APCLQUIT)
 W !!?2,"TOTAL",?40,$$C($P(APCLTOT,U,1),0,7),?68,$$C($P(APCLTOT,U,2),0,7)
ILISEX ;
 D ILISEX^APCLSILT
 Q:$D(APCLQUIT)
ILIAVM ;
 D ILIAVM^APCLSILT
 Q:$D(APCLQUIT)
 D ILIAGEP^APCLSILT
 Q:$D(APCLQUIT)
 D VACAGEP^APCLSILT
 Q:$D(APCLQUIT)
 D TAB7^APCLSILT
 Q:$D(APCLQUIT)
 D TAB8^APCLSILT
 Q:$D(APCLQUIT)
 D TAB9^APCLSILT
 Q
SUBHEAD2 ;
 W "Table 2:  Hospitalizations for Severe Respiratory Disease",!
 W !?40,"# of Hospitalizations",?68,"# patients"
 W !,?40,"w/Severe Respiratory",!?40,"Disease Diagnosis",!
 Q
 ; 
SUBHEAD1 ;
 W !,"Table 1:  ILI / H1N1 Visits"
 W !?40,"Total # Visits",?56,"Total # Visits",?72,"% w/ILI"
 W !?40,"w/ILI/H1N1",?72,"/H1N1 "
 W !,"FACILITY",?40,"Diagnosis",?71,"Diagnosis"
 W !
 Q
HEADER ;EP - report header
 I 'APCLPG G HEAD1
 I $E(IOST)="C",IO=IO(0) W ! S DIR(0)="EO" D ^DIR K DIR I Y=0!(Y="^")!($D(DTOUT)) S APCLQUIT="" Q
HEAD1 ;
 I APCLPG W:$D(IOF) @IOF
 S APCLPG=APCLPG+1
 W ?3,$P(^DIC(4,DUZ(2),0),U),?58,$$FMTE^XLFDT(DT),?72,"Page ",APCLPG,!
 W !,$$CTR("*** Aggregate ILI/H1N1/Surveillance Report ***",80),!
 S X="Visit Dates: "_$$FMTE^XLFDT(APCLBD)_" through "_$$FMTE^XLFDT(APCLED) W $$CTR(X,80),!
 W $$REPEAT^XLFSTR("-",79),!!
 Q
 ;
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
