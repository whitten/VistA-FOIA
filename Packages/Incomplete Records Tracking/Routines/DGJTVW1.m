DGJTVW1 ;ALB/MAF - DISPLAY SCREENS FOR INCOMPLETE RECORDS TRACKING (LIST PROCESSOR) CONT. ; SEP 31,1992@900
 ;;5.3;Registration;;Aug 13, 1993
EN Q:'$D(^VAS(393,+$P(DGJTEDT,"^",2),0))  S DGJTNO=^VAS(393,$P(DGJTEDT,"^",2),0),DFN=+DGJTNO
 I $D(^VAS(393,$P(DGJTEDT,"^",2),"DT")) S DGJTNDT=^VAS(393,$P(DGJTEDT,"^",2),"DT")
 I '$D(^VAS(393,$P(DGJTEDT,"^",2),"DT")) S DGJTNDT="^^^^^^^^^^"
 S X=$P(^VAS(393,$P(DGJTEDT,"^",2),0),"^",6) S DGJTDEL=$S($D(^DG(40.8,+X,"DT")):^("DT"),1:DGJTDEL)
 S DGJTFL=0,DGJTHDR="INCOMPLETE RECORDS TRACKING "_$S($D(DGJTVIEW):"<View>",1:"<Edit>"),$P(DGJTCL,"=",81)="",DGJTNM=$P(^DPT(+DGJTNO,0),"^",1) D PID^VADPT6 S DGJTPTID=VA("PID") K VA
 D ^DGJTVW2
 S RTE=DFN_";DPT(",RTYPE=1 D LATEST^RTUTL3
 S X=""
 S DGJVAL="          "_$S('$D(^XUSEC("DGJ TS UPDATE",DUZ))&($P(DGJTNO,"^",2)=$O(^VAS(393.3,"B","DISCHARGE SUMMARY",0)))&('$D(DGJTVIEW)):"*",1:" ")_"Specialty: "
 S X=$$SETSTR^VALM1(DGJVAL,X,1,22)
 S DGJVAL=$P(DGJTNO,"^",7) S DGJVAL=$S($D(^DIC(45.7,+DGJVAL,0)):$P(^(0),"^"),1:"")
 S X=$$SETSTR^VALM1(DGJVAL,X,23,18)
 S DGJVAL=$P(RTDATA,"^",2)
 S X=$$SETSTR^VALM1($S($D(DGJTVIEW):"           Borrower: ",1:"          *Borrower: "),X,42,21)
 S X=$$SETSTR^VALM1(DGJVAL,X,63,18) D TMP^DGJTVW2
 S DGJVAL="  "_$S('$D(^XUSEC("DGJ TS UPDATE",DUZ))&($P(DGJTNO,"^",2)=$O(^VAS(393.3,"B","DISCHARGE SUMMARY",0)))&('$D(DGJTVIEW)):"*",1:" ")_"Primary Physician: "
 S X=""
 S X=$$SETSTR^VALM1(DGJVAL,X,1,22)
 S DGJVAL=$P(DGJTNO,"^",9) S DGJVAL=$S($D(^VA(200,+DGJVAL,0)):$P(^(0),"^"),1:"")
 S X=$$SETSTR^VALM1(DGJVAL,X,23,18)
 S DGJVAL=$P(RTDATA,"^",3)
 S X=$$SETSTR^VALM1($S($D(DGJTVIEW):"           Phone/Rm: ",1:"          *Phone/Rm: "),X,42,21)
 S X=$$SETSTR^VALM1(DGJVAL,X,63,18) D TMP^DGJTVW2
 I $P(DGJTDEL,"^",3)=1!($P(DGJTDEL,"^",3)=0&($P(DGJTDEL,"^",10)="A")) S DGJVAL=$S('$D(^XUSEC("DGJ TS UPDATE",DUZ))&($P(DGJTNO,"^",2)=$O(^VAS(393.3,"B","DISCHARGE SUMMARY",0)))&('$D(DGJTVIEW)):"*",1:" ")_"Attending Physician: "
 S X=""
 I $P(DGJTDEL,"^",3)=1!($P(DGJTDEL,"^",3)=0&($P(DGJTDEL,"^",10)="A")) S X=$$SETSTR^VALM1(DGJVAL,X,1,22)
 I $P(DGJTDEL,"^",3)=1!($P(DGJTDEL,"^",3)=0&($P(DGJTDEL,"^",10)="A")) S DGJVAL=$P(DGJTNO,"^",10) S DGJVAL=$S($D(^VA(200,+DGJVAL,0)):$P(^(0),"^"),1:"")
 I $P(DGJTDEL,"^",3)=1!($P(DGJTDEL,"^",3)=0&($P(DGJTDEL,"^",10)="A")) S X=$$SETSTR^VALM1(DGJVAL,X,23,18)
 S DGJVAL=$P(RTDATA,"^",4),Y=DGJVAL I DGJVAL]"" X ^DD("DD") S DGJVAL=Y
 S X=$$SETSTR^VALM1($S($D(DGJTVIEW):"       Date Charged: ",1:"      *Date Charged: "),X,42,21)
 S X=$$SETSTR^VALM1(DGJVAL,X,63,18) D TMP^DGJTVW2
 K RTDATA,RTE,RTYPE
 D CODDT^DGJTVW,CODBY^DGJTVW
 I '$D(^VAS(393,$P(DGJTEDT,"^",2),"MSG")) S X="",X=$$SETSTR^VALM1("4)",X,1,2) D TMP^DGJTVW2 S X="",X=$$SETSTR^VALM1("Comments:",X,1,9) D TMP^DGJTVW2 G STAT
 D COM^DGJTVW
STAT S:'$D(DGJTVIEW) X="",X=$$SETSTR^VALM1("* For display only!",X,1,19) D:'$D(DGJTVIEW) TMP^DGJTVW2
 D STAT1^DGJTVW
 K DGJTSF
 I $D(DGJTVIEW) K DGJTVIEW Q
 Q
REDSP S VALMBCK="R" D EN Q
ALLEDIT ;Edit all 4 groups
 I $P(DGJTNO,"^",2)'=$O(^VAS(393.3,"B","DISCHARGE SUMMARY",0)) F DGJTNUM=1:1:4 S:DGJTNUM=3 X=3 D EDIT  Q:'$D(DGJTUP)!(X="^")!($D(DTOUT))  K DGJTUP
 I $P(DGJTNO,"^",2)=$O(^VAS(393.3,"B","DISCHARGE SUMMARY",0)) F DGJTNUM=1,3,4,2 S:DGJTNUM=3 X=3 D EDIT  Q:'$D(DGJTUP)!(X="^")!($D(DTOUT))  K DGJTUP
 Q
EDIT ;EDIT
 D FULL^VALM1
 I DGJTNUM["2",$P(DGJTNO,"^",2)=$O(^VAS(393.3,"B","DISCHARGE SUMMARY",0)) D CK,EN S VALMBCK="R" Q
 D EDIT1
 D:'$D(DGJTDEF) EN D:$D(DGJTDEF) ^DGJTVW3 S VALMBCK="R" Q
QUIT1 K %,D,DA,D0,DIE,DR,VA,DGJT1,DGJTCL,DGJTEDT,DGJTHDR,DGJTNDT,DGJTNM,DGJTNO,DGJTNST,DGJTNUM,DGJTNUM1,DGJTPTID,DGJTRC,DGJTUP,DGJTXX,DGJX,DGX,DGJTYPX,Y,^TMP("DGJRPT",$J),^TMP("RPTIDX",$J) Q
DISTS S DGPMT=6,DGPMCA=$P(DGJTNO,"^",4) D CA^DGPMV S:Y]"" DGJTUP=1 Q:$D(DGJTSEDT)  D STAT1^DGJTVW Q
CK S DGJTNUM1=DGJTNUM I DGJTNUM["1" S DGJTNUM=1 D EDIT1 S DGJTNUM=DGJTNUM1 Q:'$D(DGJTUP)  K DGJTUP
 I '$D(^XUSEC("DGJ TS UPDATE",DUZ)) W !!,"A security key must be issued to edit data in item 2." R !!,"Hit return to continue ",X:DTIME Q:X="^"!('$T)  G CK1
 S DGJT1=2 D 1^DGJTVW
 I DGJVAL]"" S Y=X X ^DD("DD") W !!,"PTF Record was closed on "_Y_" for patient.... You Must",!,"reopen the record before you can enter any changes for group 2",! S DGJTNUM1=DGJTNUM R !!,"Hit return to continue ",X:DTIME Q:X="^"!('$T)  G CK1
 I DGJTNUM1[2 D MESS S DGJTNUM=2 D DISTS S DGJTNUM=DGJTNUM1 Q:'$D(DGJTUP)
CK1 I DGJTNUM1[3 S DGJTNUM=3 D EDIT1 Q:'$D(DGJTUP)  K DGJTUP
 S DGJTNUM=DGJTNUM1 I DGJTNUM1[4 S DGJTNUM=4 D EDIT1 S DGJTNUM=DGJTNUM1
 Q
EDIT1 S DIE="^VAS(393,",DA=$P(DGJTEDT,"^",2),DR="[DGJ EDIT IRT RECORD]" D ^DIE K DR,DIC("S")
 K DR Q
MESS W !!?3,"When editing this section you must edit/create a new Treating Specialty"
 W !,"Entering '^' at any prompt will exit you out of the "
 W "treating Specialty edit only"
