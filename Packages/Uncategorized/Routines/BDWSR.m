BDWSR ; IHS/CMI/LAB - DW REPORT ;
 ;;1.0;IHS DATA WAREHOUSE;;JAN 23, 2006
 ;IHS/CMI/LAB - changed TMP to XTMP
 ;
START ;
 W:$D(IOF) @IOF
 W !,"**********   DATA WAREHOUSE EXPORT SUMMARY REPORT   **********",!
 W !!,"This report presents a summary of data for a single Data Warehouse ",!,"Export Log entry.",!!
LOG ;
 S BDWSR("LOG")=""
 K DIC S DIC="^BDWXLOG(",DIC(0)="AEQM" D ^DIC I Y<0 G XIT
 S BDWSR("LOG")=+Y
 I $P(^BDWXLOG(BDWSR("LOG"),0),U,7)'="R" W !!,"This report can only be run on regular exports, not date range exports.",! G LOG
 I '$D(^BDWXLOG(BDWSR("LOG"),21)) W !!,"Visit data has already been purged!!" G LOG
 S X=^BDWXLOG(BDWSR("LOG"),0),BDWSR("RUN BEGIN")=$P(X,U),BDWSR("RUN END")=$P(X,U,2),BDWSR("COUNT")=$P(X,U,6),BDWSR("ORIG TX DATE")=$P($P(X,U,3),".")
 S Y=BDWSR("RUN BEGIN") X ^DD("DD") S BDWSR("PRINT BEGIN")=Y
 S Y=BDWSR("RUN END") X ^DD("DD") S BDWSR("PRINT END")=Y
 S BDWSR("VISITS")=$P(^BDWXLOG(BDWSR("LOG"),21,0),U,4)
 W !!,"Log entry ",BDWSR("LOG"),", was for date range ",BDWSR("PRINT BEGIN")," through",!,BDWSR("PRINT END")," and exported the following:",!
 W ?5,"Patient Registration updates:  ",$P(^BDWXLOG(BDWSR("LOG"),0),U,11)
 W !?5,"              PCC Encounters:  ",$P(^BDWXLOG(BDWSR("LOG"),0),U,18)
 W !?5," Total transactions exported:  ",$P(^BDWXLOG(BDWSR("LOG"),0),U,6)
 S Y=DT D DD^%DT S BDWSR("DTP")=Y
 S BDWS("PROV FILE")=$S($P(^DD(9000010.06,.01,0),U,2)[200:200,1:6)
ZIS ;call to XBDBQUE
 S XBRP="PRINT^BDWSR",XBRC="PROCESS^BDWSR",XBRX="XIT^BDWSR",XBNS="BDW"
 D ^XBDBQUE
 D XIT
 Q
PROCESS ; Entry point for Taskman
 S BDWJOB=$J,BDWBTH=$H
 K ^XTMP("BDWSR",BDWJOB,BDWBTH),^XTMP("BDWSRP",BDWJOB,BDWBTH)
 S ^XTMP("BDWSR",0)=$$FMADD^XLFDT(DT,14)_"^"_DT_"^"_"DATA WAREHOUSE SUM REPORT"
 S BDWSR("BT")=$H
 ;D CHKSITE^BDWRDRI
 S APCDOVRR=1,BDWSR="BDWSR"
 D V^BDWSR1
 D SET
 S BDWSR("ET")=$H
 Q
PRINT ;EP
 D ^BDWSRP
 K ^XTMP("BDWSR",BDWJOB,BDWBTH),^XTMP("BDWSRP",BDWJOB,BDWBTH)
 Q
XIT ;
 K DA,DIE,DIC,POP,DIR
 D EN^XBVK("BDW")
 Q
SET ;EP
 S BDWSR("1")="ERRORSC",BDWSR("3")="ERRORS" D SET1
 S BDWSR("1")="PROVC",BDWSR("3")="PROV" D SET2
 S BDWSR("1")="TYPEC",BDWSR("3")="TYPE" D SET2
 S BDWSR("1")="SCC",BDWSR("3")="SC" D SET2
 S BDWSR("1")="CLINICC",BDWSR("3")="CLINIC" D SET2
 S BDWSR("1")="LOCC",BDWSR("3")="LOC" D SET2
 S BDWSR("1")="PROV REM CC",BDWSR("3")="PROV REM" D SET1
 S BDWSR("1")="CLINIC REM CC",BDWSR("3")="CLINIC REM" D SET1
 S BDWSR("1")="LOC REM CC",BDWSR("3")="LOC REM" D SET1
 S BDWSR("1")="SC REM CC",BDWSR("3")="SC REM" D SET1
 S BDWSR("1")="TYPE REM CC",BDWSR("3")="TYPE REM" D SET1
 S BDWSR("1")="PROV CHA CC",BDWSR("3")="PROV CHA" D SET2
 S BDWSR("1")="CLINIC CHA CC",BDWSR("3")="CLINIC CHA" D SET2
 S BDWSR("1")="LOC CHA CC",BDWSR("3")="LOC CHA" D SET2
 S BDWSR("1")="SC CHA CC",BDWSR("3")="SC CHA" D SET2
 S BDWSR("1")="TYPE CHA CC",BDWSR("3")="TYPE CHA" D SET2
 S BDWSR("1")="PROV STATDB CC",BDWSR("3")="PROV STATDB" D SET2
 S BDWSR("1")="CLINIC STATDB CC",BDWSR("3")="CLINIC STATDB" D SET2
 S BDWSR("1")="LOC STATDB CC",BDWSR("3")="LOC STATDB" D SET2
 S BDWSR("1")="SC STATDB CC",BDWSR("3")="SC STATDB" D SET2
 S BDWSR("1")="TYPE STATDB CC",BDWSR("3")="TYPE STATDB" D SET2
 S BDWSR("1")="TYPE ERROR CC",BDWSR("3")="TYPE ERROR" D SET1
 S BDWSR("1")="CLINIC ERROR CC",BDWSR("3")="CLINIC ERROR" D SET1
 S BDWSR("1")="PROV HOSP CC",BDWSR("3")="PROV HOSP" D SET2
 S BDWSR("1")="TYPE HOSP CC",BDWSR("3")="TYPE HOSP" D SET2
 S BDWSR("1")="SC HOSP CC",BDWSR("3")="SC HOSP" D SET2
 S BDWSR("1")="LOC HOSP CC",BDWSR("3")="LOC HOSP" D SET2
 S BDWSR("1")="SC ERROR CC",BDWSR("3")="SC ERROR" D SET1
 Q
SET1 ;
 S BDWSR("PROC")=""""_BDWSR_""",BDWJOB,BDWBTH,"_""""_"SKIPPED"_""""
 S BDWSR("2")="^XTMP("_BDWSR("PROC")_","""_BDWSR("3")_""",X)"
 S X="" F  S X=$O(@BDWSR("2")) Q:X=""  S %=^(X) S ^XTMP("BDWSR",BDWJOB,BDWBTH,"SKIPPED",BDWSR("1"),9999999-%,X)=%
 Q
SET2 ;
 S BDWSR("PROC")=""""_BDWSR_""",BDWJOB,BDWBTH,"_""""_"GEN"_""""
 S BDWSR("2")="^XTMP("_BDWSR("PROC")_","""_BDWSR("3")_""",X)"
 S X="" F  S X=$O(@BDWSR("2")) Q:X=""  S %=^(X) S ^XTMP("BDWSR",BDWJOB,BDWBTH,"GEN",BDWSR("1"),9999999-%,X)=%
 Q
