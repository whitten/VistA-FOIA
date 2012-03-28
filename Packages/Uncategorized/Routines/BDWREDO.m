BDWREDO ; IHS/CMI/LAB - REDO A RUN ;
 ;;1.0;IHS DATA WAREHOUSE;**2**;JAN 23, 2006
START ;
 D EN^XBVK("BDW")
 I $D(^BDWTMP) W !!,"Previous run not completed." Q
 S BDWO("RUN")="REDO" ;     Let ^BDWRDRI know this is a 'REDO'
 S BDWO("RUN TYPE")="REX"
 D ^BDWRDRI ;           
 I BDW("QFLG")=66 W:'$D(ZTQUEUED) !,"Contact your site manager.  ^BDWTMP still exists." D  D EOJ Q
 .S DIR(0)="EO",DIR("A")="Press any key to continue" K DA D ^DIR K DIR
 I BDW("QFLG") D EOJ W !!,"Bye",!! Q
 D INIT ;               Get Log entry to redo
 I BDW("QFLG") D EOJ W !!,"Bye",!! Q
 D QUEUE^BDWRDRI
 I BDW("QFLG") D EOJ W !!,"Bye",!! Q
 I $D(BDWO("QUEUE")) D EOJ W !!,"Okay your request is queued!",!! Q
 ;
EN ;EP FROM TASKMAN
 S BDWLOG=BDW("RUN LOG")
 S BDWCNT=$S('$D(ZTQUEUED):"X BDWCNT1  X BDWCNT2",1:"S BDWCNTR=BDWCNTR+1"),BDWCNT1="F BDWCNTL=1:1:$L(BDWCNTR)+1 W @BDWBS",BDWCNT2="S BDWCNTR=BDWCNTR+1 W BDWCNTR,"")"""
 D NOW^%DTC S BDW("RUN START")=%,BDW("MAIN TX DATE")=$P(%,".") K %,%H,%I
 D ^XBFMK S DA=BDWLOG,DIE="^BDWXLOG(",DR=".03////"_BDW("RUN START")_";.15///R"_";.22///1" D ^DIE,^XBFMK
 S BDW("BT")=$HOROLOG
 D PROCESS ;            Generate transactions
 I BDW("QFLG") W:'$D(ZTQUEUED) !!,"Abnormal termination!  QFLG=",BDW("QFLG") D:$D(ZTQUEUED) ABORT D EOJ Q
 D LOG ;                Update Log entry
 I BDW("QFLG") W:'$D(ZTQUEUED) !!,"Log error! ",BDW("QFLG") D:$D(ZTQUEUED) ABORT D EOJ Q
 D RUNTIME^BDWRDR
 S BDWMSGT=$$DW1TRLR^BHLEVENT(90213,BDW("RUN LOG"))
 S ^BDWTMP(BDWIEDST,BDWMSGT)=""
 S DA=BDW("RUN LOG"),DIE="^BDWXLOG(",DR=".13////"_BDWRUN_";.14////"_BDWMSGT_";.15////C" D ^DIE
 D:'$D(ZTQUEUED) CHKLOG ;             See if Log needs cleaning
 D RESETV ;             Reset VISITs processed in Log
 I '$D(ZTQUEUED) S DIR(0)="EO",DIR("A")="DONE -- Press ENTER to Continue" K DA D ^DIR K DIR
 D EOJ
 Q
 ;
PROCESS ;
 D GIS^BDW1VBLI
 S BDWMSGH=$$DW1HDR^BHLEVENT(90213,BDW("RUN LOG"))
 S ^BDWTMP(BDWIEDST,BDWMSGH)=""
 D ^XBFMK S DA=BDWLOG,DIE="^BDWXLOG(",DR=".12////"_BDWMSGH D ^DIE,^XBFMK
 S ^XTMP("BDWREDO",0)=$$FMADD^XLFDT(DT,14)_U_DT_U_"DATA WAREHOUSE EXPORT REDO" ;IHS/CMI/LAB
 W:'$D(ZTQUEUED) !,"Generating transactions.  Counting encounters.  (1)" S BDWCNTR=0
 S BDWDFN=0 F  S BDWDFN=$O(^BDWXLOG(BDW("RUN LOG"),41,BDWDFN)) Q:BDWDFN'=+BDWDFN  D
 .K INA("DELETE"),BDWDELF,BDWDELT
 .I '$D(^AUPNPAT(BDWDFN,0)) Q
 .I '$D(^DPT(BDWDFN,0)) Q
 .S BDWDELF=$P(^BDWXLOG(BDW("RUN LOG"),41,BDWDFN,0),U,13),BDWDELT=$P(^DPT(BDWDFN,0),U,19)
 .I 'BDWDELF,'$$ORF^BDWUTIL1(BDWDFN) Q
 .K BDWR S Y=^BDWXLOG(BDW("RUN LOG"),41,BDWDFN,0),BDWR("BASE")=$P(Y,U,2),BDWR("DEMO")=$P(Y,U,3),BDWR("ALIAS")=$P(Y,U,4),BDWR("CHART")=$P(Y,U,5),BDWR("ELIG")=$P(Y,U,6)
 .I BDWDELF S INA("DELETE")=BDWDELT
 .S INA=0
 .I BDWDELF D
 ..S INA("DELETE")=BDWDFN
 ..S BDWM=$$DW1MRG^BHLEVENT(BDWDELT,.INA)
 .I 'BDWDELF D
 ..S BDWM=$$DW1REG^BHLEVENT(BDWDFN,.INA)
 .S BDW("REG")=BDW("REG")+1,^BDWTMP(BDWIEDST,BDWM)=""
 .S $P(^BDWXLOG(BDW("RUN LOG"),41,BDWDFN,0),U,7)=BDWM
 .K INA("DELETE"),BDWDELT,BDWDELF
 K ^BDWXLOG(BDW("RUN LOG"),51)  ;clean out old error log
 S BDW("V DFN")=0 F  S BDW("V DFN")=$O(^BDWXLOG(BDW("RUN LOG"),21,BDW("V DFN"))) Q:BDW("V DFN")'=+BDW("V DFN")  D PROCESS2 Q:BDW("QFLG")
 Q
PROCESS2 ;
 K BDWE,BDWV
 X BDWCNT
 S ^XTMP("BDWREDO","MAIN TX",BDW("V DFN"))="",BDWV("TX GENERATED")=0
 I '$D(^AUPNVSIT(BDW("V DFN"))) Q
 S BDW("VPROC")=BDW("VPROC")+1
 S BDWV("V REC")=^AUPNVSIT(BDW("V DFN"),0)
 S BDWV("V DATE")=+BDWV("V REC")\1
 I $P(BDWV("V REC"),U,11),$P($G(^AUPNVSIT(BDW("V DFN"),11)),U,6)="" D  G SETUTIL
 .S BDWE("ERROR")=100 D ^BDWRERR
 K BDWVMSG D ^BDWRDR2
SETUTIL S ^XTMP("BDWREDO",BDW("V DFN"))=BDW("V DFN")_U_BDWV("TX GENERATED")_U_$G(BDWVMSG)
 Q
 ;
 ;
CHKLOG ; CHECK LOG FILE
 S BDW("X")=0 F BDW("I")=BDW("RUN LOG"):-1:1 Q:'$D(^BDWXLOG(BDW("I")))  I $O(^BDWXLOG(BDW("I"),21,0)) S BDW("X")=BDW("X")+1
 I BDW("X")>3 W !!,"-->There are more than three generations of VISITs stored in the LOG file.",!,"-->Time to do a purge."
 Q
 ;
RESETV ; RESET VISIT DATA IN LOG
 W:'$D(ZTQUEUED) !,"Resetting VISIT specific data in Log file.  (1)" S BDWCNTR=0
 S BDW("X")=0 F  S BDW("X")=$O(^XTMP("BDWREDO",BDW("X"))) Q:BDW("X")'=+BDW("X")  S BDW("Y")=^(BDW("X")),^BDWXLOG(BDW("RUN LOG"),21,BDW("X"),0)=BDW("Y") X BDWCNT ;FORGIVE ME LORD
 W:'$D(ZTQUEUED) !,"Resetting VISIT TX Flags. (1)" S BDWCNTR=0
 S BDW("X")=0 F  S BDW("X")=$O(^XTMP("BDWREDO","MAIN TX",BDW("X"))) Q:BDW("X")'=+BDW("X")  D
 .S DIE="^AUPNVSIT(",DA=BDW("X"),DR="1106///"_$S(^XTMP("BDWREDO","MAIN TX",BDW("X"))]"":^XTMP("BDWREDO","MAIN TX",BDW("X")),1:"@") D ^DIE K DA,DR X BDWCNT
 .Q
 K ^XTMP("BDWREDO")
 Q
 ;
LOG ; UPDATE LOG
 S BDW("COUNT")=BDW("REG")+BDW("VISITS") W:'$D(ZTQUEUED) !!,BDW("COUNT")," HL7 Messages were generated."
 W:'$D(ZTQUEUED) !,"Updating log entry."
 D NOW^%DTC S BDW("RUN STOP")=%
 S DA=BDW("RUN LOG"),DIE="^BDWXLOG(",DR=".04////"_BDW("RUN STOP")_";.05////"_BDW("SKIP")_";.06////"_BDW("COUNT")_";.08///"_BDW("VPROC") D ^DIE I $D(Y) S BDW("QFLG")=26 Q
 K DIE,DA,DR
 S DA=BDW("RUN LOG"),DIE="^BDWXLOG(",DR=".11////"_BDW("REG")_";.12////"_BDWMSGH_";.18////"_$G(BDW("VISITS"))_";.23///REX" D ^DIE I $D(Y) S BDW("QFLG")=26 Q
 K DR,DIE,DA,DIV,DIU
 S DIE="^BDWXLOG(",DA=BDW("RUN LOG"),DR="3101////"_BDW("DEMO")_";3102////"_BDW("ZERO")_";3103////"_BDW("DEL")_";3104////"_BDW("NO PAT")_";3105////"_BDW("NO LOC")_";3106////"_BDW("NO TYPE")_";3107////"_BDW("NO CAT")_";3111////"_BDW("MFI")
 D ^DIE I $D(Y) S BDW("QFLG")=26 Q
 S DA=BDW("RUN LOG"),DIK="^BDWXLOG(" D IX1^DIK K DA,DIK
 D ^XBFMK
 ;
 Q
INIT ;
 D INIT^BDWRED1
 Q
ABORT ; ABNORMAL TERMINATION
 I $D(BDW("RUN LOG")) S BDW("QFLG1")=$O(^BDWERRC("B",BDW("QFLG"),"")),DA=BDW("RUN LOG"),DIE="^BDWXLOG(",DR=".15///F;.16////"_BDW("QFLG1")
 I $D(ZTQUEUED) D ERRBULL^BDWRDRI3,EOJ Q
 W !!,"Abnormal termination!!  QFLG=",BDW("QFLG")
 S DIR(0)="EO",DIR("A")="Press any key to continue" K DA D ^DIR K DIR
 Q
 ;
EOJ ;
 K AUPNVSIT
 D EN^XBVK("BDW"),KILL^AUPNPAT
 I $D(ZTQUEUED) S ZTREQ="@"
 Q
