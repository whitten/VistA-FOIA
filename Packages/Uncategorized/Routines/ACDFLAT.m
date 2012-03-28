ACDFLAT ;IHS/ADC/EDE/KML - GENERATE FLAT FILE;
 ;;4.1;CHEMICAL DEPENDENCY MIS;;MAY 11, 1998
 ;
 ; This routine generates a flat ascii file for cdmis visits
 ; by a date range entered by the user.
 ;
START ;
 D INIT
 I ACDQ D EOJ Q
 ;D VISITLP
 I ACDQ D EOJ Q
 D PRVNTLP
 I ACDQ D EOJ Q
 D GENFILE
 D EOJ
 Q
 ;
INIT ; INITIALIZATION
 K ^TMP("ACDFLAT",$J)
 S ACDQ=1
 I '$G(ACDPGM) W !!,"No Program Specified.",!! Q
 I $G(ACD6DIG)="" S ACD6DIG=$P($G(^AUTTLOC(ACDPGM,0)),U,10) I $L(ACD6DIG)'=6 W !!,"Invalid ASUFAC code for Program.",!! Q
 W !!,"This routine will generate a flat ascii file containing CDMIS visits within",!,"the date range you specify.",!
 D GETDTR^ACDDEU
 Q:ACDQ
 S ACDRCTR=0
 S ACDQ=0
 D FILEMSG
 Q
 ;
FILEMSG ; GENERATE FILE NAME AND TELL USER
 S ACDFILE="ACD"_DUZ_"."
 S X2=$E(DT,1,3)_"0101",X1=DT D ^%DTC S ACDJD=X+1
 S ACDFILE=ACDFILE_ACDJD
 W !!,"I am going to create a file called ",ACDFILE," which will reside in ",!,"the ",$S($P(^AUTTSITE(1,0),U,21)=1:"/usr/spool/uucppublic",1:"C:\EXPORT")," directory.",!
 W !,"Actually, the file will be placed in the same directory that the data export"
 W !,"globals are placed.  See your site manager for assistance in finding the file",!,"after it is created.  PLEASE jot down and remember the following file name:",!?15,"**********    ",ACDFILE,"    **********",!
 W !,"As a reminder, the records that are generated and placed in file ",ACDFILE,!
 W "are in a standard, pre-defined record format.  For a definition of the format",!,"please see your user manual.",!
 S DIR(0)="Y",DIR("A")="Is everything ok?  Do you want to continue?",DIR("B")="Y" K DA D ^DIR K DIR
 I $D(DIRUT)!(Y'=1) S ACDQ=1 Q
 Q
 ;
VISITLP ; LOOP ON VISITS BY DATE UNTIL DONE
 S ACDVDT=$O(^ACDVIS("B",ACDDTLO),-1)
 F  S ACDVDT=$O(^ACDVIS("B",ACDVDT)) Q:ACDVDT=""!(ACDVDT>ACDDTHI)  S ACDVIEN=0 F  S ACDVIEN=$O(^ACDVIS("B",ACDVDT,ACDVIEN)) Q:'ACDVIEN  D:$P($G(^ACDVIS(ACDVIEN,"BWP")),U)=ACDPGM BLDRECV
 Q
 ;
BLDRECV ; BUILD FLAT RECORD FROM ONE CDMIS VISIT
 S C=$$FLAT^ACDFLAT2(ACDVIEN,.ACDFREC)
 I C F Y=1:1:C D
 . S ACDRCTR=ACDRCTR+1
 . S ^TMP("ACDFLAT",$J,ACDRCTR)=ACDFREC(Y)
 .;D:$E(ACDFREC(Y),18,19)="IN" EP^XBCLM(ACDFREC(Y)) ;*****DEBUG*****
 . W:'(ACDRCTR#100) "."
 . Q
 Q
 ;
PRVNTLP ; LOOP ON PREVENTIONS BY DATE UNTIL DONE
 S ACDPDT=$O(^ACDPD("B",ACDDTLO),-1)
 F  S ACDPDT=$O(^ACDPD("B",ACDPDT)) Q:ACDPDT=""!(ACDPDT>ACDDTHI)  S ACDPIEN=0 F  S ACDPIEN=$O(^ACDPD("B",ACDPDT,ACDPIEN)) Q:'ACDPIEN  D:$P($G(^ACDPD(ACDPIEN,0)),U,4)=ACDPGM BLDRECP
 Q
 ;
BLDRECP ; BUILD FLAT RECORD FROM ONE CDMIS PREVENTION
 S C=$$FLAT^ACDFLAT4(ACDPIEN,.ACDFREC)
 I C F Y=1:1:C D
 . S ACDRCTR=ACDRCTR+1
 . S ^TMP("ACDFLAT",$J,ACDRCTR)=ACDFREC(Y)
 . D:$E(ACDFREC(Y),18,19)="IN" EP^XBCLM(ACDFREC(Y)) ;*****DEBUG*****
 . W:'(ACDRCTR#100) "."
 . Q
 Q
 ;
GENFILE ; GENERATE FLAT ASCII FILE
 K ^TMP($J,"ACDFLAT")
 S %X="^TMP(""ACDFLAT"",$J,"
 S %Y="^TMP($J,""ACDFLAT"","
 D %XY^%RCR
 K %X,%Y
 ;
 S XBGL="TMP("_$J_",""ACDFLAT"","
 S XBMED="F",XBFN=ACDFILE,XBTLE="SAVE OF CDMIS RECORDS FOR DATE RANGE "_ACDDTLO_"-"_ACDDTHI_" GENERATED BY -"_$P($G(^VA(200,DUZ,0)),U)
 S XBF=0,XBQ="N",XBFLT=1,XBE=$J
 D ^XBGSAVE
 ;check for error
 K ^TMP($J,"ACDFLAT")
 K XBGL,XBMED,XBTLE,XBFN,XBF,XBQ,XBFLT
 D PAUSE^ACDDEU
 Q
 ;
EOJ ;
 K ^TMP("ACDFLAT",$J)
 D ^ACDKILL
 Q
