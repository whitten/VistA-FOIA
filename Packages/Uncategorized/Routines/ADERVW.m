ADERVW ; IHS/HQT/MJL - DENTAL CHART REVW PART 1 ;  [ 03/24/1999   9:04 AM ]
 ;;6.0;ADE;;APRIL 1999
 G EN1
START N ADEADA,ADEBRK,ADEC,ADECAT,ADECKP,ADECVD,ADEDAT,ADEDFN,ADEDLM,ADEDLT,ADEDTD,ADEDTU,ADEF,ADEFLG,ADEFO,ADEHD2,ADEHDR,ADEICL,ADEILN
 N ADEMDFN,ADENDM,ADENDN,ADENFLG,ADENOD,ADENRQ,ADENSH,ADEOP,ADEPG,ADEPRT,ADEQ,ADER,ADESFC,ADESFN,ADESITE,ADESITX,ADESUB,ADESUBD,ADESVD
 N ADETXT,ADETYP,ADETYPN,ADEX,ADEY
 N ADECNT,ADEDES,ADEJ,ADENOT,ADEPC,ADEPRV,ADEREP,ADEV,ADENDMS
 D INIT
 ;------->LIST FOLLOWUP, FAILED APPTS
 D ^ADERVW2
 ;------->LIST DENTAL PROCEDURES
 D ^ADERVW1
 ;------->CALL TURNAROUND FORM FROM HERE
 D:$D(ADETUR) ^ADETUR
 ;------->RETURN TO CALLING ROUTINE
END ;EP
 I $D(IOST)=11,'ADEPCC,$P(IOST,"-")="C" S ADEHDR="",%="END CONFIDENTIAL PATIENT DATA",$P(ADEHDR,"*",(IOM-$L(%)/2))=%,$P(ADEHDR,"*",IOM-$L(%))="" W *7,ADEHDR,!,"<>" R X:DTIME
KILL I 'ADEPCC D ^%ZISC K APCHSQIT,APCHSNPG
 K ADEADA,ADEBRK,ADEC,ADECAT,ADECKP,ADECVD,ADEDAT,ADEDFN,ADEDLM,ADEDLT,ADEDTD,ADEDTU,ADEF,ADEFLG,ADEFO,ADEHD2,ADEHDR,ADEICL,ADEILN
 K ADEMDFN,ADENDM,ADENDN,ADENOD,ADENRQ,ADENSH,ADEOP,ADEPG,ADEPRT,ADEQ,ADER,ADESFC,ADESFN,ADESITE,ADESITX,ADESUB,ADESUBD,ADESVD
 K ADETXT,ADETYP,ADETYPN,ADEX,ADEY
 K ADECNT,ADEDES,ADEJ,ADENOT,ADEPC,ADEPRV,ADEREP,ADEV,ADENDMS
 Q
 K ADEPCC,ADETUR,ADEZ ;*NE
 ;
INIT I '$D(ADEPCC) S Y=0 Q
 G:'ADEPCC I2
 ;
 S ADEPAT=APCHSPAT
 S ADECVD=APCHSCVD,ADECKP=APCHSCKP,ADEBRK=APCHSBRK,ADEDLM=APCHSDLM,ADENDM=APCHSNDM
 ;S ADEPRT="S APCHSNRQ=ADENRQ,APCHSTXT=ADETXT,APCHSICL=ADEICL D PRTTXT^APCHSUTL K APCHSNRQ,APCHSTXT,APCHSICL"
 S ADEPRT="D PRTTXT^ADERVW0"
 ;S ADESITX="S APCHSITE=ADESITE D GETSITE^APCHSUTL S ADENSH=APCHSNSH K APCHSITE,APCHSNFL,APCHSNAB,APCHSNSH"
 S ADESITX="D GETSITE^ADERVW0"
 S Y=1
 Q
I2 ;EP
 S ADEPG=0
 S %="CONFIDENTIAL PATIENT DATA",$P(ADEHDR,"*",(IOM-$L(%)/2))=%,$P(ADEHDR,"*",IOM-$L(%))=""
 S ADEPRT="D PRTTXT^ADERVW0"
 S ADESITX="D GETSITE^ADERVW0"
 S ADECKP="Q:$D(APCHSQIT)  S APCHSNPG=0 I $Y>(IOSL-2) "
 S ADEBRK=""
 I $P(IOST,"-")="C" S ADECKP=ADECKP_"W *7,""<>"" R X:DTIME S:'$T X=U W *13 S:X[U APCHSQIT="""" I '$D(APCHSQIT) "
 S ADECKP=ADECKP_"W @IOF D HEADER^ADERVW0 S APCHSNPG=1"
 I $D(DUZ(2)),DUZ(2),$D(^ADEPARAM(DUZ(2),0)) S ADEDLM=$P(^ADEPARAM(DUZ(2),0),U,9) S ADEDLM=$S(ADEDLM["D":ADEDLM,ADEDLM["M":ADEDLM*30,ADEDLM["Y":ADEDLM*365,1:5*365)
 S:'$D(ADEDLM) ADEDLM=5*365
 S X1=DT,X2=-ADEDLM D C^%DTC S ADEDLM=9999999-X K X1,X2
 S:$D(ADELAST) ADENDM=1
 I '$D(ADENDM),$D(DUZ(2)),DUZ(2),$D(^ADEPARAM(DUZ(2),0)) S ADENDM=$P(^ADEPARAM(DUZ(2),0),U,8) S:'ADENDM ADENDM=10
 S:'$D(ADENDM) ADENDM=10
 S ADECVD="S:Y]"""" Y=+Y,Y=$E(Y,4,5)_""/""_$E(Y,6,7)_""/""_$E(Y,2,3)"
 U IO W @IOF D HEADER^ADERVW0 S APCHSNPG=1
 S Y=1 Q
QUE S ZTRTN="START^ADERVW",ZTDESC="DENTAL PATIENT INQUIRE",ZTSAVE("ADEPAT")="",ZTSAVE("ADEPCC")="" D ^%ZTLOAD Q
 ;
PCC ;PEP
 ;Published Entry point from PCC HEALTH SUMMARY routines
 N ADEPCC,ADEPAT
 S ADEPAT=APCHSPAT
 S ADEPCC=1
 D START
 Q
 ;
EN1 ;EP -Enter here to prompt for device, patient
 N ADEPCC,ADEPAT
 S ADEPCC=0
 K DIC S DIC="^AUPNPAT(",DIC(0)="AEQMZ" D ^DIC
 I Y<1 G EN1END
 S ADEPAT=+Y
 S %ZIS="Q" D ^%ZIS
 I POP G EN1END
 ;FHL 9/9/98 I $D(IO("Q")) K IO("Q") D QUE W:$D(ZTSK) !,"REQUEST QUEUED!" G EN1END
 I $D(IO("Q")) K IO("Q") D QUE W:$D(ZTQUEUED) !,"REQUEST QUEUED!" G EN1END
 D START
EN1END Q
EN2 ;EP - Enter here with ADETMP defined as ADEPAT, device will be home
 N ADEPCC
 S ADEPCC=0
 S IOP=0 D ^%ZIS
 D START
 Q
EN3 ;EP - Enter here with ADEPAT and device defined. FM entry point
 N ADEPCC
 S ADEPCC=0
 D START
 Q
