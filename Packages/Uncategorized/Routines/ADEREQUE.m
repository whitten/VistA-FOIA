ADEREQUE ; IHS/HQT/MJL  - RESTART DENTAL BACKGROUND ;  [ 03/24/1999   9:04 AM ]
 ;;6.0;ADE;;APRIL 1999
 D ^XBKVAR
 I '$D(DUZ(2)) W:'$D(ZTQUEUED) !,"CANNOT PROCEED: DIVISION DUZ(2) NOT SET" G END
 I DUZ(2)=0 W:'$D(ZTQUEUED) !,"CANNOT PROCEED:  DIVISION SET TO ZERO (UNIVERSAL). DIVISION MUST BE SET TO THE ",!,"SITE IN THE DENTAL SITE PARAMETER FILE." G END
PRE I '$D(^ADEPARAM(+^AUTTSITE(1,0),0))!('$D(^ADEPARAM(DUZ(2),0))) D PRE1 G END
 I $P(^ADEPARAM(DUZ(2),0),U,4)'="y" W:'$D(ZTQUEUED) !,"CANNOT PROCEED: BACKGROUND MODE MUST BE ENABLED IN DENTAL SITE PARAMETER FILE.",!,"CONSULT THE DDS DOCUMENTATION." G END
 I $S($D(^%ZTSCH("RUN"))[0:1,^("RUN")-$H:1,1:$P($H,",",2)-150>$P(^("RUN"),",",2)) W *7,!,"CANNOT PROCEED: TASK MANAGER NOT RUNNING." G END
 I '$D(^%ZOSF("TRAP")) W:'$D(ZTQUEUED) !,"CANNOT PROCEED: ^%ZOSF(""TRAP"") NODE DOES NOT EXIST.  THIS NODE",!,"SHOULD BE SET TO ""$ZT=X"" ON MSM AND DSM SYSTEMS." G END
 I ^%ZOSF("TRAP")'="$ZT=X",$D(^%ZOSF("OS")),^("OS")["MSM"!(^("OS")["DSM") W:'$D(ZTQUEUED) !,"CANNOT PROCEED: ^%ZOSF(""TRAP"") IS SET TO ",$C(34),^%ZOSF("TRAP"),$C(34),!,"IT SHOULD BE SET TO ""$ZT=X"" ON MSM AND DSM SYSTEMS." G END
 S ^ADEPOST("CHECKED")=$H
 ; ^ADEPOST is a transient, non-fileman working global
 I $D(^ADEPOST(0)),$O(^(0)) S ^ADEUTL("ADEDQUE")=1,ZTRTN="^ADEDQUE",ZTDTH=$H,ZTDESC="DENTAL DISC WRITES",ZTIO="" D ^%ZTLOAD W:'$D(ZTQUEUED) !,"DENTAL BACKGROUND PROCESS QUEUED!" ;IHS/MFD MOD TO THIS LINE AND ADDED NEXT LINE
 E  W:'$D(ZTQUEUED) !,"NO DENTAL DATA TO POST, BACKGROUND PROCESS NOT REQUEUED!"
END Q
PRE1 W:'$D(ZTQUEUED) !,"Dental Site Parameter File has not been set up for ",$S($D(^AUTTLOC(DUZ(2),0)):$P(^AUTTLOC(DUZ(2),0),U,2),1:"this site"),".",!,"Consult the DDS Documentation."
 Q
