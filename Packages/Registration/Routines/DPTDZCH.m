DPTDZCH ; IHS/TUCSON/JCM - DISPLAYS CHART NUMBERS FROM PATIENT FILE ; [ 09/12/2001  5:45 AM ]
 ;;1.0;PATIENT MERGE;;FEB 02, 1994
 ;IHS/ANMC/LJF 9/10/2001 added code to display other names
 ;             9/11/2001 fixed code under CHARTS2 to quit if no more
 ;
START ;
 D NAMES  ;IHS/ANMC/LJF 9/10/2001
 D CHARTS
 D EOJ
 Q
 ;
CHARTS ; Displays chart numbers and facilities for patients
 W !!,"*** HEALTH RECORD NO."
 S Y=XDRMCD,Y(2)=XDRMCD2
 S DPTDLKF=0,DPTDLKFF=0,DPTDLKI=0
 I $D(^AUPNPAT(Y,41,0)) F DPTDLKI=0:1 S DPTDLKF=$O(^AUPNPAT(Y,41,DPTDLKF)) Q:'DPTDLKF  W:DPTDLKI ! W ?23," ",$J($P(^AUTTLOC(DPTDLKF,0),U,7),4)," ",$P(^AUPNPAT(Y,41,DPTDLKF,0),U,2) I $D(^AUPNPAT(Y(2),41,0)) D CHART2
 I $D(^AUPNPAT(Y(2),41,0)),$P(^AUPNPAT(Y(2),41,0),U,4)>DPTDLKI F DPTDLKII=1:1 Q:'$O(^AUPNPAT(Y(2),41,DPTDLKFF))  W ! D CHART2
 Q
CHART2 ;prints second patient's chart numbers
 I '$D(DPTDLKII),$P(^AUPNPAT(Y(2),41,0),U,4)<(DPTDLKI+1) G CHART2X
 S DPTDLKFF=$O(^AUPNPAT(Y(2),41,DPTDLKFF))
 Q:'DPTDLKFF  Q:'$D(^AUTTLOC(DPTDLKFF,0))  ;IHS/ANMC/LJF 9/12/2001
 W ?49 W " ",$J($P(^AUTTLOC(DPTDLKFF,0),U,7),4)," ",$P(^AUPNPAT(Y(2),41,DPTDLKFF,0),U,2)
 ;
CHART2X ;
 Q
 ;
 ;IHS/ANMC/LJF 9/10/2001 added NAMES and NAMES2 subrtns
NAMES ; Displays other names for patients
 W !!,"*** ALIAS DETAILS:"
 S Y=XDRMCD,Y(2)=XDRMCD2
 S DPTDLKF=0,DPTDLKFF=0,DPTDLKI=0
 I $D(^DPT(Y,.01,0)) F DPTDLKI=0:1 S DPTDLKF=$O(^DPT(Y,.01,DPTDLKF)) Q:'DPTDLKF  W:DPTDLKI ! W ?23," ",$P(^DPT(Y,.01,DPTDLKF,0),U) I $D(^DPT(Y(2),.01,0)) D NAMES2
 I $D(^DPT(Y(2),.01,0)),$P(^DPT(Y(2),.01,0),U,4)>DPTDLKI F DPTDLKII=1:1 Q:'$O(^DPT(Y(2),.01,DPTDLKFF))  W ! D NAMES2
 Q
NAMES2 ;prints second patient's other names
 I '$D(DPTDLKII),$P(^DPT(Y(2),.01,0),U,4)<(DPTDLKI+1) G NAMES2X
 S DPTDLKFF=$O(^DPT(Y(2),.01,DPTDLKFF)) Q:'DPTDLKFF
 W ?49 W " ",$P(^DPT(Y(2),.01,DPTDLKFF,0),U)
 ;
NAMES2X ;
 Q
 ;
EOJ ;
 K DPTD,Y,DPTDLKF,DPTDLKFF,DPTDLKI,DPTDLKII
 Q
