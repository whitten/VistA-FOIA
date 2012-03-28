BMCPXTEN ; IHS/PHXAO/TMJ - TOP FPR PROCEDURES ; 
 ;;4.0;REFERRED CARE INFO SYSTEM;;JAN 09, 2006
 ;
 W !!?20,"*****  FREQUENCY OF PROCEDURES REPORT  *****",!!
 D EXIT
GETDATES ;
BD ;get beginning date
 W ! S DIR(0)="D^:DT:EP",DIR("A")="Enter beginning Visit Date" D ^DIR S:$D(DUOUT) DIRUT=1 K DIR S:$D(DUOUT) DIRUT=1
 I $D(DIRUT) G EXIT
 S APCLBD=Y
ED ;get ending date
 W ! S DIR(0)="DA^"_APCLBD_":DT:EP",DIR("A")="Enter ending Visit Date:  " S Y=APCLBD D DD^%DT S Y="" D ^DIR S:$D(DUOUT) DIRUT=1 K DIR S:$D(DUOUT) DIRUT=1
 I $D(DIRUT) G BD
 S APCLED=Y
 S X1=APCLBD,X2=-1 D C^%DTC S APCLSD=X
 I $D(DIRUT) G GETDATES
 S APCLNCAN=1 D ADD^APCLVL01 I $D(APCLQUIT) D DEL^APCLVL K APCLQUIT G GETDATES
 ;
NUM S DIR(0)="NO^5:100:0",DIR("A")="How many entries do you want in the list",DIR("B")="10",DIR("?")="" D ^DIR S:$D(DUOUT) DIRUT=1 K DIR
 S APCLLNO=Y
 S Y=APCLBD D DD^%DT S APCLBDD=Y S Y=APCLED D DD^%DT S APCLEDD=Y
 S APCLTCW=0,APCLPTVS="V",APCLTYPE="D",APCLCTYP="T"
 K ^APCLVRPT(APCLRPT,11) S APCLCNTL="S" D ^APCLVL4 K APCLCNTL I $D(APCLQUIT) D DEL^APCLVL G GETDATES
ZIS ;
 K APCLANS,APCLCNT,APCLCRIT,AMQQTAX,APCLCUT,APCLDISP,APCLHIGH,APCLI,APCLNCAN,APCLSEL,APCLSKIP,APCLTEXT,APCLVAR,APCLVIEN,APCLVREC
 S XBRC="^APCLFPR1",XBRP="^APCLFPRP",XBNS="APCL",XBRX="EXIT^APCLFPR"
 D ^XBDBQUE
 D EXIT
 Q
EXIT ;
 K APCLBD,APCLED,APCLDOB1,APCLDOB2,APCLSEX,X,Y,Z,%,APCLFAC,APCLJOB,APCLLNO,ZTIO,ZTQUEUED,APCLCLN,APCLTYPE,APCLSC,APCLC,APCLPREC,APCLSD,APCLA,APCLC,APCLF,APCLGTOT,APCLPRC,APCLTOT,APCLD,APCLPRCN,APCLET
 K APCLQUIT,APCLAPC,APCLDATE,APCLPOV,APCLVSIT,APCLNOCK,APCLTOT,APCLPROV,APCLVTOT,APCLLINO,L,I,APCLCMA,APCLPOVN,APCLV,APCLTYPP,APCLSCP,APCLPRIM,APCLALL,APCLSEAT
 K APCLANS,AMQQTAX,APCLBDD,APCLCNT,APCLCRIT,APCLCTYP,APCLCUT,APCLDISP,APCLEDD,APCLHIGH,APCLI,APCLNCAN,APCLPTVS,APCLRPT,APCLSEL,APCLSKIP,APCLTCW,APCLTEXT,APCLVAR,APCLVIEN,APCLVREC,DFN,APCLX,APCLY
 K APCLBT
 Q
 ;
 ;
 ;
 ;
