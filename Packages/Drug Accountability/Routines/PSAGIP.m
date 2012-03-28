PSAGIP ;BIR/LTL,JMB-DA receiving from GIP ;7/23/97
 ;;3.0; DRUG ACCOUNTABILITY/INVENTORY INTERFACE;**8**; 10/24/97
 Q
EN(PSAGIP,PSAITEM,PSAQTY,PSAISS,PSAEX,PSATR,PSACOST,PSANDC)     ; GIP passes recing data
 ;PSAGIP=D0 from #445, PSAITEM=D0 from #441,
 ;PSAQTY=qty rec'd converted to dispensing unit, PSAISS=D0 from #410,
 ;PSAEX=external form of D0 from either #410 or #442,
 ;PSATR=D0 from #445.2, PSACOST=total cost of receipt,
 ;PSANDC=NDC with dashes
 Q:'$G(PSAQTY)
 Q:'$O(^PSD(58.8,"P",+$G(PSAGIP),""))  ; GIP not linked to DA location
 ;check item linked to drug, drug stocked by DA loc, rec fail flag
 N PSALOC S PSALOC=$O(^PSD(58.8,"P",+$G(PSAGIP),0)),PSADRUG=+$O(^PSDRUG("AB",+$G(PSAITEM),0))
 S ^TMP("PSAC",$J,+PSALOC)=$G(PSAGIP)_U_$G(PSAEX)
 I 'PSADRUG,$P($G(^PSD(58.8,+PSALOC,4,+$G(PSAGIP),0)),U,2) S ^TMP("PSAB",$J,+$G(PSAITEM))="#"+$G(PSAITEM)_"  "_$$DESCR^PRCPUX1($G(PSAGIP),$G(PSAITEM))_" NOT LINKED." Q
 I '$D(^PSD(58.8,+PSALOC,1,PSADRUG,0)),$P($G(^PSD(58.8,+PSALOC,4,+$G(PSAGIP),0)),U,2) S ^TMP("PSAB",$J,+$G(PSAITEM))="#"_$G(PSAITEM)_"  "_$$DESCR^PRCPUX1($G(PSAGIP),$G(PSAITEM))_" NOT STOCKED." Q
 Q:'$D(^PSD(58.8,+PSALOC,1,$G(PSADRUG),0))
 S ^TMP("PSA",$J,$G(PSADRUG))=$G(PSAQTY)_U_$G(PSAISS)_U_$G(PSAEX)_U_$G(PSATR)_U_$G(PSACOST)_U_$G(PSAITEM)_U_$G(PSANDC)
 Q
EX N ZTDESC,ZTIO,ZTRTN,ZTSAVE,ZTDTH,ZTSK,PSALOC,PSADAT,PSAB,PSAT,PSAGIP
 Q:'$O(^TMP("PSAC",$J,0))
 Q:'$O(^TMP("PSA",$J,0))&('$O(^TMP("PSAB",$J,0)))
 S PSALOC=$O(^TMP("PSAC",$J,0)),PSAGIP=$P($G(^TMP("PSAC",$J,+PSALOC)),U)
 S ZTDTH=$H,ZTIO="",ZTRTN="TSK^PSAGIP",ZTDESC="GIP/DA Receiving"
 S ZTSAVE("PSALOC")="",ZTSAVE("PSAGIP")=""
 S:$O(^TMP("PSA",$J,0)) ZTSAVE("^TMP(""PSA"",$J,")=""
 S:$O(^TMP("PSAB",$J,0)) ZTSAVE("^TMP(""PSAB"",$J,")=""
 S ZTSAVE("^TMP(""PSAC"",$J,")=""
 D ^%ZTLOAD,HOME^%ZIS
 K IO("Q"),^TMP("PSA",$J),^TMP("PSAB",$J),^TMP("PSAC",$J)
 Q
TSK N PSAM
 S:$P($G(^PSD(58.8,+PSALOC,0)),U,2)="M" PSAM=1
 F PSADRUG=0:0 S PSADRUG=$O(^TMP("PSA",$J,PSADRUG)) Q:'PSADRUG  S PSAQTY=$P($G(^TMP("PSA",$J,PSADRUG)),U),PSAISS=$P($G(^(PSADRUG)),U,2),PSAP=$P($G(^(PSADRUG)),U,3),PSATR=$P($G(^(PSADRUG)),U,4),PSACOST=$P($G(^(PSADRUG)),U,5) D
 .S PSANDC=$P($G(^TMP("PSA",$J,PSADRUG)),U,7) D ^PSAGIP1
 .S:'$P(PSAP,"-",3) PSAPO=PSAP
 .L +^PSD(58.8,+PSALOC,1,+PSADRUG):5
 .D NOW^%DTC S PSADAT=+$E(%,1,12) K %
 .S PSAB=$P($G(^PSD(58.8,+PSALOC,1,+PSADRUG,0)),U,4)
 .S $P(^PSD(58.8,+PSALOC,1,+PSADRUG,0),U,4)=$G(PSAQTY)+PSAB
 .L -^PSD(58.8,+PSALOC,1,+PSADRUG)
MON .S:'$D(^PSD(58.8,+PSALOC,1,+PSADRUG,5,0)) ^(0)="^58.801A^^"
 .I '$D(^PSD(58.8,+PSALOC,1,+PSADRUG,5,$E(DT,1,5)*100,0)) S DIC="^PSD(58.8,+PSALOC,1,+PSADRUG,5,",DIC(0)="LM",DIC("DR")="1////^S X=$G(PSAB)" D
 ..S (X,DINUM)=$E(DT,1,5)*100,DA(2)=PSALOC,DA(1)=PSADRUG,DLAYGO=58.8 D ^DIC K DIC,DINUM,DLAYGO,X
 ..S X="T-1M" D ^%DT S DIC="^PSD(58.8,+PSALOC,1,+PSADRUG,5,",DIC(0)="L",(X,DINUM)=$E(Y,1,5)*100,DA(2)=PSALOC,DA(1)=PSADRUG,DLAYGO=58.8 D ^DIC K DIC,DINUM,DLAYGO,X S DA=+Y K Y
 ..S DIE="^PSD(58.8,+PSALOC,1,+PSADRUG,5,",DA(2)=PSALOC,DA(1)=PSADRUG
 ..S DR="3////^S X=$G(PSAB)" D ^DIE K DIE,DR
 .S DIE="^PSD(58.8,+PSALOC,1,+PSADRUG,5,",DA(2)=PSALOC,DA(1)=PSADRUG,DA=$E(DT,1,5)*100,DR="5////^S X=$P($G(^(0)),U,3)+$G(PSAQTY)" D ^DIE K DIE,DR
TR .F  L +^PSD(58.81,0):0 I  Q
FIND .D FIND1 S DIC="^PSD(58.81,",DIC(0)="L",DLAYGO=58.81,(DINUM,X)=PSAT D ^DIC K DIC,DINUM,DLAYGO L -^PSD(58.81,0)
 .S DIE="^PSD(58.81,",DA=PSAT
 .S DR="1////^S X=$S($E($G(PSATR))=""R"":1,1:9);2////^S X=$G(PSALOC);3////^S X=PSADAT;4////^S X=$G(PSADRUG);5////^S X=$G(PSAQTY);6////^S X=DUZ;7////^S X=$G(PSAISS);8///^S X=$G(PSAPO);9////^S X=PSAB;100////^S X=$G(PSAM)"
 .D ^DIE K DIE,DR
 .S:'$D(^PSD(58.8,+PSALOC,1,+PSADRUG,4,0)) ^(0)="^58.800119PA^^"
 .S DIC="^PSD(58.8,+PSALOC,1,+PSADRUG,4,",DIC(0)="L",(X,DINUM)=PSAT
 .S DA(2)=PSALOC,DA(1)=PSADRUG,DLAYGO=58.8 D ^DIC K DA,DIC,DLAYGO,DINUM,PSAB,PSAISS,PSANDC,PSAPO,PSAQTY,PSATR
 K ^TMP("PSA",$J)
 Q:'$O(^TMP("PSAB",$J,0))
 S:'$G(PSAP) PSAP=$P($G(^TMP("PSAC",$J,PSALOC)),U,2)
 S XMDUZ="Failed Receipt Notifier",XMSUB="Failed DA/GIP Receipts - "_PSAP
 S XMY(DUZ)=""
 I $P($G(^PSD(58.8,+PSALOC,4,+$G(PSAGIP),0)),U,3)'="" S XX=$P(^(0),"^",3),XXX="G."_XX,XMY(XXX)="" K XX,XXX
 S XMTEXT="^TMP(""PSAB"",$J,"
 G:'$D(XMY) QUIT1 D ^XMD
QUIT1 K XMDUZ,XMSUB,XMTEXT,XMY
 S:$D(ZTQUEUED) ZTREQ="@" K ^TMP("PSAB",$J)
QUIT Q
FIND1 S PSAT=$P(^PSD(58.81,0),U,3)+1 I $D(^PSD(58.81,PSAT)) S $P(^PSD(58.81,0),U,3)=$P(^PSD(58.81,0),U,3)+1 G FIND1
 Q
