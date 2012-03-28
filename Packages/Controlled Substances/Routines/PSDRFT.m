PSDRFT ;B'ham ISC/JPW,LTL - File Delayed Dispensing Info ; 13 Dec 93
 ;;3.0; CONTROLLED SUBSTANCES ;;13 Feb 97
UPDAT I $G(PSDPN) F JJ=0:0 S JJ=$O(^PSD(58.8,"F",PSDPN,NAOU,PSDR,JJ)) Q:'JJ  S ORD=+JJ
 ;$S(WQTY:18,CQTY:9,1:17) S:PSDTYP=9 QTY=CQTY-OQTY
 W ?40,"Recording transaction...  "
 D UPDATE W "done."
END ;kill variables
 K %,%DT,%H,%I,BAL,CQTY,DA,DIC,DIE,DIK,DINUM,DLAYGO,DR,JJ,LQTY,NAOUN,NODE,NUR2,OK,ORD
 K PSD,PSDER,PSDREC,PSDRN,PSDT,PSDTN,QTY,WQTY,X,Y
 Q
UPDATE ;update 58.8 and 58.81
 ;updating drug balance in 58.8
 F  L +^PSD(58.8,NAOU,1,PSDR,0):0 I  Q
 D NOW^%DTC S PSDTN=+%
 S BAL=$P(^PSD(58.8,NAOU,1,PSDR,0),"^",4),$P(^(0),"^",4)=$P(^(0),"^",4)-PSDQ
 L -^PSD(58.8,NAOU,1,PSDR,0)
 ;update order balance
 I $G(ORD),$D(^PSD(58.8,NAOU,1,PSDR,3,ORD,0)),PSDTYP'=9 S $P(^(0),"^",22)=$P(^(0),"^",22)-PSDQ,DA=+$P(^(0),"^",17) D:$P(^(0),"^",22)=0  K DA,DIE,DR
 .K DIE,DR S DIE="^PSD(58.81,",DR="10////12;11////1" D ^DIE K DA,DIE,DR
 .K DA,DIE,DR S DIE="^PSD(58.8,"_NAOU_",1,"_PSDR_",3,",DA=+ORD,DA(1)=+PSDR,DA(2)=+NAOU,DR="10////12;11////1" D ^DIE K DA,DIE,DR
ADD ;find entry number in 58.81
 F  L +^PSD(58.81,0):0 I  Q
FIND S PSDREC=$P(^PSD(58.81,0),"^",3)+1 I $D(^PSD(58.81,PSDREC)) S $P(^PSD(58.81,0),"^",3)=PSDREC G FIND
 K DIC,DLAYGO S DIC(0)="L",(DIC,DLAYGO)=58.81,(X,DINUM)=PSDREC D ^DIC K DIC,DLAYGO
 L -^PSD(58.81,0)
EDIT ;edit new transaction in 58.81
 S ^PSD(58.81,PSDREC,0)=PSDREC_"^"_PSDTYP_"^"_NAOU_"^"_PSDT_"^"_PSDR_"^"_$S(PSDTYP=9:-PSDQ,1:PSDQ)_"^"_$G(NUR1(1))_"^^^"_BAL_"^^^^^^Dose signed out during computer down time^"_$G(PSDPN)_"^"_NAOU_"^^"_$G(ORD)
 S ^PSD(58.81,PSDREC,9)=$G(PAT)_"^"_NUR1_"^"_OQTY_"^"_$G(WQTY)_"^"_$G(LQTY)_"^"_$G(NUR2)_"^"_BAL
 ;S:PATL ^PSD(58.81,PSDREC,9.5)=PATL
 S ^PSD(58.81,PSDREC,"CS")=1
 K DA,DIK S DA=PSDREC,DIK="^PSD(58.81," D IX^DIK K DA,DIK
 I PSDTYP'=17 D ERR
 Q
ERR ;err log update
 F  L +^PSD(58.89,0):0 I  Q
FIND9 S PSDER=$P(^PSD(58.89,0),"^",3)+1 I $D(^PSD(58.89,PSDER)) S $P(^PSD(58.89,0),"^",3)=PSDER G FIND9
 K DIC,DLAYGO S DIC(0)="L",(DIC,DLAYGO)=58.89,(X,DINUM)=PSDER D ^DIC K DIC,DLAYGO
 L -^PSD(58.89,0)
EDIT9 ;edit error log
 K DA,DIE,DR S DA=PSDER,DIE=58.89,DR="1////"_PSDREC_";2////"_PSDT_";6////"_NAOU D ^DIE K DA,DIE,DR
 S PHARM1=NUR1,QTY=PSDQ
 S:$G(NAOUN)']"" NAOUN=$P($G(^PSD(58.8,NAOU,0)),U) D ^PSDRFM
 Q
