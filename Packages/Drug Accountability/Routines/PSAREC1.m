PSAREC1 ;BIR/LTL,JMB-Receiving Directly into Drug Accountability - CONT'D  ;7/23/97
 ;;3.0; DRUG ACCOUNTABILITY/INVENTORY INTERFACE;**21,37**; 10/24/97
 ;References to ^PSDRUG( are covered by IA #2095
 ;This routine posts non-prime vendor's drugs into pharmacy locations.
 ;The balances are incremented in the pharmacy location & the DRUG file.
 ;
POST ;Posts the data in 58.8, 58.81, and 50
 D NOW^%DTC S PSADT=+$E(%,1,12) K %
 I '$D(^PSD(58.8,PSALOC,1,PSADRG,0)) D
 .S:'$D(^PSD(58.8,PSALOC,1,0)) DIC("P")=$P(^DD(58.8,10,0),"^",2)
 .S DA(1)=PSALOC,DIC="^PSD(58.8,"_DA(1)_",1,",(DA,X)=PSADRG,DIC(0)="L",DLAYGO=58.8
 .F  L +^PSD(58.8,PSALOC,0):0 I  Q
 .D ^DIC L -^PSD(58.8,PSALOC,0) K DIC,DA
 W !!,"There were ",$S($P($G(^PSD(58.8,PSALOC,1,PSADRG,0)),"^",4):$P($G(^(0)),"^",4),1:0)," on hand.",?40,"There are now ",$P($G(^(0)),"^",4)+PSAREC," on hand.",!
 F  L +^PSD(58.8,PSALOC,1,PSADRG,0):0 I  Q
 S $P(^PSD(58.8,PSALOC,1,PSADRG,0),"^",4)=PSAREC+PSACBAL
 L -^PSD(58.8,PSALOC,1,PSADRG,0)
 ;
MONTHLY I '$D(^PSD(58.8,PSALOC,1,PSADRG,5,$E(DT,1,5)*100,0)) D
 .;PSA*3*31 Set zero node to correct DD (20 not 28) Dave B.
 .S:'$D(^PSD(58.8,PSALOC,1,PSADRG,5,0)) DIC("P")=$P(^DD(58.8001,20,0),"^",2)
 .S DIC="^PSD(58.8,PSALOC,1,PSADRG,5,",DIC(0)="L",DIC("DR")="1////^S X=$G(PSACBAL)",(X,DINUM)=$E(DT,1,5)*100,DA(2)=PSALOC,DA(1)=PSADRG,DLAYGO=58.8 D ^DIC K DIC,DINUM,DLAYGO
 .S X="T-1M" D ^%DT
 .S DIC="^PSD(58.8,PSALOC,1,PSADRG,5,",DIC(0)="L",(X,DINUM)=$E(Y,1,5)*100,DA(2)=PSALOC,DA(1)=PSADRG,DLAYGO=58.8 D ^DIC K DIC,DINUM,DLAYGO S DA=+Y
 .S DIE="^PSD(58.8,PSALOC,1,PSADRG,5,",DA(2)=PSALOC,DA(1)=PSADRG,DR="3////^S X=$G(PSACBAL)" D ^DIE K DIE
 S DIE="^PSD(58.8,"_PSALOC_",1,"_PSADRG_",5,",DA(2)=PSALOC,DA(1)=PSADRG,DA=$E(DT,1,5)*100,DR="5////^S X="_$P($G(^(0)),"^",3)+PSAREC D ^DIE
 W !,"Updating monthly receipts and transaction history.",!
TR F  L +^PSD(58.81,0):0 I  Q
FIND S PSAT=$P(^PSD(58.81,0),"^",3)+1 I $D(^PSD(58.81,PSAT)) S $P(^PSD(58.81,0),"^",3)=$P(^PSD(58.81,0),"^",3)+1 G FIND
 S DIC="^PSD(58.81,",DIC(0)="L",DLAYGO=58.81,(DINUM,X)=PSAT D ^DIC K DIC,DINUM,DLAYGO
 S DIE="^PSD(58.81,",DA=PSAT,DR="1////1;2////^S X=PSALOC;3////^S X=PSADT;4////^S X=PSADRG;5////^S X=PSAREC;6////^S X=DUZ;7////^S X=PSACON;8////^S X=PSAPO;9////^S X=PSACBAL;71////^S X=$G(PSAPV)" D ^DIE
 L -^PSD(58.81,0)
 S:'$D(^PSD(58.8,PSALOC,1,PSADRG,4,0)) DIC("P")=$P(^DD(58.8001,19,0),"^",2)
ACT S DIC="^PSD(58.8,PSALOC,1,PSADRG,4,",DIC(0)="L",(X,DINUM)=PSAT,DA(2)=PSALOC,DA(1)=PSADRG,DLAYGO=58.8
 F  L +^PSD(58.8,PSALOC,0):0 I  Q
 D ^DIC
 L -^PSD(58.8,PSALOC,0) K DA,DIC,DINUM,DLAYGO
 ;
50 S (PSATDRG,PSA)=0 F  S PSA=$O(^PSD(58.8,"C",PSADRG,PSA)) Q:'PSA  D
 .I PSA=PSALOC Q:PSACBAL<0  S PSATDRG=PSATDRG+PSACBAL Q
 .I +$G(^PSD(58.8,PSA,"I")),+^PSD(58.8,PSA,"I")'>DT Q
 .Q:$P($G(^PSD(58.8,PSA,0)),"^",2)'="M"&($P($G(^PSD(58.8,PSA,0)),"^",2)'="P")
 .S PSATDRG=PSATDRG+$P($G(^PSD(58.8,PSA,1,PSADRG,0)),"^",4)
 S PSANODE=$G(^PSDRUG(PSADRG,660))
 I PSACBAL>0!(PSATDRG>0) D
 .S PSACOST=PSACOST+(PSATDRG*+$P(PSANODE,"^",6)),PSATDRG=PSAREC+PSATDRG,PSANPDU=+$J((PSACOST/PSATDRG),0,3),PSANPOU=PSANPDU*PSADUOU
 .S PSALEN=$L($P(PSANPOU,".")),PSANPOU=$J(PSANPOU,(PSALEN+3),2)
 E  S PSATDRG=PSATDRG+PSACBAL,PSANPOU=PSAPOU,PSANPDU=PSAPDU
 S DIE="^PSDRUG(",DA=PSADRG,DR="50////^S X="_(PSAREC+$G(^PSDRUG(PSADRG,660.1)))
 F  L +^PSDRUG(DA,0):0 I  Q
 D ^DIE L -^PSDRUG(DA,0) K DIE,DA
 S PSAODASH=$P($G(^PSDRUG(PSADRG,2)),"^",2)
 S PSAONDC=$S(PSAODASH'="":$E("000000",1,(6-$L($P(PSAODASH,"-"))))_$P(PSAODASH,"-")_$E("0000",1,(4-$L($P(PSAODASH,"-",2))))_$P(PSAODASH,"-",2)_$E("00",1,(2-$L($P(PSAODASH,"-",3))))_$P(PSAODASH,"-",3),1:"")
 I +PSANPDU=+$P(PSANODE,"^",6),PSANDC=PSAONDC,PSANDC'="" G NEXT
 I ($P(PSANODE,"^",2)=PSAOU&($P(PSANODE,"^",5)=PSADUOU))!('$P(PSANODE,"^",2)&('$P(PSANODE,"^",5))) D
 .I PSANDC'="",PSANDC'=PSAONDC D
 ..S DIE="^PSDRUG(",DA=PSADRG,DR="31////^S X=PSADASH"
 ..F  L +^PSDRUG(DA,0):0 I  Q
 ..D ^DIE L -^PSDRUG(DA,0) K DIE,DA
 .I +PSANPDU,+PSANPDU'=+$P(PSANODE,"^",6),+PSANPOU D
 ..S DIE="^PSDRUG(",DA=PSADRG,DR="13///^S X="_PSANPOU
 ..F  L +^PSDRUG(DA,0):0 I  Q
 ..D ^DIE L -^PSDRUG(DA,0) K DIE,DA
 .I '$P(PSANODE,"^",2),'$P(PSANODE,"^",5),PSAOU,PSADUOU D
 ..S DIE="^PSDRUG(",DA=PSADRG,DR="12////^S X=PSAOU;15////^S X=PSADUOU"
 ..F  L +^PSDRUG(DA,0):0 I  Q
 ..D ^DIE L -^PSDRUG(DA,0) K DIE,DA
NEXT Q:$G(PSANDC)=""
SYNONYM D PSANDC1^PSAHELP S PSADASH=PSANDCX K PSANDCX
 S PSA50SYN=+$O(^PSDRUG("C",PSANDC,PSADRG,0))
 K DA,DR S:'$D(^PSDRUG(PSADRG,1,0)) DIC("P")=$P(^DD(50,9,0),"^",2)
 S DA(1)=PSADRG
 I 'PSA50SYN!(PSA50SYN&('$D(^PSDRUG(PSADRG,1,PSA50SYN,0)))) D  Q:Y<0
 .S DIC="^PSDRUG("_DA(1)_",1,",DIC(0)="LM",X=PSANDC,DLAYGO=50
 .F  L +^PSDRUG(PSADRG,0):0 I  Q
 .D ^DIC L -^PSDRUG(PSADRG,0) K DIC,DLAYGO S PSA50SYN=+Y
 S DA=PSA50SYN,DIE="^PSDRUG("_DA(1)_",1,"
 S DR="2////^S X=PSADASH;1////D"_$S(+PSAOU:";401////^S X=PSAOU",1:"")_$S(+PSAPOU:";402////^S X=PSAPOU",1:"")_";403////^S X=PSADUOU"_$S(+$G(PSAPDU):";404////^S X=PSAPDU",1:"")_$S(PSAVEND'="":";405///^S X=PSAVEND",1:"")
 F  L +^PSDRUG(PSADRG,0):0 I  Q
 D ^DIE L -^PSDRUG(PSADRG,0)
 K DIE,DR
 Q
PRICEHLP ;Extended help for price per order unit
 W !?5,"Enter the cost for each order unit."
 Q
