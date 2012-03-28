ACRFPA ;IHS/OIRM/DSD/THL,AEF - PURCHASE OFFICE MANAGEMENT; [ 11/01/2001   9:44 AM ]
 ;;2.1;ADMIN RESOURCE MGT SYSTEM;;NOV 05, 2001
 ;;ROUTINE FOR CREATION AND MANAGEMENT OF PURCHASING OFFICES AND
 ;;EDITING PURCHASING AGENT DATA
PA ;EP;TO ADD PURCHASING AGENT
 F  D PA1 Q:$D(ACRQUIT)!$D(ACROUT)
 K ACRQUIT
 Q
PA1 W @IOF
 W !?30,"ADD PURCHASING AGENT"
 W !?30,"===================="
 S DIC="^ACRPA("
 S DIC("A")="Select PURCHASING AGENT: "
 S DIC(0)="AELMQZ"
 W !
 D DIC^ACRFDIC
 I U[$E(X)!(+Y<1) S ACRQUIT="" Q
 S DA=+Y
 S DIE="^ACRPA("
 S DR="[ACR PURCHASING AGENT]"
 D DDS^ACRFDIC
 Q:'$D(ACRSCREN)
 K ACRSCREN
 D DIE^ACRFDIC
 Q
PO ;EP;TO ADD PURCHASING OFFICE
 F  D PO1 Q:$D(ACRQUIT)!$D(ACROUT)
 K ACRQUIT
 Q
PO1 D OFFICE
 Q:$D(ACRQUIT)
 S DA=ACRDA
 S DIE="^ACRPO("
 S DR="[ACR PURCHASING OFFICE]"
 D DDS^ACRFDIC
 S DA=ACRDA
 S DIE="^ACRPO("
 S DR="[ACR PO PRINTER CONTROL]"
 D DDS^ACRFDIC
 Q:'$D(ACRSCREN)
 K ACRSCREN
 I $P(Y,U,3)'=1 D  Q:$D(ACROUT)
 .S D0=+Y
 .W @IOF
 .D ^ACRPPUR
 .W !
 .S DIR(0)="YO"
 .S DIR("A")="Edit this data"
 .S DIR("B")="NO"
 .D DIR^ACRFDIC
 I $G(Y)=1 D
 .S DA=ACRDA
 .S DIE="^ACRPO("
 .S DR="[ACR PURCHASING OFFICE]"
 .D DDS^ACRFDIC
 .I $D(ACRSCREN) K ACRSCREN D DIE^ACRFDIC
 .S DA=ACRDA
 .S DIE="^ACRPO("
 .S DR="[ACR PO PRINTER CONTROL]"
 .D DDS^ACRFDIC
 .I $D(ACRSCREN) K ACRSCREN D DIE^ACRFDIC
 S X=$G(^AUTTLOC(+^ACRPO(ACRDA,0),0))
 W !!?5,"MAILING ADDRESS-STREET.: ",$P(X,U,12)
 W !?5,"MAILING ADDRESS-CITY...: ",$P(X,U,13)
 W !?5,"MAILING ADDRESS-STATE..: ",$P($G(^DIC(5,+$P(X,U,14),0)),U)
 W !?5,"MAILING ADDRESS-ZIPCODE: ",$P(X,U,15)
 W !?5,"MAILING ADDRESS-PHONE..: ",$P(X,U,11)
 W !
 S DIR(0)="YO"
 S DIR("A")="Edit LOCATION address"
 S DIR("B")="NO"
 D DIR^ACRFDIC
 Q:$G(Y)'=1
 S DA=+^ACRPO(ACRDA,0)
 S DIE="^AUTTLOC("
 S DR=".14MAILING ADDRESS-STREET.;.15MAILING ADDRESS-CITY...;.16MAILING ADDRESS-STATE..;.17MAILING ADDRESS-ZIPCODE;.13MAILING ADDRESS-PHONE.."
 W !
 D DIE^ACRFDIC
 Q
OFFICE ;EP;TO SELECT PURCHASING OFFICE
 W @IOF
 W !?26,"PURCHASING OFFICE"
 W !?26,"====================="
 S DIC="^ACRPO("
 S DIC(0)="AELMQZ"
 S DIC("A")="Select PURCHASING OFFICE: "
 W !
 D DIC^ACRFDIC
 I U[$E(X)!(+Y<1)!'$D(^ACRPO(+Y,0)) S ACRQUIT="" Q
 S ACRDA=+Y
 Q
