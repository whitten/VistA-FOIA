BMCCHS1 ;IHS/OIT/FCJ - CHS INTERFACE 2 OF 2 RTNS [ 09/22/2006  10:03 AM ]
 ;;4.0;REFERRED CARE INFO SYSTEM;**2**;JAN 09, 2006
 ;
 ;
APPEAL(BMCCHS) ;APPEAL INFORMATION
 ; d appeal^bmcchs(.array_name)
 Q:'$D(^BMCREF("CD",BMCCHS(1128)))
 S BMCRIEN="",BMCRIEN=$O(^BMCREF("CD",BMCCHS(1128),BMCRIEN))
 S BMCAPDT=$P($G(^BMCREF(BMCRIEN,61)),U,16)
 Q:BMCAPDT>BMCCHS(6116)
 S DIE="^BMCREF(",DA=BMCRIEN,DR=""
 F BMCCHSX=1112,1113,1122,6116,6117,6118,6119 S:$G(BMCCHS(BMCCHSX))'="" DR=DR_$S(DR="":"",1:";")_BMCCHSX_"///"_BMCCHS(BMCCHSX)
 I $P($G(^BMCPARM(DUZ(2),4100)),U,5)="Y",BMCCHS(1112)="A" D
 .S DR=DR_$S(DR="":"",1:";")_".15///"_"A"
 Q:DR=""
 D DIE^BMCFMC
 Q
