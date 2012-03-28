BQINTEG1 ;ISC/XTSUMBLD KERNEL - Package checksum checker ;3110207.111622
 ;;2.1;ICARE MANAGEMENT SYSTEM;;Feb 07, 2011
 ;;7.3;3110207.111622
 S XT4="I 1",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
CONT F XT1=1:1 S XT2=$T(ROU+XT1) Q:XT2=""  S X=$P(XT2," ",1),XT3=$P(XT2,";",3) X XT4 I $T W !,X X ^%ZOSF("TEST") S:'$T XT3=0 X:XT3 ^%ZOSF("RSUM") W ?10,$S('XT3:"Routine not in UCI",XT3'=Y:"Calculated "_$C(7)_Y_", off by "_(Y-XT3),1:"ok")
 ;
 K %1,%2,%3,X,Y,XT1,XT2,XT3,XT4 Q
ONE S XT4="I $D(^UTILITY($J,X))",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
 W !,"Check a subset of routines:" K ^UTILITY($J) X ^%ZOSF("RSEL")
 W ! G CONT
ROU ;;
BQITREDU ;;8515278
BQITRGLS ;;2301757
BQITRHLP ;;2306171
BQITRMT ;;5126621
BQITRPAT ;;842804
BQITRPHS ;;4077119
BQITRPPT ;;4068060
BQITRPRT ;;4394082
BQITRREF ;;3941426
BQITRRSK ;;6028241
BQITRRUN ;;1206322
BQITRSK ;;16314898
BQITRUT1 ;;5846904
BQITRUTL ;;15218854
BQITUIX ;;6499061
BQITUTL ;;1695300
BQITX ;;1360706
BQITXA ;;1543341
BQITXB ;;906835
BQITXC ;;1161277
BQITXD ;;900860
BQITXE ;;1206498
BQITXF ;;1014718
BQITXG ;;1237944
BQITXH ;;1023356
BQITXI ;;1243242
BQITXIB ;;3565956
BQITXJ ;;899806
BQITXK ;;963101
BQITXL ;;990445
BQITXM ;;1064693
BQITXN ;;1034664
BQITXO ;;918674
BQITXP ;;1298928
BQITXQ ;;1981315
BQITXR ;;1802706
BQITXS ;;1271242
BQITXT ;;939013
BQITXU ;;1695511
BQITXV ;;969540
BQITXW ;;1020236
BQITXX ;;1024914
BQITXY ;;1912887
BQIUG1 ;;93659
BQIUL1 ;;3518052
BQIUL2 ;;2056706
BQIUL3 ;;883282
BQIULDT ;;1154249
BQIULLK ;;3064672
BQIULPT ;;9822881
BQIULSC ;;231147
BQIUSPRF ;;2099453
BQIUTB ;;15386443
BQIUTB1 ;;4815574
BQIUTB2 ;;4779061
BQIUTB3 ;;1542920
BQIUTIL ;;1652904
BQIVER ;;2669994
BQIVFADD ;;5784411
BQIVFCHC ;;4035342
BQIVFDEF ;;17195400
BQIVFTLK ;;12966054
BQIVFTRB ;;4372779
BQIVFTRE ;;1368233
BQIVFTRF ;;6973433
BQIVFTRH ;;1278520
BQIVFTRI ;;1256615
BQIVFTRL ;;4358539
BQIVFTRM ;;2088828
BQIVFTRO ;;1082140
BQIVFTRP ;;1010316
BQIVFVAL ;;13746976
BQIWHPRF ;;3778795
