ACRFSSD ;IHS/OIRM/DSD/THL,AEF - DISPLAY SERVICES/SUPPLIES; [ 11/01/2001   9:44 AM ]
 ;;2.1;ADMIN RESOURCE MGT SYSTEM;;NOV 05, 2001
 ;;ROUTINE TO DISPALY SERVICES/SUPPLIES
DISP ;EP;TO DISPLAY SUPPLIES/SERVICE
 Q:$D(ACROUT)
 K ACRQUIT
 N ACRX
 D SSCHK^ACRFSSA
 I $D(ACRQUIT)!$D(ACROUT) K ACRQUIT Q
 I $D(ACRQUIT)!$D(ACROUT) K ACRQUIT Q
 N ACRLBDA,ACRCANDA,ACROBJDA,ACRSSADA,ACRALWDA,ACRDA,ACRFY,ACRI
 S ACRJ=ACRJ+1
DISP1 S X=^ACRSS(ACRSSDA,0)
 S ACRSS=ACRSSDA_U_X
 S ACRSS3(ACRJ)=$G(^ACRSS(ACRSSDA,"NMS"))
 S ACRSS1=^ACRSS(ACRSSDA,"DT")
 S ACRITEM=+X
 S ACRVENDA=$P($G(^ACRSS(ACRSSDA,"VND")),U)
 S ACRSS2=$G(^ACRSS(ACRSSDA,"DESC"))
 S ACRSS4=$G(^ACRSS(ACRSSDA,"NOTES"))
 S ACRSS(ACRJ)=ACRSS
 S ACRSSITM=$S($P(ACRSS3(ACRJ),U)]"":$P(ACRSS3(ACRJ),U),$P(ACRSS3(ACRJ),U,2)]"":$P(ACRSS3(ACRJ),U,2),1:"")
 S ACRSSITM=$E(ACRSSITM,1,30)
 S ACROBJDA=$P(ACRSS,U,5)
 S ACRSSUP=$P(ACRSS1,U,3)
 S ACRSSQA=$P(ACRSS1,U,5)
 S ACRSSDS1=$E($P(ACRSS2,U),1,30)
 S ACRSSDS2=$E($P(ACRSS2,U,2),1,30)
 S ACRSSNT1=$P(ACRSS4,U)
 S ACRSSNT2=$P(ACRSS4,U,2)
 S ACRSSNT3=$P(ACRSS4,U,3)
 S ACRSSNT4=$P(ACRSS4,U,4)
 S ACRSSNT5=$P(ACRSS4,U,5)
 S ACRSSIT=$P(ACRSS1,U,4)
 S ACRSSDS3=$P(ACRSS2,U,3)
 S ACRSSDS4=$P(ACRSS2,U,4)
 S ACRSSDS5=$P(ACRSS2,U,5)
 S ACRSSORD=$P(ACRSS1,U)
 S ACRSSUNT=$P(ACRSS1,U,2)
 S ACRSSUNT=$S($D(^ACRUI(+ACRSSUNT,0)):$P(^(0),U),1:"**")
 S ACRSSCAN=$P(ACRSS,U,6)
 S ACRVENAM=""
 S ACRSSACP=$P(ACRSS1,U,6)
 I ACRVENDA,$D(^AUTTVNDR(ACRVENDA,0)) S ACRVENAM=$P(^(0),U)
 S ACROBJ=$S(ACROBJDA]"":$P(^AUTTOBJC(ACROBJDA,0),U),1:"")
 S:'$D(ACRVENO) ACRVENO=ACRVENDA
 S ACRCAN=$S(ACRSSCAN]"":$P(^AUTTCAN(ACRSSCAN,0),U),1:"")
 I ACRITEM'=ACRJ,ACRITEM'=999 D
 .S $P(^ACRSS(ACRSSDA,0),U)=ACRJ
 .S ACRITEM=ACRJ
 D P
 Q:$D(ACRQUIT)
 W !,ACRITEM
 I $P(ACRSS3(ACRJ),U)]"" D
 .W:'$D(ACRORIGF) ?3,"|"
 .W ?4,"VON: ",$P(ACRSS3(ACRJ),U)
 .D W
 .D P
 .W !
 I $P(ACRSS3(ACRJ),U,3)]"" D
 .W:'$D(ACRORIGF) ?3,"|"
 .W ?4,"NDC: ",$P(ACRSS3(ACRJ),U,3)
 .D W
 .D P
 .W !
 I $P(ACRSS3(ACRJ),U,2)]"" D
 .W:'$D(ACRORIGF) ?3,"|"
 .W ?4,"NSN: ",$P(ACRSS3(ACRJ),U,2)
 .D W
 .D P
 .W !
 W:'$D(ACRORIGF) ?3,"|"
 W ?4,ACRSSDS1
 I +ACRSSUP]"" D
 .S X="O"
 .D W1
 .W ACRSSORD
 .S X="U"
 .D W1
 .W ACRSSUNT
 .S X="C"
 .D W1
 .W:'$D(ACRPQT) $J($FN(ACRSSUP,"P",$S($L($P(ACRSSUP,".",2))>2:$L($P(ACRSSUP,".",2)),1:2)),10)
 .S X="T"
 .D W1
 .W:'$D(ACRPQT) $J($FN(ACRSSIT,"P",2),11)
 .S X="F"
 .D W1
 I ACRREFX'=103&(ACRREFX'=349)&(ACRREFX'=326)&'$D(ACRPRT)&'$D(ACRPQT) D
 .W ?68,ACROBJ
 .W ?72,"|"
 .W ?73,ACRCAN
 D P
 Q:$D(ACRQUIT)
 F ACRSSD=2:1:5 I @("ACRSSDS"_ACRSSD)]"" D
 .W !?3
 .W:'$D(ACRORIGF) "|"
 .W ?4,@("ACRSSDS"_ACRSSD)
 .D W
 .D P
 D SSNOTES^ACRFSSD1
 D ADDM^ACRFSSD1
 W:$P(^ACRSS(ACRSSDA,"DT"),U,10)="8" !?6,"(3.5 % VA SURCHARGE ADDED TO THIS ITEM.)"
 D EQUIP:$E(ACROBJ,1,2)=31&(ACROBJ'="319Y")&(ACRREFX=116)&(+^ACRSS(ACRSSDA,0)'=999)
 D REPAIR:"^257A^257E^257J^2576^"[(U_ACROBJ_U)&(ACRREFX=116)
 K ACRSSNT1,ACRSSNT2,ACRSSNT3,ACRSSNT4,ACRSSNT5,ACRSSDS1,ACRSSDS2,ACRSSDS3,ACRSSDS4,ACRSSDS5,ACRSSD
 Q
P ;EP;FOR PAGE CONTROL OF REQUISITION/PO ITEMS
 ;;CALLED FROM PRINT TEMPLATES.  DO NOT DELETE OR MOVE
 Q:$D(ACRQUIT)
 S:$D(D0) ACRD0=D0
 S ACRPHEAD=$S(ACRREFX=103:4,ACRREFX=349!(ACRREFX=326):20,1:10)
 S:$D(ACRORIGF)&(+$G(ACRPSC)=347!(+$G(ACRPSC)=326)) ACRPHEAD=10
 I IOSL-$S($E(IOST,1,2)="C-":10,1:ACRPHEAD)<$Y!($D(ACRORIGF)&($Y>$P($G(ACRPSC),U,2))) D
 .S ACRPHEAD=$S($E(IOST,1,2)="C-":5,1:ACRPHEAD)
 .I $E(IOST,1,2)="C-" D
 ..S DIR(0)="YO"
 ..S DIR("A")="Display Remaining Items"
 ..S DIR("B")="YES"
 ..W !
 ..D DIR^ACRFDIC
 ..S:Y'=1 ACRQUIT=""
 .K ACROUT
 .I $D(ACRORIGF) D
 ..N X
 ..S X="DISP^ACRF"_+ACRPSC
 ..S ACRTOP=""
 ..D @X
 ..K ACRORIGF
 .I '$D(ACRQUIT),'$D(ACRORIGF) D
 ..D PHEAD^ACRFSS12
 ..D HEAD^ACRFSSD1
 S:$D(ACRD0) D0=ACRD0
 K ACRD0
 Q
W ;EP;LINES FOR DOCUMENT FORMATTING
 Q:$D(ACRORIGF)
 W:$X<34 ?34,"|"
 W:$X<41 ?41,"|"
 W:$X<44 ?44,"|"
 W:$X<55 ?55,"|"
 W:$X<67 ?67,"|"
 I ACRREFX'=103,ACRREFX'=349,ACRREFX'=326,'$D(ACRPRT),$X<72 W ?72,"|"
 Q
W1 ;EP;LINES FOR DOCUMENT FORMATTING
 I $D(ACRORIGF) D @("W"_+ACRPSC_X) Q
 D @("W1"_X)
 Q
 I $X<3!($X>75) W !?3,"|"
 Q
W1O I $X<35 W ?34,"|"
 Q
W1U I $X<42 W ?41,"|"
 Q
W1C I $X<45 W ?44,"|"
 Q
W1T I $X<56 W ?55,"|"
 Q
W1F I $X<68 W ?67,"|"
 Q
 I ACRREFX'=103,ACRREFX'=349,ACRREFX'=326,'$D(ACRPRT),$X<72 W ?72,"|"
 Q
W26 I $X<3 W !?2
 Q
W26O I $X<41 W ?41
 Q
W26U I $X<53 W ?53
 Q
W26C I $X<59 W ?59
 Q
W26T I $X<69 W ?69
 Q
W26F I $X<69 W ?69
 Q
W1449 I $X<3 W !?2
 Q
W1449O I $X<45 W ?44
 Q
W1449U I $X<52 W ?51
 Q
W1449C I $X<56 W ?56
 Q
W1449T I $X<67 W ?67
 Q
W1449F I $X<67 W ?67
 Q
W347 I $X<5 W !?5
 Q
W347O I $X<47 W ?47
 Q
W347U I $X<53 W ?53
 Q
W347C I $X<56 W ?56
 Q
W347T I $X<64 W ?64
 Q
W347F I $X<64 W ?64
 Q
W326 I $X<5 W !?5
 Q
W326O I $X<47 W ?47
 Q
W326U I $X<53 W ?53
 Q
W326C I $X<56 W ?56
 Q
W326T I $X<64 W ?64
 Q
W326F I $X<64 W ?64
 Q
W33 I $X<5 W !?5
 Q
W33O I $X<47 W ?47
 Q
W33U I $X<53 W ?53
 Q
W33C I $X<56 W ?56
 Q
W33T I $X<64 W ?64
 Q
W33F I $X<64 W ?64
 Q
EQUIP N DXS,DIP,DC,D0,DN
 S ACREQUIP=""
 S D0=ACRSSDA
 D ^ACRPEQP
 K ACREQUIP
 Q
REPAIR N DXS,DIP,DC,D0,DN
 S ACREQUIP=""
 S D0=ACRSSDA
 D ^ACRPRQR
 K ACREQUIP
 Q
