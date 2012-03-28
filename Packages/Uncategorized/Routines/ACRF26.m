ACRF26 ;IHS/OIRM/DSD/THL,AEF - AWARD/CONTRACT; [ 09/22/2005   9:44 AM ]
 ;;2.1;ADMIN RESOURCE MGT SYSTEM;**19**;NOV 05, 2001
EN D ^ACRFSSPO,EN1
EXIT K ACRX,ACR7A,ACR7B,ACR13A,ACR13B,ACR17B,ACR18B,ACR27A,ACR27A1,ACR27B,ACR27B1,ACREFFD
 F X=1:1:18 K @("ACR"_X)
 Q
EN1 I '$D(ACRORIGF) D  Q
 .W !,"You must use a pre-printed form in order to print SF-33, SOLICITATION, OFFER AND AWARD."
 .D PAUSE^ACRFWARN
 D ^ACRF262:$D(ACRORIGF)
 Q
GATHER ;GATHER ALL INFO REQUIRED TO PRINT THE DOCUMENT
 D SETDOC^ACRFEA1
 S ACRPO=$G(^ACRDOC(ACRDOCDA,"PO"))
 S ACR9=$G(^ACRDOC(ACRDOCDA,"POIO"))
 S ACR15=$G(^ACRDOC(ACRDOCDA,"POST"))
 S ACR18=$G(^ACRDOC(ACRDOCDA,"POMI"))
 S ACR26=$G(^ACRDOC(ACRDOCDA,26))
 S ACR33=$G(^ACRDOC(ACRDOCDA,33))
 S ACR21=$G(^ACRDOC(ACRDOCDA,21))
 S ACR21=$TR(ACR21,"1","X")
 S ACR21=$TR(ACR21,"0","")
 S ACR17=$P(ACRPO,U,5)
 S ACR5=$P(ACR33,U,3)
 S X=$P(ACR33,U,2)
 S ACR6=$E(X,4,5)_"/"_$E(X,6,7)_"/"_$E(X,2,3)
 S X=$P(ACR33,U,11)
 S ACR8=$E(X,4,5)_"/"_$E(X,6,7)_"/"_$E(X,2,3)
 S X=$G(^AUTTPRG(+ACR9,"DT"))
 S ACR9(1)=$P($G(^AUTTPRG(+ACR9,0)),U)
 S ACR9(2)=$P(X,U)
 S ACR9(3)=$P(X,U,2)
 S ACR9(4)=$P(X,U,3)
 S ACR9(5)=$P(X,U,4)
 S ACR9(6)=$P(X,U,5)
 S ACR9(7)=$P(X,U,6)
 S ACR9(5)=$P($G(^DIC(5,+ACR9(5),0)),U,2)
DELIVER S X=$G(^AUTTPRG(+ACR15,"DT"))
 S ACR15(1)=$P($G(^AUTTPRG(+ACR15,0)),U)
 S ACR15(2)=$P(X,U)
 S ACR15(3)=$P(X,U,2)
 S ACR15(4)=$P(X,U,3)
 S ACR15(5)=$P(X,U,4)
 S ACR15(6)=$P(X,U,5)
 S ACR15(7)=$P(X,U,6)
 S ACR15(5)=$P($G(^DIC(5,+ACR15(5),0)),U,2)
ADMIN S X=$G(^AUTTPRG(+$P(ACR26,U,6),"DT"))
 S ACR16(1)=$P($G(^AUTTPRG(+$P(ACR26,U,6),0)),U)
 S ACR16(2)=$P(X,U)
 S ACR16(3)=$P(X,U,2)
 S ACR16(4)=$P(X,U,3)
 S ACR16(5)=$P(X,U,4)
 S ACR16(6)=$P(X,U,5)
 S ACR16(7)=$P(X,U,6)
 S ACR16(5)=$P($G(^DIC(5,+ACR16(5),0)),U,2)
VENDOR ;
 S X=$G(^AUTTVNDR(+ACR17,11))
 S ACR17(1)=$P($G(^AUTTVNDR(+ACR17,0)),U)
 S ACR17(7)=$P(X,U,13)
 S X=$G(^AUTTVNDR(+ACR17,13))
 S ACR17(2)=$P(X,U)
 S ACR17(3)=$P(X,U,10)
 S ACR17(4)=$P(X,U,2)
 S ACR17(5)=$P(X,U,3)
 S ACR17(6)=$P(X,U,4)
 S ACR17(5)=$P($G(^DIC(5,+ACR17(5),0)),U,2)
PAY S X=$G(^AUTTPRG(+ACR18,"DT"))
 S ACR18(1)=$P($G(^AUTTPRG(+ACR18,0)),U)
 S ACR18(2)=$P(X,U)
 S ACR18(3)=$P(X,U,2)
 S ACR18(4)=$P(X,U,3)
 S ACR18(5)=$P(X,U,4)
 S ACR18(6)=$P(X,U,5)
 S ACR18(7)=$P(X,U,6)
 S ACR18(5)=$P($G(^DIC(5,+ACR18(5),0)),U,2)
 S ACR1=$P(ACRDOC0,U)
 S ACR2=$P(ACRDOC0,U,2)
 S Y=$P(ACRPO,U)
 X ^DD("DD")
 S (ACR3,ACREFFD)=Y
 S ACR4=$P(ACRPO,U,3)
 S ACR7A=+$G(^ACRDOC(ACRDOCDA,"PA"))
 S ACR7B=$P($G(^ACRPO(+$G(ACRPODA),0)),U,9)
 ;S X=$P($G(^VA(200,+ACR7A,0)),U)  ;ACR*2.1*19.02 IM16848
 S X=$$NAME2^ACRFUTL1(+ACR7A)  ;ACR*2.1*19.02 IM16848
 S X=$P($P(X,",",2)," ")_" "_$P(X,",")
 S ACR7A=X
 S ACR12=$P(ACRPO,U,13)
 S ACR13A=$S($P(ACR26,0):"XX",1:"")
 S ACR13B=$P(ACR26,U)
 Q
DISP ;EP;
 D DISP^ACRF262
 Q
