ACRCR3 ; COMPILED XREF FOR FILE #9002189 ; 09/30/09
 ; 
 S DIKZK=1
 S DIKZ(0)=$G(^ACROBL(DA,0))
 S X=$P(DIKZ(0),U,2)
 I X'="" S ^ACROBL("C",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,3)
 I X'="" S ^ACROBL("D",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,3)
 I X'="" D
 .N DIK,DIV,DIU,DIN
 .K DIV S DIV=X,D0=DA,DIV(0)=D0 S Y(1)=$S($D(^ACROBL(D0,0)):^(0),1:"") S X=$P(Y(1),U,2),X=X S DIU=X K Y X ^DD(9002189,.03,1,2,1.1) X ^DD(9002189,.03,1,2,1.4)
 S X=$P(DIKZ(0),U,3)
 I X'="" D
 .N DIK,DIV,DIU,DIN
 .K DIV S DIV=X,D0=DA,DIV(0)=D0 S Y(1)=$S($D(^ACROBL(D0,0)):^(0),1:"") S X=$P(Y(1),U,12),X=X S DIU=X K Y X ^DD(9002189,.03,1,3,1.1) X ^DD(9002189,.03,1,3,1.4)
 S X=$P(DIKZ(0),U,3)
 I X'="" D
 .N DIK,DIV,DIU,DIN
 .K DIV S DIV=X,D0=DA,DIV(0)=D0 X ^DD(9002189,.03,1,4,89.2) S X=$P(Y(101),U,2) S D0=I(0,0) S DIU=X K Y X ^DD(9002189,.03,1,4,1.1) X ^DD(9002189,.03,1,4,1.4)
 S X=$P(DIKZ(0),U,3)
 I X'="" D
 .N DIK,DIV,DIU,DIN
 .K DIV S DIV=X,D0=DA,DIV(0)=D0 S Y(1)=$S($D(^ACROBL(D0,0)):^(0),1:"") S X=$P(Y(1),U,13),X=X S DIU=X K Y X ^DD(9002189,.03,1,5,1.1) X ^DD(9002189,.03,1,5,1.4)
 S DIKZ(0)=$G(^ACROBL(DA,0))
 S X=$P(DIKZ(0),U,4)
 I X'="" S ^ACROBL("CAN",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,7)
 I X'="" D
 .N DIK,DIV,DIU,DIN
 .K DIV S DIV=X,D0=DA,DIV(0)=D0 X ^DD(9002189,.07,1,1,89.2) S X=$P(Y(101),U,2) S D0=I(0,0) S DIU=X K Y X ^DD(9002189,.07,1,1,1.1) X ^DD(9002189,.07,1,1,1.4)
 S X=$P(DIKZ(0),U,7)
 I X'="" D
 .N DIK,DIV,DIU,DIN
 .K DIV S DIV=X,D0=DA,DIV(0)=D0 X ^DD(9002189,.07,1,2,89.2) S X=$P(Y(101),U,3) S D0=I(0,0) S DIU=X K Y X ^DD(9002189,.07,1,2,1.1) X ^DD(9002189,.07,1,2,1.4)
 S DIKZ(0)=$G(^ACROBL(DA,0))
 S X=$P(DIKZ(0),U,9)
 I X'="" D
 .N DIK,DIV,DIU,DIN
 .K DIV S DIV=X,D0=DA,DIV(0)=D0 X ^DD(9002189,.09,1,1,89.2) S X=$P(Y(101),U,2) S D0=I(0,0) S DIU=X K Y X ^DD(9002189,.09,1,1,1.1) X ^DD(9002189,.09,1,1,1.4)
 S X=$P(DIKZ(0),U,9)
 I X'="" D
 .N DIK,DIV,DIU,DIN
 .K DIV S DIV=X,D0=DA,DIV(0)=D0 X ^DD(9002189,.09,1,2,89.2) S X=$P(Y(101),U,3) S D0=I(0,0) S DIU=X K Y X ^DD(9002189,.09,1,2,1.1) X ^DD(9002189,.09,1,2,1.4)
 S DIKZ(0)=$G(^ACROBL(DA,0))
 S X=$P(DIKZ(0),U,12)
 I X'="" S ^ACROBL("LOT",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,13)
 I X'="" S ^ACROBL("PROP",$E(X,1,30),DA)=""
 S DIKZ("DT")=$G(^ACROBL(DA,"DT"))
 S X=$P(DIKZ("DT"),U,1)
 I X'="" S ^ACROBL("DOC",$E(X,1,30),DA)=""
 S X=$P(DIKZ("DT"),U,3)
 I X'="" S ^ACROBL("OC",$E(X,1,30),DA)=""
 S DIKZ("VND")=$G(^ACROBL(DA,"VND"))
 S X=$P(DIKZ("VND"),U,1)
 I X'="" S ^ACROBL("VND",$E(X,1,30),DA)=""
 S DIKZ("APV")=$G(^ACROBL(DA,"APV"))
 S X=$P(DIKZ("APV"),U,1)
 I X'="" I X="A",$P($G(^ACROBL(DA,"APV")),U,6)'=1,+$G(^ACRDOC(DA,"POST")) S ^ACRDOC("RL",+$G(^("POST")),DA)=""
 S X=$P(DIKZ("APV"),U,3)
 I X'="" ;I $D(ACRFINSS),$P(^ACROBL(DA,"APV"),U,3)="Y" D TSK^ACRPRINT
 S DIKZ("CONV")=$G(^ACROBL(DA,"CONV"))
 S X=$P(DIKZ("CONV"),U,1)
 I X'="" I $D(^ACROBL(DA,"CONV")),$P(^("CONV"),U)["Y" S ^ACROBL("ACONV",$P(^ACROBL(DA,0),U,3),$P(^("CONV"),U),DA)=""
 S X=$P(DIKZ("APV"),U,4)
 I X'="" D
 .N DIK,DIV,DIU,DIN
 .K DIV S DIV=X,D0=DA,DIV(0)=D0 S Y(1)=$S($D(^ACROBL(D0,"APV")):^("APV"),1:"") S X=$P(Y(1),U,7),X=X S DIU=X K Y S X=DIV S X=DIV,X=X X ^DD(9002189,907,1,1,1.4)
 S X=$P(DIKZ("APV"),U,4)
 I X'="" D
 .N DIK,DIV,DIU,DIN
 .K DIV S DIV=X,D0=DA,DIV(0)=D0 S Y(1)=$S($D(^ACROBL(D0,0)):^(0),1:"") S X=$P(Y(1),U,10),X=X S DIU=X K Y X ^DD(9002189,907,1,2,1.1) X ^DD(9002189,907,1,2,1.4)
 S DIKZ("APV")=$G(^ACROBL(DA,"APV"))
 S X=$P(DIKZ("APV"),U,6)
 I X'="" I X'=1,$E($G(^ACROBL(DA,"APV")))="A",+$G(^ACRDOC(DA,"POST")) S ^ACRDOC("RL",+^("POST"),DA)=""
 S X=$P(DIKZ("APV"),U,7)
 I X'="" S ^ACROBL("TV",$E(X,1,30),DA)=""
 S X=$P(DIKZ("APV"),U,8)
 I X'="" K:X]"" ^ACRDOC("PO",+$P($G(^ACRDOC(DA,0)),U,8),"A",DA)
 S X=$P(DIKZ("APV"),U,8)
 I X'="" K:X]"" ^ACRDOC("PA",+$G(^ACRDOC(DA,"PA")),"A",DA)
END G ^ACRCR4
