DGPTXX8 ; COMPILED XREF FOR FILE #45.02 ; 02/13/06
 ; 
 S DA=0
A1 ;
 I $D(DISET) K DIKLM S:DIKM1=1 DIKLM=1 G @DIKM1
0 ;
A S DA=$O(^DGPT(DA(1),"M",DA)) I DA'>0 S DA=0 G END
1 ;
 S DIKZ(0)=$G(^DGPT(DA(1),"M",DA,0))
 S X=$P(DIKZ(0),U,2)
 I X'="" D
 .N DIK,DIV,DIU,DIN
 .K DIV S DIV=X,D0=DA(1),DIV(0)=D0,D1=DA,DIV(1)=D1 S Y(1)=$S($D(^DGPT(D0,"M",D1,0)):^(0),1:"") S X=$P(Y(1),U,16),X=X S DIU=X K Y X ^DD(45.02,2,1,1,1.1) X ^DD(45.02,2,1,1,1.4)
 S DIKZ(0)=$G(^DGPT(DA(1),"M",DA,0))
 S X=$P(DIKZ(0),U,5)
 I X'="" S ^DGPT(DA(1),"M","AC",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,5)
 I X'="" X ^DD(45.02,5,1,992,1)
 S DIKZ(0)=$G(^DGPT(DA(1),"M",DA,0))
 S X=$P(DIKZ(0),U,6)
 I X'="" S ^DGPT(DA(1),"M","AC",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,6)
 I X'="" X ^DD(45.02,6,1,992,1)
 S DIKZ(0)=$G(^DGPT(DA(1),"M",DA,0))
 S X=$P(DIKZ(0),U,7)
 I X'="" S ^DGPT(DA(1),"M","AC",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,7)
 I X'="" X ^DD(45.02,7,1,992,1)
 S DIKZ(0)=$G(^DGPT(DA(1),"M",DA,0))
 S X=$P(DIKZ(0),U,8)
 I X'="" S ^DGPT(DA(1),"M","AC",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,8)
 I X'="" X ^DD(45.02,8,1,992,1)
 S DIKZ(0)=$G(^DGPT(DA(1),"M",DA,0))
 S X=$P(DIKZ(0),U,9)
 I X'="" S ^DGPT(DA(1),"M","AC",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,9)
 I X'="" X ^DD(45.02,9,1,992,1)
 S DIKZ(0)=$G(^DGPT(DA(1),"M",DA,0))
 S X=$P(DIKZ(0),U,10)
 I X'="" S ^DGPT(DA(1),"M","AM",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,11)
 I X'="" S ^DGPT(DA(1),"M","AC",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,11)
 I X'="" X ^DD(45.02,11,1,992,1)
 S DIKZ(0)=$G(^DGPT(DA(1),"M",DA,0))
 S X=$P(DIKZ(0),U,12)
 I X'="" S ^DGPT(DA(1),"M","AC",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,12)
 I X'="" X ^DD(45.02,12,1,992,1)
 S DIKZ(0)=$G(^DGPT(DA(1),"M",DA,0))
 S X=$P(DIKZ(0),U,13)
 I X'="" S ^DGPT(DA(1),"M","AC",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,13)
 I X'="" X ^DD(45.02,13,1,992,1)
 S DIKZ(0)=$G(^DGPT(DA(1),"M",DA,0))
 S X=$P(DIKZ(0),U,14)
 I X'="" S ^DGPT(DA(1),"M","AC",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,14)
 I X'="" X ^DD(45.02,14,1,992,1)
 S DIKZ(0)=$G(^DGPT(DA(1),"M",DA,0))
 S X=$P(DIKZ(0),U,15)
 I X'="" S ^DGPT(DA(1),"M","AC",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,15)
 I X'="" X ^DD(45.02,15,1,992,1)
 G:'$D(DIKLM) A Q:$D(DISET)
END G ^DGPTXX9
