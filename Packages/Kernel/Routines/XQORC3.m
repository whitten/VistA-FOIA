XQORC3 ; COMPILED XREF FOR FILE #101.02 ; 11/17/04
 ; 
 S DA=0
A1 ;
 I $D(DIKILL) K DIKLM S:DIKM1=1 DIKLM=1 G @DIKM1
0 ;
A S DA=$O(^ORD(101,DA(1),2,DA)) I DA'>0 S DA=0 G END
1 ;
 S DIKZ(0)=$G(^ORD(101,DA(1),2,DA,0))
 S X=$P(DIKZ(0),U,1)
 I X'="" K ^ORD(101,DA(1),2,"B",$E(X,1,30),DA)
 S X=$P(DIKZ(0),U,1)
 I X'="" I $D(^ORD(101,"B",$E(X,1,30),DA(1),DA)),^(DA) K ^(DA)
 S X=$P(DIKZ(0),U,1)
 I X'="" D K22^XQORDD1
 S X=$P(DIKZ(0),U,1)
 I X'="" D REDOM^XQORD101
 G:'$D(DIKLM) A Q:$D(DIKILL)
END G ^XQORC4
