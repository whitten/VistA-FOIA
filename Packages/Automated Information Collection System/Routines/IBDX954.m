IBDX954 ; COMPILED XREF FOR FILE #357.9511 ; 02/13/06
 ; 
 S DA=0
A1 ;
 I $D(DIKILL) K DIKLM S:DIKM1=1 DIKLM=1 G @DIKM1
0 ;
A S DA=$O(^IBD(357.95,DA(1),11,DA)) I DA'>0 S DA=0 G END
1 ;
 S DIKZ(0)=$G(^IBD(357.95,DA(1),11,DA,0))
 S X=$P(DIKZ(0),U,1)
 I X'="" K ^IBD(357.95,DA(1),11,"B",$E(X,1,30),DA)
 G:'$D(DIKLM) A Q:$D(DIKILL)
END G ^IBDX955
