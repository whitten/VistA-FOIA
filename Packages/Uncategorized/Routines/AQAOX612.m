AQAOX612 ; COMPILED XREF FOR FILE #9002166.11 ; 02/13/06
 ; 
 S DA(1)=DA S DA=0
A1 ;
 I $D(DIKILL) K DIKLM S:DIKM1=1 DIKLM=1 G @DIKM1
0 ;
A S DA=$O(^AQAQX(DA(1),"PG",DA)) I DA'>0 S DA=0 G END
1 ;
 S DIKZ(0)=$G(^AQAQX(DA(1),"PG",DA,0))
 S X=$P(DIKZ(0),U,1)
 I X'="" K ^AQAQX(DA(1),"PG","B",$E(X,1,30),DA)
 G:'$D(DIKLM) A Q:$D(DIKILL)
END G ^AQAOX613
