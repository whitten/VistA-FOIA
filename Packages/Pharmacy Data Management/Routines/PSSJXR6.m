PSSJXR6 ; COMPILED XREF FOR FILE #55.05 ; 01/17/11
 ; 
 S DA=0
A1 ;
 I $D(DIKILL) K DIKLM S:DIKM1=1 DIKLM=1 G @DIKM1
0 ;
A S DA=$O(^PS(55,DA(1),"NVA",DA)) I DA'>0 S DA=0 G END
1 ;
 S DIKZ(0)=$G(^PS(55,DA(1),"NVA",DA,0))
 S X=$P(DIKZ(0),U,6)
 I X'="" K:$G(PSODEATH) ^PS(55,DA(1),"NVA","APSOD",DA)
 S X=$P(DIKZ(0),U,10)
 I X'="" K ^PS(55,"ADCDT",$E(X,1,30),DA(1),DA)
 S DIKZ(999999911)=$G(^PS(55,DA(1),"NVA",DA,999999911))
 S X=$P(DIKZ(999999911),U,1)
 I X'="" K ^PS(55,"APCC",$E(X,1,30),DA(1),DA)
 S X=$P(DIKZ(0),U,1)
 I X'="" K ^PS(55,DA(1),"NVA","B",$E(X,1,30),DA)
 G:'$D(DIKLM) A Q:$D(DIKILL)
END G ^PSSJXR7
