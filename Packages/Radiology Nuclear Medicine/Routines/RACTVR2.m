RACTVR2 ; ;11/17/04
 S X=DE(18),DIC=DIE
 X "D ^RABUL2 Q"
 S X=DE(18),DIC=DIE
 N RAXREF K RASET S RAXREF="ARES",RARAD=12,RAKILL="" D XREF^RAUTL2 S RASECOND="SRR" D SECXREF^RADD1 K RAKILL,RARAD
 S X=DE(18),DIC=DIE
 N RAXREF K RASET S RAXREF="ASTF",RARAD=15,RAKILL="" D XREF^RAUTL2 S RASECOND="SSR" D SECXREF^RADD1 K RAKILL,RARAD
 S X=DE(18),DIC=DIE
 K ^RARPT("ASTAT",$E(X,1,30),DA)
