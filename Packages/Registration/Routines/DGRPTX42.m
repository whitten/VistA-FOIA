DGRPTX42 ; ;05/27/04
 S X=DE(4),DIC=DIE
 K ^DPT("APOS",$E(X,1,30),DA)
 S X=DE(4),DIC=DIE
 ;
 S X=DE(4),DIC=DIE
 I ($T(AVAFC^VAFCDD01)'="") S VAFCF=".323;" D AVAFC^VAFCDD01(DA)
