IBDX51 ; COMPILED XREF FOR FILE #357.5 ; 02/13/06
 ; 
 S DIKZK=2
 S DIKZ(0)=$G(^IBE(357.5,DA,0))
 S X=$P(DIKZ(0),U,2)
 I X'="" K ^IBE(357.5,"C",$E(X,1,30),DA)
 S X=$P(DIKZ(0),U,1)
 I X'="" K ^IBE(357.5,"B",$E(X,1,30),DA)
END G ^IBDX52
