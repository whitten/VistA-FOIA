IBXF14 ; COMPILED XREF FOR FILE #357.1 ; 11/29/04
 ; 
 S DIKZK=1
 S DIKZ(0)=$G(^IBE(357.1,DA,0))
 S X=$P(DIKZ(0),U,1)
 I X'="" S ^IBE(357.1,"B",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,2)
 I X'="" S ^IBE(357.1,"C",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,14)
 I X'="" S ^IBE(357.1,"D",$E(X,1,30),DA)=""
END G ^IBXF15
