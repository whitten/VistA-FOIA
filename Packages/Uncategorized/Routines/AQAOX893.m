AQAOX893 ; COMPILED XREF FOR FILE #9002168.9 ; 02/13/06
 ; 
 S DIKZK=1
 S DIKZ(0)=$G(^AQAO(9,DA,0))
 S X=$P(DIKZ(0),U,1)
 I X'="" S ^AQAO(9,"B",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,6)
 I X'="" S ^AQAO(9,"AC",$E(X,1,30),DA)=""
END G ^AQAOX894
