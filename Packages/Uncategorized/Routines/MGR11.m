MGR11 ;NEW ROUTINE FOR DEMO [ 08/07/01  11:51 AM ]
START ;LINE TAG LINE
 S X=2,Y=3
 S Z=X+Y
 WRITE "BOY THIS IS FUN",!
 WRITE ?5,X,?10,Y,?15,Z,!
END ;
 KILL X,Y,Z QUIT
