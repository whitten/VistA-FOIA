PSGWI029 ; ; 04-JAN-1994
 ;;2.3; Automatic Replenishment/Ward Stock ;;4 JAN 94
 F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,"DIBT",23,0)
 ;;=PSGW ITEM LOC^2840915^^58.17^^^^
 ;;^UTILITY(U,$J,"DIBT",23,2,1)
 ;;=58.17^.01^ITEM ADDRESS CODE^^
 ;;^UTILITY(U,$J,"DIBT",23,2,1,"IX")
 ;;=^PSI(58.17,"B",^PSI(58.17,^2
 ;;^UTILITY(U,$J,"DIBT",23,2,2)
 ;;=58.17^^("A"="A")^'@"@B^;L1
 ;;^UTILITY(U,$J,"DIBT",23,2,2,"CM")
 ;;=S X="A"="A" I D0>0 S X(2)=X
 ;;^UTILITY(U,$J,"DIBT",23,2,2,"F")
 ;;=0
 ;;^UTILITY(U,$J,"DIBT",23,2,2,"T")
 ;;=1
 ;;^UTILITY(U,$J,"DIBT",23,"DIPT")
 ;;=PSGW ITEM LOC
 ;;^UTILITY(U,$J,"DIBT",25,0)
 ;;=PSGW INV TYPE^2840915^^58.16^^^^
 ;;^UTILITY(U,$J,"DIBT",25,2,1)
 ;;=58.16^.01^NAME^^
 ;;^UTILITY(U,$J,"DIBT",25,2,1,"IX")
 ;;=^PSI(58.16,"B",^PSI(58.16,^2
 ;;^UTILITY(U,$J,"DIBT",25,2,2)
 ;;=58.16^^("A"="A")^'"@B^;"";L1
 ;;^UTILITY(U,$J,"DIBT",25,2,2,"CM")
 ;;=S X="A"="A" I D0>0 S X(2)=X
 ;;^UTILITY(U,$J,"DIBT",25,2,2,"F")
 ;;=0
 ;;^UTILITY(U,$J,"DIBT",25,2,2,"T")
 ;;=1
 ;;^UTILITY(U,$J,"DIBT",46,0)
 ;;=PSGW SHOW AREA OF USE^2890627.1455^^58.1^^^^
 ;;^UTILITY(U,$J,"DIBT",46,2,1)
 ;;=58.1^^INPATIENT SITE^#"4^
 ;;^UTILITY(U,$J,"DIBT",46,2,1,"CM")
 ;;=S Y(1)=$S($D(^PSI(58.1,D0,"SITE")):^("SITE"),1:"") S X=$S('$D(^PS(59.4,+$P(Y(1),U,1),0)):"",1:$P(^(0),U,1)) I D0>0 S X(1)=X
 ;;^UTILITY(U,$J,"DIBT",46,2,2)
 ;;=58.1^.01^AREA OF USE (AOU)^^
 ;;^UTILITY(U,$J,"DIBT",46,2,2,"IX")
 ;;=^PSI(58.1,"B",^PSI(58.1,^2
 ;;^UTILITY(U,$J,"DIBT",49,0)
 ;;=PSGW PERCENTAGE^2880212^^58.1^^^^
 ;;^UTILITY(U,$J,"DIBT",49,2,1)
 ;;=58.12^5^PERCENTAGE OF STOCK ON HAND^@^
 ;;^UTILITY(U,$J,"DIBT",49,2,1,58.1)
 ;;=1
 ;;^UTILITY(U,$J,"DIBT",49,2,1,58.11)
 ;;=1
 ;;^UTILITY(U,$J,"DIBT",49,2,1,"ASK")
 ;;=
 ;;^UTILITY(U,$J,"DIBT",49,2,1,"F")
 ;;=59.99999
 ;;^UTILITY(U,$J,"DIBT",49,2,1,"T")
 ;;=100
 ;;^UTILITY(U,$J,"DIBT",49,2,2)
 ;;=58.11^^ITEM^@".01^
 ;;^UTILITY(U,$J,"DIBT",49,2,2,58.1)
 ;;=1
 ;;^UTILITY(U,$J,"DIBT",49,2,2,"CM")
 ;;=S Y(1)=$S($D(^PSI(58.1,D0,1,D1,0)):^(0),1:"") S X=$S('$D(^PSDRUG(+$P(Y(1),U,1),0)):"",1:$P(^(0),U,1)) I D1>0 S X(2)=X
 ;;^UTILITY(U,$J,"DIBT",49,2,3)
 ;;=58.12^^DATE(DATE/TIME FOR INVENTORY)^@"^^^D
 ;;^UTILITY(U,$J,"DIBT",49,2,3,58.1)
 ;;=1
 ;;^UTILITY(U,$J,"DIBT",49,2,3,58.11)
 ;;=1
 ;;^UTILITY(U,$J,"DIBT",49,2,3,"ASK")
 ;;=
 ;;^UTILITY(U,$J,"DIBT",49,2,3,"CM")
 ;;=S Y(1)=$S($D(^PSI(58.1,D0,1,D1,1,D2,0)):^(0),1:"") S X=$S('$D(^PSI(58.19,+$P(Y(1),U,1),0)):"",1:$P(^(0),U,1)),X=$P(X,".",1) I D2>0 S X(3)=X
 ;;^UTILITY(U,$J,"DIBT",49,2,3,"F")
 ;;=2870700.99999
 ;;^UTILITY(U,$J,"DIBT",49,2,3,"T")
 ;;=z
 ;;^UTILITY(U,$J,"DIE",22,0)
 ;;=PSGW WARD INVENTORY^2880502^^58.2^^^^
 ;;^UTILITY(U,$J,"DIE",22,"DIAB",1,2,58.22,0)
 ;;=ALL
 ;;^UTILITY(U,$J,"DIE",22,"DR",1,58.2)
 ;;=.01;2;S PSGWSK=0,PSGWSKP=0;@1;S PSGWSK=$O(^PSI(58.2,DA,1,"D",PSGWSK));I 'PSGWSK S PSGWS=$S(PSGWSKP>0:(PSGWSKP+100),1:100),Y="@2";S PSGWSKP=PSGWSK;S Y="@1";@2;1;K PSGWS,PSGWSK,PSGWSKP,X,Y,%X,%Y,W;
 ;;^UTILITY(U,$J,"DIE",22,"DR",2,58.21)
 ;;=.01;1;I $P(^PSI(58.2,D0,1,D1,0),"^",2)'="" S Y="@3";2///^S X=PSGWS;S PSGWS=PSGWS+100;@3;
 ;;^UTILITY(U,$J,"DIE",22,"DR",3,58.22)
 ;;=.01
 ;;^UTILITY(U,$J,"DIE",28,0)
 ;;=PSGW AREA OF USE EDIT^2930527.1056^^58.1^^^^
 ;;^UTILITY(U,$J,"DIE",28,"DR",1,58.1)
 ;;=.01;4;.5;.75;5;6;S:X'=1 Y=2;7;2;
 ;;^UTILITY(U,$J,"DIE",28,"DR",2,58.14)
 ;;=.01;1;2;
 ;;^UTILITY(U,$J,"DIE",28,"DR",3,58.27)
 ;;=.01;1;
 ;;^UTILITY(U,$J,"DIE",34,0)
 ;;=PSGW ENTER/EDIT AMIS DATA^2880502^^50^^^^
 ;;^UTILITY(U,$J,"DIE",34,"DR",1,50)
 ;;=12;13;15;301;302;K %X,%Y;
 ;;^UTILITY(U,$J,"DIE",114,0)
 ;;=PSGW INACTIVATE ITEM^2890330^^58.1^^^^
 ;;^UTILITY(U,$J,"DIE",114,"DR",1,58.1)
 ;;=1;K %DT,%X,C,DIG,DIH,DIU;K DIV,K,PSGWDRUG,W,X,Y;
 ;;^UTILITY(U,$J,"DIE",114,"DR",2,58.11)
 ;;=30;S:X="" Y="@1";31;S:X'="O" Y="@1";33;@1;
 ;;^UTILITY(U,$J,"DIPT",54,0)
 ;;=PSGW ITEM LOC^2840915^^58.17^^^^
 ;;^UTILITY(U,$J,"DIPT",54,"F",1)
 ;;=W "";"";Z;"W """~.01;C7;"ITEM ADDR CODE"~.5~
 ;;^UTILITY(U,$J,"DIPT",54,"H")
 ;;=AREA OF USE ITEM LOCATION EXPANSIONS
 ;;^UTILITY(U,$J,"DIPT",55,0)
 ;;=PSGW INV TYPE^2840915^^58.16^^^^
 ;;^UTILITY(U,$J,"DIPT",55,"F",1)
 ;;=W "";"";Z;"W """~.01;C6;S~1,.01~
 ;;^UTILITY(U,$J,"DIPT",55,"H")
 ;;=AREA OF USE INVENTORY TYPES
 ;;^UTILITY(U,$J,"DIPT",56,0)
 ;;=PSGW PERCENTAGE^2880211^^58.1^^^^
