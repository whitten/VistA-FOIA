FHINI04R	; ; 11-OCT-1995
	;;5.0;Dietetics;;Oct 11, 1995
	Q:'DIFQR(112)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q	Q
	;;^UTILITY(U,$J,112,1597,3)
	;;=.205^.801^.842^1.486^1.679^.541^.196^.714^.617^.942^1.094^.538^1.106^1.872^2.729^.878^.646^.746
	;;^UTILITY(U,$J,112,1597,4)
	;;=^^.183^.56^.305^.186^.949
	;;^UTILITY(U,$J,112,1597,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,1598,0)
	;;=SALT, TABLE^1963-0^tsp.^5.5^100^N
	;;^UTILITY(U,$J,112,1598,1)
	;;=0^0^0^0^.2^^^45^.1^^0^8^38758^^^^^0^0^0
	;;^UTILITY(U,$J,112,1598,2)
	;;=0^0^^^^^0^^0^0^^^^99.8
	;;^UTILITY(U,$J,112,1598,4)
	;;=^^^^^^0^^.01
	;;^UTILITY(U,$J,112,1598,20)
	;;=USDA  Std. Reference, Release 8
	;;^UTILITY(U,$J,112,1599,0)
	;;=SALT STICKS, REGULAR, WO/SALT COATING^1965-1^sticks^10^100^N
	;;^UTILITY(U,$J,112,1599,1)
	;;=12^2.9^75.3^384^5^^^28^4.3^^99^92^700^^^^^0^0^.65
	;;^UTILITY(U,$J,112,1599,2)
	;;=.52^6.42^^^^^.86^^3^.6^^^^4.8
	;;^UTILITY(U,$J,112,1599,4)
	;;=^^^^^^1.24
	;;^UTILITY(U,$J,112,1599,20)
	;;=USDA  Std. Reference, Release 8
	;;^UTILITY(U,$J,112,1600,0)
	;;=SALT STICKS, REGULAR, W/SALT COATING^1965-2^sticks^10^100^N
	;;^UTILITY(U,$J,112,1600,1)
	;;=12^2.9^75.3^384^5^^^28^4.3^^99^92^1674^^^^^0^0^.65
	;;^UTILITY(U,$J,112,1600,2)
	;;=.52^6.42^^^^^.86^^3^.6^^^^4.8
	;;^UTILITY(U,$J,112,1600,4)
	;;=^^^^^^1.24
	;;^UTILITY(U,$J,112,1600,20)
	;;=USDA  Std. Reference, Release 8
	;;^UTILITY(U,$J,112,1601,0)
	;;=SALT STICKS, VIENNA BREAD TYPE^1966-0^sticks^35^100^N
	;;^UTILITY(U,$J,112,1601,1)
	;;=9.5^3.1^58^304^25^^^45^.8^^89^94^1565^^^^^0^0^.05
	;;^UTILITY(U,$J,112,1601,2)
	;;=.08^.8^^^^^.84^^3^.69^^^^4.4
	;;^UTILITY(U,$J,112,1601,4)
	;;=^^^^^^1.39
	;;^UTILITY(U,$J,112,1601,20)
	;;=USDA  Std. Reference, Release 8
	;;^UTILITY(U,$J,112,1602,0)
	;;=SANDWICH SPREAD, DIET, 5 CAL/TSP^1968-0^tbsp.^15^100^N
	;;^UTILITY(U,$J,112,1602,1)
	;;=1^9^8^112^80.2^^^15^.7^^20^92^626^^^^^280^6^.01
	;;^UTILITY(U,$J,112,1602,2)
	;;=.03^0^^^^^4^^50^1^^^^1.8
	;;^UTILITY(U,$J,112,1602,4)
	;;=^^^^^^2
	;;^UTILITY(U,$J,112,1602,20)
	;;=USDA  Std. Reference, Release 8
	;;^UTILITY(U,$J,112,1603,0)
	;;=SARDINES, CND, IN OIL, DRAINED SOLIDS^15-088^oz.^28.3^100^N
	;;^UTILITY(U,$J,112,1603,1)
	;;=24.62^11.45^0^208^59.61^^^382^2.92^39^490^397^505^1.31^.186^.108^^224^0^.08
	;;^UTILITY(U,$J,112,1603,2)
	;;=.227^5.245^.642^.167^11.8^8.94^3.543^.498^142^1.528^3.869^5.148^67^3.38^^^0
	;;^UTILITY(U,$J,112,1603,3)
	;;=.276^1.079^1.134^2.001^2.26^.729^.264^.961^.831^1.268^1.473^.725^1.489^2.52^3.674^1.181^.87^1.004
	;;^UTILITY(U,$J,112,1603,4)
	;;=^^.192^.993^.22^.343^2.145
	;;^UTILITY(U,$J,112,1603,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,1604,0)
	;;=SAUERKRAUT, CND^11-439^cups^235^100^N
	;;^UTILITY(U,$J,112,1604,1)
	;;=.91^.14^4.28^19^92.52^^^30^1.47^13^20^170^661^.19^.096^.151^^18^14.7^.021
	;;^UTILITY(U,$J,112,1604,2)
	;;=.022^.143^.093^.13^23.7^0^.026^.025^0^.035^.013^.061^2^2.15^^^2.5
	;;^UTILITY(U,$J,112,1604,4)
	;;=.001^.001^0^.021^0^.002^.01^0^25
	;;^UTILITY(U,$J,112,1604,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,1605,0)
	;;=SAUERKRAUT JUICE, CND^1978-0^fl. oz.^30.25^100^N
	;;^UTILITY(U,$J,112,1605,1)
	;;=.7^0^2.3^10^94.6^^^37^1.1^^14^140^787^^^^^50^18^.03
	;;^UTILITY(U,$J,112,1605,2)
	;;=.04^.2^^^^^0^^0^0^^^^2.4
	;;^UTILITY(U,$J,112,1605,4)
	;;=^^^^^^0
	;;^UTILITY(U,$J,112,1605,20)
	;;=USDA  Std. Reference, Release 8
	;;^UTILITY(U,$J,112,1606,0)
	;;=SAUSAGE, BROWN&SERVE, BEFORE BROWNING^1987-0^oz.^28.3^100^N
	;;^UTILITY(U,$J,112,1606,1)
	;;=13.5^36^2.7^393^45.3^^^7^2.4^^162^269^958^^^^^0^0^.79
	;;^UTILITY(U,$J,112,1606,2)
	;;=.34^3.7^^^^^3^^62^13^^^^2.5
	;;^UTILITY(U,$J,112,1606,4)
	;;=^^^^^^15
	;;^UTILITY(U,$J,112,1606,20)
	;;=USDA  Std. Reference, Release 8
	;;^UTILITY(U,$J,112,1607,0)
	;;=SAUSAGE, COUNTRY-STYLE^1992-0^oz.^28.3^100^N
	;;^UTILITY(U,$J,112,1607,1)
	;;=15.1^31.1^0^345^49.9^^^9^2.3^^168^269^958^^^^^0^0^.22
	;;^UTILITY(U,$J,112,1607,2)
	;;=.19^3.1^^^^^2.8^^62^11.2^^^^3.9
	;;^UTILITY(U,$J,112,1607,4)
	;;=^^^^^^13.06
	;;^UTILITY(U,$J,112,1607,20)
	;;=USDA  Std. Reference, Release 8
	;;^UTILITY(U,$J,112,1608,0)
	;;=HAM, DEVILED, CND^1993-0^oz.^28.3^100^N
	;;^UTILITY(U,$J,112,1608,1)
	;;=13.9^32.3^0^351^50.5^^^8^.62^^92^222^1234^^^^^0^0^.14
	;;^UTILITY(U,$J,112,1608,2)
	;;=.1^1.6^^^^^2.91^^65^11.63^^^^3.3
	;;^UTILITY(U,$J,112,1608,4)
	;;=^^^^^^13.57
	;;^UTILITY(U,$J,112,1608,20)
	;;=USDA  Std. Reference, Release 8
	;;^UTILITY(U,$J,112,1609,0)
	;;=FRANKFURTERS, RAW, W/NFDM & CEREAL^1998-0^franks^45^100^N
	;;^UTILITY(U,$J,112,1609,1)
	;;=14.2^21.7^0^274^50.5^^^20^1.9^^133^220^1100^^^^^0^0^.16
	;;^UTILITY(U,$J,112,1609,2)
	;;=.2^2.7^^^^^1^^65^9^^^^3.1
	;;^UTILITY(U,$J,112,1609,4)
	;;=^^^^^^9
	;;^UTILITY(U,$J,112,1609,20)
	;;=USDA  Std. Reference, Release 8
