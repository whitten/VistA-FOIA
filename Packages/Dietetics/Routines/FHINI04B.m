FHINI04B	; ; 11-OCT-1995
	;;5.0;Dietetics;;Oct 11, 1995
	Q:'DIFQR(112)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q	Q
	;;^UTILITY(U,$J,112,1430,1)
	;;=.7^.1^5^20^93.9^^^5^.4^^14^564^25^^^^^610^68^.03
	;;^UTILITY(U,$J,112,1430,2)
	;;=.03^.7^^^^^0^^0^0^^^^.3
	;;^UTILITY(U,$J,112,1430,4)
	;;=^^^^^^0
	;;^UTILITY(U,$J,112,1430,20)
	;;=USDA  Std. Reference, Release 8
	;;^UTILITY(U,$J,112,1431,0)
	;;=PEPPERS, HOT, CHILI, RED, CND, CHILI SAUCE^1542-0^cups^245^100^N
	;;^UTILITY(U,$J,112,1431,1)
	;;=.9^.6^3.9^21^94.1^^^9^.5^^16^564^25^^^^^9590^30^.01
	;;^UTILITY(U,$J,112,1431,2)
	;;=.09^.6^^^^^0^^0^0^^^^.5
	;;^UTILITY(U,$J,112,1431,4)
	;;=^^^^^^0
	;;^UTILITY(U,$J,112,1431,20)
	;;=USDA  Std. Reference, Release 8
	;;^UTILITY(U,$J,112,1432,0)
	;;=PEPPERS, HOT, CHILI, RED, DRIED PODS^1543-0^oz.^28.3^100^N
	;;^UTILITY(U,$J,112,1432,1)
	;;=12.9^9.1^59.8^321^12.6^^^130^7.8^^240^1201^373^^^^^77000^12^.23
	;;^UTILITY(U,$J,112,1432,2)
	;;=1.33^10.5^^^^^0^^0^0^^^^7.4
	;;^UTILITY(U,$J,112,1432,4)
	;;=^^^^^^0
	;;^UTILITY(U,$J,112,1432,20)
	;;=USDA  Std. Reference, Release 8
	;;^UTILITY(U,$J,112,1434,0)
	;;=PEPPERS, GREEN, RAW^11-333^peppers^90^82^N
	;;^UTILITY(U,$J,112,1434,1)
	;;=.89^.19^6.43^27^92.19^^^9^.46^10^19^177^2^.12^.065^.116^.68^632^89.3^.066
	;;^UTILITY(U,$J,112,1434,2)
	;;=.03^.509^.08^.248^22^0^.093^.009^0^.028^.013^.102^63^.3^^^1.7
	;;^UTILITY(U,$J,112,1434,3)
	;;=.011^.033^.029^.046^.039^.011^.017^.027^.018^.037^.043^.018^.036^.127^.117^.033^.039^.036
	;;^UTILITY(U,$J,112,1434,4)
	;;=^0^0^.021^.001^.007^.011^^17
	;;^UTILITY(U,$J,112,1434,20)
	;;=USDA Std. Reference, Release 10;HCF Nutrition Research Foundation, Inc, 1990
	;;^UTILITY(U,$J,112,1435,0)
	;;=PEPPERS, GREEN, CKD WO/SALT^11-334^peppers^73^100^N
	;;^UTILITY(U,$J,112,1435,1)
	;;=.92^.2^6.7^28^91.87^^^9^.46^10^18^166^2^.12^.065^.115^^592^74.4^.059
	;;^UTILITY(U,$J,112,1435,2)
	;;=.03^.477^.079^.233^16^0^.096^.01^0^.029^.013^.106^59^.31^^^1.2
	;;^UTILITY(U,$J,112,1435,3)
	;;=.012^.034^.03^.048^.041^.011^.018^.029^.019^.039^.044^.019^.038^.132^.122^.034^.04^.037
	;;^UTILITY(U,$J,112,1435,4)
	;;=^0^0^.022^.001^.007^.012
	;;^UTILITY(U,$J,112,1435,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,1436,0)
	;;=PEPPERS, GREEN, STUFFED WITH BEEF AND CRUMBS^1547-0^peppers^185^100^N
	;;^UTILITY(U,$J,112,1436,1)
	;;=13^5.5^16.8^170^63.1^^^42^2.1^^121^258^314^^^^^280^40^.09
	;;^UTILITY(U,$J,112,1436,2)
	;;=.17^2.5^^^^^.34^^38^2.61^^^^1.6
	;;^UTILITY(U,$J,112,1436,4)
	;;=^^^^^^2.15
	;;^UTILITY(U,$J,112,1436,20)
	;;=USDA  Std. Reference, Release 8
	;;^UTILITY(U,$J,112,1437,0)
	;;=PEPPERS, SWEET, RED, RAW^11-821^peppers^73.8^82^N
	;;^UTILITY(U,$J,112,1437,1)
	;;=.89^.19^6.43^27^92.19^^^9^.46^10^19^177^2^.12^.065^.116^.68^5700^190^.066
	;;^UTILITY(U,$J,112,1437,2)
	;;=.03^.509^.08^.248^22^0^.093^.009^0^.028^.013^.102^570^.3^^^2
	;;^UTILITY(U,$J,112,1437,3)
	;;=.011^.033^.029^.046^.039^.011^.017^.027^.018^.037^.043^.018^.036^.127^.117^.033^.039^.036
	;;^UTILITY(U,$J,112,1437,4)
	;;=^0^0^.021^.001^.007^.011
	;;^UTILITY(U,$J,112,1437,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,1438,0)
	;;=PICKLES, DILL^11-937^pickles^65^100^N
	;;^UTILITY(U,$J,112,1438,1)
	;;=.62^.19^4.13^18^91.67^^^9^.53^11^21^116^1282^.14^.079^.015^^329^1.9^.014
	;;^UTILITY(U,$J,112,1438,2)
	;;=.029^.06^.054^.013^1^0^.033^.044^0^.048^.003^.077^33^3.4^^^1.2
	;;^UTILITY(U,$J,112,1438,3)
	;;=.005^.017^.019^.026^.026^.005^.004^.017^.01^.02^.04^.009^.021^.037^.177^.022^.014^.018
	;;^UTILITY(U,$J,112,1438,4)
	;;=^.002^.002^.041^0^.005^.003^^26
	;;^UTILITY(U,$J,112,1438,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,1439,0)
	;;=PICKLES, FRESH, (BREAD AND BUTTER PICKLES)^1559-0^slices^7.5^100^N
	;;^UTILITY(U,$J,112,1439,1)
	;;=.9^.2^17.9^73^78.7^^^32^1.8^^27^200^673^^^^^140^9^0
	;;^UTILITY(U,$J,112,1439,2)
	;;=.03^0^^^^^0^^0^0^^^^2.3
	;;^UTILITY(U,$J,112,1439,4)
	;;=^^^^^^0
	;;^UTILITY(U,$J,112,1439,20)
	;;=USDA  Std. Reference, Release 8
	;;^UTILITY(U,$J,112,1440,0)
	;;=PICKLES, SOUR^11-941^pickles^65^100^N
	;;^UTILITY(U,$J,112,1440,1)
	;;=.33^.2^2.25^11^94.08^^^0^.4^4^14^23^1208^.02^.085^.011^^145^1^0
	;;^UTILITY(U,$J,112,1440,2)
	;;=.01^0^.038^.009^.711^0^.035^.047^0^.052^.003^.082^15^3.13^^^1.2
	;;^UTILITY(U,$J,112,1440,3)
	;;=.003^.009^.01^.014^.014^.003^.002^.009^.006^.011^.021^.005^.011^.02^.095^.012^.008^.01
	;;^UTILITY(U,$J,112,1440,4)
	;;=^.002^.002^.044^0^.005^.003
	;;^UTILITY(U,$J,112,1440,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,1441,0)
	;;=PICKLES, SWEET^11-940^pickles^15^100^N
	;;^UTILITY(U,$J,112,1441,1)
	;;=.37^.26^31.81^117^65.26^^^4^.59^4^12^32^939^.08^.105^.015^^126^1.2^.009
	;;^UTILITY(U,$J,112,1441,2)
	;;=.032^.174^.12^.015^1^0^.046^.06^0^.067^.004^.106^13^2.3^^^1.1
