FHINI07A	; ; 11-OCT-1995
	;;5.0;Dietetics;;Oct 11, 1995
	Q:'DIFQR(112)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q	Q
	;;^UTILITY(U,$J,112,2541,3)
	;;=.107^.338^.38^.633^.49^.192^.148^.382^.279^.412^.532^.184^.445^.692^1.803^.442^.588^.443
	;;^UTILITY(U,$J,112,2541,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2542,0)
	;;=FF, PIZZA, WITH CHEESE^21-049^slices^63^100^N
	;;^UTILITY(U,$J,112,2542,1)
	;;=12.19^5.1^32.54^223^47.81^^^185^.92^25^179^174^533^1.29^.128^.368^^607^2^.29
	;;^UTILITY(U,$J,112,2542,2)
	;;=.26^3.94^.35^.07^93^.53^.692^.087^15^2.445^1.571^.779^117^2.36
	;;^UTILITY(U,$J,112,2542,3)
	;;=.145^.397^.504^.995^.868^.276^.121^.582^.552^.635^.478^.373^.361^.743^3.044^.28^1.197^.625
	;;^UTILITY(U,$J,112,2542,4)
	;;=.057^.078^.398^1.319^.089^.561^1.482^^^21.4
	;;^UTILITY(U,$J,112,2542,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2543,0)
	;;=FF, PIZZA, WITH CHEESE, MEAT, AND VEGETABLES^21-050^slices^79^100^N
	;;^UTILITY(U,$J,112,2543,1)
	;;=16.47^6.79^26.95^233^47.72^^^128^1.94^23^166^226^484^1.41^.15^.15^^663^2^.27
	;;^UTILITY(U,$J,112,2543,2)
	;;=.22^2.48^1.05^.12^34^.46^1.088^.069^26^1.943^3.219^1.158^128^2.07
	;;^UTILITY(U,$J,112,2543,3)
	;;=.199^.571^.689^1.344^1.218^.376^.164^.757^.7^.85^.746^.51^.612^1.111^3.812^.555^1.462^.805
	;;^UTILITY(U,$J,112,2543,4)
	;;=.052^.035^.19^1.102^.421^.497^2.798
	;;^UTILITY(U,$J,112,2543,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2544,0)
	;;=FF, PIZZA, WITH PEPPERONI^21-051^slices^71^100^N
	;;^UTILITY(U,$J,112,2544,1)
	;;=14.26^9.8^27.98^255^46.54^^^91^1.32^12^106^215^376^.73^.09^.14^^397^2.3^.19
	;;^UTILITY(U,$J,112,2544,2)
	;;=.33^4.29^.35^.08^74^.26^1.583^.058^20^3.149^4.422^1.641^77^1.42
	;;^UTILITY(U,$J,112,2544,3)
	;;=.167^.474^.595^1.159^1.024^.325^.144^.671^.634^.739^.588^.439^.462^.904^3.463^.381^1.361^.722
	;;^UTILITY(U,$J,112,2544,4)
	;;=.048^.05^.164^1.767^.226^1.072^4.191
	;;^UTILITY(U,$J,112,2544,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2545,0)
	;;=FF, SALAD, VEGETABLE, TOSSED, WITHOUT DRESSING^21-052^cups^138^100^N
	;;^UTILITY(U,$J,112,2545,1)
	;;=1.25^.07^3.22^16^95.51^^^13^.63^11^39^172^26^.21^.05^.147^^1136^23.2^.03
	;;^UTILITY(U,$J,112,2545,2)
	;;=.05^.55^.12^.08^37^0^^^0^.01^.004^.034^114^.53
	;;^UTILITY(U,$J,112,2545,3)
	;;=.01^.05^.066^.068^.069^.014^.014^.046^.027^.059^.065^.022^.05^.137^.228^.046^.073^.044
	;;^UTILITY(U,$J,112,2545,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2546,0)
	;;=FF, SALAD, VEG, TOSSED, WO/ DRESSING, W/ CHEESE AND EGG^21-053^cups^144.7^100^N
	;;^UTILITY(U,$J,112,2546,1)
	;;=4.04^2.67^2.19^47^90.46^^^46^.31^11^61^171^55^.46^.041^.126^^379^4.5^.04
	;;^UTILITY(U,$J,112,2546,2)
	;;=.08^.45^.24^.05^39^.14^^^45^1.371^.81^.219^53^.76
	;;^UTILITY(U,$J,112,2546,3)
	;;=.047^.157^.23^.318^.276^.092^.047^.191^.152^.243^.185^.104^.153^.337^.76^.106^.272^.215
	;;^UTILITY(U,$J,112,2546,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2547,0)
	;;=FF, SALAD, VEGETABLE, TOSSED, WO/ DRESSING, W/ CHICKEN^21-054^cups^145.3^100^N
	;;^UTILITY(U,$J,112,2547,1)
	;;=8^1^1.71^48^87.05^^^17^.5^15^78^205^96^.41^.043^.114^^429^8^.05
	;;^UTILITY(U,$J,112,2547,2)
	;;=.06^2.7^.27^.2^31^.09^^^33^.265^.311^.255^44^.7
	;;^UTILITY(U,$J,112,2547,3)
	;;=.091^.337^.424^.589^.663^.211^.102^.316^.264^.396^.475^.239^.428^.732^1.208^.388^.325^.273
	;;^UTILITY(U,$J,112,2547,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2548,0)
	;;=FF, SALAD, VEG, TOSSED, WO/ DRESSING, W/ PASTA AND SEAFOOD^21-055^cups^278^100^N
	;;^UTILITY(U,$J,112,2548,1)
	;;=3.94^5^7.67^91^80.37^^^17^.76^12^49^144^377^.4^.087^.16^^1498^9.2^.07
	;;^UTILITY(U,$J,112,2548,2)
	;;=.05^.85^.09^.08^24^.41^^^12^.616^1.156^2.178^153^1.12
	;;^UTILITY(U,$J,112,2548,3)
	;;=.046^.147^.179^.287^.254^.088^.061^.165^.118^.187^.224^.092^.186^.337^.829^.172^.224^.161
	;;^UTILITY(U,$J,112,2548,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2549,0)
	;;=FF, SALAD, VEGETABLE, TOSSED, WO/ DRESSING, W/ SHRIMP^21-056^cups^157.3^100^N
	;;^UTILITY(U,$J,112,2549,1)
	;;=6.15^1.05^2.8^45^89.1^^^25^.38^16^68^171^207^.54^.068^.06^^335^3.9^.05
	;;^UTILITY(U,$J,112,2549,2)
	;;=.07^.49^.21^.06^37^1.6^^^76^.276^.348^.205^33^.9
	;;^UTILITY(U,$J,112,2549,3)
	;;=.084^.256^.311^.476^.487^.166^.084^.27^.206^.314^.479^.126^.337^.635^1.028^.321^.207^.277
	;;^UTILITY(U,$J,112,2549,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2550,0)
	;;=FF, SALAD, VEG, TOSSED, WO/ DRESSING, W/ TURKEY,HAM,CHEESE^21-057^cups^217.3^100^N
	;;^UTILITY(U,$J,112,2550,1)
	;;=7.98^4.93^1.45^82^82.45^^^72^.6^15^123^123^228^.96^.051^.11^^323^5^.12
	;;^UTILITY(U,$J,112,2550,2)
	;;=.12^1.83^.28^.13^31^.26^^^43^2.508^1.585^.443^42^1.29
