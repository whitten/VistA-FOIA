FHINI036	; ; 11-OCT-1995
	;;5.0;Dietetics;;Oct 11, 1995
	Q:'DIFQR(112)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q	Q
	;;^UTILITY(U,$J,112,984,2)
	;;=.565^.586^.893^.114^0^1.32^.115^.005^5^2.408^1.326^.119^3^4.1^^18^1.2
	;;^UTILITY(U,$J,112,984,3)
	;;=.15^.453^.572^.923^.75^.22^.103^.499^.474^.671^.42^.256^.378^.841^2.013^.274^.863^.528
	;;^UTILITY(U,$J,112,984,4)
	;;=^0^.004^1.025^.009^1.34^1.313
	;;^UTILITY(U,$J,112,984,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,985,0)
	;;=COCOA, BEVERAGE POWDER, DRY, WO/MILK^14-175^heap-tbsp.^7.1^100^N
	;;^UTILITY(U,$J,112,985,1)
	;;=3.3^3.1^90.3^349^.9^^^37^3.14^98^128^591^210^1.55^.705^.707^^20^.7^.032
	;;^UTILITY(U,$J,112,985,2)
	;;=.146^.511^.05^.01^5.7^0^.087^.004^0^1.834^1.009^.091^2^1.8^^36^5.8
	;;^UTILITY(U,$J,112,985,3)
	;;=.045^.111^.108^.171^.142^.025^.035^.137^.108^.166^.157^.048^.123^.28^.412^.127^.112^.113
	;;^UTILITY(U,$J,112,985,4)
	;;=^0^.003^.781^.007^1.02^1^^^2.6
	;;^UTILITY(U,$J,112,985,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,987,0)
	;;=COCOA, DRY POWDER, HI-FAT OR BREAKFAST, PLAIN^781-0^oz.^28.3^100^N
	;;^UTILITY(U,$J,112,987,1)
	;;=16.8^23.7^48.3^299^3^^^133^10.7^^648^1522^6^^^^^30^0^.11
	;;^UTILITY(U,$J,112,987,2)
	;;=.46^2.4^^^^^0^^0^13^^^^5
	;;^UTILITY(U,$J,112,987,4)
	;;=^^^^^^9
	;;^UTILITY(U,$J,112,987,20)
	;;=USDA  Std. Reference, Release 8
	;;^UTILITY(U,$J,112,988,0)
	;;=COCOA, DRY POWDER, HI-FAT OR BREAKFAST, PROC W/ALKALI^782-0^oz.^28.3^100^N
	;;^UTILITY(U,$J,112,988,1)
	;;=16.8^23.7^45.4^295^3^^^133^10.7^^648^651^717^^^^^30^0^.11
	;;^UTILITY(U,$J,112,988,2)
	;;=.46^2.4^^^^^0^^0^13^^^^8.2
	;;^UTILITY(U,$J,112,988,4)
	;;=^^^^^^9
	;;^UTILITY(U,$J,112,988,20)
	;;=USDA  Std. Reference, Release 8
	;;^UTILITY(U,$J,112,989,0)
	;;=COCOA, DRY POWDER, LOW FAT^787-0^oz.^28.3^100^N
	;;^UTILITY(U,$J,112,989,1)
	;;=20.2^7.9^58^187^4.4^^^153^10.7^^752^1522^6^^^^^10^0^.11
	;;^UTILITY(U,$J,112,989,2)
	;;=.46^2.4^^^^^0^^0^4^^^^5.7
	;;^UTILITY(U,$J,112,989,4)
	;;=^^^^^^3
	;;^UTILITY(U,$J,112,989,20)
	;;=USDA  Std. Reference, Release 8
	;;^UTILITY(U,$J,112,990,0)
	;;=COCONUT, DRIED, SWEETENED, SHREDDED^12-179^oz.^28.3^100^N
	;;^UTILITY(U,$J,112,990,1)
	;;=2.88^35.49^47.67^501^12.55^^^15^1.92^50^107^337^262^1.82^.313^2.475^^0^.7^.031
	;;^UTILITY(U,$J,112,990,2)
	;;=.02^.474^.722^.271^8.1^0^.388^^0^31.468^1.51^.388^0^1.42^^^4.5
	;;^UTILITY(U,$J,112,990,3)
	;;=.034^.105^.113^.214^.127^.054^.057^.146^.089^.175^.473^.066^.147^.282^.659^.137^.119^.149
	;;^UTILITY(U,$J,112,990,4)
	;;=1.976^15.743^6.216^3.008^^1.838^1.51
	;;^UTILITY(U,$J,112,990,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,991,0)
	;;=COCONUT MILK, LIQUID FROM GRATED MEAT AND WATER^12-117^fl. oz.^30^100^N
	;;^UTILITY(U,$J,112,991,1)
	;;=2.29^23.84^5.54^230^67.62^^^16^1.64^37^100^263^15^.67^.266^.916^^0^2.8^.026
	;;^UTILITY(U,$J,112,991,2)
	;;=0^.76^.183^.033^16.1^0^.261^^0^21.14^1.014^.261^0^.72^^^2.2
	;;^UTILITY(U,$J,112,991,3)
	;;=.027^.083^.09^.17^.101^.043^.045^.116^.071^.139^.376^.053^.117^.224^.524^.108^.095^.118
	;;^UTILITY(U,$J,112,991,4)
	;;=1.327^10.576^4.176^2.021^^1.234^1.014
	;;^UTILITY(U,$J,112,991,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,992,0)
	;;=COD, BROILED^15-016^oz.^28.3^100^N
	;;^UTILITY(U,$J,112,992,1)
	;;=22.83^.86^0^105^75.92^^^14^.49^42^138^244^78^.58^.036^.02^^46^1^.088
	;;^UTILITY(U,$J,112,992,2)
	;;=.079^2.513^.18^.283^8.1^1.048^.006^.001^55^.168^.124^.292^14^1.49^^^0
	;;^UTILITY(U,$J,112,992,3)
	;;=.256^1.001^1.052^1.856^2.097^.676^.245^.891^.771^1.176^1.366^.672^1.381^2.338^3.408^1.096^.807^.932
	;;^UTILITY(U,$J,112,992,4)
	;;=^^.012^.117^.021^.038^.078^.028^^37.6
	;;^UTILITY(U,$J,112,992,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,993,0)
	;;=COD, CND^15-017^cups^140^100^N
	;;^UTILITY(U,$J,112,993,1)
	;;=22.76^.86^0^105^75.61^^^21^.49^41^260^528^218^.58^.037^.02^^46^1^.087
	;;^UTILITY(U,$J,112,993,2)
	;;=.079^2.505^.17^.282^8.1^1.045^.006^.001^55^.167^.124^.291^14^1.49^^^0
	;;^UTILITY(U,$J,112,993,3)
	;;=.255^.998^1.049^1.85^2.091^.674^.244^.889^.768^1.173^1.362^.67^1.377^2.331^3.398^1.093^.805^.929
	;;^UTILITY(U,$J,112,993,4)
	;;=^^.012^.116^.02^.038^.078^.028
	;;^UTILITY(U,$J,112,993,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,994,0)
	;;=COFFEE, INSTANT, DRY POWDER^14-214^tsp.^.8^100^N
	;;^UTILITY(U,$J,112,994,1)
	;;=12.2^.5^41.1^241^3.1^^^141^4.41^327^303^3535^37^.35^.139^1.712^^0^0^.008
	;;^UTILITY(U,$J,112,994,2)
	;;=.074^28.173^.097^.029^0^0^.18^.015^0^.197^.041^.196^0^8.8^^3142^0
	;;^UTILITY(U,$J,112,994,3)
	;;=.03^.142^.172^.478^.096^.023^.202^.262^.165^.276^.053^.165^.335^.478^2.03^.441^.351^.126
	;;^UTILITY(U,$J,112,994,4)
	;;=^^^.146^^.035^.04^^^12.6
