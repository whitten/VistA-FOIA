FHINI00P	; ; 11-OCT-1995
	;;5.0;Dietetics;;Oct 11, 1995
	Q:'DIFQR(112)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q	Q
	;;^UTILITY(U,$J,112,90,3)
	;;=.501^1.603^2.148^3.478^2.816^.89^.328^1.714^1.714^2.376^1.285^.963^1.224^2.693^7.434^.751^3.439^1.931
	;;^UTILITY(U,$J,112,90,4)
	;;=.005^.006^.02^.053^.004^.024^.05^0
	;;^UTILITY(U,$J,112,90,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,91,0)
	;;=MILK, BUTTERMILK, DRIED^1-094^tbsp.^6.5^100^N
	;;^UTILITY(U,$J,112,91,1)
	;;=34.3^5.78^49^386.897^2.97^^^1184.4^.3^109.5^932.6^1592^517.4^4.02^.111^.023^^218^5.66^.392
	;;^UTILITY(U,$J,112,91,2)
	;;=1.579^.876^3.17^.338^47.4^3.823^.131^.084^69.3^3.598^1.669^.215^54^7.95^^^0
	;;^UTILITY(U,$J,112,91,3)
	;;=.484^1.548^2.075^3.36^2.72^.86^.317^1.656^1.656^2.296^1.242^.93^1.183^2.602^7.183^.726^3.322^1.866
	;;^UTILITY(U,$J,112,91,4)
	;;=.145^.162^.581^1.52^.129^.7^1.454^0
	;;^UTILITY(U,$J,112,91,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,92,0)
	;;=MILK, CND, CONDENSED, SWEETENED^1-095^fl oz.^38.2^100^N
	;;^UTILITY(U,$J,112,92,1)
	;;=7.91^8.7^54.4^320.777^27.16^^^283.5^.19^25.65^253.3^371.4^127^.94^.015^.006^^328^2.6^.09
	;;^UTILITY(U,$J,112,92,2)
	;;=.416^.21^.75^.051^11.2^.444^.216^.121^33.9^5.486^2.427^.337^81^1.83^^^0
	;;^UTILITY(U,$J,112,92,3)
	;;=.112^.357^.479^.775^.627^.198^.073^.382^.382^.529^.286^.214^.273^.6^1.656^.167^.766^.43
	;;^UTILITY(U,$J,112,92,4)
	;;=.073^.18^.783^2.396^.137^1.209^2.188^0
	;;^UTILITY(U,$J,112,92,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,93,0)
	;;=MILK, CND, EVAPORATED, UNSW, WO/ADDED VIT A^1-096^fl oz.^31.5^100^N
	;;^UTILITY(U,$J,112,93,1)
	;;=6.81^7.56^10.04^134.386^74.04^^^260.8^.19^24.19^202.5^303.1^105.8^.77^.016^.006^^243^1.88^.047
	;;^UTILITY(U,$J,112,93,2)
	;;=.316^.194^.638^.05^7.9^.163^.167^.078^29.4^4.591^2.335^.245^54^1.55^^^0
	;;^UTILITY(U,$J,112,93,3)
	;;=.096^.307^.412^.667^.54^.171^.063^.329^.329^.456^.247^.185^.235^.517^1.426^.144^.66^.37
	;;^UTILITY(U,$J,112,93,4)
	;;=.11^.161^.733^2.027^.159^.921^2.101^0^^2.3
	;;^UTILITY(U,$J,112,93,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,94,0)
	;;=MILK, CND, EVAPORATED, SKIM^1-097^fl oz.^31.9^100^N
	;;^UTILITY(U,$J,112,94,1)
	;;=7.55^.2^11.35^77.921^79.4^^^289.5^.29^27^194.9^331.5^115^.9^.016^.006^^392^1.24^.045
	;;^UTILITY(U,$J,112,94,2)
	;;=.309^.174^.738^.055^8.6^.239^.004^.002^3.6^.121^.062^.006^117^1.5^^^0
	;;^UTILITY(U,$J,112,94,3)
	;;=.107^.341^.457^.74^.599^.189^.07^.364^.364^.505^.273^.205^.26^.573^1.581^.16^.731^.411
	;;^UTILITY(U,$J,112,94,4)
	;;=.003^.004^.019^.054^.004^.024^.056^0
	;;^UTILITY(U,$J,112,94,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,95,0)
	;;=MILK, MALTED, DRY POWDER^1-098^tsp.^7^100^N
	;;^UTILITY(U,$J,112,95,1)
	;;=11.2^7.9^75.8^414^2^^^298^.7^93^358^758^493^.99^.2^.423^^291^2.9^.505
	;;^UTILITY(U,$J,112,95,2)
	;;=.92^5.246^.683^.41^46^.78^1.007^.2^20^4.199^2.044^1.207^88^3.2^^^.7
	;;^UTILITY(U,$J,112,95,3)
	;;=.125^.298^.348^.667^.317^.16^.233^.391^.322^.397^.43^.233^.41^.901^2.488^.251^1.151^.646
	;;^UTILITY(U,$J,112,95,4)
	;;=.157^.176^.631^1.91^.146^.769^1.803^0
	;;^UTILITY(U,$J,112,95,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,96,0)
	;;=MILK, MALTED, BEVERAGE^1-099^cups^265^100^N
	;;^UTILITY(U,$J,112,96,1)
	;;=3.9^3.7^10.3^89^81.2^^^134^.1^20^114^200^84^.43^.025^.035^^139^1.1^.075
	;;^UTILITY(U,$J,112,96,2)
	;;=.222^.493^.289^.071^8.2^.39^.149^.061^14^2.247^1.05^.21^36^.9^^^0
	;;^UTILITY(U,$J,112,96,3)
	;;=.052^.161^.211^.349^.265^.089^.046^.177^.172^.234^.144^.1^.136^.302^.832^.084^.385^.216
	;;^UTILITY(U,$J,112,96,4)
	;;=.09^.1^.359^.961^.081^.434^.916^0
	;;^UTILITY(U,$J,112,96,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,97,0)
	;;=MILK, MALTED, DRY POWDER, CHOCOLATE^1-100^tsp.^7^100^N
	;;^UTILITY(U,$J,112,97,1)
	;;=5.1^3.8^87.8^375^1.3^^^60^2.28^70^175^618^251^.8^.2^.627^^90^1.5^.173
	;;^UTILITY(U,$J,112,97,2)
	;;=.2^2.003^.683^.156^20^.2^.286^.071^5^2.159^1.045^.357^19^1.9^^37^5.6
	;;^UTILITY(U,$J,112,97,3)
	;;=.059^.143^.16^.292^.161^.064^.094^.185^.151^.197^.206^.098^.186^.414^1.01^.133^.438^.264
	;;^UTILITY(U,$J,112,97,4)
	;;=.075^.084^.302^.947^.069^.491^.93^0
	;;^UTILITY(U,$J,112,97,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,98,0)
	;;=MILK, MALTED, PPD W/.75 OZ POWDER W/1 CUP WHOLE MILK^1-101^cups^265^100^N
	;;^UTILITY(U,$J,112,98,1)
	;;=3.4^3.4^11.3^86^81.1^^^115^.23^18^100^188^65^.41^.059^.051^^123^1^.049
	;;^UTILITY(U,$J,112,98,2)
	;;=.165^.236^.289^.051^6.2^.35^.092^.051^13^2.085^.971^.142^30^.8^^3^0
	;;^UTILITY(U,$J,112,98,3)
	;;=.047^.149^.196^.32^.253^.081^.035^.161^.158^.218^.126^.09^.119^.263^.714^.075^.328^.186
