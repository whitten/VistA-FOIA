FHINI06L	; ; 11-OCT-1995
	;;5.0;Dietetics;;Oct 11, 1995
	Q:'DIFQR(112)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q	Q
	;;^UTILITY(U,$J,112,2259,2)
	;;=.195^.175^.492^.044^3^.31^.063^^11^1.734^^^29^.9
	;;^UTILITY(U,$J,112,2259,3)
	;;=.047^.152^.204^.329^.266^.084^.031^.162^.162^.225^.121^.092^.116^.254^.703^.072^.325^.183
	;;^UTILITY(U,$J,112,2259,4)
	;;=^^^^^^.701
	;;^UTILITY(U,$J,112,2259,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2261,0)
	;;=CRANBERRY JUICE COCKTAIL, PREPARED WITH WATER FROM FROZEN^14-431^fl oz.^31.2^100^N
	;;^UTILITY(U,$J,112,2261,1)
	;;=0^0^14^55^85.9^^^5^.09^2^1^14^3^.04^.01^.041^^10^9.9^.007
	;;^UTILITY(U,$J,112,2261,2)
	;;=.009^.012^.141^.014^0^0^0^^0^0^^^1^.1
	;;^UTILITY(U,$J,112,2261,4)
	;;=^^^^^^0
	;;^UTILITY(U,$J,112,2261,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2262,0)
	;;=BEVERAGE, COLA, LOW CAL, W/ ASPARTAME AND NA SACCHARIN^14-535^fl oz.^29.6^100^N
	;;^UTILITY(U,$J,112,2262,1)
	;;=.1^0^.1^1^99.8^^^4^.03^1^9^0^9^.08^.011^.035^^0^0^.005
	;;^UTILITY(U,$J,112,2262,2)
	;;=.023^0^0^0^0^0^0^0^0^0^0^0^0^0^^14
	;;^UTILITY(U,$J,112,2262,4)
	;;=0^0^0^0^0^0^0^0
	;;^UTILITY(U,$J,112,2262,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2263,0)
	;;=BEVERAGE, CARBONATED, LOW CAL, NOT COLA OR PEPPER, W/ SAC^14-537^fl oz.^29.6^100^N
	;;^UTILITY(U,$J,112,2263,1)
	;;=0^0^.1^0^99.8^^^4^.04^1^0^2^16^.05^.025^.017^^0^0^0
	;;^UTILITY(U,$J,112,2263,2)
	;;=0^0^0^0^0^0^0^0^0^0^0^0^0^.1^^11^0
	;;^UTILITY(U,$J,112,2263,4)
	;;=0^0^0^0^0^0^0^0
	;;^UTILITY(U,$J,112,2263,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2265,0)
	;;=FRUIT PUNCH FLAVOR DRINK, POWDER, WO/ ADD NA, PREP W/ WATER^14-541^fl oz.^32.8^100^N
	;;^UTILITY(U,$J,112,2265,1)
	;;=0^0^9.5^37^90.4^^^16^.05^1^20^1^4^.03^.018^.003^^0^11.8^0
	;;^UTILITY(U,$J,112,2265,2)
	;;=.002^.001^0^0^.1^0^.001^0^0^.006^.001^.001^0^.1
	;;^UTILITY(U,$J,112,2265,4)
	;;=0^.003^.001^.001^0^0^.001
	;;^UTILITY(U,$J,112,2265,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2266,0)
	;;=COCOA MIX, ASPARTAME SWEETENED, ADDED CA & P, PREPARED^14-546^fl oz.^32^100^N
	;;^UTILITY(U,$J,112,2266,1)
	;;=2^.2^4.4^25^92.4^^^114^.39^17^127^211^54^.29^.063^.052^^125^0^.021
	;;^UTILITY(U,$J,112,2266,2)
	;;=.109^.085^.298^.025^1.2^.15^.007^0^1^.139^.076^.007^28^.9^^8
	;;^UTILITY(U,$J,112,2266,4)
	;;=^0^0^.059^.001^.077^.076
	;;^UTILITY(U,$J,112,2266,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2267,0)
	;;=BEANS, BLACK, BOILED, WO/SALT^16-015^cups^172^100^N
	;;^UTILITY(U,$J,112,2267,1)
	;;=8.86^.54^23.71^132^65.74^^^27^2.1^70^140^355^1^1.12^.209^.444^^6^0^.244
	;;^UTILITY(U,$J,112,2267,2)
	;;=.059^.505^.242^.069^148.8^0^.126^.105^0^.139^.047^.231^1^1.15^^^7.1
	;;^UTILITY(U,$J,112,2267,3)
	;;=.105^.373^.391^.708^.608^.133^.096^.479^.25^.464^.549^.247^.372^1.072^1.351^.346^.376^.482
	;;^UTILITY(U,$J,112,2267,4)
	;;=^^0^.13^^.008^.047
	;;^UTILITY(U,$J,112,2267,20)
	;;=USDA Std. Reference, Release 10;HCF Nutrition Research Foundation, Inc, 1990
	;;^UTILITY(U,$J,112,2268,0)
	;;=BEANS, CRANBERRY (ROMAN), BOILED, WO/SALT^16-020^cups^177^100^N
	;;^UTILITY(U,$J,112,2268,1)
	;;=9.34^.46^24.46^136^64.65^^^50^2.09^50^135^387^1^1.14^.231^.37^^0^0^.21
	;;^UTILITY(U,$J,112,2268,2)
	;;=.069^.515^.24^.081^206.8^0^.108^.091^0^.119^.04^.199^0^1.09^^^6.1
	;;^UTILITY(U,$J,112,2268,3)
	;;=.111^.393^.412^.746^.641^.14^.102^.505^.263^.489^.578^.26^.391^1.129^1.424^.365^.396^.508
	;;^UTILITY(U,$J,112,2268,4)
	;;=^^0^.112^^.007^.04
	;;^UTILITY(U,$J,112,2268,20)
	;;=USDA Std. Reference, Release 10;HCF Nutrition Research Foundation, Inc, 1990
	;;^UTILITY(U,$J,112,2269,0)
	;;=BEANS, GREAT NORTHERN, BOILED, WO/SALT^16-025^cups^177^100^N
	;;^UTILITY(U,$J,112,2269,1)
	;;=8.33^.45^21.09^118^69^^^68^2.13^50^165^391^2^.88^.247^.518^^1^1.3^.158
	;;^UTILITY(U,$J,112,2269,2)
	;;=.059^.681^.266^.117^102.2^0^.103^.084^0^.14^.021^.187^0^1.14^^^7
	;;^UTILITY(U,$J,112,2269,3)
	;;=.099^.351^.368^.665^.572^.125^.091^.451^.235^.436^.516^.232^.349^1.008^1.27^.325^.353^.453
	;;^UTILITY(U,$J,112,2269,4)
	;;=^^^.132^^.008^.021^^^4.1
	;;^UTILITY(U,$J,112,2269,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2270,0)
	;;=BEANS, GREAT NORTHERN, CANNED^16-026^cups^262^100^N
	;;^UTILITY(U,$J,112,2270,1)
	;;=7.37^.39^21.03^114^69.89^^^53^1.57^51^136^351^4^.65^.16^.408^^1^1.3^.143
	;;^UTILITY(U,$J,112,2270,2)
	;;=.06^.461^.278^.106^81.3^0^.089^.072^0^.12^.018^.161^0^1.33^^^4.9
	;;^UTILITY(U,$J,112,2270,3)
	;;=.087^.31^.325^.588^.506^.111^.08^.399^.208^.386^.456^.205^.309^.891^1.124^.288^.312^.401
	;;^UTILITY(U,$J,112,2270,4)
	;;=^^^.113^^.007^.018
	;;^UTILITY(U,$J,112,2270,20)
	;;=USDA Std. Reference, Release 10
