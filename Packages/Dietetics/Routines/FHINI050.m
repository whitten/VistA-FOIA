FHINI050	; ; 11-OCT-1995
	;;5.0;Dietetics;;Oct 11, 1995
	Q:'DIFQR(112)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q	Q
	;;^UTILITY(U,$J,112,1689,20)
	;;=USDA Std. Reference, Release 10;HCF Nutrition Research Foundation, Inc, 1990
	;;^UTILITY(U,$J,112,1690,0)
	;;=GRAPEFRUIT, SECTIONS, CND, JUICE PACK, SOL&LIQ^9-120^cups^248^100^N
	;;^UTILITY(U,$J,112,1690,1)
	;;=.7^.09^9.21^37^89.67^^^15^.21^11^12^169^7^.08^.037^.007^^0^33.9^.029
	;;^UTILITY(U,$J,112,1690,2)
	;;=.018^.249^.12^.02^8.8^0^.017^.004^0^.012^.012^.021^0^.34^^^.4
	;;^UTILITY(U,$J,112,1690,3)
	;;=.002^^^^.018^.002
	;;^UTILITY(U,$J,112,1690,4)
	;;=^^^.011^.001^.001^.01
	;;^UTILITY(U,$J,112,1690,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,1691,0)
	;;=GRAPEFRUIT, SECTIONS, CND, LIGHT SIRUP PACK, SOL&LIQ^9-121^cups^254^100^N
	;;^UTILITY(U,$J,112,1691,1)
	;;=.56^.1^15.44^60^83.59^^^14^.4^10^10^129^2^.08^.066^.007^^0^21.3^.038
	;;^UTILITY(U,$J,112,1691,2)
	;;=.02^.243^.12^.02^8.5^0^.019^.005^0^.014^.013^.024^0^.31^^^.4
	;;^UTILITY(U,$J,112,1691,3)
	;;=.002^^^^.014^.002
	;;^UTILITY(U,$J,112,1691,4)
	;;=^^^.012^.001^.001^.012
	;;^UTILITY(U,$J,112,1691,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,1692,0)
	;;=GRAPEFRUIT JUICE, CND, UNSW^9-123^fl oz.^30.9^100^N
	;;^UTILITY(U,$J,112,1692,1)
	;;=.52^.1^8.96^38^90.1^^^7^.2^10^11^153^1^.09^.038^.02^.04^7^29.2^.042
	;;^UTILITY(U,$J,112,1692,2)
	;;=.02^.231^.13^.02^10.4^0^.018^.005^0^.013^.013^.023^1^.32^^^.1
	;;^UTILITY(U,$J,112,1692,4)
	;;=^^^.012^.001^.001^.012^^.2
	;;^UTILITY(U,$J,112,1692,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,1693,0)
	;;=GRAPEFRUIT JUICE, CND, SWEETENED^9-124^fl oz.^31.2^100^N
	;;^UTILITY(U,$J,112,1693,1)
	;;=.58^.09^11.13^46^87.38^^^8^.36^10^11^162^2^.06^.048^.02^^0^26.9^.04
	;;^UTILITY(U,$J,112,1693,2)
	;;=.023^.319^.13^.02^10.4^0^.017^.004^0^.012^.012^.021^0^.82^^^.1
	;;^UTILITY(U,$J,112,1693,4)
	;;=^^^.011^.001^.001^.011
	;;^UTILITY(U,$J,112,1693,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,1694,0)
	;;=GRAPEFRUIT JUICE, FRZ CONC, UNSW, DILUTED W/3 VOL WATER^9-126^oz.^28.3^100^N
	;;^UTILITY(U,$J,112,1694,1)
	;;=.55^.13^9.73^41^89.3^^^8^.14^11^14^136^1^.05^.033^.02^^9^33.7^.041
	;;^UTILITY(U,$J,112,1694,2)
	;;=.022^.217^.189^.044^3.6^0^.025^.007^0^.019^.018^.032^1^.28^^^.1
	;;^UTILITY(U,$J,112,1694,4)
	;;=^^.001^.017^.002^.001^.016
	;;^UTILITY(U,$J,112,1694,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,1695,0)
	;;=GRAPEFRUIT JUICE, WHITE, RAW^9-128^oz.^28.3^100^N
	;;^UTILITY(U,$J,112,1695,1)
	;;=.5^.1^9.2^39^90^^^9^.2^12^15^162^1^.05^.033^.02^^10^38^.04
	;;^UTILITY(U,$J,112,1695,2)
	;;=.02^.2^.189^.044^10.2^0^.019^.005^0^.014^.013^.024^1^.2^^^.1
	;;^UTILITY(U,$J,112,1695,4)
	;;=^^^.012^.001^.001^.012^^.02
	;;^UTILITY(U,$J,112,1695,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,1696,0)
	;;=GRAPES, AMERICAN TYPE (SLIP SKIN), RAW^9-131^grapes^2.4^58^N
	;;^UTILITY(U,$J,112,1696,1)
	;;=.63^.35^17.15^63^81.3^^^14^.29^5^10^191^2^.04^.04^.718^^100^4^.092
	;;^UTILITY(U,$J,112,1696,2)
	;;=.057^.3^.024^.11^3.9^0^.079^.024^0^.114^.014^.102^10^.57^^^.7
	;;^UTILITY(U,$J,112,1696,3)
	;;=.003^.017^.005^.013^.014^.021^.01^.013^.011^.017^.046^.023^.026^.077^.131^.019^.021^.03
	;;^UTILITY(U,$J,112,1696,4)
	;;=^^.003^.098^^.013^.014
	;;^UTILITY(U,$J,112,1696,20)
	;;=USDA Std. Reference, Release 10;HCF Nutrition Research Foundation, Inc, 1990
	;;^UTILITY(U,$J,112,1697,0)
	;;=GRAPES, EUROPEAN TYPE (ADHERENT SKIN), RAW^9-132^grapes^5.0^96^N
	;;^UTILITY(U,$J,112,1697,1)
	;;=.66^.58^17.77^71^80.56^^^11^.26^6^13^185^2^.05^.09^.058^^73^10.8^.092
	;;^UTILITY(U,$J,112,1697,2)
	;;=.057^.3^.024^.11^3.9^0^.13^.039^0^.189^.023^.169^7^.44^^^.8
	;;^UTILITY(U,$J,112,1697,3)
	;;=.003^.018^.005^.014^.015^.022^.011^.014^.012^.018^.049^.024^.028^.081^.138^.02^.022^.032
	;;^UTILITY(U,$J,112,1697,4)
	;;=^^.005^.162^^.022^.023^^3^.2
	;;^UTILITY(U,$J,112,1697,20)
	;;=USDA Std. Reference, Release 10;HCF Nutrition Research Foundation, Inc, 1990
	;;^UTILITY(U,$J,112,1698,0)
	;;=GRAPES, CND, WATER PACK^9-133^cups^244^100^N
	;;^UTILITY(U,$J,112,1698,1)
	;;=.5^.11^10.3^40^88.84^^^10^.98^6^18^107^6^.05^.056^.039^^66^1^.031
	;;^UTILITY(U,$J,112,1698,2)
	;;=.023^.13^.041^.065^2.6^0^.024^.007^0^.035^.004^.031^7^.25^^^1
	;;^UTILITY(U,$J,112,1698,3)
	;;=.002^.014^.004^.01^.011^.017^.008^.01^.009^.014^.037^.018^.021^.061^.104^.015^.017^.024
	;;^UTILITY(U,$J,112,1698,4)
	;;=^^.001^.03^^.004^.004
	;;^UTILITY(U,$J,112,1698,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,1699,0)
	;;=GRAPES, CND, HEAVY SIRUP PACK^9-134^cups^256^100^N
	;;^UTILITY(U,$J,112,1699,1)
	;;=.48^.1^19.65^73^79.53^^^10^.94^6^17^103^5^.05^.054^.038^^64^1^.03
