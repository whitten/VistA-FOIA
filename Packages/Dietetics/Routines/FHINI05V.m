FHINI05V	; ; 11-OCT-1995
	;;5.0;Dietetics;;Oct 11, 1995
	Q:'DIFQR(112)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q	Q
	;;^UTILITY(U,$J,112,2024,1)
	;;=3.99^.69^3.78^29^91.14^^^32^.96^27^70^79^6^.92^.157^.188^^155^8.2^.076
	;;^UTILITY(U,$J,112,2024,2)
	;;=.126^.481^.563^.034^36^0^.234^.175^0^.069^.056^.409^16^.4^^^2.5
	;;^UTILITY(U,$J,112,2024,3)
	;;=^.134^.143^.267^.214^^^^^.145
	;;^UTILITY(U,$J,112,2024,4)
	;;=^^.002^.059^^.008^.056
	;;^UTILITY(U,$J,112,2024,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2025,0)
	;;=ARTICHOKES, FRZ, CKD, BOILED, WO/SALT^11-010^buds^120^100^N
	;;^UTILITY(U,$J,112,2025,1)
	;;=3.11^.5^9.18^45^86.5^^^21^.56^31^61^264^53^.36^.061^.273^^164^5^.062
	;;^UTILITY(U,$J,112,2025,2)
	;;=.158^.915^.2^.087^118.8^0^.154^.058^0^.117^.015^.212^16^.71
	;;^UTILITY(U,$J,112,2025,4)
	;;=^.005^.005^.097^^.01^.015
	;;^UTILITY(U,$J,112,2025,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2026,0)
	;;=BAMBOO SHOOTS, BOILED, WO/SALT^11-027^cups^120^105^N
	;;^UTILITY(U,$J,112,2026,1)
	;;=1.53^.22^1.92^12^95.92^^^12^.24^3^20^533^4^.47^.082^.113^^0^0^.02
	;;^UTILITY(U,$J,112,2026,2)
	;;=.05^.3^.066^.098^2.3^0^.083^.015^0^.051^.005^.098^0^.41
	;;^UTILITY(U,$J,112,2026,3)
	;;=.016^.05^.051^.082^.079^.017^.013^.053^^.062^.057^.025^.072^.249^.145^.051^.129^.075
	;;^UTILITY(U,$J,112,2026,4)
	;;=^.001^.002^.038^^.004^.005
	;;^UTILITY(U,$J,112,2026,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2027,0)
	;;=BAMBOO SHOOTS, CND^11-028^cups^131^100^N
	;;^UTILITY(U,$J,112,2027,1)
	;;=1.72^.4^3.22^19^94.32^^^8^.32^4^25^80^7^.65^.114^.157^^8^1.1^.026
	;;^UTILITY(U,$J,112,2027,2)
	;;=.026^.14^.092^.136^3.2^0^.151^.027^0^.092^.009^.178^1^.34^^^3
	;;^UTILITY(U,$J,112,2027,3)
	;;=.018^.057^.058^.093^.089^.02^.014^.06^^.071^.064^.028^.082^.282^.164^.057^.145^.084
	;;^UTILITY(U,$J,112,2027,4)
	;;=^.003^.003^.069^^.008^.009
	;;^UTILITY(U,$J,112,2027,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2028,0)
	;;=BEAN SPROUTS, KIDNEY, RAW^11-029^cups^184^100^N
	;;^UTILITY(U,$J,112,2028,1)
	;;=4.2^.5^4.1^29^90.7^^^17^.81^21^37^187^6^.4^.159^.182^^2^38.7^.37
	;;^UTILITY(U,$J,112,2028,2)
	;;=.25^2.92^.368^.085^58.9^0^.107^.169^0^.072^.039^.276^0^.5
	;;^UTILITY(U,$J,112,2028,3)
	;;=.044^.176^.186^.302^.239^.044^.048^.212^.144^.216^.228^.118^.174^.546^.512^.144^.169^.224
	;;^UTILITY(U,$J,112,2028,4)
	;;=^^^.064^^.009^.039
	;;^UTILITY(U,$J,112,2028,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2029,0)
	;;=BEAN SPROUTS, KIDNEY, CKD, WO/SALT^11-030^cups^154^100^N
	;;^UTILITY(U,$J,112,2029,1)
	;;=4.83^.58^4.72^33^89.3^^^19^.89^23^38^194^7^.44^.174^.199^^2^35.6^.362
	;;^UTILITY(U,$J,112,2029,2)
	;;=.273^3.024^.381^.093^47.4^0^.123^.194^0^.083^.045^.318^0^.58
	;;^UTILITY(U,$J,112,2029,3)
	;;=.05^.203^.214^.347^.275^.05^.055^.243^.166^.248^.263^.135^.2^.628^.589^.166^.195^.258
	;;^UTILITY(U,$J,112,2029,4)
	;;=^^^.074^^.01^.045
	;;^UTILITY(U,$J,112,2029,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2030,0)
	;;=BEAN SPROUTS, MUNG, STIR-FRIED^11-045^cups^124^100^N
	;;^UTILITY(U,$J,112,2030,1)
	;;=4.3^.21^10.59^50^84.3^^^13^1.9^33^79^219^9^.9^.255^.292^^31^16^.14
	;;^UTILITY(U,$J,112,2030,2)
	;;=.18^1.2^.559^.13^69.6^0^.058^.011^0^.039^.056^.068^3^.6
	;;^UTILITY(U,$J,112,2030,3)
	;;=.058^.122^.207^.275^.261^.053^.026^.183^.081^.204^.309^.109^.155^.752^.253^.098^^.052
	;;^UTILITY(U,$J,112,2030,4)
	;;=^^0^.027^0^.009^.056
	;;^UTILITY(U,$J,112,2030,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2031,0)
	;;=BEAN SPROUTS, NAVY, RAW^11-046^cups^104^100^N
	;;^UTILITY(U,$J,112,2031,1)
	;;=6.15^.7^13.05^67^79.15^^^15^1.93^101^100^307^13^.89^.356^.408^^4^18.8^.39
	;;^UTILITY(U,$J,112,2031,2)
	;;=.215^1.22^.825^.191^132^0^.147^.26^0^.085^.052^.407^0^.95
	;;^UTILITY(U,$J,112,2031,3)
	;;=.064^.258^.273^.442^.35^.064^.07^.31^.212^.316^.335^.172^.255^.799^.75^.212^.248^.329
	;;^UTILITY(U,$J,112,2031,4)
	;;=^^^.076^^.009^.052
	;;^UTILITY(U,$J,112,2031,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2032,0)
	;;=BEAN SPROUTS, NAVY, CKD, WO/SALT^11-047^cups^87.36^100^N
	;;^UTILITY(U,$J,112,2032,1)
	;;=7.07^.81^15.01^78^76.02^^^16^2.11^111^103^317^14^.97^.389^.446^^4^17.3^.381
	;;^UTILITY(U,$J,112,2032,2)
	;;=.235^1.263^.854^.198^106.3^0^.169^.299^0^.098^.06^.468^0^1.09
	;;^UTILITY(U,$J,112,2032,3)
	;;=.074^.297^.314^.508^.403^.074^.08^.357^.243^.363^.385^.198^.293^.919^.863^.243^.285^.378
	;;^UTILITY(U,$J,112,2032,4)
	;;=^^^.087^^.011^.06
	;;^UTILITY(U,$J,112,2032,20)
	;;=USDA Std. Reference, Release 10
	;;^UTILITY(U,$J,112,2033,0)
	;;=BEANS, PINTO, FRZ, CKD, WO/SALT^11-049^cups^188^100^N
	;;^UTILITY(U,$J,112,2033,1)
	;;=9.31^.48^30.88^162^58.01^^^52^2.71^54^100^646^83^.69^.088^.493^^0^.7^.274
