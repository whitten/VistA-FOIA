IBINI01H	; ; 21-MAR-1994
	;;Version 2.0 ; INTEGRATED BILLING ;; 21-MAR-94
	Q:'DIFQR(350.1)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q	Q
	;;^UTILITY(U,$J,350.1,12,0)
	;;=IB OPT MEDICARE RATE 6^RATE 6^^^1^^^HCFA AMB. SURG. RATE 6^12^^
	;;^UTILITY(U,$J,350.1,13,0)
	;;=IB OPT MEDICARE RATE 7^RATE 7^^^1^^^HCFA AMB. SURG. RATE 7^13^^
	;;^UTILITY(U,$J,350.1,14,0)
	;;=IB OPT MEDICARE RATE 8^RATE 8^^^1^^^HCFA AMB. SURG. RATE 8^14^^
	;;^UTILITY(U,$J,350.1,15,0)
	;;=DG INPT COPAY (MED) NEW^MED CO^5^18^1^25^35^GEN MED INPT COPAY^15^1^2^
	;;^UTILITY(U,$J,350.1,15,20)
	;;=S IBDESC="INPT CO-PAY (MED)"
	;;^UTILITY(U,$J,350.1,16,0)
	;;=DG INPT COPAY (SUR) NEW^SUR CO^5^18^1^26^36^SURGICAL INPT COPAY^16^1^2^
	;;^UTILITY(U,$J,350.1,16,20)
	;;=S IBDESC="INPT CO-PAY (SUR)"
	;;^UTILITY(U,$J,350.1,17,0)
	;;=DG INPT COPAY (SPI) NEW^SPI CO^5^18^1^27^37^SPINAL CORD INPT COPAY^17^1^2^
	;;^UTILITY(U,$J,350.1,17,20)
	;;=S IBDESC="INPT CO-PAY (SPI)"
	;;^UTILITY(U,$J,350.1,18,0)
	;;=DG INPT COPAY (PSY) NEW^PSY CO^5^18^1^28^38^PSYCHIATRIC INPT COPAY^18^1^2^
	;;^UTILITY(U,$J,350.1,18,20)
	;;=S IBDESC="INPT CO-PAY (PSY)"
	;;^UTILITY(U,$J,350.1,19,0)
	;;=DG INPT COPAY (INT) NEW^INT CO^5^18^1^29^39^INTER CARE INPT COPAY^19^1^2^
	;;^UTILITY(U,$J,350.1,19,20)
	;;=S IBDESC="INPT CO-PAY (INT)"
	;;^UTILITY(U,$J,350.1,20,0)
	;;=DG INPT COPAY (REH) NEW^REH CO^5^18^1^30^40^REHAB MED INPT COPAY^20^1^2^
	;;^UTILITY(U,$J,350.1,20,20)
	;;=S IBDESC="INPT CO-PAY (REH)"
	;;^UTILITY(U,$J,350.1,21,0)
	;;=DG INPT COPAY (BLI) NEW^BLI CO^5^18^1^31^41^BLIND REHAB INPT COPAY^21^1^2^
	;;^UTILITY(U,$J,350.1,21,20)
	;;=S IBDESC="INPT CO-PAY (BLI)"
	;;^UTILITY(U,$J,350.1,22,0)
	;;=DG INPT COPAY (NEU) NEW^NEU CO^5^18^1^32^42^NEUROLOGY INPT COPAY^22^1^2^
	;;^UTILITY(U,$J,350.1,22,20)
	;;=S IBDESC="INPT CO-PAY (NEU)"
	;;^UTILITY(U,$J,350.1,23,0)
	;;=DG INPT COPAY (ALC) NEW^ALC CO^5^18^1^33^43^ALC & DRUG INPT COPAY^23^1^2^
	;;^UTILITY(U,$J,350.1,23,20)
	;;=S IBDESC="INPT CO-PAY (ALC)"
	;;^UTILITY(U,$J,350.1,24,0)
	;;=DG NHCU COPAY NEW^NHC CO^3^18^1^34^44^NHCU COPAY^24^1^2^
	;;^UTILITY(U,$J,350.1,24,20)
	;;=S IBDESC="NHCU CO-PAYMENT"
	;;^UTILITY(U,$J,350.1,25,0)
	;;=DG INPT COPAY (MED) CANCEL^CAN MED^5^18^2^25^35^^15^^
	;;^UTILITY(U,$J,350.1,26,0)
	;;=DG INPT COPAY (SUR) CANCEL^CAN SUR^5^18^2^26^36^^16^^^
	;;^UTILITY(U,$J,350.1,27,0)
	;;=DG INPT COPAY (SPI) CANCEL^CAN SPI^5^18^2^27^37^^17^^
	;;^UTILITY(U,$J,350.1,28,0)
	;;=DG INPT COPAY (PSY) CANCEL^CAN PSY^5^18^2^28^38^^18^^
	;;^UTILITY(U,$J,350.1,29,0)
	;;=DG INPT COPAY (INT) CANCEL^CAN INT^5^18^2^29^39^^19^^
	;;^UTILITY(U,$J,350.1,30,0)
	;;=DG INPT COPAY (REH) CANCEL^CAN REH^5^18^2^30^40^^20^^
	;;^UTILITY(U,$J,350.1,31,0)
	;;=DG INPT COPAY (BLI) CANCEL^CAN BLI^5^18^2^31^41^^21^^
	;;^UTILITY(U,$J,350.1,32,0)
	;;=DG INPT COPAY (NEU) CANCEL^CAN NEU^5^18^2^32^42^^22^^
	;;^UTILITY(U,$J,350.1,33,0)
	;;=DG INPT COPAY (ALC) CANCEL^CAN ALC^5^18^2^33^43^^23^^
	;;^UTILITY(U,$J,350.1,34,0)
	;;=DG NHCU COPAY CANCEL^CAN NHC^3^18^2^34^44^^24^^
	;;^UTILITY(U,$J,350.1,35,0)
	;;=DG INPT COPAY (MED) UPDATE^UPD MED^5^18^3^25^35^^15^1^2^
	;;^UTILITY(U,$J,350.1,36,0)
	;;=DG INPT COPAY (SUR) UPDATE^UPD SUR^5^18^3^26^36^^16^1^2^
	;;^UTILITY(U,$J,350.1,37,0)
	;;=DG INPT COPAY (SPI) UPDATE^UPD SPI^5^18^3^27^37^^17^1^2^
	;;^UTILITY(U,$J,350.1,38,0)
	;;=DG INPT COPAY (PSY) UPDATE^UPD PSY^5^18^3^28^38^^18^1^2^
	;;^UTILITY(U,$J,350.1,39,0)
	;;=DG INPT COPAY (INT) UPDATE^UPD INT^5^18^3^29^39^^19^1^2^
	;;^UTILITY(U,$J,350.1,40,0)
	;;=DG INPT COPAY (REH) UPDATE^UPD REH^5^18^3^30^40^^20^1^2^
	;;^UTILITY(U,$J,350.1,41,0)
	;;=DG INPT COPAY (BLI) UPDATE^UPD BLI^5^18^3^31^41^^21^1^2^
	;;^UTILITY(U,$J,350.1,42,0)
	;;=DG INPT COPAY (NEU) UPDATE^UPD NEU^5^18^3^32^42^^22^1^2^
