FHINI0MI	; ; 11-OCT-1995
	;;5.0;Dietetics;;Oct 11, 1995
	Q:'DIFQR(119.4)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q	Q
	;;^UTILITY(U,$J,119.4)
	;;=^FH(119.4,
	;;^UTILITY(U,$J,119.4,0)
	;;=ISOLATION/PRECAUTION TYPE^119.4sI^4^4
	;;^UTILITY(U,$J,119.4,1,0)
	;;=RESPIRATORY^C^N
	;;^UTILITY(U,$J,119.4,2,0)
	;;=STRICT^C^N
	;;^UTILITY(U,$J,119.4,3,0)
	;;=PROTECTIVE^P^F
	;;^UTILITY(U,$J,119.4,4,0)
	;;=WOUND/SKIN^P^F
