IBINI0E6	; ; 21-MAR-1994
	;;Version 2.0 ; INTEGRATED BILLING ;; 21-MAR-94
	F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q	Q
	;;^UTILITY(U,$J,"OR",200,1,22,0)
	;;=IBACM PATIENT CHANGE
	;;^UTILITY(U,$J,"OR",200,1,22,1,0)
	;;=^100.99511PA^1^1
	;;^UTILITY(U,$J,"OR",200,1,22,1,1,0)
	;;=IBACM1 MENU
	;;^UTILITY(U,$J,"OR",200,1,23,0)
	;;=IBACM UPDATE CHARGE
	;;^UTILITY(U,$J,"OR",200,1,23,1,0)
	;;=^100.99511PA^1^1
	;;^UTILITY(U,$J,"OR",200,1,23,1,1,0)
	;;=IBACM1 MENU
	;;^UTILITY(U,$J,"OR",200,1,24,0)
	;;=IBACM UPDATE CHARGE ONE
	;;^UTILITY(U,$J,"OR",200,1,24,1,0)
	;;=^100.99511PA^1^1
	;;^UTILITY(U,$J,"OR",200,1,24,1,1,0)
	;;=IBACM UPDATE CHARGE
	;;^UTILITY(U,$J,"OR",200,1,25,0)
	;;=IBACM1 DATE CHANGE
	;;^UTILITY(U,$J,"OR",200,1,25,1,0)
	;;=^100.99511PA^1^1
	;;^UTILITY(U,$J,"OR",200,1,25,1,1,0)
	;;=IBACM1 MENU
	;;^UTILITY(U,$J,"OR",200,1,39,0)
	;;=IBACM PASS CHARGE
	;;^UTILITY(U,$J,"OR",200,1,39,1,0)
	;;=^100.99511PA^1^1
	;;^UTILITY(U,$J,"OR",200,1,39,1,1,0)
	;;=IBACM1 MENU
	;;^UTILITY(U,$J,"OR",200,1,40,0)
	;;=IBDF PRINT MANAGER CLINIC SETUP
	;;^UTILITY(U,$J,"OR",200,1,40,1,0)
	;;=^100.99511PA^1^1
	;;^UTILITY(U,$J,"OR",200,1,40,1,1,0)
	;;=SD PARM PARAMETERS MENU
	;;^UTILITY(U,$J,"OR",200,1,41,0)
	;;=IBDF PRINT MANAGER DIVISION SETUP
	;;^UTILITY(U,$J,"OR",200,1,41,1,0)
	;;=^100.99511PA^1^1
	;;^UTILITY(U,$J,"OR",200,1,41,1,1,0)
	;;=SD PARM PARAMETERS MENU
	;;^UTILITY(U,$J,"OR",200,1,42,0)
	;;=SD PARM PARAMETERS MENU
	;;^UTILITY(U,$J,"PKG",200,0)
	;;=INTEGRATED BILLING^IB^INTEGRATED BILLING
	;;^UTILITY(U,$J,"PKG",200,3,0)
	;;=^9.43^5^5
	;;^UTILITY(U,$J,"PKG",200,3,1,0)
	;;=IB
	;;^UTILITY(U,$J,"PKG",200,3,1,5)
	;;=M
	;;^UTILITY(U,$J,"PKG",200,3,2,0)
	;;=IBE
	;;^UTILITY(U,$J,"PKG",200,3,2,5)
	;;=M
	;;^UTILITY(U,$J,"PKG",200,3,3,0)
	;;=DGCR
	;;^UTILITY(U,$J,"PKG",200,3,3,5)
	;;=M
	;;^UTILITY(U,$J,"PKG",200,3,4,0)
	;;=IBA
	;;^UTILITY(U,$J,"PKG",200,3,4,5)
	;;=M
	;;^UTILITY(U,$J,"PKG",200,3,5,0)
	;;=IBT
	;;^UTILITY(U,$J,"PKG",200,3,5,5)
	;;=M
	;;^UTILITY(U,$J,"PKG",200,3,"B","DGCR",3)
	;;=
	;;^UTILITY(U,$J,"PKG",200,3,"B","IB",1)
	;;=
	;;^UTILITY(U,$J,"PKG",200,3,"B","IBA",4)
	;;=
	;;^UTILITY(U,$J,"PKG",200,3,"B","IBE",2)
	;;=
	;;^UTILITY(U,$J,"PKG",200,3,"B","IBT",5)
	;;=
	;;^UTILITY(U,$J,"PKG",200,4,0)
	;;=^9.44PA^92^89
	;;^UTILITY(U,$J,"PKG",200,4,1,0)
	;;=350
	;;^UTILITY(U,$J,"PKG",200,4,1,222)
	;;=y^y^^n^^^n^^
	;;^UTILITY(U,$J,"PKG",200,4,2,0)
	;;=350.1
	;;^UTILITY(U,$J,"PKG",200,4,2,222)
	;;=y^y^^n^^^y^m^y
	;;^UTILITY(U,$J,"PKG",200,4,3,0)
	;;=350.2
	;;^UTILITY(U,$J,"PKG",200,4,3,222)
	;;=y^y^^n^^^y^m^y
	;;^UTILITY(U,$J,"PKG",200,4,4,0)
	;;=350.3
	;;^UTILITY(U,$J,"PKG",200,4,4,222)
	;;=y^y^^n^^^y^m^y
	;;^UTILITY(U,$J,"PKG",200,4,5,0)
	;;=350.4
	;;^UTILITY(U,$J,"PKG",200,4,5,222)
	;;=y^y^^n^^^n^^
	;;^UTILITY(U,$J,"PKG",200,4,6,0)
	;;=350.41
	;;^UTILITY(U,$J,"PKG",200,4,6,222)
	;;=y^y^^n^^^n^^
	;;^UTILITY(U,$J,"PKG",200,4,7,0)
	;;=350.5
	;;^UTILITY(U,$J,"PKG",200,4,7,222)
	;;=y^y^^n^^^n^^
	;;^UTILITY(U,$J,"PKG",200,4,8,0)
	;;=350.7
	;;^UTILITY(U,$J,"PKG",200,4,8,222)
	;;=y^y^^n^^^n^^
	;;^UTILITY(U,$J,"PKG",200,4,9,0)
	;;=350.71
	;;^UTILITY(U,$J,"PKG",200,4,9,222)
	;;=y^y^^n^^^n^^
	;;^UTILITY(U,$J,"PKG",200,4,10,0)
	;;=350.8
	;;^UTILITY(U,$J,"PKG",200,4,10,222)
	;;=y^y^^n^^^y^o^n
	;;^UTILITY(U,$J,"PKG",200,4,11,0)
	;;=350.9
	;;^UTILITY(U,$J,"PKG",200,4,11,222)
	;;=y^y^^n^^^n^^
	;;^UTILITY(U,$J,"PKG",200,4,12,0)
	;;=351
	;;^UTILITY(U,$J,"PKG",200,4,12,222)
	;;=y^y^^n^^^n^^
	;;^UTILITY(U,$J,"PKG",200,4,13,0)
	;;=351.1
	;;^UTILITY(U,$J,"PKG",200,4,13,1,0)
	;;=^9.45A
	;;^UTILITY(U,$J,"PKG",200,4,13,222)
	;;=y^y^^n^^^n^^
	;;^UTILITY(U,$J,"PKG",200,4,14,0)
	;;=352.1
	;;^UTILITY(U,$J,"PKG",200,4,14,1,0)
	;;=^9.45A
	;;^UTILITY(U,$J,"PKG",200,4,14,222)
	;;=y^y^^n^^^y^m^y
	;;^UTILITY(U,$J,"PKG",200,4,15,0)
	;;=399
