FHINI0HT	; ; 11-OCT-1995
	;;5.0;Dietetics;;Oct 11, 1995
	Q:'DIFQR(113)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q	Q
	;;^UTILITY(U,$J,113,768,0)
	;;=CELERY CRESCENTS, FRESH^CELERY CRES FRSH^^^LB^LB^^1^^^^^3^^^LB^1^^^^951^1
	;;^UTILITY(U,$J,113,769,0)
	;;=CELERY STIX, FRESH^CELERY STIX, FRESH^^^LB^LB^^1^^^^^3^^^LB^1^^^^950^1
	;;^UTILITY(U,$J,113,770,0)
	;;=CHEESE, COTTAGE^CHEESE, COTTAGE^^^LB^LB^^1^^^^^2^^^LB^1^^^^12^1
	;;^UTILITY(U,$J,113,771,0)
	;;=CHEESE, AMER/SWISS, SLICED^CHEESE AMER/SWI SLC^^^LB^LB^^1^^^^^2^^^LB^1^^^^44^1
	;;^UTILITY(U,$J,113,772,0)
	;;=CHICKEN, DRUMS, FZN^CHICKEN, DRUMS, FZN^^^LB^LB^^1^^^^^1^^^LB^1^^^^^1
	;;^UTILITY(U,$J,113,773,0)
	;;=CHICKEN THIGHS, FZN^CHICKEN THIGHS, FZN^^^LB^LB^^1^^^^^1^^^LB^1^^^^444^1
	;;^UTILITY(U,$J,113,774,0)
	;;=ICE CREAM, CHOCOLATE, IND^ICE CREAM, CHOC^^^DZ^DZ^^1^^^^^2^^^EACH^12^^^^60^.148
	;;^UTILITY(U,$J,113,775,0)
	;;=MILK, CHOCOLATE, LOWFAT^MILK, CHOC LOWFAT^^^GL^GL CO^^1^^^^^2^^^GAL^1^^^^100^8.819
	;;^UTILITY(U,$J,113,776,0)
	;;=CELERY, CHOPPED FRESH^CELERY, CHOPPED FRESH^^^LB^LB^^1^^^^^3^^^LB^1^^^^951^1
	;;^UTILITY(U,$J,113,777,0)
	;;=ONIONS, CHOPPED, FRESH^ONION, CHOPPED, FRESH^^^LB^LB^^1^^^^^3^^^LB^1^^^^1378^1
	;;^UTILITY(U,$J,113,778,0)
	;;=COD RAW BREADED^COD RAW BRD^^^LB^LB^^1^^^^^1^^^LB^1^^^^1225^1
	;;^UTILITY(U,$J,113,779,0)
	;;=CREAMER, NONDAIRY LIQUID, BULK^CREAM NONDAIRYLIQ 1GL^^^BT^GL BT^^1^^^^^6^^^GAL^1^^^^66^8.466
	;;^UTILITY(U,$J,113,780,0)
	;;=TOPPING, NONDAIRY, FROZEN^TOPPING NONDAIRY 12OZ^^^CN^12-OZ CN^^1^^^^^6^^^LB^.75^^^^71^1
	;;^UTILITY(U,$J,113,781,0)
	;;=CUCUMBERS, FRESH^CUCUMBERS, FRESH^^^LB^LB^^1^^^^^3^^^LB^1^^^^1089^1
	;;^UTILITY(U,$J,113,782,0)
	;;=CURRY POWDER^CURRY PWD 1#CN^^^CN^1# CN^^1^^^^^6^^^LB^1^^^^273^1
	;;^UTILITY(U,$J,113,783,0)
	;;=DANISH, DEMI, FRESH^DEMI DANISH 48S^^^CS^CS-48^^1^^^^^4^^^EACH^48^^^^1569^.093
	;;^UTILITY(U,$J,113,784,0)
	;;=CAKE, DEVILSFOOD, FZN^CAKE, DEVILSFOOD, FZN^^^CK^4# CK^^1^^^^^4^^^EACH^36^^^^858^.209
	;;^UTILITY(U,$J,113,785,0)
	;;=ROLLS, SOFT DINNER, FRESH^ROLLS, SOFT DIN FRSH^^^PG^PG-12^^1^^^^^4^^^EACH^12^^^^1567^.062
	;;^UTILITY(U,$J,113,786,0)
	;;=DONUTS, FRESH^DONUTS, FRESH^^^DZ^DZ^^1^^^^^4^^^EACH^12^^^^1096^.093
	;;^UTILITY(U,$J,113,787,0)
	;;=EGGPLANT^EGGPLANT^^^LB^LB^^1^^^^^3^^^LB^1^^^^1098^1
	;;^UTILITY(U,$J,113,788,0)
	;;=EGGS, SHELL, FRESH^EGGS, SHELL, FRESH^^^DZ^DZ^^1^^^^^1^^^EACH^12^^^^6498^.097
	;;^UTILITY(U,$J,113,789,0)
	;;=ENDIVE^ENDIVE^^^LB^LB^^1^^^^^3^^^LB^1^^^^1100^1
	;;^UTILITY(U,$J,113,790,0)
	;;=ESCAROLE^ESCAROLE^^^LB^LB^^1^^^^^3^^^LB^1^^^^1100^1
	;;^UTILITY(U,$J,113,791,0)
	;;=FRENCH TOAST, FZN^FRENCH TOAST, FZN^^^LB^LB^^1^^^^^4^^^LB^1^^^^6427^1
	;;^UTILITY(U,$J,113,792,0)
	;;=FRANKFURTERS, FRESH^FRANKFURTERS, FRESH^^^LB^LB^^1^^^^^1^^^LB^1^^^^1610^1
	;;^UTILITY(U,$J,113,793,0)
	;;=FRENCH BREAD, FRESH^FRENCH BREAD, FRESH^^^LF^1# LF^^1^^^^^4^^^EACH^12^^^^812^.033
	;;^UTILITY(U,$J,113,794,0)
	;;=OKRA, FRZ^OKRA,FZN^^^LB^LB^^1^^^^^3^^^LB^1^^^^1370^1
	;;^UTILITY(U,$J,113,795,0)
	;;=PANCAKE BATTER, FZN^PANCAKE BATTER, FZN^^^LB^LB^^1^^^^^4^^^LB^1^^^^^1
	;;^UTILITY(U,$J,113,796,0)
	;;=CAKE, GERMAN CHOC, FZN^CAKE, GER CHOC FZN^^^CK^4# CK^^1^^^^^4^^^EACH^36^^^^859^.209
	;;^UTILITY(U,$J,113,797,0)
	;;=ONIONS, GREEN FRESH^ONIONS, GREEN FRESH^^^LB^LB^^1^^^^^3^^^LB^1^^^^1385^1
	;;^UTILITY(U,$J,113,798,0)
	;;=PEPPERS, GREEN, FRESH^GR PEPPERS^^^LB^LB^^1^^^^^3^^^LB^1^^^^1434^1
	;;^UTILITY(U,$J,113,799,0)
	;;=GRAPES, FRESH^GRAPES, FRESH^^^LB^LB^^1^^^^^3^^^LB^1^^^^1696^1
	;;^UTILITY(U,$J,113,800,0)
	;;=GRAPEFRUIT SECTIONS^GRAPEFRUIT SECT 1GAL^^^GL^GL JR^^1^^^^^3^^^GAL^1^^^^2785^8.113
	;;^UTILITY(U,$J,113,801,0)
	;;=GREENS MUSTARD, FZN^GREENS MUSTARD, FZN^^^LB^LB^^1^^^^^3^^^LB^1^^^^1353^1
	;;^UTILITY(U,$J,113,802,0)
	;;=HAM SMKD, BONELESS^HAM SMKD, BONELESS^^^LB^LB^^1^^^^^1^^^LB^1^^^^221^1
	;;^UTILITY(U,$J,113,803,0)
	;;=ROLLS, HAMBURGER, FRESH^ROLLS, HAMBURG FRSH^^^PG^PG-12^^1^^^^^4^^^EACH^12^^^^1568^.088
	;;^UTILITY(U,$J,113,804,0)
	;;=ROLLS, FRANKFURTER, FRESH^ROLLS, FRANK FRSH^^^PG^PG-12^^1^^^^^4^^^EACH^12^^^^1568^.088
	;;^UTILITY(U,$J,113,805,0)
	;;=SAUSAGE, ITALIAN^SAUSAGE, ITALIAN^^^LB^LB^^1^^^^^1^^^LB^1^^^^656^1
	;;^UTILITY(U,$J,113,806,0)
	;;=KNOCKWURST^KNOCKWURST^^^LB^LB^^1^^^^^1^^^LB^1^^^^642^1
	;;^UTILITY(U,$J,113,807,0)
	;;=LAMB LEG ROAST, BONELESS, FRESH^LAMB LEG RST BND FRSH^^^LB^LB^^1^^^^^1^^^LB^1^^^^1281^1
	;;^UTILITY(U,$J,113,808,0)
	;;=LETTUCE, LEAF^LEAF LETTUCE^^^LB^LB^^1^^^^^3^^^LB^1^^^^1308^1
	;;^UTILITY(U,$J,113,809,0)
	;;=LEMON WEDGES, FRESH^LEMON WEDGES, FRESH^^^LB^LB^^1^^^^^3^^^EACH^16^^^^2806^.128
	;;^UTILITY(U,$J,113,810,0)
	;;=LEMONS, FRESH^LEMONS, FRESH^^^LB^LB^^1^^^^^3^^^EACH^4^^^^2806^.128
	;;^UTILITY(U,$J,113,811,0)
	;;=LETTUCE, ICEBERG^LETTUCE, ICEBERG^^^LB^LB^^1^^^^^3^^^LB^1^^^^1307^1
