AQACPREI ;CREATED BY AUBPI ON NOV 6,1989
 ;2.04
 K ^UTILITY("AUDSET",$J) F AUBPI=1:1 S AUBPIX=$P($T(Q+AUBPI),";;",2) Q:AUBPIX=""  S AUBPIY=$P(AUBPIX,"=",2,99),AUBPIX=$P(AUBPIX,"=",1) S @AUBPIX=AUBPIY
 K AUBPI,AUBPIX,AUBPIY D EN2^%AUKD
Q Q
 ;;^UTILITY("AUDSET",$J,9002157)=S^S
 ;;^UTILITY("AUDSET",$J,9002159)=S^S
 ;;^UTILITY("AUDSET",$J,9002160.1)=S^S
 ;;^UTILITY("AUDSET",$J,9002160.2)=S^S
 ;;^UTILITY("AUDSET",$J,9002160.3)=S^S
 ;;^UTILITY("AUDSET",$J,9002160.4)=S^S
 ;;^UTILITY("AUDSET",$J,9002160.5)=S^S
 ;;^UTILITY("AUDSET",$J,9002160.6)=S^S
 ;;^UTILITY("AUDSET",$J,9002160.7)=S^S
 ;;^UTILITY("AUDSET",$J,9002160.8)=S^S
