IBDEINI5	; ; 18-MAR-1994
	;;Version 2.0 ; INTEGRATED BILLING ;; 21-MAR-94
	K ^UTILITY("DIF",$J) S DIFRDIFI=1 F I=1:1:20 S ^UTILITY("DIF",$J,DIFRDIFI)=$T(IXF+I),DIFRDIFI=DIFRDIFI+1
	Q
IXF	;;IB ENCOUNTER FORM IMP/EXP^IBDE
	;;358I;IMP/EXP ENCOUNTER FORM;^IBE(358,;0;y;n;;n;;;y;o;n
	;;
	;;358.1I;IMP/EXP ENCOUNTER FORM BLOCK;^IBE(358.1,;0;y;n;;n;;;y;o;n
	;;
	;;358.2I;IMP/EXP SELECTION LIST;^IBE(358.2,;0;y;n;;n;;;y;o;n
	;;
	;;358.3I;IMP/EXP SELECTION;^IBE(358.3,;0;y;n;;n;;;y;o;n
	;;
	;;358.4I;IMP/EXP SELECTION GROUP;^IBE(358.4,;0;y;n;;n;;;y;o;n
	;;
	;;358.5I;IMP/EXP DATA FIELD;^IBE(358.5,;0;y;n;;n;;;y;o;n
	;;
	;;358.6IO;IMP/EXP PACKAGE INTERFACE;^IBE(358.6,;0;y;n;;n;;;y;o;n
	;;
	;;358.7I;IMP/EXP FORM LINE;^IBE(358.7,;0;y;n;;n;;;y;o;n
	;;
	;;358.8;IMP/EXP TEXT AREA;^IBE(358.8,;0;y;n;;n;;;y;o;n
	;;
	;;358.91;IMP/EXP MARKING AREA;^IBE(358.91,;0;y;n;;n;;;y;o;n
	;;
