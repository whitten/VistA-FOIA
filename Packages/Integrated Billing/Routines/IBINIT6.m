IBINIT6	; ; 21-MAR-1994
	;;Version 2.0 ; INTEGRATED BILLING ;; 21-MAR-94
	F I=1:1:62 S ^UTILITY("DIF",$J,DIFRDIFI)=$T(IXF+I),DIFRDIFI=DIFRDIFI+1
	Q
IXF	;;INTEGRATED BILLING^IB
	;;357.2I;SELECTION LIST;^IBE(357.2,;0;y;y;;n;;;n;;
	;;
	;;357.3I;SELECTION;^IBE(357.3,;0;y;y;;n;;;n;;
	;;
	;;357.4I;SELECTION GROUP;^IBE(357.4,;0;y;y;;n;;;n;;
	;;
	;;357.5I;DATA FIELD;^IBE(357.5,;0;y;y;;n;;;n;;
	;;
	;;357.6I;PACKAGE INTERFACE;^IBE(357.6,;0;y;y;;n;;;y;o;y
	;;
	;;357.7I;FORM LINE;^IBE(357.7,;0;y;y;;n;;;n;;
	;;
	;;357.8;TEXT AREA;^IBE(357.8,;0;y;y;;n;;;n;;
	;;
	;;357.91;MARKING AREA TYPE;^IBE(357.91,;0;y;y;;n;;;y;m;y
	;;
	;;357.92;PRINT CONDITIONS;^IBE(357.92,;0;y;y;;n;;;y;m;y
	;;
	;;358I;IMP/EXP ENCOUNTER FORM;^IBE(358,;0;y;y;;n;;;n;;
	;;
	;;358.1I;IMP/EXP ENCOUNTER FORM BLOCK;^IBE(358.1,;0;y;y;;n;;;n
	;;
	;;358.2I;IMP/EXP SELECTION LIST;^IBE(358.2,;0;y;y;;n;;;n
	;;
	;;358.3I;IMP/EXP SELECTION;^IBE(358.3,;0;y;y;;n;;;n
	;;
	;;358.4I;IMP/EXP SELECTION GROUP;^IBE(358.4,;0;y;y;;n;;;n
	;;
	;;358.5I;IMP/EXP DATA FIELD;^IBE(358.5,;0;y;y;;n;;;n
	;;
	;;358.6IO;IMP/EXP PACKAGE INTERFACE;^IBE(358.6,;0;y;y;;n;;;n
	;;
	;;358.7I;IMP/EXP FORM LINE;^IBE(358.7,;0;y;y;;n;;;n
	;;
	;;358.8;IMP/EXP TEXT AREA;^IBE(358.8,;0;y;y;;n;;;n
	;;
	;;358.91;IMP/EXP MARKING AREA;^IBE(358.91,;0;y;y;;n;;;n
	;;
	;;362.1I;IB AUTOMATED BILLING COMMENTS;^IBA(362.1,;0;y;y;;n;;;n
	;;
	;;362.3IP;IB BILL/CLAIMS DIAGNOSIS;^IBA(362.3,;0;y;y;;n;;;n
	;;
	;;362.4I;IB BILL/CLAIMS PRESCRIPTION REFILL;^IBA(362.4,;0;y;y;;n;;;n
	;;
	;;362.5D;IB BILL/CLAIMS PROSTHETICS;^IBA(362.5,;0;y;y;;n;;;n
	;;
	;;399I;BILL/CLAIMS;^DGCR(399,;0;y;y;;n;;;n;;
	;;
	;;399.1I;MCCR UTILITY;^DGCR(399.1,;0;y;y;;n;;;y;m;y
	;;
	;;399.2I;REVENUE CODE;^DGCR(399.2,;0;y;y;;n;;;y;m;y
	;;
	;;399.3I;RATE TYPE;^DGCR(399.3,;0;y;y;;n;;;y;m;y
	;;
	;;399.4;MCCR INCONSISTENT DATA ELEMENTS;^DGCR(399.4,;0;y;y;;n;;;y;m;y
	;;
	;;399.5ID;BILLING RATES;^DGCR(399.5,;0;y;y;;n;;;y;m;y
	;;
	;;409.95P;PRINT MANAGER CLINIC SETUP;^SD(409.95,;0;y;n;;n;;;n
	;;
	;;409.96P;PRINT MANAGER DIVISION SETUP;^SD(409.96,;0;y;n;;n;;;n
	;;
