IBOCPD	;ALB/ARH - CLERK PRODUCTIVITY REPORTS ; 10/8/91
	;;Version 2.0 ; INTEGRATED BILLING ;; 21-MAR-94
	;
EN	;
	;***
	;I $D(XRT0) S:'$D(XRTN) XRTN="IBOCPD" D T1^%ZOSV ;stop rt clock
	;S XRTL=$ZU(0),XRTN="IBOCPD-1" D T0^%ZOSV ;start rt clock
	S DIR(0)="SO^F:FULL CLERK PRODUCTIVITY REPORT;S:SUMMARY OF CLERK PRODUCTIVITY REPORT;"
	D ^DIR K DIR G:$D(DIRUT) EXIT I Y="F" D FULL G EXIT
	I Y="S" D ^IBOCPDS
EXIT	K X,Y,DTOUT,DUOUT,DIRUT,DIROUT,DIOEND
	;***
	;I $D(XRT0) S:'$D(XRTN) XRTN="IBOCPD" D T1^%ZOSV ;stop rt clock
	Q
	;
FULL	D HOME^%ZIS S IBHDR="CLERK PRODUCTIVITY REPORT" W @IOF,?27,IBHDR,!!
	W !,"Report requires 132 columns."
	S IBFLD="Date Entered" D RANGE G:IBQUIT ENDF
	;
PRINTF	;print report
	;***
	;I $D(XRT0) S:'$D(XRTN) XRTN="IBOCPD" D T1^%ZOSV ;stop rt clock
	;S XRTL=$ZU(0),XRTN="IBOCPD-2" D T0^%ZOSV ;start rt clock
	S DHD=IBHDR_" FOR "_IBBEGE_" - "_IBENDE,DIOEND="D PAUSE^IBOCPDS"
	S FR=IBBEG_",?,?,",TO=IBEND_",?,?,",L=0,DIC="^DGCR(399,",(BY,FLDS)="[IB CLK PROD]"
	D EN1^DIP
ENDF	;
	K DIC,L,FLDS,BY,DHD,FR,TO,IBHDR,IBBEG,IBEND,IBBEGE,IBENDE,IBFLD,IBQUIT,X,Y
	Q
	;
	;external entry point to get a range of dates
RANGE	;get date range
	S DIR(0)="D^:NOW:EX",DIR("A")="START WITH "_IBFLD
	D ^DIR K DIR I $D(DIRUT) S IBQUIT=1 Q
	S IBBEG=Y X ^DD("DD") S IBBEGE=Y
	S DIR(0)="D^"_IBBEG_":NOW:EX",DIR("A")="GO TO "_IBFLD,DIR("B")="TODAY"
	D ^DIR K DIR I $D(DIRUT) S IBQUIT=1 Q
	S IBEND=Y X ^DD("DD") S IBENDE=Y,IBQUIT=0
	Q
