FHORD8	; HISC/REL/NCA - Diet Order Lists ;4/24/95  09:15
	;;5.0;Dietetics;;Oct 11, 1995
	W @IOF,!!?29,"WARD DIET ORDER LISTS",!!
D0	R !!,"Select by C=COMMUNICATION OFFICE or W=WARD: ",FHXX:DTIME G:'$T!("^"[FHXX) KIL I "cw"[FHXX S X=FHXX D TR^FH S FHXX=X
	I FHXX'?1U!("CW"'[FHXX) W *7," Enter C or W" G D0
	I FHXX="C" G D2
F1	R !!,"Select WARD (or ALL): ",X:DTIME G:'$T!("^"[X) KIL D:X="all" TR^FH I X="ALL" S WRD=0
	E  K DIC S DIC="^FH(119.6,",DIC(0)="EQM" D ^DIC G:Y<1 F1 S WRD=+Y
	G S0
D2	R !!,"Select COMMUNICATION OFFICE (or ALL): ",X:DTIME G:'$T!("^"[X) KIL D:X="all" TR^FH I X="ALL" S WRD=0
	E  K DIC S DIC="^FH(119.73,",DIC(0)="EMQ" D ^DIC G:Y<1 D2 S WRD=+Y
S0	R !!,"Sort Patients: (A=Alphabetically  R=Room-Bed) R// ",SRT:DTIME G:'$T!(SRT["^") KIL S:SRT="" SRT="R" I "ar"[SRT S X=SRT D TR^FH S SRT=X
	I SRT'?1U!("AR"'[SRT) W *7," Enter A or R" G S0
S1	R !!,"Select Type of Service (T, C, D or ALL): ALL// ",SER:DTIME G:'$T!(SER["^") KIL S:SER="" SER="A" S X=SER D TR^FH S SER=X
	I $P("TRAY",SER,1)'="",$P("CAFETERIA",SER,1)'="",$P("DINING ROOM",SER,1)'="",$P("ALL",SER,1)'="" W *7,!," Enter T for Tray, C for Cafeteria, D for Dining Room, or A for All" G S1
	S SER=$E(SER,1)
L0	W ! K IOP,%ZIS S %ZIS("A")="Select LIST Printer: ",%ZIS="MQ" D ^%ZIS K %ZIS,IOP G:POP KIL
	I $D(IO("Q")) S FHPGM="^FHORD81",FHLST="FHXX^WRD^SRT^SER" D EN2^FH G KIL:'WRD,D0
	U IO D ^FHORD81 D ^%ZISC K %ZIS,IOP G KIL:'WRD,D0
KIL	; Final variable kill
	K ^TMP($J),^TMP("FH",$J),%,%H,%I,%T,A1,ADM,ALG,BAG,BID,C,CAL,COM,CT,D3,DAS,DFN,DIC,DTE,DTP,FHA1,FHDU,FHLD,FHLST,FHOR,FHORD,FHPGM,I,IOP,J,K,K1,K2,K3,KK,L,L1,LEN,LL,LST,MEAL,N,NM,NOW
	K NX,NXW,OLD,OLN,P,POP,P0,P1,P2,P3,PG,PID,Q,QTY,REC,RM,SER,SRT,TF,TF2,TIM,TP,TU,TUN,TW,WRD,WRDN,WRDS,W1,WW,X,X0,X1,X2,X3,X5,FHXX,XX,XY,XZ,Y,YY,Z,Z1,Z5,ZZ Q
