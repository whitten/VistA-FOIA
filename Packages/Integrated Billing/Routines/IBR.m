IBR	;ALB/AAS - INTEGRATED BILLING - A/R INTERFACE ; 25-FEB-91
	;;Version 2.0 ; INTEGRATED BILLING ;; 21-MAR-94
	;;Per VHA Directive 10-93-142, this routine should not be modified.
	;
	;  - handles calls to AR
	;  -  input   IBSEQNO = 1,2, or 3
	;  -          IBDUZ   = user causing entry
	;  -          IBNOS   = IBnumber^Ibnumber... to process
	;  -          DFN     = patient number
	;  -  output  Y       = 1 if successful
	;  -                   =-1^error code if unsuccessful
	S IBERR=""
	I '$D(IBSEQNO) S IBERR="IB017;"_IBERR G END
	D @IBSEQNO
	G END
	;
1	;  -pass new entries to a/r
	S IBTOTL=0 N IBNOW
	F I=1:1 S IBN=$P(IBNOS,"^",I) Q:'IBN  S X=$S($D(^IB(IBN,0)):^(0),1:"") S:X="" IBERR="IB018;"_IBERR D TRCHK S IBTOTL=IBTOTL+$P(X,"^",7)
	Q:IBNOS=""!(IBTOTL<1)
	S IBSERV="",IBATYP=$P(X,"^",3) I $D(^IBE(350.1,+IBATYP,0)) S IBSERV=$P(^(0),"^",4)
	D ARPARM^IBAUTL
	S IBWHER=3
	D BILLNO^IBAUTL I +Y<1 G ERR
	S IBWHER=4
	;
	F I=1:1 S IBN=$P(IBNOS,"^",I) Q:'IBN  D UP1,UP3:IBSEQNO=3
	Q
UP1	;  -update IB data and reindex
	S DIE="^IB(",DA=IBN,DR=".05////"_$S(IBERR="":3,1:9)_";.11////"_IBIL_";.12////"_IBTRAN
	D ^DIE K DIE,DR,DA
	I $D(Y) S IBERR="IB020;"_IBERR
	S DA=IBN,DIK="^IB(" D IX^DIK
	K DIK,DA
	Q
2	S IBTOTL=0 N IBNOW
	F I=1:1 S IBN=$P(IBNOS,"^",I) Q:'IBN  S X=$S($D(^IB(IBN,0)):^(0),1:"") S:X="" IBERR="IB018;"_IBERR S:$P($G(^IB(+$P(X,"^",9),0)),"^",5)'=8 IBTOTL=IBTOTL+$P(X,"^",7)
	S IBIL=$P(X,"^",11)
	;
	S IBSERV="",IBATYP=$P(X,"^",3) I $D(^IBE(350.1,+IBATYP,0)) S IBSERV=$P(^(0),"^",4)
	D ARPARM^IBAUTL
	S IBWHER=3
	; - piece 1 of X (21) denotes the AR Trans. Type of Decrease Adjustment
	I IBTOTL>0 S X="21^"_IBTOTL_"^"_IBIL_"^"_IBDUZ_"^"_$P(IBNOW,".")_"^"_$S($D(^IBE(350.3,+$P(^IB(IBNOS,0),"^",10),0)):$P(^(0),"^",1),1:"") D ^PRCASER1 I +Y<0 G ERR
	;
	S IBWHER=4
	F I=1:1 S IBN=$P(IBNOS,"^",I) Q:'IBN  D UP2
	Q
UP2	;  -update IB data and reindex
	S DIE="^IB(",DA=IBN,DR=".05////"_$S(IBERR="":3,1:9)
	D ^DIE K DIE,DR,DA
	I $D(Y) S IBERR="IB020;"_IBERR
	S DA=IBN,DIK="^IB(" D IX^DIK
	;W "FILING UPDATED ENTRY IN IB",!
	K DIK,DA
	;  -update parent to cancelled
	S IBPARNT=$P(^IB(IBN,0),"^",9),IBCRES=$P(^IB(IBN,0),"^",10)
	S DIE="^IB(",DA=IBPARNT,DR=".05////10;.1////"_IBCRES D ^DIE K DIE,DA,DR
	Q
	;
3	D 1
	Q
UP3	;  -update status of all previous bills to updated
	;
	S IBJ="" F IBI=0:0 S IBJ=$O(^IB("AD",$P(^IB(IBN,0),"^",9),IBJ)) Q:'IBJ  I $D(^IB(IBJ,0)),$P(^(0),"^",5)=3,IBN'=IBJ S DIE="^IB(",DA=IBJ,DR=".05////4" D ^DIE
	Q
	;
ERR	D ^IBAERR:$D(ZTQUEUED) Q
END	;
	S Y=$S(IBERR="":1,1:"-1^"_IBERR)
	K IBERR Q
	;
TRCHK	;  - if entry has an ar transaction number take out of list
	I $P(X,"^",12)!($$HOLD^IBRUTL(X,IBN,IBDUZ,IBSEQNO)) D
	. I I=1 S IBNOS=$P(IBNOS,"^",2,99)
	. E  S IBNOS=$P(IBNOS,"^",1,I-1)_"^"_$P(IBNOS,"^",I+1,99)
	. S $P(X,"^",7)=0,I=I-1
	Q
