FHPRC	; HISC/REL - Menu Cycle Management ;5/10/95  08:29
	;;5.0;Dietetics;;Oct 11, 1995
EN2	; Enter/Edit Menu Cycles
	S (DIC,DIE)="^FH(116,",DIC(0)="AEQLM",DIC("DR")=".01",DLAYGO=116 W ! D ^DIC K DIC,DLAYGO G KIL:U[X!$D(DTOUT),EN2:Y<1
	S DA=+Y,DR=".01;2:99;1" S:$D(^XUSEC("FHMGR",DUZ)) DIDEL=116 D ^DIE K DA,DR,DIDEL,DIE,X,Y G EN2
EN3	; Enter/Edit Meals
	S (DIC,DIE)="^FH(116.1,",DIC(0)="AEQLM",DIC("DR")=".01",DLAYGO=116.1 W ! D ^DIC K DIC,DLAYGO G KIL:U[X!$D(DTOUT),EN3:Y<1
	S (DA,M1)=+Y,DR=".01:99",DR(2,116.11)=".01;S L1=+X;2;1;S COD=X;3//^S X=$P($G(^FH(114,+L1,0)),U,7),X=$P($G(^FH(114.1,+X,0)),U,1);S REC=X;10"
	S DR(3,116.12)=".01;S CHK=+X;1//^S X=$S(REC=CHK:COD,1:"""")"
	S:$D(^XUSEC("FHMGR",DUZ)) DIDEL=116.1 W ! D ^DIE K DIDEL G:'$D(DA)!($D(Y)) A2
A0	K DIC S DIC="^FH(116.1,",DIC(0)="AEQM",DIC("A")="Select ADD-ON Meal: " W ! D ^DIC K DIC,DIE G EN3:"^"[X!$D(DTOUT),A0:Y<1 S A1=+Y
	I '$D(^FH(116.1,M1,"RE",0)) S ^(0)="^116.11P^^"
	F K=0:0 S K=$O(^FH(116.1,A1,"RE",K)) Q:K<1  S L1=+^(K,0) D A1
A2	K CHK,COD,DA,DR,M1,X,Y,A1,K,L,L1,REC G EN3
A1	I $D(^FH(116.1,M1,"RE","B",L1)) W !,$P(^FH(114,L1,0),"^",1)," already part of meal" Q
	K DIC,DD,DO,DINUM S DIC="^FH(116.1,M1,""RE"",",DIC(0)="LM",DA(1)=M1,DLAYGO=116.1,X=L1 D FILE^DICN K DIC,DLAYGO
	S $P(^FH(116.1,M1,"RE",+Y,0),"^",2,3)=$P(^FH(116.1,A1,"RE",K,0),"^",2,3)
	G:'$D(^FH(116.1,A1,"RE",K,"D",0)) A3 S ^FH(116.1,M1,"RE",+Y,"D",0)=^FH(116.1,A1,"RE",K,"D",0)
	F L=0:0 S L=$O(^FH(116.1,A1,"RE",K,"D",L)) Q:L<1  S X=^(L,0),^FH(116.1,M1,"RE",+Y,"D",L,0)=X,^FH(116.1,M1,"RE",+Y,"D","B",+X,L)=""
A3	Q:'$D(^FH(116.1,A1,"RE",K,"R",0))  S ^FH(116.1,M1,"RE",+Y,"R",0)=^FH(116.1,A1,"RE",K,"R",0)
	F L=0:0 S L=$O(^FH(116.1,A1,"RE",K,"R",L)) Q:L<1  S X=^(L,0),^FH(116.1,M1,"RE",+Y,"R",L,0)=X,^FH(116.1,M1,"RE",+Y,"R","B",+X,L)=""
	Q
EN4	; Enter/Edit Holiday Meals
	S (DIC,DIE)="^FH(116.3,",DIC(0)="AEQLM",DIC("DR")=".01",DLAYGO=116.3 W ! D ^DIC K DIC,DLAYGO G KIL:U[X!$D(DTOUT),EN4:Y<1
	S DA=+Y,DR=".01:99" S:$D(^XUSEC("FHMGR",DUZ)) DIDEL=116.3 D ^DIE K DA,DR,DIDEL,DIE,X,Y G EN4
KIL	G KILL^XUSCLEAN
