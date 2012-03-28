BARPST3 ; IHS/SD/LSL - PAYMENT COMMAND PROCESSOR ; 12/29/2008
 ;;1.8;IHS ACCOUNTS RECEIVABLE;**3,4,6,7,10,20**;OCT 26, 2005
 ;** 'Select Command' processor
 ; ********************************************************************
 ;
EN ;EP - command processor
 K DIR,^TEMP($J,"BARPOST"),BARTR
 S (BARADJ,BARPMT)=0
 S BARDFLT=""
 W !!
 ; -------------------------------
EN1 ;
 ;K BARCOM,BARTYP,BARCAT,BARATYP,BARAMT,BARLIN
 K BARCOM,BARTYP,BARCAT,BARATYP,BARAMT,BARLIN,REVERSAL,REVSCHED  ;BAR*1.8*4
 S BARDSP=1
 D HIT1^BARPST2(BARPASS)
 ; -------------------------------
EN2 ;
 W !!
 K BARCOM,BARAMT
 D:$D(BARHLP)<10 SETHLP^BARPSTU
 ; -------------------------------
ASKLIN ;
 I BARCNT=1 S (BARLIN,BARDFLT)=1 G ASKCOM1
 D ASKLIN^BARFPST3
 I $G(BARLIN)["^" G FINISH
 I $G(BARLIN)=0 G FINISH
 I BARLIN>0,BARLIN<(BARCNT+1) G ASKCOM1
 ;
LNHLP ;
ASKCOM ;EP - select command
 K BARCOM,BARTYP,BARCAT,BARATYP,BARAMT
 S BARDSP=1
 D HIT1^BARPST2(BARPASS)
 W !
 ; -------------------------------
ASKCOM1 ;
 K REVERSAL,REVSCHED  ;BAR*1.8*4 SCR56,SCR58
 W !,"Select Command (Line # "_BARLIN_") : "
 R BARCOM:DTIME
 S BARCOM=$$UPC^BARUTL(BARCOM)
0 ;start new code IHS/SD/SDR bar*1.8*4 DD item 4.1.7.1
 I ("P1A2"[BARCOM) D  I $D(DIROUT)!$D(DIRUT)!$D(DTOUT)!$D(DUOUT)!($G(Y)=0) G ASKCOM
 .S BARBLDA=$O(^BARTMP($J,"B",BARLIN,""))
 .S BARTPB=$$FIND3PB^BARUTL(DUZ(2),BARBLDA)
 .K DIROUT,DIRUT,DTOUT,DUOUT
 .K DIR,DIE,DIC,X,Y,DA,DR
 .Q:$G(BARTPB)=""
 .S BARSTAT=$P($G(^ABMDBILL($P(BARTPB,","),$P(BARTPB,",",2),0)),U,4)
 .Q:BARSTAT'="X"
 .W !!,"STOP!  3P BILL ",$P($P($G(^BARBL(DUZ(2),BARBLDA,0)),U),"-")," has been cancelled."
 .S DIR(0)="Y"
 .S DIR("A")="Are you sure you want to post to this invoice"
 .S DIR("B")="N"
 .D ^DIR K DIR
 ;end new code IHS/SD/SDR bar*1.8*4 DD item 4.1.7.1
 S Q=0
 F J=1:1 D  Q:Q
 .S BARCOM(J)=$P(BARCOM,",",J)
 .Q:$L(BARCOM(J))
 .K BARCOM(J)
 .S J=J-1
 .S Q=1 Q
 I 'J!($L($G(BARCOM(1)))=0) G ASKCOM
 I BARCOM(1)=1 S BARCOM(1)="P" W *7,*7,*7
 I BARCOM(1)=2 S BARCOM(1)="A" W *7,*7,*7
 I BARCOM(1)=3 S BARCOM(1)="Q" W *7,*7,*7
 I '$D(BARHLP(BARCOM(1))) G COMHLP
 I J=1,BARCOM(J)="M" D  G ASKCOM
 .N DA,DIC,BARBLDA,BARACC
 .S DA=BARITDA
 .S DA(1)=BARCOL
 .S DIC="^BARCOL(DUZ(2),DA(1),1,"
 .S BARACC=$$GET1^DIQ(DIC,.DA,7,"I")
 .S BARBLDA=$O(^BARTMP($J,"B",BARLIN,""))
 .D EN^BARPST6(BARPAT,BARBLDA,BARACC)
 .Q
 I J=1,BARCOM(J)="T" D  G ASKCOM
 .S Y=$$DSPLY^BARPST4(BARLIN)
 .D EOP^BARUTL(1)
 I J=1,BARCOM(J)="H" D  G ASKCOM
 .S BARBLDA=$O(^BARTMP($J,"B",BARLIN,""))
 .D EN^BARPST5(BARBLDA)
 ; -------------------------------
GOQ ;
 ;I J=1,BARCOM(J)="Q" G:BARCNT>1 EN1 G FINISH    ;BAR*1.8*4 DD 4.1.7.2
 I J=1,BARCOM(J)="Q" D  G:BARCNT>1 EN1 G FINISH  ;BAR*1.8*4 DD 4.1.7.2
 .D CKNEG(BARLIN)                                ;BAR*1.8*4 DD 4.1.7.2
GOP ;
 I J=1,BARCOM(J)="P" S BARTYP="P" G ASKAMT
 I J=1,BARCOM(J)="1" S BARTYP="P" G ASKAMT
 I J=1,BARCOM(J)="R" D ROLL G ASKCOM
 ; enable posting rollback
GOA ;
 I J=1,BARCOM(J)="A" S BARTYP="A" G ASKAMT
 I J=1,BARCOM(J)="2" S BARTYP="A" G ASKAMT
GOD ;
 I J=1,BARCOM(J)="D" D  G ASKCOM
 . S DFN=BARPAT
 . D VIEWR^XBLM("START^AGFACE")
GOB ;
 I J=1,BARCOM(J)="B" D  G ASKCOM
 . S BARBLDA=$O(^BARTMP($J,"B",BARLIN,""))
 . D DIQ^XBLM(90050.01,BARBLDA)
 I J=1,BARCOM(J)="E" G ^BARPST4
B I J=2,BARCOM(1)="P" D  G:'BARAMT ASKCOM D SETTMP^BARPST3A(BARTYP,BARAMT,BARLIN,BARCAT,,0) G ASKCOM
 .S BARAMT=0,BARTYP="P"
 .S X=$$AMT^BARPSTU(BARCOM(2))
 .I X="^"!(X="?") W *7," (You must enter a valid number)" Q
 .S BARAMT=BARCOM(2)
 .S BARCAT=$O(^BAR(90052.01,"B","PAYMENT TYPE",""))
 .W "  ($"_$J(BARAMT,0,2)_" payment applied to Line # "_BARLIN_")" H 2
 W *7
 W "   Sorry.. ["_BARHLP(BARCOM(1))_"] not active!"
 G ASKCOM
 ; *********************************************************************
ASKAMT ;
 S (BARCAT,BARATYP)=""
 S BARASK=$S(BARTYP="P":"Payment ",BARTYP="A":"Adjustment ",1:"")_"Amount: "
 W !,BARASK R X:DTIME
 ;S X=$$AMT^BARPSTU(X)  ;bar*1.8*20
 S X=$$AMT^BARPSTU(X,-9999999.99,9999999.99)  ;bar*1.8*20
 I X="^" G ASKCOM
 I X="?" W *7,"  Must be a valid number!" G ASKAMT
 S BARAMT=X
 ;IHS/SD/TPF BAR*1.8*3 UFMS REQUEST FOR REVERSAL RECEIPT
 ;ONLY FOR IHS AFFILIATED SITES
 ;I $$IHS^BARUFUT(DUZ(2)),(BARAMT<0),(BARTYP="P") D  G:'REVERSAL!$D(DTOUT)!$D(DUOUT)!(Y="") ASKCOM  ;bar*1.8*20
 I $$IHS^BARUFUT(DUZ(2)),(+BARAMT<0),(BARTYP="P") D  G:'REVERSAL!$D(DTOUT)!$D(DUOUT)!(Y="") ASKCOM  ;bar*1.8*20
 .K REVERSAL,REVSCHED,REVERS
 .S REVERS=$$REVERSAL()
 .S REVERSAL=$P(REVERS,U)
 .S REVSCHED=$P(REVERS,U,2)
 .S Y=$G(Y)                                ;BAR*1.8*4 DD 4.1.7.3
 ;END IHS/SD/TPF BAR*1.8*3, BAR*1.8*4
 ;ONLY ALLOW ZERO DOLLAR PAYMENTS ON NONPAYMENT BATCHES; BAR*1.8*4 DD4.1.5.6
 ;
 ;I $$IHS^BARUFUT(DUZ(2)),(BARAMT'=0),(BARTYP="P"),$P(BARCOL(0),U,28)["NONP" D  G ASKCOM  ;MRS:BAR*1.8*7 IM??
 I $$IHS^BARUFUT(DUZ(2)),(+BARAMT'=0),(BARTYP="P"),$P(BARCOL(0),U,28)["NONP" D  G ASKCOM  ;MRS:BAR*1.8*7 IM??
 .W !!,"You can not post a payment of anything other than $0 if the TDN is NONPAYMENT"
 .D EOP^BARUTL(1)
 ;
 I BARTYP="P" D  G S1
 .S BARCAT=$O(^BAR(90052.01,"B","PAYMENT TYPE",""))
 ;
 ;** adjustment category/type dialog
 S DIC=90052.01
 S DIC(0)="AEMNQZ"
 S DIC("A")="Adjustment Category: "
 S DIC("S")="I "",3,4,13,14,15,16,20,21,22,""[("",""_Y_"","")"
 K DD,DO
 D ^DIC
 K DIC
 I +Y<0 W *7 K BARAMT W !! G ASKAMT
 S BARCAT=+Y
 S:BARCAT=16 BARAMT=-BARAMT ;grouper
 S BARX=0,BARJ=0
 K BARATYP
 F  S BARX=$O(^BARTBL("D",BARCAT,BARX)) Q:'BARX  D  Q:BARJ>1
 .S BARJ=BARJ+1
 .Q:BARJ>1
 .S BARATYP=BARX
 I BARJ=1,$G(BARATYP) G S1
 S DIC=90052.02
 S DIC(0)="AEMNQZ"
 S DIC("A")="Adjustment Type: "
 S DIC("S")="I $P(^(0),U,2)=BARCAT"
 K DD,DO
 D ^DIC
 K DIC
 I +Y<0 K BARAMT W *7,!! G ASKAMT
 S BARATYP=+Y
 ;--------------------------------
S1 ;
 D SETTMP^BARPST3A(BARTYP,BARAMT,BARLIN,BARCAT,BARATYP,0) ;BAR*1.8*4 DD 4.1.7.2
 ;D SETTMP^BARPST3A(BARTYP,BARAMT,BARLIN,BARCAT,BARATYP) ;BAR*1.8*4 DD 4.1.7.2
 G ASKCOM
 ; *********************************************************************
COMHLP ;
 D COMHLP^BARPSTU
 G ASKCOM1
 ; *********************************************************************
FINISH ;
 I '$G(BARPMT)&('$G(BARADJ))&('$D(BARROLL))&'$D(BARTR) D CANCEL Q
 ; enable posting rollback
FIN S BARQ=$$POST()                            ;BAR*1.8*4 DD 4.1.7.2
 I BARQ="M" G EN1
 I BARQ="C" D CANCEL Q
 ;I BARQ="P" D POSTTX^BARPSTU,EN^BARROLL Q  ;BAR*1.8*4 DD 4.1.7.2
 I BARQ="P" D POSTTX^BARPSTU                ;BAR*1.8*4 DD 4.1.7.2 
 I $G(BARSTOP)=1 G FIN                      ;BAR*1.8*4 DD 4.1.7.2
 D EN^BARROLL
 K ^BARTMP($J)                              ;BAR*1.8*4 DD 4.1.7.2
 Q
 ;--------------------------------
POST() ;
P1 ;
 D HIT1^BARPST2(BARPASS)
 D EOP^BARUTL(2)
PDIR ;
 K DIR
 S DIR(0)="SAO^P:POST TO A/R;M:MORE;C:CANCEL"
 S DIR("A")="Select Action (P/M/C): "
 D ^DIR
 K DIR
 I $D(DUOUT)!(Y="") W *7 G PDIR
 Q Y
 ; *********************************************************************
ROLL ;EP - tag a bill for rollback to 3P
 ; enable posting rollback
 N BARBLDA
 S BARBLDA=$O(^BARTMP($J,"B",BARLIN,""))
 S BARROLL(BARBLDA)=""
 K DIC,DIE
 S DIE="^BARBL(DUZ(2),"
 S DA=BARBLDA
 S DR="214///@"
 D ^DIE
 K DIC,DIE,X,Y,DR
 K DIR
 S DIR("A")="TAGGED for Rolling. Enter RETURN to continue."
 D EOP^BARUTL(0)
ROLLE Q
 ; *********************************************************************
CANCEL ;
 K ^BARTMP($J)
 K BARPMT,BARADJ,BARTR,BARROLL
 Q
 ;IHS/SD/TPF BAR*1.8*3 UFMS LATE REQUEST FOR RECEIPT REVERSAL
REVERSAL() ;EP - GET THE ORIGINAL TRANSACTION
ASKREV ;EP - ASK AGAIN
 ;Begin new code                  ;MRS:BAR*1.8*10 D158-3
 ;PREVENT ALL PAYMENT REVERSALS
 W !!,"PAYMENT REVERSALS ARE NO LONGER ALLOWED,"
 W !,"PLEASE USE THE 'PAYMENT CREDIT' TRANSACTION TYPE"
 D EOP^BARUTL(1)
 Q 0  ;End ;MRS:BAR*1.8*10 D158-3
 D REVHDR
 N ARBILLIN,TRANSDAT,TRANDATE,EXTRDT,TRANTYP,ACCT,RETURN,ARRAY,AMOUNT,BALANCE
 N COLDA,ITEMDA,REVSCHED
 N CREDIT,DEBIT
 N BARCK S BARCK=0                       ;BAR*1.8*4 DD 4.1.7.3
 S ARBILLIN=$O(^BARTMP($J,"B",BARLIN,""))
 ;I $G(ARBILLIN)="" W !!,"There are no transactions to reverse!" H 2 Q  ;BAR*1.8*4 DD 4.1.7.3
 I $G(ARBILLIN)="" D CKREV Q 0           ;BAR*1.8*4 DD 4.1.7.3
 I $G(BARPMT)>0 S BARCK=1                ;BAR*1.8*4 DD 4.1.7.3
 S BARBAL=0
 S TRANSDAT=""
 S CNT=0
 K BARNOTZ                 ;FOR NEW CHECK;BAR*1.8*6 DD 4.2.6
 F  S TRANSDAT=$O(^BARTR(DUZ(2),"AC",ARBILLIN,TRANSDAT)) Q:'TRANSDAT  D
 .S DEBIT=$$GET1^DIQ(90050.03,TRANSDAT_",",3,"E")
 .S CREDIT=$$GET1^DIQ(90050.03,TRANSDAT_",",2,"E")
 .S:CREDIT>0 BARCK=1
 .Q:+$G(BARAMT)'=-CREDIT
 .S TRANTYP=$$GET1^DIQ(90050.03,TRANSDAT_",",101,"E")
 .S COLDA=$$GET1^DIQ(90050.03,TRANSDAT_",",14,"I")
 .S ITEMDA=$$GET1^DIQ(90050.03,TRANSDAT_",",15,"I")
 .S REVSCHED=$$GET1^DIQ(90051.1101,ITEMDA_","_COLDA_",",20,"I")
 .Q:TRANTYP'="PAYMENT"
 .I '$$EXCHK(ARBILLIN,TRANSDAT) Q            ;NEW CHECK; BAR*1.8*6 DD 4.2.6
 .S CNT=CNT+1
 .S ARRAY(CNT)=TRANSDAT_U_REVSCHED
 .W !,CNT,"."
 .S EXTRDT=$P(TRANSDAT,".")
 .S Y=EXTRDT X ^DD("DD") S EXTRDT=Y
 .S ACCT=$$GET1^DIQ(90050.03,TRANSDAT_",",6,"E")
 .S COLBAT=$$GET1^DIQ(90050.03,TRANSDAT_",",14,"E")
 .S COLITEM=$$GET1^DIQ(90050.03,TRANSDAT_",",15,"E")
 .S (BARX,X)=$S($G(CREDIT):-CREDIT,1:$G(DEBIT))
 .S X2=2
 .S X3=11
 .D COMMA^%DTC
 .I TRANTYP["PENDING" S X="**"_X_"**"
 .S AMOUNT=X
 .N X
 .I TRANTYP'["PENDING"&(TRANTYP'["GENERAL") D
 .. S BARBAL=BARBAL+BARX
 . S X=BARBAL,X2=2,X3=11 D COMMA^%DTC
 . S BALANCE=X
 .W EXTRDT
 .W ?25,TRANTYP
 .W ?45,AMOUNT
 .W ?60,BALANCE
 .W !
 .W ?10,ACCT
 .W ?25,$E(COLBAT,1,20)
 .W ?60,COLITEM
 I 'BARCK D CKREV Q 0
 I $G(BARNOTZ),CNT=0 D  Q 0                ;NEW CHECK; BAR*1.8*6 DD 4.2.6
 .W !!,"TRANSACTION HAS ALREADY BEEN LINKED TO ANOTHER REVERSAL"
 .D EOP^BARUTL(1)
 I CNT=0 W !!,"NO TRANSACTIONS MATCH THE REVERSAL AMOUNT ENTERED!" H 2 Q 0
 W !!,BARDSH
 K DIR
 S DIR(0)="NO^1:"_CNT_":0"""
 S DIR("A")="Choose One"
 D ^DIR
 Q:$D(DTOUT)!$D(DUOUT)!(Y="") 0
 W !,"You have chosen "_TRANTYP_" dated "_EXTRDT_"."
 K DIR
 S DIR(0)="YO"
 S DIR("B")="Y"
 S DIR("A")="Is that correct"
 D ^DIR
 Q:$D(DTOUT)!$D(DUOUT) 0
 G ASKREV:'Y
 Q ARRAY(Y)
REVHDR ;EP - REVERSAL HEADER
 W @IOF
 W !!,"Which Original Payment does this apply to?"
 W !
 W "TRANS DATE",?25,"TRANS TYPE",?50,"AMOUNT",?65,"BALANCE"
 W !,?10,"A/R ACCT",?25,"BATCH",?55,"BATCH ITEM"
 W !,BARDSH
 Q
CKNEG(LIN) ;EP; CHECK FOR NEGATIVE BALANCE  ;BAR*1.8*4 DD 4.1.7.2
 Q:'$$IHS^BARUFUT(DUZ(2))              ;IGNORE NON-IHS
 N BARDA,BARB
 S BARDA=$O(^BARTMP($J,"B",LIN,""))
 S BARB=$P(^BARTMP($J,BARDA,LIN),U,5)
 I BARB<0 D
 .W !!,"THE TRANSACTION(S) YOU ARE ATTEMPTING TO POST WILL PUT THIS BILL INTO A NEGATIVE"
 .W !,"BALANCE BY -$"_-BARB,"  PLEASE CANCEL OR USE 'M' FOR MORE TO EDIT YOUR TRANSACTION(S)"
 .W !,"TO PREVENT A NEGATIVE BALANCE"
 .D EOP^BARUTL(1)
 Q
CKREV ; CHECK FOR PAYMENT PRECEDING REVERSAL  ;BAR*1.8*4 DD 4.1.7.3
 W !!,"<<YOU ARE ATTEMPTING TO POST A REVERSAL WHEN THERE IS NO PAYMENT ON THE BILL"  ;BAR*1.8*4 DD 4.1.7.3
 W !,"PLEASE CHECK YOUR TRANSACTION AND TRY AGAIN"
 D EOP^BARUTL(1)
 Q
EXCHK(BARDA,TX) ; BAR*1.8*6 DD 4.2.6
 ;     ENTERS WITH TRANSACTION DATE/TIME OF ORIGINAL PAYMENT
 I BARCOL'=COLDA!(BARITM'=ITEMDA) D  Q 0  ;MUST BE IN SAME COLLECTION BATCH/ITEM
 .W !!,TX," CANNOT BE LINKED BECAUSE IT IS NOT IN SAME COLLECTION BATCH/ITEM"
 .D EOP^BARUTL(1)
 I TX<$P($G(^BAR(90052.06,DUZ(2),DUZ(2),15)),U,5) D  Q 0  ;MRS:BAR*1.8*10 D158-2
 .W !!,TX," CANNOT BE LINKED BECAUSE IT IS BEFORE THE UFMS BOOKING DATE,"
 .W !,"USE THE PAYMENT CREDIT TRANSACTION TYPE"
 .D EOP^BARUTL(1)
 N X,Y,Z                                  ;NOW CHECK IF PREVIOUS REVERSAL
 S Z=1
 S X=0
 F  S X=$O(^BARTR(DUZ(2),"AC",BARDA,X)) Q:'X  D
 .S Y=$P($G(^BARTR(DUZ(2),X,1)),U,10)     ;TRANSACTION REVERSAL DATE STAMP
 .I Y=TX D
 ..S Z=0
 I Z D                                    ;CHECK IF PAIRED IN THIS SESSION
 .S X=0
 .F  S X=$O(BARTR(BARLIN,X)) Q:'X  D
 ..I $P(BARTR(BARLIN,X),U,5)=TX S Z=0
 I 'Z S BARNOTZ=1                         ;SET FLAG FOR DISPLAY MESSAGE
 Q Z
