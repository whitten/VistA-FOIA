LACD1600 ; IHS/DIR/FJE - CELL-DYN 1500,1600,2000 AUTOMATED DATA 6/2/89 8:57 AM ;
 ;;5.2;LA;;NOV 01, 1997
 ;;5.2;AUTOMATED LAB INSTRUMENTS;**6**;Sep 27, 1994;
 ;CROSS LINK BY ID OR IDE
LA1 S LANM=$T(+0),TSK=$O(^LAB(62.4,"C",LANM,0)) Q:TSK<1
 Q:'$D(^LA(TSK,"I",0))  S LAC=","
 K LATOP D ^LASET Q:'TSK  S X="TRAP^"_LANM,@^%ZOSF("TRAP")
LA2 K TV,Y S TOUT=0 D IN G QUIT:TOUT
 D LA2:$P(IN,LAC)'="""   """ S TOUT=0,A=1 S Y(A)=IN
LAC I IN="" D IN G QUIT:TOUT S TOUT=0,A=A+1,Y(A)=IN G LAC
 ;S V=$P(Y(1),",",2) D NUM S IDE=+V,V=$E($P(Y(1),",",3),2,99) D NUM
 ;S (ID,CUP)=+V,TRAY=1,V=$P(Y(1),",",38) D NUM S TYPE=V
 S IDE=+$$NUM($P(Y(1),LAC,2)),(ID,CUP)=+$$NUM($E($P(Y(1),LAC,3),2,99))
 S TRAY=1,TYPE=$$NUM($P(Y(1),LAC,38))
 F I=0:0 S I=$O(TC(I)) Q:I=""  X TC(I,2) S @TC(I,1)=V ;V COULD BE MODIFIED ON THIS LINE
LA3 G:'ID LA2 X LAGEN G LA2:'ISQN ;Can be changed by the cross-link code
 F I=0:0 S I=$O(TV(I)) Q:I=""  S:TV(I,1)>0 ^LAH(LWL,1,ISQN,I)=TV(I,1)
 G QUIT:TOUT>9 G LA2 ;<543/EPH> ADDED G QUIT:TOUT>9
QC ;QC TESTING HERE; S BAD=1 IF DONT STORE
 S Y(A)=IN Q
NUM(X) N LAX,JJ S LAX="" F JJ=1:1:$L(X) S:$A(X,JJ)>32 LAX=LAX_$E(X,JJ)
 Q LAX
IN G QUIT:'$D(^LA(TSK,"I",0)) S CNT=^LA(TSK,"I",0)+1 IF '$D(^(CNT)) S TOUT=TOUT+1 Q:TOUT>9  H 2 G IN
 S ^LA(TSK,"I",0)=CNT,IN=^(CNT),TOUT=0 I ECHOALL S Z=IN D WRITE
 S:IN["~" CTRL=$P(IN,"~",2),IN=$P(IN,"~",1)
 Q
OUT S CNT=^LA(TSK,"O")+1,^("O")=CNT,^("O",CNT)=OUT
 LOCK +^LA("Q") S Q=^LA("Q")+1,^("Q")=Q,^("Q",Q)=TSK LOCK -^LA("Q")
 Q
WRITE I IO]"" O IO::1 IF $T U IO W:'ECHOALL *7 W !,Z C IO
 Q
QUIT S TOUT=9 K ^LA(TSK),^LA("LOCK",TSK) I $D(ZTSK) D KILL^%ZTLOAD K ZTSK
 Q
TRAP D ^LABERR S T=TSK D SET^LAB G @("LA2^"_LANM) ;ERROR TRAP
