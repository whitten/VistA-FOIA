LAIL13 ;SLC/RWF- PROCESS IL- 1303 DATA ;8/16/90  10:35 ;
 ;;5.2;LA;;NOV 01, 1997
 ;;5.1;LAB;;04/11/91 11:06
 ;CROSS LINK BY ID sequence # OR IDE pt. identification OR IDA accession #
LA1 S LANM=$T(+0),TSK=$O(^LAB(62.4,"C",LANM,0)) Q:TSK<1
 Q:'$D(^LA(TSK,"I",0))
 D ^LASET Q:'TSK  S X="TRAP^"_LANM,@^%ZOSF("TRAP")
LA2 S TOUT=0 D IN G QUIT:TOUT,LA2:$E(IN,1,3)'[":" S Y(1)=$P(IN,":",2)
 S TOUT=0,BAD=0 F A=2:1:10 D IN,QC G QUIT:TOUT
 F I=0:0 S I=$O(TC(I)) Q:I<1  X TC(I,2) S:V @TC(I,1)=V
 S ID=+$E(Y(1),1,4),IDA=+$E(Y(5),8,11),TRAY=1,CUP=IDA,IDE=+$E(Y(5),15,26)
LA3 X LAGEN
 F I=0:0 S I=$O(TV(I)) Q:I<1  S:TV(I,1)]"" ^LAH(LWL,1,ISQN,I)=TV(I,1)
 G LA2
QC ;QC TESTING HERE; S BAD=1 IF DONT STORE
 S Y(A)=IN Q
NUM S X="" F JJ=1:1:$L(V) S:$A(V,JJ)>32 X=X_$E(V,JJ)
 S V=X Q
IN S CNT=^LA(TSK,"I",0)+1 IF '$D(^(CNT)) S TOUT=TOUT+1 Q:TOUT>9  H 5 G IN
 S ^LA(TSK,"I",0)=CNT,IN=^(CNT),TOUT=0
 S:IN["~" CTRL=$P(IN,"~",2),IN=$P(IN,"~",1)
 Q
QUIT LOCK ^LA(TSK) H 1 K ^LA(TSK),^LA("LOCK",TSK),^UTILITY($J),^UTILITY("LA",$J) I $D(ZTSK) D KILL^%ZTLOAD K ZTSK
 Q
TRAP D ^LABERR S T=TSK D SET^LAB G @("LA2^"_LANM) ;ERROR TRAP
