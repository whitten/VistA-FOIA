LABMD87P ; IHS/DIR/FJE - BMD 8700 ROUTINE USING REPORT FORMAT 7/20/90 07:35 ;
 ;;5.2;LA;;NOV 01, 1997
 ;;5.2;AUTOMATED LAB INSTRUMENTS;;Sep 27, 1994
 ;CROSS LINK BY ID OR IDE
LA1 S:$D(ZTQUEUED) ZTREQ="@"
 S LANM=$T(+0),TSK=$O(^LAB(62.4,"C",LANM,0)) Q:TSK<1
 Q:'$D(^LA(TSK,"I",0))
 K LATOP D ^LASET Q:'TSK  S X="TRAP^"_LANM,@^%ZOSF("TRAP")
LA2 K TV S TOUT=0 D IN G QUIT:TOUT,LA2:IN'["SAMPLE #"
LA2X S V=$E(IN,9,16) D NUM S (ID,CUP)=+V,IDE=0
 F A=2:1:40 S TOUT=0 D IN Q:TOUT!(IN["SAMPLE #")  D LA2A
 D LA3 G LA2:IN'["SAMPLE #" K TV G LA2X
LA3 Q:'IDE  X LAGEN Q:'ISQN  ;Can be changed by the cross-link code
 F I=0:0 S I=$O(TV(I)) Q:I<1  S:TV(I,1)]"" ^LAH(LWL,1,ISQN,I)=TV(I,1)
 Q
LA2A I IN["PATIENT ID" S V=$E(IN,11,21) D NUM S IDE=+V
 I $E(IN,1,4)="    ",IN'["*",$E(IN,5)'=" " S V=$E(IN,5,9) D NUM F I=0:0 S I=$O(TC(I)) Q:I<1  I V=TC(I,4) S V=$E(IN,10,40) D NUM S V=+V S:V @TC(I,1)=V
 Q
NUM S X="" F JJ=1:1:$L(V) S:$A(V,JJ)>32 X=X_$E(V,JJ)
 S V=X Q
IN S CNT=^LA(TSK,"I",0)+1 IF '$D(^(CNT)) S TOUT=TOUT+1 Q:TOUT>9  H 5 G IN
 S ^LA(TSK,"I",0)=CNT,IN=^(CNT),TOUT=0
 Q
QUIT LOCK ^LA(TSK) H 1 K ^LA(TSK),^LA("LOCK",TSK),^TMP($J),^TMP("LA",$J)
 Q
TRAP D ^LABERR S T=TSK D SET^LAB G @("LA2^"_LANM) ;ERROR TRAP
