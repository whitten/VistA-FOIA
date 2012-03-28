BDMD01T ; IHS/CMI/LAB - 2010 DIABETES AUDIT ;
 ;;2.0;DIABETES MANAGEMENT SYSTEM;**3**;JUN 14, 2007
 ;
 ;cmi/anch/maw 9/12/2007 code set versioning in TOBACCO1,ASAPOV
 ;
TOBACCO(P,BDATE,EDATE) ;EP
 I '$G(P) Q ""
 NEW BDMTOB,BDMSDX,BDMXPND,BDM1320,BDMSCPT,BDMALL,D,%,F,BD1Y
 S BDMTOB=$$TOBACCOS(P,BDATE,EDATE)  ;get last recorded HF
 I BDMTOB]"" S BDMALL(9999999-$P(BDMTOB,U,3))=BDMTOB
 S BDMSDX=$$DX(P,BDATE,EDATE)
 I BDMSDX]"" S D=$P(BDMSDX,U,3),D=9999999-D I '$D(BDMALL(D)) S BDMALL(D)=BDMSDX
 S BDMXPND=$$PED(P,BDATE,EDATE)
 I BDMXPND]"" S D=$P(BDMXPND,U,3),D=9999999-D I '$D(BDMALL(D)) S BDMALL(D)=BDMXPND
 S BDM1320=$$DENT(P,BDATE,EDATE)
 I BDM1320]"" S D=$P(BDM1320,U,3),D=9999999-D I '$D(BDMALL(D)) S BDMALL(D)=BDM1320
 S BDMSCPT=$$CPTSM(P,BDATE,EDATE)
 I BDMSCPT]"" S D=$P(BDMSCPT,U,3),D=9999999-D I '$D(BDMALL(D)) S BDMALL(D)=BDMSCPT
 K ^TMP($J,"A")
 I '$D(BDMALL) Q "3  Not Documented "
 S D=0,D=$O(BDMALL(D))
 S F=$P(BDMALL(D),U)
 I F["305.13"!(F["V15.82")!(F="1036F") Q "2  Not a Current User "_$P(BDMALL(D),U,1)_"  "_$P(BDMALL(D),U,2)
 I F="SMOKER IN HOME"!(F="SMOKE FREE HOME")!(F["CEREMONIAL")!(F["NON-TOBACCO")!(F["PREVIOUS")!(F["EXPOSURE TO")!(F="NEVER USED TOBACCO") Q "2  Not a Current User "_$P(BDMALL(D),U,1)_"  "_$P(BDMALL(D),U,2)
 Q "1  Current User "_$P(BDMALL(D),U,1)_"  "_$P(BDMALL(D),U,2)
 ;
DX(P,BDATE,EDATE) ;EP
 NEW BDMG,T,X,G,Y
 S BDMG=$$LASTDXT^BDMAPIU(P,BDATE,EDATE,"BGP GPRA SMOKING DXS","E")
 I BDMG]"" Q $P($$ICDDX^ICDCODE($P(BDMG,U,4),$P(BDMG,U,1)),U,2)_U_$P(BDMG,U,3)
 S T=$O(^ATXAX("B","BGP GPRA SMOKING DXS",0))
 S X=0,G="" F  S X=$O(^AUPNPROB("AC",P,X)) Q:X'=+X!(G]"")  D
 .Q:$P(^AUPNPROB(X,0),U,12)'="A"
 .Q:$P(^AUPNPROB(X,0),U,3)>EDATE
 .Q:$P(^AUPNPROB(X,0),U,3)<BDATE
 .S Y=$P(^AUPNPROB(X,0),U)
 .Q:'$$ICD^ATXCHK(Y,T,9)
 .S G=$P($$ICDDX^ICDCODE(Y),U,2)_" PROBLEM LIST "_U_$$FMTE^XLFDT($P(^AUPNPROB(X,0),U,3))_U_$P(^AUPNPROB(X,0),U,3)
 .Q
 Q G
TOBACCOS(P,BDATE,EDATE) ;EP
 K BDMTOB,BDM
 K BDMTOB S BDMTOB=$$LASTHF(P,"TOBACCO",BDATE,EDATE) K O,D,H
 Q BDMTOB
 ;
LASTHF(P,C,BDATE,EDATE) ;EP - get last factor in category C for patient P
 S C=$O(^AUTTHF("B",C,0)) ;ien of category passed
 I '$G(C) Q ""
 S (H,D)=0 K O
 F  S H=$O(^AUTTHF("AC",C,H))  Q:'+H  D
 .Q:'$D(^AUPNVHF("AA",P,H))
 .S D="" F  S D=$O(^AUPNVHF("AA",P,H,D)) Q:D'=+D  D
 ..Q:(9999999-D)>EDATE  ;after time frame
 ..Q:(9999999-D)<BDATE  ;before time frame
 ..S Z=$O(^AUPNVHF("AA",P,H,D,0))
 ..S F=$$VAL^XBDIQ1(9000010.23,Z,.01)
 ..I F="SMOKER IN HOME"!(F="SMOKE FREE HOME")!(F["CEREMONIAL")!(F["EXPOSURE TO") Q
 ..S O(D)=$O(^AUPNVHF("AA",P,H,D,""))
 .Q
 S D=$O(O(0))
 ;I D="" Q D
 I D]"" Q $$VAL^XBDIQ1(9000010.23,O(D),.01)_"^"_$$FMTE^XLFDT(9999999-D)_"^"_(9999999-D)
 S (H,D)=0 K O
 F  S H=$O(^AUTTHF("AC",C,H))  Q:'+H  D
 .Q:'$D(^AUPNVHF("AA",P,H))
 .S D="" F  S D=$O(^AUPNVHF("AA",P,H,D)) Q:D'=+D  D
 ..Q:(9999999-D)>EDATE  ;after time frame
 ..Q:(9999999-D)<BDATE  ;before time frame
 ..S Z=$O(^AUPNVHF("AA",P,H,D,0))
 ..S F=$$VAL^XBDIQ1(9000010.23,Z,.01)
 ..I F="SMOKER IN HOME"!(F="SMOKE FREE HOME")!(F["CEREMONIAL")!(F["EXPOSURE TO") S O(D)=$O(^AUPNVHF("AA",P,H,D,""))
 .Q
 S D=$O(O(0))
 Q D
 ;
PED(P,BDATE,EDATE) ;EP
 NEW BDMG,X,Y,T,D,%
 S Y="BDMG("
 S X=P_"^ALL EDUC;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE) S E=$$START1^APCLDF(X,Y)
 I '$D(BGPG) Q ""
 S (X,D)=0,%="",T="" F  S X=$O(BDMG(X)) Q:X'=+X!(%]"")  D
 .S T=$P(^AUPNVPED(+$P(BDMG(X),U,4),0),U)
 .Q:'T
 .Q:'$D(^AUTTEDT(T,0))
 .S T=$P(^AUTTEDT(T,0),U,2)
 .I $P(T,"-")="TO" S %=T_U_$$FMTE^XLFDT($P(BDMG(X),U))_U_$P(BDMG(X),U) Q
 .I $P(T,"-",2)="TO" S %=T_U_$$FMTE^XLFDT($P(BDMG(X),U))_U_$P(BDMG(X),U) Q
 .I $P(T,"-",2)="SHS" S %=T_U_$$FMTE^XLFDT($P(BDMG(X),U))_U_$P(BDMG(X),U) Q
 .I $P(T,"-")="305.1" S %=T_U_$$FMTE^XLFDT($P(BDMG(X),U))_U_$P(BDMG(X),U) Q
 .I $P(T,"-")="305.10" S %=T_U_$$FMTE^XLFDT($P(BDMG(X),U))_U_$P(BDMG(X),U) Q
 .I $P(T,"-")="305.11" S %=T_U_$$FMTE^XLFDT($P(BDMG(X),U))_U_$P(BDMG(X),U) Q
 .I $P(T,"-")="305.12" S %=T_U_$$FMTE^XLFDT($P(BDMG(X),U))_U_$P(BDMG(X),U) Q
 .I $P(T,"-")="305.13" S %=T_U_$$FMTE^XLFDT($P(BDMG(X),U))_U_$P(BDMG(X),U) Q
 .I $P(T,"-")="649.00" S %=T_U_$$FMTE^XLFDT($P(BDMG(X),U))_U_$P(BDMG(X),U) Q
 .I $P(T,"-")="649.01" S %=T_U_$$FMTE^XLFDT($P(BDMG(X),U))_U_$P(BDMG(X),U) Q
 .I $P(T,"-")="649.02" S %=T_U_$$FMTE^XLFDT($P(BDMG(X),U))_U_$P(BDMG(X),U) Q
 .I $P(T,"-")="649.03" S %=T_U_$$FMTE^XLFDT($P(BDMG(X),U))_U_$P(BDMG(X),U) Q
 .I $P(T,"-")="649.04" S %=T_U_$$FMTE^XLFDT($P(BDMG(X),U))_U_$P(BDMG(X),U) Q
 .I $P(T,"-")="V15.82" S %=T_U_$$FMTE^XLFDT($P(BDMG(X),U))_U_$P(BDMG(X),U) Q
 Q %
 ;
DENT(P,BDATE,EDATE) ;EP
 K ^TMP($J,"A")
 NEW A,B,E,X,G,Z
 S A="^TMP($J,""A"",",B=P_"^ALL VISITS;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE),E=$$START1^APCLDF(B,A)
 I '$D(^TMP($J,"A",1)) Q ""
 S (X,G)=0 F  S X=$O(^TMP($J,"A",X)) Q:X'=+X!(G)  S V=$P(^TMP($J,"A",X),U,5) D
 .Q:'$D(^AUPNVSIT(V,0))
 .Q:'$P(^AUPNVSIT(V,0),U,9)
 .Q:$P(^AUPNVSIT(V,0),U,11)
 .S Z=0 F  S Z=$O(^AUPNVDEN("AD",V,Z)) Q:Z'=+Z!(G)  S B=$P($G(^AUPNVDEN(Z,0)),U) I B S B=$P($G(^AUTTADA(B,0)),U) I B=1320 S G=1_U_$P($P(^AUPNVSIT(V,0),U),".")
 .Q
 K ^TMP($J,"A")
 I G=0 Q ""
 Q "ADA 1320"_U_$$FMTE^XLFDT($P(G,U,2))_U_$P(G,U,2)
 ;
CPTSM(P,BDATE,EDATE) ;EP - did pat have smoking cpt?
 NEW X
 S X=$$LASTCPTT^BDMAPIU(P,BDATE,EDATE,$O(^ATXAX("B","BGP SMOKING CPTS",0)),"E")
 I X]"" Q $P(X,U,2)_U_$$FMTE^XLFDT($P(X,U,1))_U_$P(X,U,1)
 Q ""
