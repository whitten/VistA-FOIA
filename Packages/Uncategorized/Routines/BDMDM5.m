BDMDM5 ; IHS/CMI/LAB - DM AUDIT ;
 ;;2.0;DIABETES MANAGEMENT SYSTEM;**2**;JUN 14, 2007
 ;
 ;
EN ;ENTRY POINT FROM BDMDM1
 D HEADER
 D IDENT
 Q
HEADER ; Set node with report header info
 S BDMAREA=$P(^AUTTAREA($P(^AUTTLOC(DUZ(2),0),U,4),0),U) ;_" - "_$P(^(0),U)
 S BDMSU=$P(^AUTTSU($P(^AUTTLOC(DUZ(2),0),U,5),0),U)
 S BDMFAC=$P(^DIC(4,DUZ(2),0),U)
 I '$D(BDMFISC) D
 . S BDMDTE=BDMBDT_" - "_BDMEDT
 S (^TMP("BDM",$J,1000),^TMP("BDMCUML",$J,1000))=BDMTDTE
 S (^TMP("BDM",$J,1001),^TMP("BDMCUML",$J,1001))=$S($D(BDMDTE):BDMDTE,1:BDMFISC)
 S (^TMP("BDM",$J,1002),^TMP("BDMCUML",$J,1002))=BDMAREA
 S (^TMP("BDM",$J,1003),^TMP("BDMCUML",$J,1003))=BDMSU
 S (^TMP("BDM",$J,1004),^TMP("BDMCUML",$J,1004))=BDMFAC
 S (^TMP("BDM",$J,1005),^TMP("BDMCUML",$J,1005))=$P(^VA(200,DUZ,0),U)
 S ^TMP("BDM",$J,42)=$$FMTE^XLFDT(BDMED)
 S BDMUED=$S(BDMED>DT:DT,1:BDMED)
 Q
 ;
IDENT ; Pt identifying factors
 I BDMCUML S ^(1)=$G(^TMP("BDMCUML",$J,1))+1
 S Y=$P(^DPT(BDMPD,0),U,3) D DD^%DT
 S ^TMP("BDM",$J,500)=$P($G(^AUPNPAT(BDMPD,41,DUZ(2),0)),U,2)
 S ^TMP("BDM",$J,501)=Y
 S (^TMP("BDM",$J,502),BDMSEX)=$P(^DPT(BDMPD,0),U,2)
 S ^TMP("BDM",$J,504)=$P(^DPT(BDMPD,0),U)
 I BDMCUML D  S BDMSUB=2 D CUML
 . I BDMSEX="F" S BDMGOT1=1
 . E  S BDMGOT1=0
 S (BDMAGE,^TMP("BDM",$J,503))=(BDMED-$P(^DPT(BDMPD,0),U,3))\10000 I BDMCUML D
 . I BDMAGE<15 S BDMGOT1=1,BDMSUB=50 D CUML F BDMSUB=51,52,53 S BDMGOT1=0 D CUML
 . I BDMAGE>14&(BDMAGE<45) S BDMGOT1=1,BDMSUB=51 D CUML F BDMSUB=50,52,53 S BDMGOT1=0 D CUML
 . I BDMAGE>44&(BDMAGE<65) S BDMGOT1=1,BDMSUB=52 D CUML F BDMSUB=50,51,53 S BDMGOT1=0 D CUML
 . I BDMAGE>64 S BDMGOT1=1,BDMSUB=53 D CUML F BDMSUB=50,51,52 S BDMGOT1=0 D CUML
 K BDMAGE
 Q
 ;
CUML ; - ENTRY POINT - Set cumulative nodes
 I '$D(^TMP("BDMCUML",$J,BDMSUB)) S ^TMP("BDMCUML",$J,BDMSUB)=BDMGOT1_"/"_1
 E  S ^(BDMSUB)=$S(BDMGOT1:$P(^TMP("BDMCUML",$J,BDMSUB),"/")+1,1:$P(^TMP("BDMCUML",$J,BDMSUB),"/"))_"/"_($P(^(BDMSUB),"/",2)+1)
 Q
 ;
CLEAN ;EP
 K ^TMP("BDMDM FETCH",$J),^TMP("BDMDM DXVS",$J),^TMP("BDMDM VST",$J)
 K BDMDX,BDMVST,BDMDXVS,BDMHT,BDMMEAS,BDML,BDMTOT,BDMMDFN,BDMVDFN,BDMYES,BDMPOD,BDMCL1,BDMCL2,BDMX,BDMY,BDMEYE1,BDMEYE2,BDMPCL1,BDMPCL2,BDMPCL3,BDMPRD,BDMPRV,BDMTD,BDMFDX
 K BDMDAYS,BDMDP,BDMHTK1
 K BDMAREA,BDMSU,BDMFAC,BDMDTE,BDMI,BDMSEX,BDMSUB,BDMGOT1,BDMER,BDMERTX,BDMHTNE,BDMDOO,BDMLL,BDMPCL
 K BDMRTYP,BDMVMED,BDMW
 Q
 ;
