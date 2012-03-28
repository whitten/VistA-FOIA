BJPCTXA ;IHS/CMI/LAB-CREATED BY ^ATXSTX ON FEB 07, 2008;
 ;;1.0;IHS PCC SUITE;**1**;MAR 14, 2008
 ;;BGP PAP SMEAR DXS
 ;
 ; This routine loads Taxonomy BGP PAP SMEAR DXS
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 D OTHER
 I $O(^TMP("ATX",$J,3.6,0)) D BULL^ATXSTX2
 I $O(^TMP("ATX",$J,9002226,0)) D TAX^ATXSTX2
 D KILL^ATXSTX2
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;21,"795.0 ")
 ;;1
 ;;21,"V67.01 ")
 ;;2
 ;;21,"V72.3 ")
 ;;3
 ;;21,"V72.32 ")
 ;;4
 ;;21,"V76.2 ")
 ;;5
 ;;21,"V76.47 ")
 ;;6
 ;;9002226,663,.01)
 ;;BGP PAP SMEAR DXS
 ;;9002226,663,.02)
 ;;@
 ;;9002226,663,.04)
 ;;n
 ;;9002226,663,.06)
 ;;@
 ;;9002226,663,.08)
 ;;0
 ;;9002226,663,.09)
 ;;3070707
 ;;9002226,663,.11)
 ;;@
 ;;9002226,663,.12)
 ;;31
 ;;9002226,663,.13)
 ;;1
 ;;9002226,663,.14)
 ;;@
 ;;9002226,663,.15)
 ;;80
 ;;9002226,663,.16)
 ;;@
 ;;9002226,663,.17)
 ;;@
 ;;9002226,663,3101)
 ;;@
 ;;9002226.02101,"663,795.0 ",.01)
 ;;795.0 
 ;;9002226.02101,"663,795.0 ",.02)
 ;;795.09 
 ;;9002226.02101,"663,V67.01 ",.01)
 ;;V67.01 
 ;;9002226.02101,"663,V67.01 ",.02)
 ;;V67.01 
 ;;9002226.02101,"663,V72.3 ",.01)
 ;;V72.3 
 ;;9002226.02101,"663,V72.3 ",.02)
 ;;V72.31 
 ;;9002226.02101,"663,V72.32 ",.01)
 ;;V72.32 
 ;;9002226.02101,"663,V72.32 ",.02)
 ;;V72.32 
 ;;9002226.02101,"663,V76.2 ",.01)
 ;;V76.2 
 ;;9002226.02101,"663,V76.2 ",.02)
 ;;V76.2 
 ;;9002226.02101,"663,V76.47 ",.01)
 ;;V76.47 
 ;;9002226.02101,"663,V76.47 ",.02)
 ;;V76.47 
 ;
OTHER ; OTHER ROUTINES
 Q
