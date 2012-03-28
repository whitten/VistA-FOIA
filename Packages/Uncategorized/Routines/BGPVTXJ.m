BGPVTXJ ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON APR 21, 2005 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;;;BGPV;;APR 21, 2005
 ;;BGP HIV/AIDS DXS
 ;
 ; This routine loads Taxonomy BGP HIV/AIDS DXS
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
 ;;21,"042. ")
 ;;1
 ;;21,"795.71 ")
 ;;2
 ;;21,"V08. ")
 ;;3
 ;;9002226,315,.01)
 ;;BGP HIV/AIDS DXS
 ;;9002226,315,.02)
 ;;@
 ;;9002226,315,.04)
 ;;n
 ;;9002226,315,.06)
 ;;@
 ;;9002226,315,.08)
 ;;0
 ;;9002226,315,.09)
 ;;3030709
 ;;9002226,315,.11)
 ;;@
 ;;9002226,315,.12)
 ;;31
 ;;9002226,315,.13)
 ;;1
 ;;9002226,315,.14)
 ;;@
 ;;9002226,315,.15)
 ;;80
 ;;9002226,315,.16)
 ;;@
 ;;9002226,315,.17)
 ;;@
 ;;9002226,315,3101)
 ;;@
 ;;9002226.02101,"315,042. ",.01)
 ;;042. 
 ;;9002226.02101,"315,042. ",.02)
 ;;044.9 
 ;;9002226.02101,"315,795.71 ",.01)
 ;;795.71 
 ;;9002226.02101,"315,795.71 ",.02)
 ;;795.71 
 ;;9002226.02101,"315,V08. ",.01)
 ;;V08. 
 ;;9002226.02101,"315,V08. ",.02)
 ;;V08. 
 ;
OTHER ; OTHER ROUTINES
 Q
