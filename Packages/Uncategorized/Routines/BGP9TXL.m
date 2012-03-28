BGP9TXL ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON MAR 25, 2009 ;
 ;;9.0;IHS CLINICAL REPORTING;;JUL 1, 2009
 ;;BGP FOBT CPTS
 ;
 ; This routine loads Taxonomy BGP FOBT CPTS
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
 ;;21,"82270 ")
 ;;1
 ;;21,"82274 ")
 ;;2
 ;;21,"89205 ")
 ;;3
 ;;21,"G0107 ")
 ;;4
 ;;21,"G0328 ")
 ;;5
 ;;21,"G0394 ")
 ;;6
 ;;9002226,881,.01)
 ;;BGP FOBT CPTS
 ;;9002226,881,.02)
 ;;@
 ;;9002226,881,.04)
 ;;n
 ;;9002226,881,.06)
 ;;@
 ;;9002226,881,.08)
 ;;0
 ;;9002226,881,.09)
 ;;3070824
 ;;9002226,881,.11)
 ;;@
 ;;9002226,881,.12)
 ;;455
 ;;9002226,881,.13)
 ;;1
 ;;9002226,881,.14)
 ;;@
 ;;9002226,881,.15)
 ;;81
 ;;9002226,881,.16)
 ;;@
 ;;9002226,881,.17)
 ;;@
 ;;9002226,881,3101)
 ;;@
 ;;9002226.02101,"881,82270 ",.01)
 ;;82270 
 ;;9002226.02101,"881,82270 ",.02)
 ;;82270 
 ;;9002226.02101,"881,82274 ",.01)
 ;;82274 
 ;;9002226.02101,"881,82274 ",.02)
 ;;82274 
 ;;9002226.02101,"881,89205 ",.01)
 ;;89205 
 ;;9002226.02101,"881,89205 ",.02)
 ;;89205 
 ;;9002226.02101,"881,G0107 ",.01)
 ;;G0107 
 ;;9002226.02101,"881,G0107 ",.02)
 ;;G0107 
 ;;9002226.02101,"881,G0328 ",.01)
 ;;G0328 
 ;;9002226.02101,"881,G0328 ",.02)
 ;;G0328 
 ;;9002226.02101,"881,G0394 ",.01)
 ;;G0394 
 ;;9002226.02101,"881,G0394 ",.02)
 ;;G0394 
 ;
OTHER ; OTHER ROUTINES
 Q
