BGP6FXA ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON AUG 21, 2005 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;;;BGP6;;AUG 21, 2005
 ;;BGP TOTAL CHOLESTEROL LOINC
 ;
 ; This routine loads Taxonomy BGP TOTAL CHOLESTEROL LOINC
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
 ;;21,"2093-3 ")
 ;;1
 ;;21,"5932-9 ")
 ;;2
 ;;9002226,361,.01)
 ;;BGP TOTAL CHOLESTEROL LOINC
 ;;9002226,361,.02)
 ;;@
 ;;9002226,361,.04)
 ;;n
 ;;9002226,361,.06)
 ;;@
 ;;9002226,361,.08)
 ;;@
 ;;9002226,361,.09)
 ;;@
 ;;9002226,361,.11)
 ;;@
 ;;9002226,361,.12)
 ;;@
 ;;9002226,361,.13)
 ;;1
 ;;9002226,361,.14)
 ;;FIHS
 ;;9002226,361,.15)
 ;;95.3
 ;;9002226,361,.16)
 ;;@
 ;;9002226,361,.17)
 ;;@
 ;;9002226,361,3101)
 ;;@
 ;;9002226.02101,"361,2093-3 ",.01)
 ;;2093-3
 ;;9002226.02101,"361,2093-3 ",.02)
 ;;2093-3
 ;;9002226.02101,"361,5932-9 ",.01)
 ;;5932-9
 ;;9002226.02101,"361,5932-9 ",.02)
 ;;5932-9
 ;
OTHER ; OTHER ROUTINES
 Q
