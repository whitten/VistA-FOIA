BGP6EXQ ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON AUG 21, 2005 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;;;BGP6;;AUG 21, 2005
 ;;BGP HYPOTENSION DXS
 ;
 ; This routine loads Taxonomy BGP HYPOTENSION DXS
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
 ;;21,"458.0 ")
 ;;1
 ;;9002226,405,.01)
 ;;BGP HYPOTENSION DXS
 ;;9002226,405,.02)
 ;;@
 ;;9002226,405,.04)
 ;;n
 ;;9002226,405,.06)
 ;;@
 ;;9002226,405,.08)
 ;;0
 ;;9002226,405,.09)
 ;;3050321
 ;;9002226,405,.11)
 ;;@
 ;;9002226,405,.12)
 ;;31
 ;;9002226,405,.13)
 ;;1
 ;;9002226,405,.14)
 ;;@
 ;;9002226,405,.15)
 ;;80
 ;;9002226,405,.16)
 ;;@
 ;;9002226,405,.17)
 ;;@
 ;;9002226,405,3101)
 ;;@
 ;;9002226.02101,"405,458.0 ",.01)
 ;;458.0 
 ;;9002226.02101,"405,458.0 ",.02)
 ;;458.9 
 ;
OTHER ; OTHER ROUTINES
 Q
