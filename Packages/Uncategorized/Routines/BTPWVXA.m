BTPWVXA ;VNGT/HS/ALA-CREATED BY ^ATXSTX ON JAN 14, 2010;
 ;;1.0;CARE MANAGEMENT EVENT TRACKING;;Feb 07, 2011
 ;;BTPW AX NODE DISECT CPTS
 ;
 ; This routine loads Taxonomy BTPW AX NODE DISECT CPTS
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
 ;;21,"38525 ")
 ;;1
 ;;21,"38740 ")
 ;;2
 ;;21,"38745 ")
 ;;3
 ;;9002226,1254,.01)
 ;;BTPW AX NODE DISECT CPTS
 ;;9002226,1254,.02)
 ;;@
 ;;9002226,1254,.04)
 ;;n
 ;;9002226,1254,.06)
 ;;@
 ;;9002226,1254,.08)
 ;;0
 ;;9002226,1254,.09)
 ;;3061103
 ;;9002226,1254,.11)
 ;;@
 ;;9002226,1254,.12)
 ;;455
 ;;9002226,1254,.13)
 ;;1
 ;;9002226,1254,.14)
 ;;@
 ;;9002226,1254,.15)
 ;;81
 ;;9002226,1254,.16)
 ;;@
 ;;9002226,1254,.17)
 ;;@
 ;;9002226,1254,3101)
 ;;@
 ;;9002226.02101,"1254,38525 ",.01)
 ;;38525
 ;;9002226.02101,"1254,38525 ",.02)
 ;;38525
 ;;9002226.02101,"1254,38740 ",.01)
 ;;38740
 ;;9002226.02101,"1254,38740 ",.02)
 ;;38740
 ;;9002226.02101,"1254,38745 ",.01)
 ;;38745
 ;;9002226.02101,"1254,38745 ",.02)
 ;;38745
 ;
OTHER ; OTHER ROUTINES
 Q
