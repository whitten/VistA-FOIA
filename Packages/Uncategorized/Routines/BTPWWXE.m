BTPWWXE ;VNGT/HS/ALA-CREATED BY ^ATXSTX ON JAN 14, 2010;
 ;;1.0;CARE MANAGEMENT EVENT TRACKING;;Feb 07, 2011
 ;;BTPW DXA CENTRAL CPTS
 ;
 ; This routine loads Taxonomy BTPW DXA CENTRAL CPTS
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
 ;;21,"77080 ")
 ;;1
 ;;9002226,1345,.01)
 ;;BTPW DXA CENTRAL CPTS
 ;;9002226,1345,.02)
 ;;@
 ;;9002226,1345,.04)
 ;;n
 ;;9002226,1345,.06)
 ;;@
 ;;9002226,1345,.08)
 ;;0
 ;;9002226,1345,.09)
 ;;@
 ;;9002226,1345,.11)
 ;;@
 ;;9002226,1345,.12)
 ;;455
 ;;9002226,1345,.13)
 ;;1
 ;;9002226,1345,.14)
 ;;@
 ;;9002226,1345,.15)
 ;;81
 ;;9002226,1345,.16)
 ;;@
 ;;9002226,1345,.17)
 ;;@
 ;;9002226,1345,3101)
 ;;@
 ;;9002226.02101,"1345,77080 ",.01)
 ;;77080
 ;;9002226.02101,"1345,77080 ",.02)
 ;;77080
 ;
OTHER ; OTHER ROUTINES
 Q
