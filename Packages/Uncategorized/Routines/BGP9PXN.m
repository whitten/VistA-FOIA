BGP9PXN ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON MAR 25, 2009 ;
 ;;9.0;IHS CLINICAL REPORTING;;JUL 1, 2009
 ;;BGP SUBSTANCE ABUSE
 ;
 ; This routine loads Taxonomy BGP SUBSTANCE ABUSE
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
 ;;21,"291.0 ")
 ;;1
 ;;21,"303.00 ")
 ;;2
 ;;9002226,527,.01)
 ;;BGP SUBSTANCE ABUSE
 ;;9002226,527,.02)
 ;;@
 ;;9002226,527,.04)
 ;;n
 ;;9002226,527,.06)
 ;;@
 ;;9002226,527,.08)
 ;;0
 ;;9002226,527,.09)
 ;;3050802
 ;;9002226,527,.11)
 ;;@
 ;;9002226,527,.12)
 ;;31
 ;;9002226,527,.13)
 ;;1
 ;;9002226,527,.14)
 ;;@
 ;;9002226,527,.15)
 ;;80
 ;;9002226,527,.16)
 ;;@
 ;;9002226,527,.17)
 ;;@
 ;;9002226,527,3101)
 ;;@
 ;;9002226.02101,"527,291.0 ",.01)
 ;;291.0 
 ;;9002226.02101,"527,291.0 ",.02)
 ;;292.9 
 ;;9002226.02101,"527,303.00 ",.01)
 ;;303.00 
 ;;9002226.02101,"527,303.00 ",.02)
 ;;305.99 
 ;
OTHER ; OTHER ROUTINES
 Q
