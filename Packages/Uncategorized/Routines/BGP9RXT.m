BGP9RXT ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON MAR 25, 2009 ;
 ;;9.0;IHS CLINICAL REPORTING;;JUL 1, 2009
 ;;BGP ISCHEMIC HEART DXS
 ;
 ; This routine loads Taxonomy BGP ISCHEMIC HEART DXS
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
 ;;21,"410.0 ")
 ;;1
 ;;21,"414.0 ")
 ;;2
 ;;21,"428.0 ")
 ;;3
 ;;21,"429.2 ")
 ;;4
 ;;9002226,316,.01)
 ;;BGP ISCHEMIC HEART DXS
 ;;9002226,316,.02)
 ;;@
 ;;9002226,316,.04)
 ;;n
 ;;9002226,316,.06)
 ;;@
 ;;9002226,316,.08)
 ;;0
 ;;9002226,316,.09)
 ;;3030731
 ;;9002226,316,.11)
 ;;@
 ;;9002226,316,.12)
 ;;31
 ;;9002226,316,.13)
 ;;1
 ;;9002226,316,.14)
 ;;@
 ;;9002226,316,.15)
 ;;80
 ;;9002226,316,.16)
 ;;@
 ;;9002226,316,.17)
 ;;@
 ;;9002226,316,3101)
 ;;@
 ;;9002226.02101,"316,410.0 ",.01)
 ;;410.0 
 ;;9002226.02101,"316,410.0 ",.02)
 ;;412. 
 ;;9002226.02101,"316,414.0 ",.01)
 ;;414.0 
 ;;9002226.02101,"316,414.0 ",.02)
 ;;414.9 
 ;;9002226.02101,"316,428.0 ",.01)
 ;;428.0 
 ;;9002226.02101,"316,428.0 ",.02)
 ;;428.9 
 ;;9002226.02101,"316,429.2 ",.01)
 ;;429.2 
 ;;9002226.02101,"316,429.2 ",.02)
 ;;429.2 
 ;
OTHER ; OTHER ROUTINES
 Q
