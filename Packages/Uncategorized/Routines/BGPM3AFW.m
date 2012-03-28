BGPM3AFW ;IHS/MSC/MMT-CREATED BY ^ATXSTX ON MAY 22, 2011;
 ;;11.0;IHS CLINICAL REPORTING;**4**;JAN 06, 2011;Build 84
 ;;BGPMU ED MENTAL DISORDERS
 ;
 ; This routine loads Taxonomy BGPMU ED MENTAL DISORDERS
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
 ;;21,"290 ")
 ;;1
 ;;9002226,861,.01)
 ;;BGPMU ED MENTAL DISORDERS
 ;;9002226,861,.02)
 ;;ICD9s for mental disorders
 ;;9002226,861,.04)
 ;;@
 ;;9002226,861,.06)
 ;;@
 ;;9002226,861,.08)
 ;;@
 ;;9002226,861,.09)
 ;;@
 ;;9002226,861,.11)
 ;;@
 ;;9002226,861,.12)
 ;;@
 ;;9002226,861,.13)
 ;;@
 ;;9002226,861,.14)
 ;;@
 ;;9002226,861,.15)
 ;;80
 ;;9002226,861,.16)
 ;;@
 ;;9002226,861,.17)
 ;;@
 ;;9002226,861,3101)
 ;;@
 ;;9002226.02101,"861,290 ",.01)
 ;;290
 ;;9002226.02101,"861,290 ",.02)
 ;;319
 ;
OTHER ; OTHER ROUTINES
 Q
