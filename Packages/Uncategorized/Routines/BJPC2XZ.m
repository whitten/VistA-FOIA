BJPC2XZ ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON JUN 23, 2008 ;
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
 ;;APCL INJ UNDETERMINED
 ;
 ; This routine loads Taxonomy APCL INJ UNDETERMINED
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
 ;;21,"E988.8 ")
 ;;1
 ;;9002226,221,.01)
 ;;APCL INJ UNDETERMINED
 ;;9002226,221,.02)
 ;;APCL UNDETERMINED ECODES
 ;;9002226,221,.04)
 ;;n
 ;;9002226,221,.06)
 ;;@
 ;;9002226,221,.08)
 ;;1
 ;;9002226,221,.09)
 ;;2961021.140348
 ;;9002226,221,.11)
 ;;@
 ;;9002226,221,.12)
 ;;157
 ;;9002226,221,.13)
 ;;1
 ;;9002226,221,.14)
 ;;BA
 ;;9002226,221,.15)
 ;;80
 ;;9002226,221,.16)
 ;;1
 ;;9002226,221,.17)
 ;;@
 ;;9002226,221,3101)
 ;;@
 ;;9002226.02101,"221,E988.8 ",.01)
 ;;E988.8
 ;;9002226.02101,"221,E988.8 ",.02)
 ;;E988.9
 ;
OTHER ; OTHER ROUTINES
 Q
