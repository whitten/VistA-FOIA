APCLTXL ; IHS/OHPRD/TMJ -CREATED BY ^ATXSTX ON JAN 09, 1997 ;
 ;;3.0;IHS PCC REPORTS;;FEB 05, 1997
 ;;APCL INJ FIREARMS
 ;
 ; This routine loads Taxonomy APCL INJ FIREARMS
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
 ;;21,"E922.0 ")
 ;;1
 ;;21,"E970. ")
 ;;2
 ;;21,"E985.0 ")
 ;;3
 ;;9002226,153,.01)
 ;;APCL INJ FIREARMS
 ;;9002226,153,.02)
 ;;APCL FIREARM ECODES
 ;;9002226,153,.04)
 ;;@
 ;;9002226,153,.06)
 ;;@
 ;;9002226,153,.08)
 ;;1
 ;;9002226,153,.09)
 ;;2961021.135813
 ;;9002226,153,.11)
 ;;@
 ;;9002226,153,.12)
 ;;157
 ;;9002226,153,.13)
 ;;1
 ;;9002226,153,.14)
 ;;BA
 ;;9002226,153,.15)
 ;;80
 ;;9002226,153,.16)
 ;;1
 ;;9002226,153,.17)
 ;;@
 ;;9002226,153,3101)
 ;;@
 ;;9002226.02101,"153,E922.0 ",.01)
 ;;E922.0
 ;;9002226.02101,"153,E922.0 ",.02)
 ;;E922.9
 ;;9002226.02101,"153,E970. ",.01)
 ;;E970.
 ;;9002226.02101,"153,E970. ",.02)
 ;;E970.
 ;;9002226.02101,"153,E985.0 ",.01)
 ;;E985.0
 ;;9002226.02101,"153,E985.0 ",.02)
 ;;E985.4
 ;
OTHER ; OTHER ROUTINES
 Q
