APCLP71X ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON MAR 30, 2000 ;
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
 ;;;APCL;;MAR 30, 2000
 ;;DM AUDIT PROBLEM DIABETES DX
 ;
 ; This routine loads Taxonomy DM AUDIT PROBLEM DIABETES DX
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
 ;;21,"250.00 ")
 ;;1
 ;;9002226,238,.01)
 ;;DM AUDIT PROBLEM DIABETES DX
 ;;9002226,238,.02)
 ;;DM AUDIT PROBLEM DIABETES DX
 ;;9002226,238,.04)
 ;;@
 ;;9002226,238,.06)
 ;;@
 ;;9002226,238,.08)
 ;;0
 ;;9002226,238,.09)
 ;;2930811
 ;;9002226,238,.11)
 ;;@
 ;;9002226,238,.12)
 ;;266
 ;;9002226,238,.13)
 ;;1
 ;;9002226,238,.14)
 ;;@
 ;;9002226,238,.15)
 ;;80
 ;;9002226,238,.16)
 ;;@
 ;;9002226,238,.17)
 ;;@
 ;;9002226,238,3101)
 ;;@
 ;;9002226.02101,"238,250.00 ",.01)
 ;;250.00
 ;;9002226.02101,"238,250.00 ",.02)
 ;;250.93
 ;
OTHER ; OTHER ROUTINES
 Q
