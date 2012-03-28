BGPM5BA ;IHS/MSC/MMT-CREATED BY ^ATXSTX ON SEP 07, 2011;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;;BGPMU LAB LOINC GLEASON TEST
 ;
 ; This routine loads Taxonomy BGPMU LAB LOINC GLEASON TEST
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
 ;;21,"35266-6 ")
 ;;1
 ;;9002226,1065,.01)
 ;;BGPMU LAB LOINC GLEASON TEST
 ;;9002226,1065,.02)
 ;;LAB LOINC GLEASON TEST
 ;;9002226,1065,.04)
 ;;@
 ;;9002226,1065,.06)
 ;;@
 ;;9002226,1065,.08)
 ;;@
 ;;9002226,1065,.09)
 ;;@
 ;;9002226,1065,.11)
 ;;@
 ;;9002226,1065,.12)
 ;;@
 ;;9002226,1065,.13)
 ;;@
 ;;9002226,1065,.14)
 ;;@
 ;;9002226,1065,.15)
 ;;95.3
 ;;9002226,1065,.16)
 ;;@
 ;;9002226,1065,.17)
 ;;@
 ;;9002226,1065,3101)
 ;;@
 ;;9002226.02101,"1065,35266-6 ",.01)
 ;;35266-6
 ;;9002226.02101,"1065,35266-6 ",.02)
 ;;35266-6
 ;
OTHER ; OTHER ROUTINES
 Q
