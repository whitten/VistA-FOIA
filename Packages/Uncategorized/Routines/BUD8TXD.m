BUD8TXD ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON DEC 16, 2007 ;
 ;;5.0;IHS/RPMS UNIFORM DATA SYSTEM;;JAN 18, 2011;Build 12
 ;;BUD CPT PAP 05
 ;
 ; This routine loads Taxonomy BUD CPT PAP 05
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
 ;;21,"88141 ")
 ;;1
 ;;21,"88160 ")
 ;;2
 ;;21,"88161 ")
 ;;3
 ;;21,"88162 ")
 ;;4
 ;;21,"88174 ")
 ;;5
 ;;9002226,520,.01)
 ;;BUD CPT PAP 05
 ;;9002226,520,.02)
 ;;@
 ;;9002226,520,.04)
 ;;n
 ;;9002226,520,.06)
 ;;@
 ;;9002226,520,.08)
 ;;0
 ;;9002226,520,.09)
 ;;3060116
 ;;9002226,520,.11)
 ;;@
 ;;9002226,520,.12)
 ;;455
 ;;9002226,520,.13)
 ;;1
 ;;9002226,520,.14)
 ;;@
 ;;9002226,520,.15)
 ;;81
 ;;9002226,520,.16)
 ;;@
 ;;9002226,520,.17)
 ;;@
 ;;9002226,520,3101)
 ;;@
 ;;9002226.02101,"520,88141 ",.01)
 ;;88141 
 ;;9002226.02101,"520,88141 ",.02)
 ;;88155 
 ;;9002226.02101,"520,88160 ",.01)
 ;;88160 
 ;;9002226.02101,"520,88160 ",.02)
 ;;88160 
 ;;9002226.02101,"520,88161 ",.01)
 ;;88161 
 ;;9002226.02101,"520,88161 ",.02)
 ;;88161 
 ;;9002226.02101,"520,88162 ",.01)
 ;;88162 
 ;;9002226.02101,"520,88162 ",.02)
 ;;88167 
 ;;9002226.02101,"520,88174 ",.01)
 ;;88174 
 ;;9002226.02101,"520,88174 ",.02)
 ;;88175 
 ;
OTHER ; OTHER ROUTINES
 Q
