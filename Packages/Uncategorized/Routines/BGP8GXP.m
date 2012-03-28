BGP8GXP ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON SEP 28, 2007 ;
 ;;8.0;IHS CLINICAL REPORTING;;MAR 12, 2008
 ;;;BGP6;;SEP 28, 2007
 ;;BGP MAJOR DEPRESSION (ADM)
 ;
 ; This routine loads Taxonomy BGP MAJOR DEPRESSION (ADM)
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
 ;;21,"296.20 ")
 ;;1
 ;;21,"298.0 ")
 ;;2
 ;;21,"300.4 ")
 ;;3
 ;;21,"309.1 ")
 ;;4
 ;;21,"311. ")
 ;;5
 ;;9002226,564,.01)
 ;;BGP MAJOR DEPRESSION (ADM)
 ;;9002226,564,.02)
 ;;@
 ;;9002226,564,.04)
 ;;n
 ;;9002226,564,.06)
 ;;@
 ;;9002226,564,.08)
 ;;0
 ;;9002226,564,.09)
 ;;3050812
 ;;9002226,564,.11)
 ;;@
 ;;9002226,564,.12)
 ;;31
 ;;9002226,564,.13)
 ;;1
 ;;9002226,564,.14)
 ;;@
 ;;9002226,564,.15)
 ;;80
 ;;9002226,564,.16)
 ;;@
 ;;9002226,564,.17)
 ;;@
 ;;9002226,564,3101)
 ;;@
 ;;9002226.02101,"564,296.20 ",.01)
 ;;296.20 
 ;;9002226.02101,"564,296.20 ",.02)
 ;;296.36 
 ;;9002226.02101,"564,298.0 ",.01)
 ;;298.0 
 ;;9002226.02101,"564,298.0 ",.02)
 ;;298.0 
 ;;9002226.02101,"564,300.4 ",.01)
 ;;300.4 
 ;;9002226.02101,"564,300.4 ",.02)
 ;;300.4 
 ;;9002226.02101,"564,309.1 ",.01)
 ;;309.1 
 ;;9002226.02101,"564,309.1 ",.02)
 ;;309.1 
 ;;9002226.02101,"564,311. ",.01)
 ;;311. 
 ;;9002226.02101,"564,311. ",.02)
 ;;311. 
 ;
OTHER ; OTHER ROUTINES
 Q
