BGP6HXA ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON AUG 21, 2005 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;;;BGP6;;AUG 21, 2005
 ;;BGP MH OPT VISIT CPT NMH 1
 ;
 ; This routine loads Taxonomy BGP MH OPT VISIT CPT NMH 1
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
 ;;21,"90801 ")
 ;;1
 ;;21,"90821 ")
 ;;2
 ;;21,"90826 ")
 ;;3
 ;;21,"90845 ")
 ;;4
 ;;21,"90847 ")
 ;;5
 ;;21,"90857 ")
 ;;6
 ;;21,"90870 ")
 ;;7
 ;;9002226,448,.01)
 ;;BGP MH OPT VISIT CPT NMH 1
 ;;9002226,448,.02)
 ;;@
 ;;9002226,448,.04)
 ;;@
 ;;9002226,448,.06)
 ;;@
 ;;9002226,448,.08)
 ;;0
 ;;9002226,448,.09)
 ;;3050803
 ;;9002226,448,.11)
 ;;@
 ;;9002226,448,.12)
 ;;455
 ;;9002226,448,.13)
 ;;1
 ;;9002226,448,.14)
 ;;@
 ;;9002226,448,.15)
 ;;81
 ;;9002226,448,.16)
 ;;@
 ;;9002226,448,.17)
 ;;@
 ;;9002226,448,3101)
 ;;@
 ;;9002226.02101,"448,90801 ",.01)
 ;;90801 
 ;;9002226.02101,"448,90801 ",.02)
 ;;90819 
 ;;9002226.02101,"448,90821 ",.01)
 ;;90821 
 ;;9002226.02101,"448,90821 ",.02)
 ;;90824 
 ;;9002226.02101,"448,90826 ",.01)
 ;;90826 
 ;;9002226.02101,"448,90826 ",.02)
 ;;90829 
 ;;9002226.02101,"448,90845 ",.01)
 ;;90845 
 ;;9002226.02101,"448,90845 ",.02)
 ;;90845 
 ;;9002226.02101,"448,90847 ",.01)
 ;;90847 
 ;;9002226.02101,"448,90847 ",.02)
 ;;90853 
 ;;9002226.02101,"448,90857 ",.01)
 ;;90857 
 ;;9002226.02101,"448,90857 ",.02)
 ;;90862 
 ;;9002226.02101,"448,90870 ",.01)
 ;;90870 
 ;;9002226.02101,"448,90870 ",.02)
 ;;90876 
 ;
OTHER ; OTHER ROUTINES
 Q
