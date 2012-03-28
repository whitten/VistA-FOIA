BGP8GXV ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON SEP 28, 2007 ;
 ;;8.0;IHS CLINICAL REPORTING;;MAR 12, 2008
 ;;;BGP6;;SEP 28, 2007
 ;;BGP MH OPT VISIT CPT NMH 3
 ;
 ; This routine loads Taxonomy BGP MH OPT VISIT CPT NMH 3
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
 ;;21,"99384 ")
 ;;1
 ;;21,"99394 ")
 ;;2
 ;;9002226,568,.01)
 ;;BGP MH OPT VISIT CPT NMH 3
 ;;9002226,568,.02)
 ;;@
 ;;9002226,568,.04)
 ;;n
 ;;9002226,568,.06)
 ;;@
 ;;9002226,568,.08)
 ;;0
 ;;9002226,568,.09)
 ;;3050803
 ;;9002226,568,.11)
 ;;@
 ;;9002226,568,.12)
 ;;455
 ;;9002226,568,.13)
 ;;1
 ;;9002226,568,.14)
 ;;@
 ;;9002226,568,.15)
 ;;81
 ;;9002226,568,.16)
 ;;@
 ;;9002226,568,.17)
 ;;@
 ;;9002226,568,3101)
 ;;@
 ;;9002226.02101,"568,99384 ",.01)
 ;;99384 
 ;;9002226.02101,"568,99384 ",.02)
 ;;99387 
 ;;9002226.02101,"568,99394 ",.01)
 ;;99394 
 ;;9002226.02101,"568,99394 ",.02)
 ;;99404 
 ;
OTHER ; OTHER ROUTINES
 Q
