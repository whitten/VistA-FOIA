BGP6GXM ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON JAN 02, 2006 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;;;BGP6;;JAN 02, 2006
 ;;BGP URI DXS
 ;
 ; This routine loads Taxonomy BGP URI DXS
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
 ;;21,"460. ")
 ;;1
 ;;21,"465.0 ")
 ;;2
 ;;9002226,380,.01)
 ;;BGP URI DXS
 ;;9002226,380,.02)
 ;;@
 ;;9002226,380,.04)
 ;;@
 ;;9002226,380,.06)
 ;;@
 ;;9002226,380,.08)
 ;;0
 ;;9002226,380,.09)
 ;;3060102
 ;;9002226,380,.11)
 ;;@
 ;;9002226,380,.12)
 ;;31
 ;;9002226,380,.13)
 ;;1
 ;;9002226,380,.14)
 ;;@
 ;;9002226,380,.15)
 ;;80
 ;;9002226,380,.16)
 ;;@
 ;;9002226,380,.17)
 ;;@
 ;;9002226,380,3101)
 ;;@
 ;;9002226.02101,"380,460. ",.01)
 ;;460. 
 ;;9002226.02101,"380,460. ",.02)
 ;;460. 
 ;;9002226.02101,"380,465.0 ",.01)
 ;;465.0 
 ;;9002226.02101,"380,465.0 ",.02)
 ;;465.9 
 ;
OTHER ; OTHER ROUTINES
 Q
