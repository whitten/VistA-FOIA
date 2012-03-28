BGPTXV ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON APR 21, 2005 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;;BGP CMS ANTI-PLATELET CLASS
 ;
 ; This routine loads Taxonomy BGP CMS ANTI-PLATELET CLASS
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
 ;;21,"BL700 ")
 ;;1
 ;;9002226,339,.01)
 ;;BGP CMS ANTI-PLATELET CLASS
 ;;9002226,339,.02)
 ;;@
 ;;9002226,339,.04)
 ;;@
 ;;9002226,339,.06)
 ;;@
 ;;9002226,339,.08)
 ;;@
 ;;9002226,339,.09)
 ;;@
 ;;9002226,339,.11)
 ;;@
 ;;9002226,339,.12)
 ;;@
 ;;9002226,339,.13)
 ;;1
 ;;9002226,339,.14)
 ;;@
 ;;9002226,339,.15)
 ;;@
 ;;9002226,339,.16)
 ;;@
 ;;9002226,339,.17)
 ;;@
 ;;9002226,339,3101)
 ;;@
 ;;9002226.02101,"339,BL700 ",.01)
 ;;BL700
 ;;9002226.02101,"339,BL700 ",.02)
 ;;BL700
 ;
OTHER ; OTHER ROUTINES
 Q
