BQIBTXC ;VNGT/HS/ALA-CREATED BY ^ATXSTX ON JUN 18, 2008;
 ;;2.1;ICARE MANAGEMENT SYSTEM;;Feb 07, 2011
 ;;BGP TOBACCO USER CPTS
 ;
 ; This routine loads Taxonomy BGP TOBACCO USER CPTS
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
 ;;21,"1034F ")
 ;;1
 ;;21,"1035F ")
 ;;2
 ;;9002226,1070,.01)
 ;;BGP TOBACCO USER CPTS
 ;;9002226,1070,.02)
 ;;Tobacco User
 ;;9002226,1070,.04)
 ;;n
 ;;9002226,1070,.06)
 ;;@
 ;;9002226,1070,.08)
 ;;0
 ;;9002226,1070,.09)
 ;;3080411
 ;;9002226,1070,.11)
 ;;@
 ;;9002226,1070,.12)
 ;;455
 ;;9002226,1070,.13)
 ;;0
 ;;9002226,1070,.14)
 ;;@
 ;;9002226,1070,.15)
 ;;81
 ;;9002226,1070,.16)
 ;;@
 ;;9002226,1070,.17)
 ;;@
 ;;9002226,1070,3101)
 ;;@
 ;;9002226.02101,"1070,1034F ",.01)
 ;;1034F
 ;;9002226.02101,"1070,1034F ",.02)
 ;;1034F
 ;;9002226.02101,"1070,1035F ",.01)
 ;;1035F
 ;;9002226.02101,"1070,1035F ",.02)
 ;;1035F
 ;
OTHER ; OTHER ROUTINES
 Q
