BGP6HXG ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON AUG 21, 2005 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;;;BGP6;;AUG 21, 2005
 ;;BGP MUMPS EVIDENCE
 ;
 ; This routine loads Taxonomy BGP MUMPS EVIDENCE
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
 ;;21,"072.0 ")
 ;;1
 ;;9002226,367,.01)
 ;;BGP MUMPS EVIDENCE
 ;;9002226,367,.02)
 ;;@
 ;;9002226,367,.04)
 ;;n
 ;;9002226,367,.06)
 ;;@
 ;;9002226,367,.08)
 ;;0
 ;;9002226,367,.09)
 ;;3040705
 ;;9002226,367,.11)
 ;;@
 ;;9002226,367,.12)
 ;;31
 ;;9002226,367,.13)
 ;;1
 ;;9002226,367,.14)
 ;;@
 ;;9002226,367,.15)
 ;;80
 ;;9002226,367,.16)
 ;;@
 ;;9002226,367,.17)
 ;;@
 ;;9002226,367,3101)
 ;;@
 ;;9002226.02101,"367,072.0 ",.01)
 ;;072.0 
 ;;9002226.02101,"367,072.0 ",.02)
 ;;072.9 
 ;
OTHER ; OTHER ROUTINES
 Q
