BGP8CXX ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON SEP 28, 2007 ;
 ;;8.0;IHS CLINICAL REPORTING;;MAR 12, 2008
 ;;;BGP6;;SEP 28, 2007
 ;;BGP CMS PACEMAKER PROCS
 ;
 ; This routine loads Taxonomy BGP CMS PACEMAKER PROCS
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
 ;;21,"00.50 ")
 ;;1
 ;;9002226,676,.01)
 ;;BGP CMS PACEMAKER PROCS
 ;;9002226,676,.02)
 ;;@
 ;;9002226,676,.04)
 ;;@
 ;;9002226,676,.06)
 ;;@
 ;;9002226,676,.08)
 ;;0
 ;;9002226,676,.09)
 ;;3070525
 ;;9002226,676,.11)
 ;;@
 ;;9002226,676,.12)
 ;;255
 ;;9002226,676,.13)
 ;;1
 ;;9002226,676,.14)
 ;;@
 ;;9002226,676,.15)
 ;;80.1
 ;;9002226,676,.16)
 ;;@
 ;;9002226,676,.17)
 ;;@
 ;;9002226,676,3101)
 ;;@
 ;;9002226.02101,"676,00.50 ",.01)
 ;;00.50 
 ;;9002226.02101,"676,00.50 ",.02)
 ;;00.54 
 ;
OTHER ; OTHER ROUTINES
 Q
