BGP8EXG ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON SEP 28, 2007 ;
 ;;8.0;IHS CLINICAL REPORTING;;MAR 12, 2008
 ;;;BGP6;;SEP 28, 2007
 ;;BGP ESRD PROCS
 ;
 ; This routine loads Taxonomy BGP ESRD PROCS
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
 ;;21,"38.95 ")
 ;;1
 ;;21,"39.27 ")
 ;;2
 ;;21,"39.42 ")
 ;;3
 ;;21,"39.53 ")
 ;;4
 ;;21,"39.93 ")
 ;;5
 ;;21,"54.98 ")
 ;;6
 ;;21,"55.61 ")
 ;;7
 ;;9002226,794,.01)
 ;;BGP ESRD PROCS
 ;;9002226,794,.02)
 ;;@
 ;;9002226,794,.04)
 ;;@
 ;;9002226,794,.06)
 ;;@
 ;;9002226,794,.08)
 ;;0
 ;;9002226,794,.09)
 ;;3070704
 ;;9002226,794,.11)
 ;;@
 ;;9002226,794,.12)
 ;;255
 ;;9002226,794,.13)
 ;;1
 ;;9002226,794,.14)
 ;;@
 ;;9002226,794,.15)
 ;;80.1
 ;;9002226,794,.16)
 ;;@
 ;;9002226,794,.17)
 ;;@
 ;;9002226,794,3101)
 ;;@
 ;;9002226.02101,"794,38.95 ",.01)
 ;;38.95 
 ;;9002226.02101,"794,38.95 ",.02)
 ;;38.95 
 ;;9002226.02101,"794,39.27 ",.01)
 ;;39.27 
 ;;9002226.02101,"794,39.27 ",.02)
 ;;39.27 
 ;;9002226.02101,"794,39.42 ",.01)
 ;;39.42 
 ;;9002226.02101,"794,39.42 ",.02)
 ;;39.43 
 ;;9002226.02101,"794,39.53 ",.01)
 ;;39.53 
 ;;9002226.02101,"794,39.53 ",.02)
 ;;39.53 
 ;;9002226.02101,"794,39.93 ",.01)
 ;;39.93 
 ;;9002226.02101,"794,39.93 ",.02)
 ;;39.95 
 ;;9002226.02101,"794,54.98 ",.01)
 ;;54.98 
 ;;9002226.02101,"794,54.98 ",.02)
 ;;54.98 
 ;;9002226.02101,"794,55.61 ",.01)
 ;;55.61 
 ;;9002226.02101,"794,55.61 ",.02)
 ;;55.69 
 ;
OTHER ; OTHER ROUTINES
 Q
