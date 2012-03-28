BGP82TT ;IHS/CMI/LAB-CREATED BY ^ATXSTX ON JUN 26, 2008;
 ;;8.0;IHS CLINICAL REPORTING;**2**;MAR 12, 2008
 ;;BGP COLO PROCS
 ;
 ; This routine loads Taxonomy BGP COLO PROCS
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
 ;;21,"45.22 ")
 ;;1
 ;;21,"45.25 ")
 ;;2
 ;;21,"45.43 ")
 ;;3
 ;;9002226,798,.01)
 ;;BGP COLO PROCS
 ;;9002226,798,.02)
 ;;@
 ;;9002226,798,.04)
 ;;@
 ;;9002226,798,.06)
 ;;@
 ;;9002226,798,.08)
 ;;@
 ;;9002226,798,.09)
 ;;3080626
 ;;9002226,798,.11)
 ;;@
 ;;9002226,798,.12)
 ;;255
 ;;9002226,798,.13)
 ;;1
 ;;9002226,798,.14)
 ;;@
 ;;9002226,798,.15)
 ;;80.1
 ;;9002226,798,.16)
 ;;@
 ;;9002226,798,.17)
 ;;@
 ;;9002226,798,3101)
 ;;@
 ;;9002226.02101,"798,45.22 ",.01)
 ;;45.22 
 ;;9002226.02101,"798,45.22 ",.02)
 ;;45.23 
 ;;9002226.02101,"798,45.25 ",.01)
 ;;45.25 
 ;;9002226.02101,"798,45.25 ",.02)
 ;;45.25 
 ;;9002226.02101,"798,45.43 ",.01)
 ;;45.43 
 ;;9002226.02101,"798,45.43 ",.02)
 ;;45.43 
 ;
OTHER ; OTHER ROUTINES
 Q
