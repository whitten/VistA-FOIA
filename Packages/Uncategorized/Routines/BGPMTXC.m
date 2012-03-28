BGPMTXC ;IHS/MSC/MMT-CREATED BY ^ATXSTX ON FEB 02, 2011;
 ;;11.0;IHS CLINICAL REPORTING;**4**;JAN 06, 2011;Build 84
 ;;BGPMU BMI FOLLOWUP CPTS
 ;
 ; This routine loads Taxonomy BGPMU BMI FOLLOWUP CPTS
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
 ;;21,"43644 ")
 ;;1
 ;;21,"43770 ")
 ;;2
 ;;21,"43842 ")
 ;;3
 ;;21,"97804 ")
 ;;4
 ;;21,"98961 ")
 ;;5
 ;;21,"99078 ")
 ;;6
 ;;21,"G8417 ")
 ;;7
 ;;21,"S9449 ")
 ;;10
 ;;21,"S9451 ")
 ;;8
 ;;21,"S9470 ")
 ;;9
 ;;9002226,711,.01)
 ;;BGPMU BMI FOLLOWUP CPTS
 ;;9002226,711,.02)
 ;;CPTS for followup
 ;;9002226,711,.04)
 ;;n
 ;;9002226,711,.06)
 ;;@
 ;;9002226,711,.08)
 ;;@
 ;;9002226,711,.09)
 ;;3101222
 ;;9002226,711,.11)
 ;;@
 ;;9002226,711,.12)
 ;;@
 ;;9002226,711,.13)
 ;;@
 ;;9002226,711,.14)
 ;;@
 ;;9002226,711,.15)
 ;;81
 ;;9002226,711,.16)
 ;;1
 ;;9002226,711,.17)
 ;;@
 ;;9002226,711,3101)
 ;;@
 ;;9002226.02101,"711,43644 ",.01)
 ;;43644
 ;;9002226.02101,"711,43644 ",.02)
 ;;43645
 ;;9002226.02101,"711,43770 ",.01)
 ;;43770
 ;;9002226.02101,"711,43770 ",.02)
 ;;43774
 ;;9002226.02101,"711,43842 ",.01)
 ;;43842
 ;;9002226.02101,"711,43842 ",.02)
 ;;43848
 ;;9002226.02101,"711,97804 ",.01)
 ;;97804
 ;;9002226.02101,"711,97804 ",.02)
 ;;97804
 ;;9002226.02101,"711,98961 ",.01)
 ;;98961
 ;;9002226.02101,"711,98961 ",.02)
 ;;98962
 ;;9002226.02101,"711,99078 ",.01)
 ;;99078
 ;;9002226.02101,"711,99078 ",.02)
 ;;99078
 ;;9002226.02101,"711,G8417 ",.01)
 ;;G8417
 ;;9002226.02101,"711,G8417 ",.02)
 ;;G8417
 ;;9002226.02101,"711,S9449 ",.01)
 ;;S9449
 ;;9002226.02101,"711,S9449 ",.02)
 ;;S9449
 ;;9002226.02101,"711,S9451 ",.01)
 ;;S9451
 ;;9002226.02101,"711,S9451 ",.02)
 ;;S9452
 ;;9002226.02101,"711,S9470 ",.01)
 ;;S9470
 ;;9002226.02101,"711,S9470 ",.02)
 ;;S9470
 ;
OTHER ; OTHER ROUTINES
 Q
