BGPM5BAE ;IHS/MSC/SAT-CREATED BY ^ATXSTX ON SEP 09, 2011;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;;BGPMU STAGE III COLON CAN CPT
 ;
 ; This routine loads Taxonomy BGPMU STAGE III COLON CAN CPT
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
 ;;21,"3388F ")
 ;;1
 ;;9002226,1075,.01)
 ;;BGPMU STAGE III COLON CAN CPT
 ;;9002226,1075,.02)
 ;;CPT FOR STAGE III COLON CANCER
 ;;9002226,1075,.04)
 ;;@
 ;;9002226,1075,.06)
 ;;@
 ;;9002226,1075,.08)
 ;;@
 ;;9002226,1075,.09)
 ;;@
 ;;9002226,1075,.11)
 ;;@
 ;;9002226,1075,.12)
 ;;@
 ;;9002226,1075,.13)
 ;;@
 ;;9002226,1075,.14)
 ;;@
 ;;9002226,1075,.15)
 ;;81
 ;;9002226,1075,.16)
 ;;1
 ;;9002226,1075,.17)
 ;;@
 ;;9002226,1075,3101)
 ;;@
 ;;9002226.02101,"1075,3388F ",.01)
 ;;3388F
 ;;9002226.02101,"1075,3388F ",.02)
 ;;3388F
 ;
OTHER ; OTHER ROUTINES
 Q
