BGP9TXX ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON MAR 25, 2009 ;
 ;;9.0;IHS CLINICAL REPORTING;;JUL 1, 2009
 ;;BGP HDL LOINC CODES
 ;
 ; This routine loads Taxonomy BGP HDL LOINC CODES
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
 ;;21,"12772-0 ")
 ;;1
 ;;21,"14646-4 ")
 ;;2
 ;;21,"18263-4 ")
 ;;3
 ;;21,"2085-9 ")
 ;;4
 ;;21,"2086-7 ")
 ;;5
 ;;21,"35197-3 ")
 ;;6
 ;;21,"49130-8 ")
 ;;7
 ;;9002226,333,.01)
 ;;BGP HDL LOINC CODES
 ;;9002226,333,.02)
 ;;@
 ;;9002226,333,.04)
 ;;n
 ;;9002226,333,.06)
 ;;@
 ;;9002226,333,.08)
 ;;@
 ;;9002226,333,.09)
 ;;@
 ;;9002226,333,.11)
 ;;@
 ;;9002226,333,.12)
 ;;@
 ;;9002226,333,.13)
 ;;1
 ;;9002226,333,.14)
 ;;FIHS
 ;;9002226,333,.15)
 ;;95.3
 ;;9002226,333,.16)
 ;;@
 ;;9002226,333,.17)
 ;;@
 ;;9002226,333,3101)
 ;;@
 ;;9002226.02101,"333,12772-0 ",.01)
 ;;12772-0
 ;;9002226.02101,"333,12772-0 ",.02)
 ;;12772-0
 ;;9002226.02101,"333,14646-4 ",.01)
 ;;14646-4
 ;;9002226.02101,"333,14646-4 ",.02)
 ;;14646-4
 ;;9002226.02101,"333,18263-4 ",.01)
 ;;18263-4
 ;;9002226.02101,"333,18263-4 ",.02)
 ;;18263-4
 ;;9002226.02101,"333,2085-9 ",.01)
 ;;2085-9
 ;;9002226.02101,"333,2085-9 ",.02)
 ;;2085-9
 ;;9002226.02101,"333,2086-7 ",.01)
 ;;2086-7
 ;;9002226.02101,"333,2086-7 ",.02)
 ;;2086-7
 ;;9002226.02101,"333,35197-3 ",.01)
 ;;35197-3
 ;;9002226.02101,"333,35197-3 ",.02)
 ;;35197-3
 ;;9002226.02101,"333,49130-8 ",.01)
 ;;49130-8
 ;;9002226.02101,"333,49130-8 ",.02)
 ;;49130-8
 ;
OTHER ; OTHER ROUTINES
 Q
