BDM2TXQ ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON JAN 12, 2010 ;
 ;;2.0;DIABETES MANAGEMENT SYSTEM;**3**;JUN 14, 2007
 ;;DM AUDIT PROBLEM TB DXS
 ;
 ; This routine loads Taxonomy DM AUDIT PROBLEM TB DXS
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
 ;;21,"010.00 ")
 ;;1
 ;;21,"137.0 ")
 ;;2
 ;;21,"795.5 ")
 ;;3
 ;;21,"V12.01 ")
 ;;4
 ;;9002226,85,.01)
 ;;DM AUDIT PROBLEM TB DXS
 ;;9002226,85,.02)
 ;;DM AUDIT PROBLEM TB DXS
 ;;9002226,85,.04)
 ;;n
 ;;9002226,85,.06)
 ;;@
 ;;9002226,85,.08)
 ;;0
 ;;9002226,85,.09)
 ;;2930811
 ;;9002226,85,.11)
 ;;@
 ;;9002226,85,.12)
 ;;266
 ;;9002226,85,.13)
 ;;1
 ;;9002226,85,.14)
 ;;@
 ;;9002226,85,.15)
 ;;80
 ;;9002226,85,.16)
 ;;@
 ;;9002226,85,.17)
 ;;@
 ;;9002226,85,3101)
 ;;@
 ;;9002226.02101,"85,010.00 ",.01)
 ;;010.00
 ;;9002226.02101,"85,010.00 ",.02)
 ;;018.96
 ;;9002226.02101,"85,137.0 ",.01)
 ;;137.0
 ;;9002226.02101,"85,137.0 ",.02)
 ;;137.4
 ;;9002226.02101,"85,795.5 ",.01)
 ;;795.5
 ;;9002226.02101,"85,795.5 ",.02)
 ;;795.5
 ;;9002226.02101,"85,V12.01 ",.01)
 ;;V12.01
 ;;9002226.02101,"85,V12.01 ",.02)
 ;;V12.01
 ;
OTHER ; OTHER ROUTINES
 Q
