BGP9SXVI ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON MAR 25, 2009 ;
 ;;9.0;IHS CLINICAL REPORTING;;JUL 1, 2009
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"747,00247-0656-10 ",.02)
 ;;00247-0656-10
 ;;9002226.02101,"747,00247-0656-14 ",.01)
 ;;00247-0656-14
 ;;9002226.02101,"747,00247-0656-14 ",.02)
 ;;00247-0656-14
 ;;9002226.02101,"747,00247-0656-20 ",.01)
 ;;00247-0656-20
 ;;9002226.02101,"747,00247-0656-20 ",.02)
 ;;00247-0656-20
 ;;9002226.02101,"747,00247-0656-28 ",.01)
 ;;00247-0656-28
 ;;9002226.02101,"747,00247-0656-28 ",.02)
 ;;00247-0656-28
 ;;9002226.02101,"747,00247-0656-30 ",.01)
 ;;00247-0656-30
 ;;9002226.02101,"747,00247-0656-30 ",.02)
 ;;00247-0656-30
 ;;9002226.02101,"747,00247-0656-60 ",.01)
 ;;00247-0656-60
 ;;9002226.02101,"747,00247-0656-60 ",.02)
 ;;00247-0656-60
 ;;9002226.02101,"747,00247-0659-07 ",.01)
 ;;00247-0659-07
 ;;9002226.02101,"747,00247-0659-07 ",.02)
 ;;00247-0659-07
 ;;9002226.02101,"747,00247-0667-08 ",.01)
 ;;00247-0667-08
 ;;9002226.02101,"747,00247-0667-08 ",.02)
 ;;00247-0667-08
 ;;9002226.02101,"747,00247-0674-41 ",.01)
 ;;00247-0674-41
 ;;9002226.02101,"747,00247-0674-41 ",.02)
 ;;00247-0674-41
 ;;9002226.02101,"747,00247-0703-07 ",.01)
 ;;00247-0703-07
 ;;9002226.02101,"747,00247-0703-07 ",.02)
 ;;00247-0703-07
 ;;9002226.02101,"747,00247-0824-06 ",.01)
 ;;00247-0824-06
 ;;9002226.02101,"747,00247-0824-06 ",.02)
 ;;00247-0824-06
 ;;9002226.02101,"747,00247-0824-10 ",.01)
 ;;00247-0824-10
 ;;9002226.02101,"747,00247-0824-10 ",.02)
 ;;00247-0824-10
 ;;9002226.02101,"747,00247-0824-30 ",.01)
 ;;00247-0824-30
 ;;9002226.02101,"747,00247-0824-30 ",.02)
 ;;00247-0824-30
 ;;9002226.02101,"747,00247-0824-60 ",.01)
 ;;00247-0824-60
 ;;9002226.02101,"747,00247-0824-60 ",.02)
 ;;00247-0824-60
 ;;9002226.02101,"747,00247-0824-90 ",.01)
 ;;00247-0824-90
 ;;9002226.02101,"747,00247-0824-90 ",.02)
 ;;00247-0824-90
 ;;9002226.02101,"747,00247-0873-02 ",.01)
 ;;00247-0873-02
 ;;9002226.02101,"747,00247-0873-02 ",.02)
 ;;00247-0873-02
 ;;9002226.02101,"747,00247-0873-52 ",.01)
 ;;00247-0873-52
 ;;9002226.02101,"747,00247-0873-52 ",.02)
 ;;00247-0873-52
 ;;9002226.02101,"747,00247-0873-60 ",.01)
 ;;00247-0873-60
 ;;9002226.02101,"747,00247-0873-60 ",.02)
 ;;00247-0873-60
 ;;9002226.02101,"747,00247-1094-17 ",.01)
 ;;00247-1094-17
 ;;9002226.02101,"747,00247-1094-17 ",.02)
 ;;00247-1094-17
 ;;9002226.02101,"747,00247-1094-88 ",.01)
 ;;00247-1094-88
 ;;9002226.02101,"747,00247-1094-88 ",.02)
 ;;00247-1094-88
 ;;9002226.02101,"747,00247-1575-13 ",.01)
 ;;00247-1575-13
 ;;9002226.02101,"747,00247-1575-13 ",.02)
 ;;00247-1575-13
 ;;9002226.02101,"747,00247-1576-12 ",.01)
 ;;00247-1576-12
 ;;9002226.02101,"747,00247-1576-12 ",.02)
 ;;00247-1576-12
 ;;9002226.02101,"747,00247-1576-13 ",.01)
 ;;00247-1576-13
 ;;9002226.02101,"747,00247-1576-13 ",.02)
 ;;00247-1576-13
 ;;9002226.02101,"747,00247-1577-13 ",.01)
 ;;00247-1577-13
 ;;9002226.02101,"747,00247-1577-13 ",.02)
 ;;00247-1577-13
 ;;9002226.02101,"747,00247-1696-93 ",.01)
 ;;00247-1696-93
 ;;9002226.02101,"747,00247-1696-93 ",.02)
 ;;00247-1696-93
 ;;9002226.02101,"747,00247-1897-00 ",.01)
 ;;00247-1897-00
 ;;9002226.02101,"747,00247-1897-00 ",.02)
 ;;00247-1897-00
 ;;9002226.02101,"747,00247-1897-30 ",.01)
 ;;00247-1897-30
 ;;9002226.02101,"747,00247-1897-30 ",.02)
 ;;00247-1897-30
 ;;9002226.02101,"747,00247-1897-60 ",.01)
 ;;00247-1897-60
 ;;9002226.02101,"747,00247-1897-60 ",.02)
 ;;00247-1897-60
 ;;9002226.02101,"747,00247-1897-77 ",.01)
 ;;00247-1897-77
 ;;9002226.02101,"747,00247-1897-77 ",.02)
 ;;00247-1897-77
 ;;9002226.02101,"747,00247-1897-90 ",.01)
 ;;00247-1897-90
 ;;9002226.02101,"747,00247-1897-90 ",.02)
 ;;00247-1897-90
 ;;9002226.02101,"747,00247-1898-00 ",.01)
 ;;00247-1898-00
 ;;9002226.02101,"747,00247-1898-00 ",.02)
 ;;00247-1898-00
 ;;9002226.02101,"747,00247-1898-14 ",.01)
 ;;00247-1898-14
 ;;9002226.02101,"747,00247-1898-14 ",.02)
 ;;00247-1898-14
 ;;9002226.02101,"747,00247-1898-30 ",.01)
 ;;00247-1898-30
 ;;9002226.02101,"747,00247-1898-30 ",.02)
 ;;00247-1898-30
 ;;9002226.02101,"747,00247-1898-60 ",.01)
 ;;00247-1898-60
 ;;9002226.02101,"747,00247-1898-60 ",.02)
 ;;00247-1898-60
 ;;9002226.02101,"747,00247-1898-77 ",.01)
 ;;00247-1898-77
 ;;9002226.02101,"747,00247-1898-77 ",.02)
 ;;00247-1898-77
 ;;9002226.02101,"747,00247-1898-90 ",.01)
 ;;00247-1898-90
 ;;9002226.02101,"747,00247-1898-90 ",.02)
 ;;00247-1898-90
 ;;9002226.02101,"747,00247-1973-60 ",.01)
 ;;00247-1973-60
 ;;9002226.02101,"747,00247-1973-60 ",.02)
 ;;00247-1973-60
 ;;9002226.02101,"747,00247-1983-60 ",.01)
 ;;00247-1983-60
 ;;9002226.02101,"747,00247-1983-60 ",.02)
 ;;00247-1983-60
 ;;9002226.02101,"747,00247-1988-30 ",.01)
 ;;00247-1988-30
 ;;9002226.02101,"747,00247-1988-30 ",.02)
 ;;00247-1988-30
 ;;9002226.02101,"747,00247-2215-60 ",.01)
 ;;00247-2215-60
 ;;9002226.02101,"747,00247-2215-60 ",.02)
 ;;00247-2215-60
 ;;9002226.02101,"747,00258-3581-01 ",.01)
 ;;00258-3581-01
 ;;9002226.02101,"747,00258-3581-01 ",.02)
 ;;00258-3581-01
 ;;9002226.02101,"747,00258-3581-05 ",.01)
 ;;00258-3581-05
 ;;9002226.02101,"747,00258-3581-05 ",.02)
 ;;00258-3581-05
 ;;9002226.02101,"747,00258-3581-10 ",.01)
 ;;00258-3581-10
 ;;9002226.02101,"747,00258-3581-10 ",.02)
 ;;00258-3581-10
 ;;9002226.02101,"747,00258-3583-01 ",.01)
 ;;00258-3583-01
