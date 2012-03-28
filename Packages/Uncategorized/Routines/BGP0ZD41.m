BGP0ZD41 ;IHS/CMI/LAB-CREATED BY ^ATXSTX ON MAY 23, 2010;
 ;;10.0;IHS CLINICAL REPORTING;;JUN 18, 2010
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"976,66267-0529-30 ",.02)
 ;;66267-0529-30
 ;;9002226.02101,"976,66267-0926-00 ",.01)
 ;;66267-0926-00
 ;;9002226.02101,"976,66267-0926-00 ",.02)
 ;;66267-0926-00
 ;;9002226.02101,"976,66336-0030-12 ",.01)
 ;;66336-0030-12
 ;;9002226.02101,"976,66336-0030-12 ",.02)
 ;;66336-0030-12
 ;;9002226.02101,"976,66336-0030-20 ",.01)
 ;;66336-0030-20
 ;;9002226.02101,"976,66336-0030-20 ",.02)
 ;;66336-0030-20
 ;;9002226.02101,"976,66336-0030-21 ",.01)
 ;;66336-0030-21
 ;;9002226.02101,"976,66336-0030-21 ",.02)
 ;;66336-0030-21
 ;;9002226.02101,"976,66336-0030-30 ",.01)
 ;;66336-0030-30
 ;;9002226.02101,"976,66336-0030-30 ",.02)
 ;;66336-0030-30
 ;;9002226.02101,"976,66336-0030-40 ",.01)
 ;;66336-0030-40
 ;;9002226.02101,"976,66336-0030-40 ",.02)
 ;;66336-0030-40
 ;;9002226.02101,"976,66336-0030-45 ",.01)
 ;;66336-0030-45
 ;;9002226.02101,"976,66336-0030-45 ",.02)
 ;;66336-0030-45
 ;;9002226.02101,"976,66336-0030-60 ",.01)
 ;;66336-0030-60
 ;;9002226.02101,"976,66336-0030-60 ",.02)
 ;;66336-0030-60
 ;;9002226.02101,"976,66336-0030-90 ",.01)
 ;;66336-0030-90
 ;;9002226.02101,"976,66336-0030-90 ",.02)
 ;;66336-0030-90
 ;;9002226.02101,"976,66336-0238-28 ",.01)
 ;;66336-0238-28
 ;;9002226.02101,"976,66336-0238-28 ",.02)
 ;;66336-0238-28
 ;;9002226.02101,"976,66336-0238-30 ",.01)
 ;;66336-0238-30
 ;;9002226.02101,"976,66336-0238-30 ",.02)
 ;;66336-0238-30
 ;;9002226.02101,"976,66336-0316-14 ",.01)
 ;;66336-0316-14
 ;;9002226.02101,"976,66336-0316-14 ",.02)
 ;;66336-0316-14
 ;;9002226.02101,"976,66336-0430-06 ",.01)
 ;;66336-0430-06
 ;;9002226.02101,"976,66336-0430-06 ",.02)
 ;;66336-0430-06
 ;;9002226.02101,"976,66336-0430-20 ",.01)
 ;;66336-0430-20
 ;;9002226.02101,"976,66336-0430-20 ",.02)
 ;;66336-0430-20
 ;;9002226.02101,"976,66336-0430-30 ",.01)
 ;;66336-0430-30
 ;;9002226.02101,"976,66336-0430-30 ",.02)
 ;;66336-0430-30
 ;;9002226.02101,"976,66336-0430-40 ",.01)
 ;;66336-0430-40
 ;;9002226.02101,"976,66336-0430-40 ",.02)
 ;;66336-0430-40
 ;;9002226.02101,"976,66336-0446-08 ",.01)
 ;;66336-0446-08
 ;;9002226.02101,"976,66336-0446-08 ",.02)
 ;;66336-0446-08
 ;;9002226.02101,"976,66336-0446-10 ",.01)
 ;;66336-0446-10
 ;;9002226.02101,"976,66336-0446-10 ",.02)
 ;;66336-0446-10
 ;;9002226.02101,"976,66336-0463-14 ",.01)
 ;;66336-0463-14
 ;;9002226.02101,"976,66336-0463-14 ",.02)
 ;;66336-0463-14
 ;;9002226.02101,"976,66336-0463-28 ",.01)
 ;;66336-0463-28
 ;;9002226.02101,"976,66336-0463-28 ",.02)
 ;;66336-0463-28
 ;;9002226.02101,"976,66336-0556-10 ",.01)
 ;;66336-0556-10
 ;;9002226.02101,"976,66336-0556-10 ",.02)
 ;;66336-0556-10
 ;;9002226.02101,"976,66336-0556-16 ",.01)
 ;;66336-0556-16
 ;;9002226.02101,"976,66336-0556-16 ",.02)
 ;;66336-0556-16
 ;;9002226.02101,"976,66336-0556-20 ",.01)
 ;;66336-0556-20
 ;;9002226.02101,"976,66336-0556-20 ",.02)
 ;;66336-0556-20
 ;;9002226.02101,"976,66336-0556-21 ",.01)
 ;;66336-0556-21
 ;;9002226.02101,"976,66336-0556-21 ",.02)
 ;;66336-0556-21
 ;;9002226.02101,"976,66336-0556-28 ",.01)
 ;;66336-0556-28
 ;;9002226.02101,"976,66336-0556-28 ",.02)
 ;;66336-0556-28
 ;;9002226.02101,"976,66336-0556-30 ",.01)
 ;;66336-0556-30
 ;;9002226.02101,"976,66336-0556-30 ",.02)
 ;;66336-0556-30
 ;;9002226.02101,"976,66336-0556-40 ",.01)
 ;;66336-0556-40
 ;;9002226.02101,"976,66336-0556-40 ",.02)
 ;;66336-0556-40
 ;;9002226.02101,"976,66336-0556-60 ",.01)
 ;;66336-0556-60
 ;;9002226.02101,"976,66336-0556-60 ",.02)
 ;;66336-0556-60
 ;;9002226.02101,"976,66336-0556-90 ",.01)
 ;;66336-0556-90
 ;;9002226.02101,"976,66336-0556-90 ",.02)
 ;;66336-0556-90
 ;;9002226.02101,"976,66336-0667-21 ",.01)
 ;;66336-0667-21
 ;;9002226.02101,"976,66336-0667-21 ",.02)
 ;;66336-0667-21
 ;;9002226.02101,"976,66336-0667-30 ",.01)
 ;;66336-0667-30
 ;;9002226.02101,"976,66336-0667-30 ",.02)
 ;;66336-0667-30
 ;;9002226.02101,"976,66336-0676-10 ",.01)
 ;;66336-0676-10
 ;;9002226.02101,"976,66336-0676-10 ",.02)
 ;;66336-0676-10
 ;;9002226.02101,"976,66336-0676-30 ",.01)
 ;;66336-0676-30
 ;;9002226.02101,"976,66336-0676-30 ",.02)
 ;;66336-0676-30
 ;;9002226.02101,"976,66336-0676-60 ",.01)
 ;;66336-0676-60
 ;;9002226.02101,"976,66336-0676-60 ",.02)
 ;;66336-0676-60
 ;;9002226.02101,"976,66336-0676-90 ",.01)
 ;;66336-0676-90
 ;;9002226.02101,"976,66336-0676-90 ",.02)
 ;;66336-0676-90
 ;;9002226.02101,"976,66336-0684-30 ",.01)
 ;;66336-0684-30
 ;;9002226.02101,"976,66336-0684-30 ",.02)
 ;;66336-0684-30
 ;;9002226.02101,"976,66336-0686-15 ",.01)
 ;;66336-0686-15
 ;;9002226.02101,"976,66336-0686-15 ",.02)
 ;;66336-0686-15
 ;;9002226.02101,"976,66336-0686-60 ",.01)
 ;;66336-0686-60
 ;;9002226.02101,"976,66336-0686-60 ",.02)
 ;;66336-0686-60
 ;;9002226.02101,"976,66336-0722-14 ",.01)
 ;;66336-0722-14
 ;;9002226.02101,"976,66336-0722-14 ",.02)
 ;;66336-0722-14
 ;;9002226.02101,"976,66336-0727-14 ",.01)
 ;;66336-0727-14
 ;;9002226.02101,"976,66336-0727-14 ",.02)
 ;;66336-0727-14
 ;;9002226.02101,"976,66336-0727-15 ",.01)
 ;;66336-0727-15
 ;;9002226.02101,"976,66336-0727-15 ",.02)
 ;;66336-0727-15
 ;;9002226.02101,"976,66336-0727-20 ",.01)
 ;;66336-0727-20
 ;;9002226.02101,"976,66336-0727-20 ",.02)
 ;;66336-0727-20
 ;;9002226.02101,"976,66336-0804-30 ",.01)
 ;;66336-0804-30
 ;;9002226.02101,"976,66336-0804-30 ",.02)
 ;;66336-0804-30
 ;;9002226.02101,"976,66336-0804-60 ",.01)
 ;;66336-0804-60
 ;;9002226.02101,"976,66336-0804-60 ",.02)
 ;;66336-0804-60
 ;;9002226.02101,"976,66336-0815-10 ",.01)
 ;;66336-0815-10
 ;;9002226.02101,"976,66336-0815-10 ",.02)
 ;;66336-0815-10
 ;;9002226.02101,"976,66336-0815-14 ",.01)
 ;;66336-0815-14
 ;;9002226.02101,"976,66336-0815-14 ",.02)
 ;;66336-0815-14
 ;;9002226.02101,"976,66336-0815-15 ",.01)
 ;;66336-0815-15
 ;;9002226.02101,"976,66336-0815-15 ",.02)
 ;;66336-0815-15
 ;;9002226.02101,"976,66336-0815-20 ",.01)
 ;;66336-0815-20
 ;;9002226.02101,"976,66336-0815-20 ",.02)
 ;;66336-0815-20
 ;;9002226.02101,"976,66336-0815-30 ",.01)
 ;;66336-0815-30
 ;;9002226.02101,"976,66336-0815-30 ",.02)
 ;;66336-0815-30
 ;;9002226.02101,"976,66336-0815-40 ",.01)
 ;;66336-0815-40
 ;;9002226.02101,"976,66336-0815-40 ",.02)
 ;;66336-0815-40
 ;;9002226.02101,"976,66336-0815-60 ",.01)
 ;;66336-0815-60
 ;;9002226.02101,"976,66336-0815-60 ",.02)
 ;;66336-0815-60
 ;;9002226.02101,"976,66336-0815-90 ",.01)
 ;;66336-0815-90
 ;;9002226.02101,"976,66336-0815-90 ",.02)
 ;;66336-0815-90
 ;;9002226.02101,"976,66336-0816-20 ",.01)
 ;;66336-0816-20
 ;;9002226.02101,"976,66336-0816-20 ",.02)
 ;;66336-0816-20
 ;;9002226.02101,"976,66336-0816-30 ",.01)
 ;;66336-0816-30
 ;;9002226.02101,"976,66336-0816-30 ",.02)
 ;;66336-0816-30
 ;;9002226.02101,"976,66336-0817-20 ",.01)
 ;;66336-0817-20
 ;;9002226.02101,"976,66336-0817-20 ",.02)
 ;;66336-0817-20
 ;;9002226.02101,"976,66336-0817-28 ",.01)
 ;;66336-0817-28
 ;;9002226.02101,"976,66336-0817-28 ",.02)
 ;;66336-0817-28
 ;;9002226.02101,"976,66336-0817-30 ",.01)
 ;;66336-0817-30
 ;;9002226.02101,"976,66336-0817-30 ",.02)
 ;;66336-0817-30
 ;;9002226.02101,"976,66336-0826-14 ",.01)
 ;;66336-0826-14
 ;;9002226.02101,"976,66336-0826-14 ",.02)
 ;;66336-0826-14
 ;;9002226.02101,"976,66336-0826-15 ",.01)
 ;;66336-0826-15
 ;;9002226.02101,"976,66336-0826-15 ",.02)
 ;;66336-0826-15
 ;;9002226.02101,"976,66336-0826-20 ",.01)
 ;;66336-0826-20
 ;;9002226.02101,"976,66336-0826-20 ",.02)
 ;;66336-0826-20
 ;;9002226.02101,"976,66336-0826-30 ",.01)
 ;;66336-0826-30
 ;;9002226.02101,"976,66336-0826-30 ",.02)
 ;;66336-0826-30
 ;;9002226.02101,"976,66336-0875-14 ",.01)
 ;;66336-0875-14
 ;;9002226.02101,"976,66336-0875-14 ",.02)
 ;;66336-0875-14
 ;;9002226.02101,"976,66336-0875-20 ",.01)
 ;;66336-0875-20
 ;;9002226.02101,"976,66336-0875-20 ",.02)
 ;;66336-0875-20
 ;;9002226.02101,"976,66336-0878-28 ",.01)
 ;;66336-0878-28
 ;;9002226.02101,"976,66336-0878-28 ",.02)
 ;;66336-0878-28
 ;;9002226.02101,"976,66336-0878-30 ",.01)
 ;;66336-0878-30
 ;;9002226.02101,"976,66336-0878-30 ",.02)
 ;;66336-0878-30
 ;;9002226.02101,"976,66336-0878-60 ",.01)
 ;;66336-0878-60
 ;;9002226.02101,"976,66336-0878-60 ",.02)
 ;;66336-0878-60
 ;;9002226.02101,"976,67253-0620-10 ",.01)
 ;;67253-0620-10
 ;;9002226.02101,"976,67253-0620-10 ",.02)
 ;;67253-0620-10
 ;;9002226.02101,"976,67253-0620-11 ",.01)
 ;;67253-0620-11
 ;;9002226.02101,"976,67253-0620-11 ",.02)
 ;;67253-0620-11
 ;;9002226.02101,"976,67253-0621-05 ",.01)
 ;;67253-0621-05
 ;;9002226.02101,"976,67253-0621-05 ",.02)
 ;;67253-0621-05
 ;;9002226.02101,"976,67253-0621-10 ",.01)
 ;;67253-0621-10
 ;;9002226.02101,"976,67253-0621-10 ",.02)
 ;;67253-0621-10
 ;;9002226.02101,"976,67253-0621-11 ",.01)
 ;;67253-0621-11
 ;;9002226.02101,"976,67253-0621-11 ",.02)
 ;;67253-0621-11
 ;;9002226.02101,"976,67253-0622-03 ",.01)
 ;;67253-0622-03
 ;;9002226.02101,"976,67253-0622-03 ",.02)
 ;;67253-0622-03
 ;;9002226.02101,"976,67253-0622-10 ",.01)
 ;;67253-0622-10
 ;;9002226.02101,"976,67253-0622-10 ",.02)
 ;;67253-0622-10
 ;;9002226.02101,"976,67253-0622-50 ",.01)
 ;;67253-0622-50
 ;;9002226.02101,"976,67253-0622-50 ",.02)
 ;;67253-0622-50
 ;;9002226.02101,"976,68115-0069-20 ",.01)
 ;;68115-0069-20
 ;;9002226.02101,"976,68115-0069-20 ",.02)
 ;;68115-0069-20
 ;;9002226.02101,"976,68115-0069-30 ",.01)
 ;;68115-0069-30
 ;;9002226.02101,"976,68115-0069-30 ",.02)
 ;;68115-0069-30
 ;;9002226.02101,"976,68115-0069-60 ",.01)
 ;;68115-0069-60
 ;;9002226.02101,"976,68115-0069-60 ",.02)
 ;;68115-0069-60
 ;;9002226.02101,"976,68115-0070-00 ",.01)
 ;;68115-0070-00
 ;;9002226.02101,"976,68115-0070-00 ",.02)
 ;;68115-0070-00
 ;;9002226.02101,"976,68115-0070-10 ",.01)
 ;;68115-0070-10
 ;;9002226.02101,"976,68115-0070-10 ",.02)
 ;;68115-0070-10
 ;;9002226.02101,"976,68115-0070-15 ",.01)
 ;;68115-0070-15
 ;;9002226.02101,"976,68115-0070-15 ",.02)
 ;;68115-0070-15
 ;;9002226.02101,"976,68115-0070-20 ",.01)
 ;;68115-0070-20
 ;;9002226.02101,"976,68115-0070-20 ",.02)
 ;;68115-0070-20
 ;;9002226.02101,"976,68115-0070-30 ",.01)
 ;;68115-0070-30
 ;;9002226.02101,"976,68115-0070-30 ",.02)
 ;;68115-0070-30
 ;;9002226.02101,"976,68115-0101-20 ",.01)
 ;;68115-0101-20
 ;;9002226.02101,"976,68115-0101-20 ",.02)
 ;;68115-0101-20
 ;;9002226.02101,"976,68115-0101-28 ",.01)
 ;;68115-0101-28
 ;;9002226.02101,"976,68115-0101-28 ",.02)
 ;;68115-0101-28
 ;;9002226.02101,"976,68115-0102-02 ",.01)
 ;;68115-0102-02
 ;;9002226.02101,"976,68115-0102-02 ",.02)
 ;;68115-0102-02
 ;;9002226.02101,"976,68115-0102-30 ",.01)
 ;;68115-0102-30
 ;;9002226.02101,"976,68115-0102-30 ",.02)
 ;;68115-0102-30
 ;;9002226.02101,"976,68115-0102-60 ",.01)
 ;;68115-0102-60
 ;;9002226.02101,"976,68115-0102-60 ",.02)
 ;;68115-0102-60
 ;;9002226.02101,"976,68115-0102-90 ",.01)
 ;;68115-0102-90
 ;;9002226.02101,"976,68115-0102-90 ",.02)
 ;;68115-0102-90
 ;;9002226.02101,"976,68115-0103-20 ",.01)
 ;;68115-0103-20
 ;;9002226.02101,"976,68115-0103-20 ",.02)
 ;;68115-0103-20
 ;;9002226.02101,"976,68115-0103-30 ",.01)
 ;;68115-0103-30
 ;;9002226.02101,"976,68115-0103-30 ",.02)
 ;;68115-0103-30
 ;;9002226.02101,"976,68115-0103-60 ",.01)
 ;;68115-0103-60
 ;;9002226.02101,"976,68115-0103-60 ",.02)
 ;;68115-0103-60
 ;;9002226.02101,"976,68115-0103-90 ",.01)
 ;;68115-0103-90
 ;;9002226.02101,"976,68115-0103-90 ",.02)
 ;;68115-0103-90
 ;;9002226.02101,"976,68115-0137-10 ",.01)
 ;;68115-0137-10
 ;;9002226.02101,"976,68115-0137-10 ",.02)
 ;;68115-0137-10
 ;;9002226.02101,"976,68115-0137-30 ",.01)
 ;;68115-0137-30
 ;;9002226.02101,"976,68115-0137-30 ",.02)
 ;;68115-0137-30
 ;;9002226.02101,"976,68115-0137-60 ",.01)
 ;;68115-0137-60
 ;;9002226.02101,"976,68115-0137-60 ",.02)
 ;;68115-0137-60
 ;;9002226.02101,"976,68115-0140-30 ",.01)
 ;;68115-0140-30
 ;;9002226.02101,"976,68115-0140-30 ",.02)
 ;;68115-0140-30
 ;;9002226.02101,"976,68115-0180-20 ",.01)
 ;;68115-0180-20
 ;;9002226.02101,"976,68115-0180-20 ",.02)
 ;;68115-0180-20
 ;;9002226.02101,"976,68115-0180-30 ",.01)
 ;;68115-0180-30
 ;;9002226.02101,"976,68115-0180-30 ",.02)
 ;;68115-0180-30
 ;;9002226.02101,"976,68115-0180-40 ",.01)
 ;;68115-0180-40
 ;;9002226.02101,"976,68115-0180-40 ",.02)
 ;;68115-0180-40
 ;;9002226.02101,"976,68115-0180-50 ",.01)
 ;;68115-0180-50
 ;;9002226.02101,"976,68115-0180-50 ",.02)
 ;;68115-0180-50
 ;;9002226.02101,"976,68115-0180-60 ",.01)
 ;;68115-0180-60
 ;;9002226.02101,"976,68115-0180-60 ",.02)
 ;;68115-0180-60
 ;;9002226.02101,"976,68115-0180-90 ",.01)
 ;;68115-0180-90
 ;;9002226.02101,"976,68115-0180-90 ",.02)
 ;;68115-0180-90
 ;;9002226.02101,"976,68115-0181-00 ",.01)
 ;;68115-0181-00
 ;;9002226.02101,"976,68115-0181-00 ",.02)
 ;;68115-0181-00
 ;;9002226.02101,"976,68115-0181-12 ",.01)
 ;;68115-0181-12
 ;;9002226.02101,"976,68115-0181-12 ",.02)
 ;;68115-0181-12
 ;;9002226.02101,"976,68115-0181-20 ",.01)
 ;;68115-0181-20
 ;;9002226.02101,"976,68115-0181-20 ",.02)
 ;;68115-0181-20
 ;;9002226.02101,"976,68115-0181-30 ",.01)
 ;;68115-0181-30
 ;;9002226.02101,"976,68115-0181-30 ",.02)
 ;;68115-0181-30
 ;;9002226.02101,"976,68115-0181-40 ",.01)
 ;;68115-0181-40
 ;;9002226.02101,"976,68115-0181-40 ",.02)
 ;;68115-0181-40
 ;;9002226.02101,"976,68115-0181-60 ",.01)
 ;;68115-0181-60
 ;;9002226.02101,"976,68115-0181-60 ",.02)
 ;;68115-0181-60
 ;;9002226.02101,"976,68115-0181-90 ",.01)
 ;;68115-0181-90
 ;;9002226.02101,"976,68115-0181-90 ",.02)
 ;;68115-0181-90
 ;;9002226.02101,"976,68115-0181-99 ",.01)
 ;;68115-0181-99
 ;;9002226.02101,"976,68115-0181-99 ",.02)
 ;;68115-0181-99
 ;;9002226.02101,"976,68115-0182-10 ",.01)
 ;;68115-0182-10
