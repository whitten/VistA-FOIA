BGP8FXME ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON SEP 28, 2007 ;
 ;;8.0;IHS CLINICAL REPORTING;;MAR 12, 2008
 ;;;BGP6;;SEP 28, 2007
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"563,52544-0415-01 ",.02)
 ;;52544-0415-01
 ;;9002226.02101,"563,52544-0416-01 ",.01)
 ;;52544-0416-01
 ;;9002226.02101,"563,52544-0416-01 ",.02)
 ;;52544-0416-01
 ;;9002226.02101,"563,52544-0471-08 ",.01)
 ;;52544-0471-08
 ;;9002226.02101,"563,52544-0471-08 ",.02)
 ;;52544-0471-08
 ;;9002226.02101,"563,52544-0471-23 ",.01)
 ;;52544-0471-23
 ;;9002226.02101,"563,52544-0471-23 ",.02)
 ;;52544-0471-23
 ;;9002226.02101,"563,52544-0472-08 ",.01)
 ;;52544-0472-08
 ;;9002226.02101,"563,52544-0472-08 ",.02)
 ;;52544-0472-08
 ;;9002226.02101,"563,52544-0473-08 ",.01)
 ;;52544-0473-08
 ;;9002226.02101,"563,52544-0473-08 ",.02)
 ;;52544-0473-08
 ;;9002226.02101,"563,52544-0487-01 ",.01)
 ;;52544-0487-01
 ;;9002226.02101,"563,52544-0487-01 ",.02)
 ;;52544-0487-01
 ;;9002226.02101,"563,52544-0487-05 ",.01)
 ;;52544-0487-05
 ;;9002226.02101,"563,52544-0487-05 ",.02)
 ;;52544-0487-05
 ;;9002226.02101,"563,52544-0488-01 ",.01)
 ;;52544-0488-01
 ;;9002226.02101,"563,52544-0488-01 ",.02)
 ;;52544-0488-01
 ;;9002226.02101,"563,52544-0488-05 ",.01)
 ;;52544-0488-05
 ;;9002226.02101,"563,52544-0488-05 ",.02)
 ;;52544-0488-05
 ;;9002226.02101,"563,52544-0528-01 ",.01)
 ;;52544-0528-01
 ;;9002226.02101,"563,52544-0528-01 ",.02)
 ;;52544-0528-01
 ;;9002226.02101,"563,52544-0884-08 ",.01)
 ;;52544-0884-08
 ;;9002226.02101,"563,52544-0884-08 ",.02)
 ;;52544-0884-08
 ;;9002226.02101,"563,52555-0651-01 ",.01)
 ;;52555-0651-01
 ;;9002226.02101,"563,52555-0651-01 ",.02)
 ;;52555-0651-01
 ;;9002226.02101,"563,52555-0717-01 ",.01)
 ;;52555-0717-01
 ;;9002226.02101,"563,52555-0717-01 ",.02)
 ;;52555-0717-01
 ;;9002226.02101,"563,52637-0332-10 ",.01)
 ;;52637-0332-10
 ;;9002226.02101,"563,52637-0332-10 ",.02)
 ;;52637-0332-10
 ;;9002226.02101,"563,52959-0222-00 ",.01)
 ;;52959-0222-00
 ;;9002226.02101,"563,52959-0222-00 ",.02)
 ;;52959-0222-00
 ;;9002226.02101,"563,52959-0223-00 ",.01)
 ;;52959-0223-00
 ;;9002226.02101,"563,52959-0223-00 ",.02)
 ;;52959-0223-00
 ;;9002226.02101,"563,52959-0223-30 ",.01)
 ;;52959-0223-30
 ;;9002226.02101,"563,52959-0223-30 ",.02)
 ;;52959-0223-30
 ;;9002226.02101,"563,52959-0323-00 ",.01)
 ;;52959-0323-00
 ;;9002226.02101,"563,52959-0323-00 ",.02)
 ;;52959-0323-00
 ;;9002226.02101,"563,52959-0326-10 ",.01)
 ;;52959-0326-10
 ;;9002226.02101,"563,52959-0326-10 ",.02)
 ;;52959-0326-10
 ;;9002226.02101,"563,54569-0802-01 ",.01)
 ;;54569-0802-01
 ;;9002226.02101,"563,54569-0802-01 ",.02)
 ;;54569-0802-01
 ;;9002226.02101,"563,54569-0804-00 ",.01)
 ;;54569-0804-00
 ;;9002226.02101,"563,54569-0804-00 ",.02)
 ;;54569-0804-00
 ;;9002226.02101,"563,54569-0811-01 ",.01)
 ;;54569-0811-01
 ;;9002226.02101,"563,54569-0811-01 ",.02)
 ;;54569-0811-01
 ;;9002226.02101,"563,54569-0812-00 ",.01)
 ;;54569-0812-00
 ;;9002226.02101,"563,54569-0812-00 ",.02)
 ;;54569-0812-00
 ;;9002226.02101,"563,54569-0812-01 ",.01)
 ;;54569-0812-01
 ;;9002226.02101,"563,54569-0812-01 ",.02)
 ;;54569-0812-01
 ;;9002226.02101,"563,54569-0812-02 ",.01)
 ;;54569-0812-02
 ;;9002226.02101,"563,54569-0812-02 ",.02)
 ;;54569-0812-02
 ;;9002226.02101,"563,54569-0812-05 ",.01)
 ;;54569-0812-05
 ;;9002226.02101,"563,54569-0812-05 ",.02)
 ;;54569-0812-05
 ;;9002226.02101,"563,54569-0813-00 ",.01)
 ;;54569-0813-00
 ;;9002226.02101,"563,54569-0813-00 ",.02)
 ;;54569-0813-00
 ;;9002226.02101,"563,54569-0813-01 ",.01)
 ;;54569-0813-01
 ;;9002226.02101,"563,54569-0813-01 ",.02)
 ;;54569-0813-01
 ;;9002226.02101,"563,54569-0849-00 ",.01)
 ;;54569-0849-00
 ;;9002226.02101,"563,54569-0849-00 ",.02)
 ;;54569-0849-00
 ;;9002226.02101,"563,54569-0849-01 ",.01)
 ;;54569-0849-01
 ;;9002226.02101,"563,54569-0849-01 ",.02)
 ;;54569-0849-01
 ;;9002226.02101,"563,54569-1394-00 ",.01)
 ;;54569-1394-00
 ;;9002226.02101,"563,54569-1394-00 ",.02)
 ;;54569-1394-00
 ;;9002226.02101,"563,54569-1852-01 ",.01)
 ;;54569-1852-01
 ;;9002226.02101,"563,54569-1852-01 ",.02)
 ;;54569-1852-01
 ;;9002226.02101,"563,54569-2580-00 ",.01)
 ;;54569-2580-00
 ;;9002226.02101,"563,54569-2580-00 ",.02)
 ;;54569-2580-00
 ;;9002226.02101,"563,54569-3007-00 ",.01)
 ;;54569-3007-00
 ;;9002226.02101,"563,54569-3007-00 ",.02)
 ;;54569-3007-00
 ;;9002226.02101,"563,54569-3009-00 ",.01)
 ;;54569-3009-00
 ;;9002226.02101,"563,54569-3009-00 ",.02)
 ;;54569-3009-00
 ;;9002226.02101,"563,54569-4367-00 ",.01)
 ;;54569-4367-00
 ;;9002226.02101,"563,54569-4367-00 ",.02)
 ;;54569-4367-00
 ;;9002226.02101,"563,54569-4367-01 ",.01)
 ;;54569-4367-01
 ;;9002226.02101,"563,54569-4367-01 ",.02)
 ;;54569-4367-01
 ;;9002226.02101,"563,54569-4618-00 ",.01)
 ;;54569-4618-00
 ;;9002226.02101,"563,54569-4618-00 ",.02)
 ;;54569-4618-00
 ;;9002226.02101,"563,54569-4628-00 ",.01)
 ;;54569-4628-00
 ;;9002226.02101,"563,54569-4628-00 ",.02)
 ;;54569-4628-00
 ;;9002226.02101,"563,54569-4673-00 ",.01)
 ;;54569-4673-00
 ;;9002226.02101,"563,54569-4673-00 ",.02)
 ;;54569-4673-00
 ;;9002226.02101,"563,54569-4711-00 ",.01)
 ;;54569-4711-00
 ;;9002226.02101,"563,54569-4711-00 ",.02)
 ;;54569-4711-00
 ;;9002226.02101,"563,54569-4866-00 ",.01)
 ;;54569-4866-00
 ;;9002226.02101,"563,54569-4866-00 ",.02)
 ;;54569-4866-00
 ;;9002226.02101,"563,54569-4907-00 ",.01)
 ;;54569-4907-00
 ;;9002226.02101,"563,54569-4907-00 ",.02)
 ;;54569-4907-00
 ;;9002226.02101,"563,54569-4908-00 ",.01)
 ;;54569-4908-00
 ;;9002226.02101,"563,54569-4908-00 ",.02)
 ;;54569-4908-00
 ;;9002226.02101,"563,54569-4925-00 ",.01)
 ;;54569-4925-00
 ;;9002226.02101,"563,54569-4925-00 ",.02)
 ;;54569-4925-00
 ;;9002226.02101,"563,54569-5152-00 ",.01)
 ;;54569-5152-00
 ;;9002226.02101,"563,54569-5152-00 ",.02)
 ;;54569-5152-00
 ;;9002226.02101,"563,54569-5153-00 ",.01)
 ;;54569-5153-00
 ;;9002226.02101,"563,54569-5153-00 ",.02)
 ;;54569-5153-00
 ;;9002226.02101,"563,54569-5164-00 ",.01)
 ;;54569-5164-00
 ;;9002226.02101,"563,54569-5164-00 ",.02)
 ;;54569-5164-00
 ;;9002226.02101,"563,54569-5218-00 ",.01)
 ;;54569-5218-00
 ;;9002226.02101,"563,54569-5218-00 ",.02)
 ;;54569-5218-00
 ;;9002226.02101,"563,54569-5380-00 ",.01)
 ;;54569-5380-00
 ;;9002226.02101,"563,54569-5380-00 ",.02)
 ;;54569-5380-00
 ;;9002226.02101,"563,54569-5399-00 ",.01)
 ;;54569-5399-00
 ;;9002226.02101,"563,54569-5399-00 ",.02)
 ;;54569-5399-00
 ;;9002226.02101,"563,54569-5462-00 ",.01)
 ;;54569-5462-00
 ;;9002226.02101,"563,54569-5462-00 ",.02)
 ;;54569-5462-00
 ;;9002226.02101,"563,54569-8006-00 ",.01)
 ;;54569-8006-00
 ;;9002226.02101,"563,54569-8006-00 ",.02)
 ;;54569-8006-00
 ;;9002226.02101,"563,54569-8006-01 ",.01)
 ;;54569-8006-01
 ;;9002226.02101,"563,54569-8006-01 ",.02)
 ;;54569-8006-01
 ;;9002226.02101,"563,54569-8006-02 ",.01)
 ;;54569-8006-02
 ;;9002226.02101,"563,54569-8006-02 ",.02)
 ;;54569-8006-02
 ;;9002226.02101,"563,54569-8014-00 ",.01)
 ;;54569-8014-00
 ;;9002226.02101,"563,54569-8014-00 ",.02)
 ;;54569-8014-00
 ;;9002226.02101,"563,54569-8500-00 ",.01)
 ;;54569-8500-00
 ;;9002226.02101,"563,54569-8500-00 ",.02)
 ;;54569-8500-00
 ;;9002226.02101,"563,54569-8500-01 ",.01)
 ;;54569-8500-01
 ;;9002226.02101,"563,54569-8500-01 ",.02)
 ;;54569-8500-01
 ;;9002226.02101,"563,54569-8500-02 ",.01)
 ;;54569-8500-02
 ;;9002226.02101,"563,54569-8500-02 ",.02)
 ;;54569-8500-02
 ;;9002226.02101,"563,54569-8505-00 ",.01)
 ;;54569-8505-00
 ;;9002226.02101,"563,54569-8505-00 ",.02)
 ;;54569-8505-00
 ;;9002226.02101,"563,54569-8505-01 ",.01)
 ;;54569-8505-01
 ;;9002226.02101,"563,54569-8505-01 ",.02)
 ;;54569-8505-01
 ;;9002226.02101,"563,54569-8505-02 ",.01)
 ;;54569-8505-02
 ;;9002226.02101,"563,54569-8505-02 ",.02)
 ;;54569-8505-02
 ;;9002226.02101,"563,54569-8517-00 ",.01)
 ;;54569-8517-00
 ;;9002226.02101,"563,54569-8517-00 ",.02)
 ;;54569-8517-00
 ;;9002226.02101,"563,54569-8517-01 ",.01)
 ;;54569-8517-01
 ;;9002226.02101,"563,54569-8517-01 ",.02)
 ;;54569-8517-01
 ;;9002226.02101,"563,54569-8518-00 ",.01)
 ;;54569-8518-00
 ;;9002226.02101,"563,54569-8518-00 ",.02)
 ;;54569-8518-00
 ;;9002226.02101,"563,54569-8518-01 ",.01)
 ;;54569-8518-01
 ;;9002226.02101,"563,54569-8518-01 ",.02)
 ;;54569-8518-01
 ;;9002226.02101,"563,54569-8525-00 ",.01)
 ;;54569-8525-00
 ;;9002226.02101,"563,54569-8525-00 ",.02)
 ;;54569-8525-00
 ;;9002226.02101,"563,54569-8528-00 ",.01)
 ;;54569-8528-00
 ;;9002226.02101,"563,54569-8528-00 ",.02)
 ;;54569-8528-00
 ;;9002226.02101,"563,54569-8551-00 ",.01)
 ;;54569-8551-00
 ;;9002226.02101,"563,54569-8551-00 ",.02)
 ;;54569-8551-00
 ;;9002226.02101,"563,54569-8577-00 ",.01)
 ;;54569-8577-00
 ;;9002226.02101,"563,54569-8577-00 ",.02)
 ;;54569-8577-00
 ;;9002226.02101,"563,54868-0365-00 ",.01)
 ;;54868-0365-00
 ;;9002226.02101,"563,54868-0365-00 ",.02)
 ;;54868-0365-00
 ;;9002226.02101,"563,54868-0365-02 ",.01)
 ;;54868-0365-02
 ;;9002226.02101,"563,54868-0365-02 ",.02)
 ;;54868-0365-02
 ;;9002226.02101,"563,54868-0451-00 ",.01)
 ;;54868-0451-00
 ;;9002226.02101,"563,54868-0451-00 ",.02)
 ;;54868-0451-00
 ;;9002226.02101,"563,54868-0451-01 ",.01)
 ;;54868-0451-01
 ;;9002226.02101,"563,54868-0451-01 ",.02)
 ;;54868-0451-01
 ;;9002226.02101,"563,54868-0451-02 ",.01)
 ;;54868-0451-02
 ;;9002226.02101,"563,54868-0451-02 ",.02)
 ;;54868-0451-02
 ;;9002226.02101,"563,54868-0451-03 ",.01)
 ;;54868-0451-03
 ;;9002226.02101,"563,54868-0451-03 ",.02)
 ;;54868-0451-03
 ;;9002226.02101,"563,54868-0452-03 ",.01)
 ;;54868-0452-03
 ;;9002226.02101,"563,54868-0452-03 ",.02)
 ;;54868-0452-03
 ;;9002226.02101,"563,54868-0453-00 ",.01)
 ;;54868-0453-00
 ;;9002226.02101,"563,54868-0453-00 ",.02)
 ;;54868-0453-00
 ;;9002226.02101,"563,54868-0453-01 ",.01)
 ;;54868-0453-01
 ;;9002226.02101,"563,54868-0453-01 ",.02)
 ;;54868-0453-01
 ;;9002226.02101,"563,54868-0453-02 ",.01)
 ;;54868-0453-02
 ;;9002226.02101,"563,54868-0453-02 ",.02)
 ;;54868-0453-02
 ;;9002226.02101,"563,54868-0453-04 ",.01)
 ;;54868-0453-04
 ;;9002226.02101,"563,54868-0453-04 ",.02)
 ;;54868-0453-04
 ;;9002226.02101,"563,54868-0453-05 ",.01)
 ;;54868-0453-05
 ;;9002226.02101,"563,54868-0453-05 ",.02)
 ;;54868-0453-05
 ;;9002226.02101,"563,54868-0494-00 ",.01)
 ;;54868-0494-00
 ;;9002226.02101,"563,54868-0494-00 ",.02)
 ;;54868-0494-00
 ;;9002226.02101,"563,54868-0494-01 ",.01)
 ;;54868-0494-01
 ;;9002226.02101,"563,54868-0494-01 ",.02)
 ;;54868-0494-01
 ;;9002226.02101,"563,54868-0494-02 ",.01)
 ;;54868-0494-02
 ;;9002226.02101,"563,54868-0494-02 ",.02)
 ;;54868-0494-02
 ;;9002226.02101,"563,54868-0495-00 ",.01)
 ;;54868-0495-00
 ;;9002226.02101,"563,54868-0495-00 ",.02)
 ;;54868-0495-00
 ;;9002226.02101,"563,54868-1261-00 ",.01)
 ;;54868-1261-00
 ;;9002226.02101,"563,54868-1261-00 ",.02)
 ;;54868-1261-00
 ;;9002226.02101,"563,54868-1262-00 ",.01)
 ;;54868-1262-00
 ;;9002226.02101,"563,54868-1262-00 ",.02)
 ;;54868-1262-00
 ;;9002226.02101,"563,54868-2702-00 ",.01)
 ;;54868-2702-00
 ;;9002226.02101,"563,54868-2702-00 ",.02)
 ;;54868-2702-00
 ;;9002226.02101,"563,54868-3653-00 ",.01)
 ;;54868-3653-00
 ;;9002226.02101,"563,54868-3653-00 ",.02)
 ;;54868-3653-00
 ;;9002226.02101,"563,54868-3795-00 ",.01)
 ;;54868-3795-00
 ;;9002226.02101,"563,54868-3795-00 ",.02)
 ;;54868-3795-00
 ;;9002226.02101,"563,54868-3796-00 ",.01)
 ;;54868-3796-00
 ;;9002226.02101,"563,54868-3796-00 ",.02)
 ;;54868-3796-00
 ;;9002226.02101,"563,54868-3797-00 ",.01)
 ;;54868-3797-00
 ;;9002226.02101,"563,54868-3797-00 ",.02)
 ;;54868-3797-00
 ;;9002226.02101,"563,54868-3799-00 ",.01)
 ;;54868-3799-00
 ;;9002226.02101,"563,54868-3799-00 ",.02)
 ;;54868-3799-00
 ;;9002226.02101,"563,54868-3800-00 ",.01)
 ;;54868-3800-00
 ;;9002226.02101,"563,54868-3800-00 ",.02)
 ;;54868-3800-00
 ;;9002226.02101,"563,54868-3857-00 ",.01)
 ;;54868-3857-00
 ;;9002226.02101,"563,54868-3857-00 ",.02)
 ;;54868-3857-00
 ;;9002226.02101,"563,54868-4030-00 ",.01)
 ;;54868-4030-00
 ;;9002226.02101,"563,54868-4030-00 ",.02)
 ;;54868-4030-00
 ;;9002226.02101,"563,54868-4030-01 ",.01)
 ;;54868-4030-01
 ;;9002226.02101,"563,54868-4030-01 ",.02)
 ;;54868-4030-01
 ;;9002226.02101,"563,54868-4089-00 ",.01)
 ;;54868-4089-00
 ;;9002226.02101,"563,54868-4089-00 ",.02)
 ;;54868-4089-00
 ;;9002226.02101,"563,54868-4170-00 ",.01)
 ;;54868-4170-00
 ;;9002226.02101,"563,54868-4170-00 ",.02)
 ;;54868-4170-00
 ;;9002226.02101,"563,54868-4384-00 ",.01)
 ;;54868-4384-00
 ;;9002226.02101,"563,54868-4384-00 ",.02)
 ;;54868-4384-00
 ;;9002226.02101,"563,54868-4386-00 ",.01)
 ;;54868-4386-00
 ;;9002226.02101,"563,54868-4386-00 ",.02)
 ;;54868-4386-00
 ;;9002226.02101,"563,54868-4462-00 ",.01)
 ;;54868-4462-00
 ;;9002226.02101,"563,54868-4462-00 ",.02)
 ;;54868-4462-00
 ;;9002226.02101,"563,54868-4463-00 ",.01)
 ;;54868-4463-00
 ;;9002226.02101,"563,54868-4463-00 ",.02)
 ;;54868-4463-00
 ;;9002226.02101,"563,54868-4671-00 ",.01)
 ;;54868-4671-00
 ;;9002226.02101,"563,54868-4671-00 ",.02)
 ;;54868-4671-00
 ;;9002226.02101,"563,54868-4679-00 ",.01)
 ;;54868-4679-00
 ;;9002226.02101,"563,54868-4679-00 ",.02)
 ;;54868-4679-00
 ;;9002226.02101,"563,54868-4830-00 ",.01)
 ;;54868-4830-00
 ;;9002226.02101,"563,54868-4830-00 ",.02)
 ;;54868-4830-00
 ;;9002226.02101,"563,54868-5480-00 ",.01)
 ;;54868-5480-00
 ;;9002226.02101,"563,54868-5480-00 ",.02)
 ;;54868-5480-00
 ;;9002226.02101,"563,55289-0047-25 ",.01)
 ;;55289-0047-25
 ;;9002226.02101,"563,55289-0047-25 ",.02)
 ;;55289-0047-25
 ;;9002226.02101,"563,55289-0047-30 ",.01)
 ;;55289-0047-30
 ;;9002226.02101,"563,55289-0047-30 ",.02)
 ;;55289-0047-30
 ;;9002226.02101,"563,55289-0047-42 ",.01)
 ;;55289-0047-42
 ;;9002226.02101,"563,55289-0047-42 ",.02)
 ;;55289-0047-42
