BGP7LXNB ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON AUG 29, 2006 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"631,49999-0130-15 ",.01)
 ;;49999-0130-15
 ;;9002226.02101,"631,49999-0130-15 ",.02)
 ;;49999-0130-15
 ;;9002226.02101,"631,49999-0130-20 ",.01)
 ;;49999-0130-20
 ;;9002226.02101,"631,49999-0130-20 ",.02)
 ;;49999-0130-20
 ;;9002226.02101,"631,49999-0130-21 ",.01)
 ;;49999-0130-21
 ;;9002226.02101,"631,49999-0130-21 ",.02)
 ;;49999-0130-21
 ;;9002226.02101,"631,49999-0130-30 ",.01)
 ;;49999-0130-30
 ;;9002226.02101,"631,49999-0130-30 ",.02)
 ;;49999-0130-30
 ;;9002226.02101,"631,49999-0314-04 ",.01)
 ;;49999-0314-04
 ;;9002226.02101,"631,49999-0314-04 ",.02)
 ;;49999-0314-04
 ;;9002226.02101,"631,49999-0416-02 ",.01)
 ;;49999-0416-02
 ;;9002226.02101,"631,49999-0416-02 ",.02)
 ;;49999-0416-02
 ;;9002226.02101,"631,49999-0429-20 ",.01)
 ;;49999-0429-20
 ;;9002226.02101,"631,49999-0429-20 ",.02)
 ;;49999-0429-20
 ;;9002226.02101,"631,50111-0311-01 ",.01)
 ;;50111-0311-01
 ;;9002226.02101,"631,50111-0311-01 ",.02)
 ;;50111-0311-01
 ;;9002226.02101,"631,50111-0311-03 ",.01)
 ;;50111-0311-03
 ;;9002226.02101,"631,50111-0311-03 ",.02)
 ;;50111-0311-03
 ;;9002226.02101,"631,50111-0372-01 ",.01)
 ;;50111-0372-01
 ;;9002226.02101,"631,50111-0372-01 ",.02)
 ;;50111-0372-01
 ;;9002226.02101,"631,50111-0372-02 ",.01)
 ;;50111-0372-02
 ;;9002226.02101,"631,50111-0372-02 ",.02)
 ;;50111-0372-02
 ;;9002226.02101,"631,50111-0372-03 ",.01)
 ;;50111-0372-03
 ;;9002226.02101,"631,50111-0372-03 ",.02)
 ;;50111-0372-03
 ;;9002226.02101,"631,50111-0373-01 ",.01)
 ;;50111-0373-01
 ;;9002226.02101,"631,50111-0373-01 ",.02)
 ;;50111-0373-01
 ;;9002226.02101,"631,50111-0373-02 ",.01)
 ;;50111-0373-02
 ;;9002226.02101,"631,50111-0373-02 ",.02)
 ;;50111-0373-02
 ;;9002226.02101,"631,50111-0373-03 ",.01)
 ;;50111-0373-03
 ;;9002226.02101,"631,50111-0373-03 ",.02)
 ;;50111-0373-03
 ;;9002226.02101,"631,50428-2325-31 ",.01)
 ;;50428-2325-31
 ;;9002226.02101,"631,50428-2325-31 ",.02)
 ;;50428-2325-31
 ;;9002226.02101,"631,51079-0202-20 ",.01)
 ;;51079-0202-20
 ;;9002226.02101,"631,51079-0202-20 ",.02)
 ;;51079-0202-20
 ;;9002226.02101,"631,51079-0203-20 ",.01)
 ;;51079-0203-20
 ;;9002226.02101,"631,51079-0203-20 ",.02)
 ;;51079-0203-20
 ;;9002226.02101,"631,52544-0838-01 ",.01)
 ;;52544-0838-01
 ;;9002226.02101,"631,52544-0838-01 ",.02)
 ;;52544-0838-01
 ;;9002226.02101,"631,52544-0838-05 ",.01)
 ;;52544-0838-05
 ;;9002226.02101,"631,52544-0838-05 ",.02)
 ;;52544-0838-05
 ;;9002226.02101,"631,52959-0404-10 ",.01)
 ;;52959-0404-10
 ;;9002226.02101,"631,52959-0404-10 ",.02)
 ;;52959-0404-10
 ;;9002226.02101,"631,52959-0404-20 ",.01)
 ;;52959-0404-20
 ;;9002226.02101,"631,52959-0404-20 ",.02)
 ;;52959-0404-20
 ;;9002226.02101,"631,52959-0405-16 ",.01)
 ;;52959-0405-16
 ;;9002226.02101,"631,52959-0405-16 ",.02)
 ;;52959-0405-16
 ;;9002226.02101,"631,52959-0405-30 ",.01)
 ;;52959-0405-30
 ;;9002226.02101,"631,52959-0405-30 ",.02)
 ;;52959-0405-30
 ;;9002226.02101,"631,52959-0405-40 ",.01)
 ;;52959-0405-40
 ;;9002226.02101,"631,52959-0405-40 ",.02)
 ;;52959-0405-40
 ;;9002226.02101,"631,52959-0700-04 ",.01)
 ;;52959-0700-04
 ;;9002226.02101,"631,52959-0700-04 ",.02)
 ;;52959-0700-04
 ;;9002226.02101,"631,52959-0700-06 ",.01)
 ;;52959-0700-06
 ;;9002226.02101,"631,52959-0700-06 ",.02)
 ;;52959-0700-06
 ;;9002226.02101,"631,52959-0700-08 ",.01)
 ;;52959-0700-08
 ;;9002226.02101,"631,52959-0700-08 ",.02)
 ;;52959-0700-08
 ;;9002226.02101,"631,54569-0203-00 ",.01)
 ;;54569-0203-00
 ;;9002226.02101,"631,54569-0203-00 ",.02)
 ;;54569-0203-00
 ;;9002226.02101,"631,54569-0203-01 ",.01)
 ;;54569-0203-01
 ;;9002226.02101,"631,54569-0203-01 ",.02)
 ;;54569-0203-01
 ;;9002226.02101,"631,54569-0203-40 ",.01)
 ;;54569-0203-40
 ;;9002226.02101,"631,54569-0203-40 ",.02)
 ;;54569-0203-40
 ;;9002226.02101,"631,54569-2017-01 ",.01)
 ;;54569-2017-01
 ;;9002226.02101,"631,54569-2017-01 ",.02)
 ;;54569-2017-01
 ;;9002226.02101,"631,54569-4583-00 ",.01)
 ;;54569-4583-00
 ;;9002226.02101,"631,54569-4583-00 ",.02)
 ;;54569-4583-00
 ;;9002226.02101,"631,54569-5188-00 ",.01)
 ;;54569-5188-00
 ;;9002226.02101,"631,54569-5188-00 ",.02)
 ;;54569-5188-00
 ;;9002226.02101,"631,54569-5189-00 ",.01)
 ;;54569-5189-00
 ;;9002226.02101,"631,54569-5189-00 ",.02)
 ;;54569-5189-00
 ;;9002226.02101,"631,54569-5576-00 ",.01)
 ;;54569-5576-00
 ;;9002226.02101,"631,54569-5576-00 ",.02)
 ;;54569-5576-00
 ;;9002226.02101,"631,54868-0036-00 ",.01)
 ;;54868-0036-00
 ;;9002226.02101,"631,54868-0036-00 ",.02)
 ;;54868-0036-00
 ;;9002226.02101,"631,54868-0036-02 ",.01)
 ;;54868-0036-02
 ;;9002226.02101,"631,54868-0036-02 ",.02)
 ;;54868-0036-02
 ;;9002226.02101,"631,54868-0036-04 ",.01)
 ;;54868-0036-04
 ;;9002226.02101,"631,54868-0036-04 ",.02)
 ;;54868-0036-04
 ;;9002226.02101,"631,54868-0042-04 ",.01)
 ;;54868-0042-04
 ;;9002226.02101,"631,54868-0042-04 ",.02)
 ;;54868-0042-04
 ;;9002226.02101,"631,54868-0473-03 ",.01)
 ;;54868-0473-03
 ;;9002226.02101,"631,54868-0473-03 ",.02)
 ;;54868-0473-03
 ;;9002226.02101,"631,54868-0473-04 ",.01)
 ;;54868-0473-04
 ;;9002226.02101,"631,54868-0473-04 ",.02)
 ;;54868-0473-04
 ;;9002226.02101,"631,54868-0473-05 ",.01)
 ;;54868-0473-05
 ;;9002226.02101,"631,54868-0473-05 ",.02)
 ;;54868-0473-05
 ;;9002226.02101,"631,54868-0473-06 ",.01)
 ;;54868-0473-06
 ;;9002226.02101,"631,54868-0473-06 ",.02)
 ;;54868-0473-06
 ;;9002226.02101,"631,54868-0877-01 ",.01)
 ;;54868-0877-01
 ;;9002226.02101,"631,54868-0877-01 ",.02)
 ;;54868-0877-01
 ;;9002226.02101,"631,54868-1990-00 ",.01)
 ;;54868-1990-00
 ;;9002226.02101,"631,54868-1990-00 ",.02)
 ;;54868-1990-00
 ;;9002226.02101,"631,54868-2366-02 ",.01)
 ;;54868-2366-02
 ;;9002226.02101,"631,54868-2366-02 ",.02)
 ;;54868-2366-02
 ;;9002226.02101,"631,54868-2366-03 ",.01)
 ;;54868-2366-03
 ;;9002226.02101,"631,54868-2366-03 ",.02)
 ;;54868-2366-03
 ;;9002226.02101,"631,54868-2366-04 ",.01)
 ;;54868-2366-04
 ;;9002226.02101,"631,54868-2366-04 ",.02)
 ;;54868-2366-04
 ;;9002226.02101,"631,54868-2366-05 ",.01)
 ;;54868-2366-05
 ;;9002226.02101,"631,54868-2366-05 ",.02)
 ;;54868-2366-05
 ;;9002226.02101,"631,54868-2366-06 ",.01)
 ;;54868-2366-06
 ;;9002226.02101,"631,54868-2366-06 ",.02)
 ;;54868-2366-06
 ;;9002226.02101,"631,54868-2366-07 ",.01)
 ;;54868-2366-07
 ;;9002226.02101,"631,54868-2366-07 ",.02)
 ;;54868-2366-07
 ;;9002226.02101,"631,55045-1250-02 ",.01)
 ;;55045-1250-02
 ;;9002226.02101,"631,55045-1250-02 ",.02)
 ;;55045-1250-02
 ;;9002226.02101,"631,55045-1250-04 ",.01)
 ;;55045-1250-04
 ;;9002226.02101,"631,55045-1250-04 ",.02)
 ;;55045-1250-04
 ;;9002226.02101,"631,55045-1506-00 ",.01)
 ;;55045-1506-00
 ;;9002226.02101,"631,55045-1506-00 ",.02)
 ;;55045-1506-00
 ;;9002226.02101,"631,55045-1506-09 ",.01)
 ;;55045-1506-09
 ;;9002226.02101,"631,55045-1506-09 ",.02)
 ;;55045-1506-09
 ;;9002226.02101,"631,55045-1850-02 ",.01)
 ;;55045-1850-02
 ;;9002226.02101,"631,55045-1850-02 ",.02)
 ;;55045-1850-02
 ;;9002226.02101,"631,55045-1850-03 ",.01)
 ;;55045-1850-03
 ;;9002226.02101,"631,55045-1850-03 ",.02)
 ;;55045-1850-03
 ;;9002226.02101,"631,55045-1850-05 ",.01)
 ;;55045-1850-05
 ;;9002226.02101,"631,55045-1850-05 ",.02)
 ;;55045-1850-05
 ;;9002226.02101,"631,55045-1850-08 ",.01)
 ;;55045-1850-08
 ;;9002226.02101,"631,55045-1850-08 ",.02)
 ;;55045-1850-08
 ;;9002226.02101,"631,55045-2079-08 ",.01)
 ;;55045-2079-08
 ;;9002226.02101,"631,55045-2079-08 ",.02)
 ;;55045-2079-08
 ;;9002226.02101,"631,55045-2341-05 ",.01)
 ;;55045-2341-05
 ;;9002226.02101,"631,55045-2341-05 ",.02)
 ;;55045-2341-05
 ;;9002226.02101,"631,55045-2530-03 ",.01)
 ;;55045-2530-03
 ;;9002226.02101,"631,55045-2530-03 ",.02)
 ;;55045-2530-03
 ;;9002226.02101,"631,55045-2530-04 ",.01)
 ;;55045-2530-04
 ;;9002226.02101,"631,55045-2530-04 ",.02)
 ;;55045-2530-04
 ;;9002226.02101,"631,55045-2530-05 ",.01)
 ;;55045-2530-05
 ;;9002226.02101,"631,55045-2530-05 ",.02)
 ;;55045-2530-05
 ;;9002226.02101,"631,55175-2540-01 ",.01)
 ;;55175-2540-01
 ;;9002226.02101,"631,55175-2540-01 ",.02)
 ;;55175-2540-01
 ;;9002226.02101,"631,55175-2540-02 ",.01)
 ;;55175-2540-02
 ;;9002226.02101,"631,55175-2540-02 ",.02)
 ;;55175-2540-02
 ;;9002226.02101,"631,55175-2540-03 ",.01)
 ;;55175-2540-03
 ;;9002226.02101,"631,55175-2540-03 ",.02)
 ;;55175-2540-03
 ;;9002226.02101,"631,55175-2540-04 ",.01)
 ;;55175-2540-04
 ;;9002226.02101,"631,55175-2540-04 ",.02)
 ;;55175-2540-04
 ;;9002226.02101,"631,55175-2540-05 ",.01)
 ;;55175-2540-05
 ;;9002226.02101,"631,55175-2540-05 ",.02)
 ;;55175-2540-05
 ;;9002226.02101,"631,55175-2540-08 ",.01)
 ;;55175-2540-08
 ;;9002226.02101,"631,55175-2540-08 ",.02)
 ;;55175-2540-08
 ;;9002226.02101,"631,55175-2623-00 ",.01)
 ;;55175-2623-00
 ;;9002226.02101,"631,55175-2623-00 ",.02)
 ;;55175-2623-00
 ;;9002226.02101,"631,55175-2623-01 ",.01)
 ;;55175-2623-01
 ;;9002226.02101,"631,55175-2623-01 ",.02)
 ;;55175-2623-01
 ;;9002226.02101,"631,55175-2623-04 ",.01)
 ;;55175-2623-04
 ;;9002226.02101,"631,55175-2623-04 ",.02)
 ;;55175-2623-04
 ;;9002226.02101,"631,55175-2624-02 ",.01)
 ;;55175-2624-02
 ;;9002226.02101,"631,55175-2624-02 ",.02)
 ;;55175-2624-02
 ;;9002226.02101,"631,55175-2624-08 ",.01)
 ;;55175-2624-08
 ;;9002226.02101,"631,55175-2624-08 ",.02)
 ;;55175-2624-08
 ;;9002226.02101,"631,55175-5210-00 ",.01)
 ;;55175-5210-00
 ;;9002226.02101,"631,55175-5210-00 ",.02)
 ;;55175-5210-00
 ;;9002226.02101,"631,55175-5210-02 ",.01)
 ;;55175-5210-02
 ;;9002226.02101,"631,55175-5210-02 ",.02)
 ;;55175-5210-02
 ;;9002226.02101,"631,55175-5210-03 ",.01)
 ;;55175-5210-03
 ;;9002226.02101,"631,55175-5210-03 ",.02)
 ;;55175-5210-03
 ;;9002226.02101,"631,55175-5210-05 ",.01)
 ;;55175-5210-05
 ;;9002226.02101,"631,55175-5210-05 ",.02)
 ;;55175-5210-05
 ;;9002226.02101,"631,55175-5210-08 ",.01)
 ;;55175-5210-08
 ;;9002226.02101,"631,55175-5210-08 ",.02)
 ;;55175-5210-08
 ;;9002226.02101,"631,55289-0066-90 ",.01)
 ;;55289-0066-90
 ;;9002226.02101,"631,55289-0066-90 ",.02)
 ;;55289-0066-90
 ;;9002226.02101,"631,55289-0066-97 ",.01)
 ;;55289-0066-97
 ;;9002226.02101,"631,55289-0066-97 ",.02)
 ;;55289-0066-97
 ;;9002226.02101,"631,55289-0203-10 ",.01)
 ;;55289-0203-10
 ;;9002226.02101,"631,55289-0203-10 ",.02)
 ;;55289-0203-10
 ;;9002226.02101,"631,55829-0196-10 ",.01)
 ;;55829-0196-10
 ;;9002226.02101,"631,55829-0196-10 ",.02)
 ;;55829-0196-10
 ;;9002226.02101,"631,55829-0197-10 ",.01)
 ;;55829-0197-10
 ;;9002226.02101,"631,55829-0197-10 ",.02)
 ;;55829-0197-10
 ;;9002226.02101,"631,55887-0880-28 ",.01)
 ;;55887-0880-28
 ;;9002226.02101,"631,55887-0880-28 ",.02)
 ;;55887-0880-28
 ;;9002226.02101,"631,55887-0880-30 ",.01)
 ;;55887-0880-30
 ;;9002226.02101,"631,55887-0880-30 ",.02)
 ;;55887-0880-30
 ;;9002226.02101,"631,57480-0323-06 ",.01)
 ;;57480-0323-06
 ;;9002226.02101,"631,57480-0323-06 ",.02)
 ;;57480-0323-06
 ;;9002226.02101,"631,57866-6590-05 ",.01)
 ;;57866-6590-05
 ;;9002226.02101,"631,57866-6590-05 ",.02)
 ;;57866-6590-05
 ;;9002226.02101,"631,57866-7382-04 ",.01)
 ;;57866-7382-04
 ;;9002226.02101,"631,57866-7382-04 ",.02)
 ;;57866-7382-04
 ;;9002226.02101,"631,57866-7382-05 ",.01)
 ;;57866-7382-05
 ;;9002226.02101,"631,57866-7382-05 ",.02)
 ;;57866-7382-05
 ;;9002226.02101,"631,58016-0554-00 ",.01)
 ;;58016-0554-00
 ;;9002226.02101,"631,58016-0554-00 ",.02)
 ;;58016-0554-00
 ;;9002226.02101,"631,58864-0100-30 ",.01)
 ;;58864-0100-30
 ;;9002226.02101,"631,58864-0100-30 ",.02)
 ;;58864-0100-30
 ;;9002226.02101,"631,58864-0371-14 ",.01)
 ;;58864-0371-14
 ;;9002226.02101,"631,58864-0371-14 ",.02)
 ;;58864-0371-14
 ;;9002226.02101,"631,58864-0371-20 ",.01)
 ;;58864-0371-20
 ;;9002226.02101,"631,58864-0371-20 ",.02)
 ;;58864-0371-20
 ;;9002226.02101,"631,58864-0371-28 ",.01)
 ;;58864-0371-28
 ;;9002226.02101,"631,58864-0371-28 ",.02)
 ;;58864-0371-28
 ;;9002226.02101,"631,58864-0651-14 ",.01)
 ;;58864-0651-14
 ;;9002226.02101,"631,58864-0651-14 ",.02)
 ;;58864-0651-14
 ;;9002226.02101,"631,60346-0289-10 ",.01)
 ;;60346-0289-10
 ;;9002226.02101,"631,60346-0289-10 ",.02)
 ;;60346-0289-10
 ;;9002226.02101,"631,60346-0289-14 ",.01)
 ;;60346-0289-14
 ;;9002226.02101,"631,60346-0289-14 ",.02)
 ;;60346-0289-14
 ;;9002226.02101,"631,60346-0289-20 ",.01)
 ;;60346-0289-20
 ;;9002226.02101,"631,60346-0289-20 ",.02)
 ;;60346-0289-20
 ;;9002226.02101,"631,60346-0289-28 ",.01)
 ;;60346-0289-28
 ;;9002226.02101,"631,60346-0289-28 ",.02)
 ;;60346-0289-28
 ;;9002226.02101,"631,60346-0446-08 ",.01)
 ;;60346-0446-08
 ;;9002226.02101,"631,60346-0446-08 ",.02)
 ;;60346-0446-08
 ;;9002226.02101,"631,60346-0446-10 ",.01)
 ;;60346-0446-10
 ;;9002226.02101,"631,60346-0446-10 ",.02)
 ;;60346-0446-10
 ;;9002226.02101,"631,60346-0446-12 ",.01)
 ;;60346-0446-12
 ;;9002226.02101,"631,60346-0446-12 ",.02)
 ;;60346-0446-12
 ;;9002226.02101,"631,60346-0446-15 ",.01)
 ;;60346-0446-15
 ;;9002226.02101,"631,60346-0446-15 ",.02)
 ;;60346-0446-15
 ;;9002226.02101,"631,60346-0446-20 ",.01)
 ;;60346-0446-20
 ;;9002226.02101,"631,60346-0446-20 ",.02)
 ;;60346-0446-20
 ;;9002226.02101,"631,60346-0446-30 ",.01)
 ;;60346-0446-30
 ;;9002226.02101,"631,60346-0446-30 ",.02)
 ;;60346-0446-30
 ;;9002226.02101,"631,60346-0446-40 ",.01)
 ;;60346-0446-40
 ;;9002226.02101,"631,60346-0446-40 ",.02)
 ;;60346-0446-40
 ;;9002226.02101,"631,60346-0651-28 ",.01)
 ;;60346-0651-28
 ;;9002226.02101,"631,60346-0651-28 ",.02)
 ;;60346-0651-28
 ;;9002226.02101,"631,60346-0789-30 ",.01)
 ;;60346-0789-30
 ;;9002226.02101,"631,60346-0789-30 ",.02)
 ;;60346-0789-30
