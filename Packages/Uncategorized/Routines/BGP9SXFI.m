BGP9SXFI ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON MAR 25, 2009 ;
 ;;9.0;IHS CLINICAL REPORTING;;JUL 1, 2009
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"561,50383-0741-20 ",.01)
 ;;50383-0741-20
 ;;9002226.02101,"561,50383-0741-20 ",.02)
 ;;50383-0741-20
 ;;9002226.02101,"561,50383-0742-25 ",.01)
 ;;50383-0742-25
 ;;9002226.02101,"561,50383-0742-25 ",.02)
 ;;50383-0742-25
 ;;9002226.02101,"561,52959-0094-03 ",.01)
 ;;52959-0094-03
 ;;9002226.02101,"561,52959-0094-03 ",.02)
 ;;52959-0094-03
 ;;9002226.02101,"561,52959-0131-00 ",.01)
 ;;52959-0131-00
 ;;9002226.02101,"561,52959-0131-00 ",.02)
 ;;52959-0131-00
 ;;9002226.02101,"561,52959-0155-00 ",.01)
 ;;52959-0155-00
 ;;9002226.02101,"561,52959-0155-00 ",.02)
 ;;52959-0155-00
 ;;9002226.02101,"561,52959-0286-03 ",.01)
 ;;52959-0286-03
 ;;9002226.02101,"561,52959-0286-03 ",.02)
 ;;52959-0286-03
 ;;9002226.02101,"561,52959-0293-00 ",.01)
 ;;52959-0293-00
 ;;9002226.02101,"561,52959-0293-00 ",.02)
 ;;52959-0293-00
 ;;9002226.02101,"561,52959-0569-01 ",.01)
 ;;52959-0569-01
 ;;9002226.02101,"561,52959-0569-01 ",.02)
 ;;52959-0569-01
 ;;9002226.02101,"561,52959-0588-01 ",.01)
 ;;52959-0588-01
 ;;9002226.02101,"561,52959-0588-01 ",.02)
 ;;52959-0588-01
 ;;9002226.02101,"561,52959-0589-00 ",.01)
 ;;52959-0589-00
 ;;9002226.02101,"561,52959-0589-00 ",.02)
 ;;52959-0589-00
 ;;9002226.02101,"561,52959-0596-01 ",.01)
 ;;52959-0596-01
 ;;9002226.02101,"561,52959-0596-01 ",.02)
 ;;52959-0596-01
 ;;9002226.02101,"561,52959-0598-01 ",.01)
 ;;52959-0598-01
 ;;9002226.02101,"561,52959-0598-01 ",.02)
 ;;52959-0598-01
 ;;9002226.02101,"561,52959-0741-20 ",.01)
 ;;52959-0741-20
 ;;9002226.02101,"561,52959-0741-20 ",.02)
 ;;52959-0741-20
 ;;9002226.02101,"561,54569-0048-00 ",.01)
 ;;54569-0048-00
 ;;9002226.02101,"561,54569-0048-00 ",.02)
 ;;54569-0048-00
 ;;9002226.02101,"561,54569-0049-00 ",.01)
 ;;54569-0049-00
 ;;9002226.02101,"561,54569-0049-00 ",.02)
 ;;54569-0049-00
 ;;9002226.02101,"561,54569-0052-00 ",.01)
 ;;54569-0052-00
 ;;9002226.02101,"561,54569-0052-00 ",.02)
 ;;54569-0052-00
 ;;9002226.02101,"561,54569-0053-00 ",.01)
 ;;54569-0053-00
 ;;9002226.02101,"561,54569-0053-00 ",.02)
 ;;54569-0053-00
 ;;9002226.02101,"561,54569-0067-00 ",.01)
 ;;54569-0067-00
 ;;9002226.02101,"561,54569-0067-00 ",.02)
 ;;54569-0067-00
 ;;9002226.02101,"561,54569-1003-00 ",.01)
 ;;54569-1003-00
 ;;9002226.02101,"561,54569-1003-00 ",.02)
 ;;54569-1003-00
 ;;9002226.02101,"561,54569-1004-00 ",.01)
 ;;54569-1004-00
 ;;9002226.02101,"561,54569-1004-00 ",.02)
 ;;54569-1004-00
 ;;9002226.02101,"561,54569-1012-00 ",.01)
 ;;54569-1012-00
 ;;9002226.02101,"561,54569-1012-00 ",.02)
 ;;54569-1012-00
 ;;9002226.02101,"561,54569-1013-00 ",.01)
 ;;54569-1013-00
 ;;9002226.02101,"561,54569-1013-00 ",.02)
 ;;54569-1013-00
 ;;9002226.02101,"561,54569-1989-00 ",.01)
 ;;54569-1989-00
 ;;9002226.02101,"561,54569-1989-00 ",.02)
 ;;54569-1989-00
 ;;9002226.02101,"561,54569-2253-00 ",.01)
 ;;54569-2253-00
 ;;9002226.02101,"561,54569-2253-00 ",.02)
 ;;54569-2253-00
 ;;9002226.02101,"561,54569-2732-00 ",.01)
 ;;54569-2732-00
 ;;9002226.02101,"561,54569-2732-00 ",.02)
 ;;54569-2732-00
 ;;9002226.02101,"561,54569-3741-00 ",.01)
 ;;54569-3741-00
 ;;9002226.02101,"561,54569-3741-00 ",.02)
 ;;54569-3741-00
 ;;9002226.02101,"561,54569-3855-00 ",.01)
 ;;54569-3855-00
 ;;9002226.02101,"561,54569-3855-00 ",.02)
 ;;54569-3855-00
 ;;9002226.02101,"561,54569-3899-00 ",.01)
 ;;54569-3899-00
 ;;9002226.02101,"561,54569-3899-00 ",.02)
 ;;54569-3899-00
 ;;9002226.02101,"561,54569-3900-00 ",.01)
 ;;54569-3900-00
 ;;9002226.02101,"561,54569-3900-00 ",.02)
 ;;54569-3900-00
 ;;9002226.02101,"561,54569-3976-00 ",.01)
 ;;54569-3976-00
 ;;9002226.02101,"561,54569-3976-00 ",.02)
 ;;54569-3976-00
 ;;9002226.02101,"561,54569-4245-00 ",.01)
 ;;54569-4245-00
 ;;9002226.02101,"561,54569-4245-00 ",.02)
 ;;54569-4245-00
 ;;9002226.02101,"561,54569-4540-00 ",.01)
 ;;54569-4540-00
 ;;9002226.02101,"561,54569-4540-00 ",.02)
 ;;54569-4540-00
 ;;9002226.02101,"561,54569-4549-00 ",.01)
 ;;54569-4549-00
 ;;9002226.02101,"561,54569-4549-00 ",.02)
 ;;54569-4549-00
 ;;9002226.02101,"561,54569-4602-00 ",.01)
 ;;54569-4602-00
 ;;9002226.02101,"561,54569-4602-00 ",.02)
 ;;54569-4602-00
 ;;9002226.02101,"561,54569-4603-00 ",.01)
 ;;54569-4603-00
 ;;9002226.02101,"561,54569-4603-00 ",.02)
 ;;54569-4603-00
 ;;9002226.02101,"561,54569-4615-00 ",.01)
 ;;54569-4615-00
 ;;9002226.02101,"561,54569-4615-00 ",.02)
 ;;54569-4615-00
 ;;9002226.02101,"561,54569-4621-00 ",.01)
 ;;54569-4621-00
 ;;9002226.02101,"561,54569-4621-00 ",.02)
 ;;54569-4621-00
 ;;9002226.02101,"561,54569-4741-00 ",.01)
 ;;54569-4741-00
 ;;9002226.02101,"561,54569-4741-00 ",.02)
 ;;54569-4741-00
 ;;9002226.02101,"561,54569-4748-00 ",.01)
 ;;54569-4748-00
 ;;9002226.02101,"561,54569-4748-00 ",.02)
 ;;54569-4748-00
 ;;9002226.02101,"561,54569-4772-00 ",.01)
 ;;54569-4772-00
 ;;9002226.02101,"561,54569-4772-00 ",.02)
 ;;54569-4772-00
 ;;9002226.02101,"561,54569-4822-00 ",.01)
 ;;54569-4822-00
 ;;9002226.02101,"561,54569-4822-00 ",.02)
 ;;54569-4822-00
 ;;9002226.02101,"561,54569-4863-00 ",.01)
 ;;54569-4863-00
 ;;9002226.02101,"561,54569-4863-00 ",.02)
 ;;54569-4863-00
 ;;9002226.02101,"561,54569-4867-00 ",.01)
 ;;54569-4867-00
 ;;9002226.02101,"561,54569-4867-00 ",.02)
 ;;54569-4867-00
