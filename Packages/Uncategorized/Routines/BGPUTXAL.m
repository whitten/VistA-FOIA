BGPUTXAL ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON APR 21, 2005 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;;;BGPU;;APR 21, 2005
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"393,53489-0355-05 ",.01)
 ;;53489-0355-05
 ;;9002226.02101,"393,53489-0355-05 ",.02)
 ;;53489-0355-05
 ;;9002226.02101,"393,53489-0356-01 ",.01)
 ;;53489-0356-01
 ;;9002226.02101,"393,53489-0356-01 ",.02)
 ;;53489-0356-01
 ;;9002226.02101,"393,53489-0356-05 ",.01)
 ;;53489-0356-05
 ;;9002226.02101,"393,53489-0356-05 ",.02)
 ;;53489-0356-05
 ;;9002226.02101,"393,53489-0366-01 ",.01)
 ;;53489-0366-01
 ;;9002226.02101,"393,53489-0366-01 ",.02)
 ;;53489-0366-01
 ;;9002226.02101,"393,53489-0367-10 ",.01)
 ;;53489-0367-10
 ;;9002226.02101,"393,53489-0367-10 ",.02)
 ;;53489-0367-10
 ;;9002226.02101,"393,53489-0430-01 ",.01)
 ;;53489-0430-01
 ;;9002226.02101,"393,53489-0430-01 ",.02)
 ;;53489-0430-01
 ;;9002226.02101,"393,53489-0431-01 ",.01)
 ;;53489-0431-01
 ;;9002226.02101,"393,53489-0431-01 ",.02)
 ;;53489-0431-01
 ;;9002226.02101,"393,53489-0529-01 ",.01)
 ;;53489-0529-01
 ;;9002226.02101,"393,53489-0529-01 ",.02)
 ;;53489-0529-01
 ;;9002226.02101,"393,53489-0529-10 ",.01)
 ;;53489-0529-10
 ;;9002226.02101,"393,53489-0529-10 ",.02)
 ;;53489-0529-10
 ;;9002226.02101,"393,53489-0530-01 ",.01)
 ;;53489-0530-01
 ;;9002226.02101,"393,53489-0530-01 ",.02)
 ;;53489-0530-01
 ;;9002226.02101,"393,53489-0530-10 ",.01)
 ;;53489-0530-10
 ;;9002226.02101,"393,53489-0530-10 ",.02)
 ;;53489-0530-10
 ;;9002226.02101,"393,54569-0442-00 ",.01)
 ;;54569-0442-00
 ;;9002226.02101,"393,54569-0442-00 ",.02)
 ;;54569-0442-00
 ;;9002226.02101,"393,54569-0557-00 ",.01)
 ;;54569-0557-00
 ;;9002226.02101,"393,54569-0557-00 ",.02)
 ;;54569-0557-00
 ;;9002226.02101,"393,54569-0557-01 ",.01)
 ;;54569-0557-01
 ;;9002226.02101,"393,54569-0557-01 ",.02)
 ;;54569-0557-01
 ;;9002226.02101,"393,54569-0557-03 ",.01)
 ;;54569-0557-03
 ;;9002226.02101,"393,54569-0557-03 ",.02)
 ;;54569-0557-03
 ;;9002226.02101,"393,54569-0559-00 ",.01)
 ;;54569-0559-00
 ;;9002226.02101,"393,54569-0559-00 ",.02)
 ;;54569-0559-00
 ;;9002226.02101,"393,54569-0559-01 ",.01)
 ;;54569-0559-01
 ;;9002226.02101,"393,54569-0559-01 ",.02)
 ;;54569-0559-01
 ;;9002226.02101,"393,54569-0559-03 ",.01)
 ;;54569-0559-03
 ;;9002226.02101,"393,54569-0559-03 ",.02)
 ;;54569-0559-03
 ;;9002226.02101,"393,54569-0561-01 ",.01)
 ;;54569-0561-01
 ;;9002226.02101,"393,54569-0561-01 ",.02)
 ;;54569-0561-01
 ;;9002226.02101,"393,54569-0561-02 ",.01)
 ;;54569-0561-02
 ;;9002226.02101,"393,54569-0561-02 ",.02)
 ;;54569-0561-02
 ;;9002226.02101,"393,54569-0561-03 ",.01)
 ;;54569-0561-03
 ;;9002226.02101,"393,54569-0561-03 ",.02)
 ;;54569-0561-03
 ;;9002226.02101,"393,54569-2499-00 ",.01)
 ;;54569-2499-00
 ;;9002226.02101,"393,54569-2499-00 ",.02)
 ;;54569-2499-00
 ;;9002226.02101,"393,54569-3432-00 ",.01)
 ;;54569-3432-00
 ;;9002226.02101,"393,54569-3432-00 ",.02)
 ;;54569-3432-00
 ;;9002226.02101,"393,54569-3432-01 ",.01)
 ;;54569-3432-01
 ;;9002226.02101,"393,54569-3432-01 ",.02)
 ;;54569-3432-01
 ;;9002226.02101,"393,54569-3432-03 ",.01)
 ;;54569-3432-03
 ;;9002226.02101,"393,54569-3432-03 ",.02)
 ;;54569-3432-03
 ;;9002226.02101,"393,54569-3432-04 ",.01)
 ;;54569-3432-04
 ;;9002226.02101,"393,54569-3432-04 ",.02)
 ;;54569-3432-04
 ;;9002226.02101,"393,54569-3432-05 ",.01)
 ;;54569-3432-05
 ;;9002226.02101,"393,54569-3432-05 ",.02)
 ;;54569-3432-05
 ;;9002226.02101,"393,54569-3654-00 ",.01)
 ;;54569-3654-00
 ;;9002226.02101,"393,54569-3654-00 ",.02)
 ;;54569-3654-00
 ;;9002226.02101,"393,54569-3654-03 ",.01)
 ;;54569-3654-03
 ;;9002226.02101,"393,54569-3654-03 ",.02)
 ;;54569-3654-03
 ;;9002226.02101,"393,54569-3787-00 ",.01)
 ;;54569-3787-00
 ;;9002226.02101,"393,54569-3787-00 ",.02)
 ;;54569-3787-00
 ;;9002226.02101,"393,54569-3787-01 ",.01)
 ;;54569-3787-01
 ;;9002226.02101,"393,54569-3787-01 ",.02)
 ;;54569-3787-01
 ;;9002226.02101,"393,54569-3787-02 ",.01)
 ;;54569-3787-02
 ;;9002226.02101,"393,54569-3787-02 ",.02)
 ;;54569-3787-02
 ;;9002226.02101,"393,54569-3790-00 ",.01)
 ;;54569-3790-00
 ;;9002226.02101,"393,54569-3790-00 ",.02)
 ;;54569-3790-00
 ;;9002226.02101,"393,54569-3885-00 ",.01)
 ;;54569-3885-00
 ;;9002226.02101,"393,54569-3885-00 ",.02)
 ;;54569-3885-00
 ;;9002226.02101,"393,54569-3885-02 ",.01)
 ;;54569-3885-02
 ;;9002226.02101,"393,54569-3885-02 ",.02)
 ;;54569-3885-02
 ;;9002226.02101,"393,54569-4288-00 ",.01)
 ;;54569-4288-00
 ;;9002226.02101,"393,54569-4288-00 ",.02)
 ;;54569-4288-00
 ;;9002226.02101,"393,54569-8574-00 ",.01)
 ;;54569-8574-00
 ;;9002226.02101,"393,54569-8574-00 ",.02)
 ;;54569-8574-00
 ;;9002226.02101,"393,54569-8591-00 ",.01)
 ;;54569-8591-00
 ;;9002226.02101,"393,54569-8591-00 ",.02)
 ;;54569-8591-00
 ;;9002226.02101,"393,54868-0052-02 ",.01)
 ;;54868-0052-02
 ;;9002226.02101,"393,54868-0052-02 ",.02)
 ;;54868-0052-02
 ;;9002226.02101,"393,54868-0053-07 ",.01)
 ;;54868-0053-07
 ;;9002226.02101,"393,54868-0053-07 ",.02)
 ;;54868-0053-07
 ;;9002226.02101,"393,54868-0293-00 ",.01)
 ;;54868-0293-00
 ;;9002226.02101,"393,54868-0293-00 ",.02)
 ;;54868-0293-00
 ;;9002226.02101,"393,54868-0293-01 ",.01)
 ;;54868-0293-01
 ;;9002226.02101,"393,54868-0293-01 ",.02)
 ;;54868-0293-01
 ;;9002226.02101,"393,54868-0293-03 ",.01)
 ;;54868-0293-03
 ;;9002226.02101,"393,54868-0293-03 ",.02)
 ;;54868-0293-03
