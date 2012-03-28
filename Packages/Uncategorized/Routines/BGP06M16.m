BGP06M16 ;IHS/CMI/LAB-CREATED BY ^ATXSTX ON MAY 23, 2010;
 ;;10.0;IHS CLINICAL REPORTING;;JUN 18, 2010
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"977,53489-0529-10 ",.02)
 ;;53489-0529-10
 ;;9002226.02101,"977,53489-0530-01 ",.01)
 ;;53489-0530-01
 ;;9002226.02101,"977,53489-0530-01 ",.02)
 ;;53489-0530-01
 ;;9002226.02101,"977,53489-0530-10 ",.01)
 ;;53489-0530-10
 ;;9002226.02101,"977,53489-0530-10 ",.02)
 ;;53489-0530-10
 ;;9002226.02101,"977,53489-0531-01 ",.01)
 ;;53489-0531-01
 ;;9002226.02101,"977,53489-0531-01 ",.02)
 ;;53489-0531-01
 ;;9002226.02101,"977,53489-0532-01 ",.01)
 ;;53489-0532-01
 ;;9002226.02101,"977,53489-0532-01 ",.02)
 ;;53489-0532-01
 ;;9002226.02101,"977,53489-0536-01 ",.01)
 ;;53489-0536-01
 ;;9002226.02101,"977,53489-0536-01 ",.02)
 ;;53489-0536-01
 ;;9002226.02101,"977,53489-0536-10 ",.01)
 ;;53489-0536-10
 ;;9002226.02101,"977,53489-0536-10 ",.02)
 ;;53489-0536-10
 ;;9002226.02101,"977,53489-0555-01 ",.01)
 ;;53489-0555-01
 ;;9002226.02101,"977,53489-0555-01 ",.02)
 ;;53489-0555-01
 ;;9002226.02101,"977,53489-0555-07 ",.01)
 ;;53489-0555-07
 ;;9002226.02101,"977,53489-0555-07 ",.02)
 ;;53489-0555-07
 ;;9002226.02101,"977,53489-0556-01 ",.01)
 ;;53489-0556-01
 ;;9002226.02101,"977,53489-0556-01 ",.02)
 ;;53489-0556-01
 ;;9002226.02101,"977,53489-0556-07 ",.01)
 ;;53489-0556-07
 ;;9002226.02101,"977,53489-0556-07 ",.02)
 ;;53489-0556-07
 ;;9002226.02101,"977,53746-0220-05 ",.01)
 ;;53746-0220-05
 ;;9002226.02101,"977,53746-0220-05 ",.02)
 ;;53746-0220-05
 ;;9002226.02101,"977,53746-0220-79 ",.01)
 ;;53746-0220-79
 ;;9002226.02101,"977,53746-0220-79 ",.02)
 ;;53746-0220-79
 ;;9002226.02101,"977,54569-0442-00 ",.01)
 ;;54569-0442-00
 ;;9002226.02101,"977,54569-0442-00 ",.02)
 ;;54569-0442-00
 ;;9002226.02101,"977,54569-0557-00 ",.01)
 ;;54569-0557-00
 ;;9002226.02101,"977,54569-0557-00 ",.02)
 ;;54569-0557-00
 ;;9002226.02101,"977,54569-0557-01 ",.01)
 ;;54569-0557-01
 ;;9002226.02101,"977,54569-0557-01 ",.02)
 ;;54569-0557-01
 ;;9002226.02101,"977,54569-0557-03 ",.01)
 ;;54569-0557-03
 ;;9002226.02101,"977,54569-0557-03 ",.02)
 ;;54569-0557-03
 ;;9002226.02101,"977,54569-0559-00 ",.01)
 ;;54569-0559-00
 ;;9002226.02101,"977,54569-0559-00 ",.02)
 ;;54569-0559-00
 ;;9002226.02101,"977,54569-0559-01 ",.01)
 ;;54569-0559-01
 ;;9002226.02101,"977,54569-0559-01 ",.02)
 ;;54569-0559-01
 ;;9002226.02101,"977,54569-0559-03 ",.01)
 ;;54569-0559-03
 ;;9002226.02101,"977,54569-0559-03 ",.02)
 ;;54569-0559-03
 ;;9002226.02101,"977,54569-0561-01 ",.01)
 ;;54569-0561-01
 ;;9002226.02101,"977,54569-0561-01 ",.02)
 ;;54569-0561-01
 ;;9002226.02101,"977,54569-0561-02 ",.01)
 ;;54569-0561-02
 ;;9002226.02101,"977,54569-0561-02 ",.02)
 ;;54569-0561-02
 ;;9002226.02101,"977,54569-0561-03 ",.01)
 ;;54569-0561-03
 ;;9002226.02101,"977,54569-0561-03 ",.02)
 ;;54569-0561-03
 ;;9002226.02101,"977,54569-0563-00 ",.01)
 ;;54569-0563-00
 ;;9002226.02101,"977,54569-0563-00 ",.02)
 ;;54569-0563-00
 ;;9002226.02101,"977,54569-0564-00 ",.01)
 ;;54569-0564-00
 ;;9002226.02101,"977,54569-0564-00 ",.02)
 ;;54569-0564-00
 ;;9002226.02101,"977,54569-0590-00 ",.01)
 ;;54569-0590-00
 ;;9002226.02101,"977,54569-0590-00 ",.02)
 ;;54569-0590-00
 ;;9002226.02101,"977,54569-0596-00 ",.01)
 ;;54569-0596-00
 ;;9002226.02101,"977,54569-0596-00 ",.02)
 ;;54569-0596-00
 ;;9002226.02101,"977,54569-1634-00 ",.01)
 ;;54569-1634-00
 ;;9002226.02101,"977,54569-1634-00 ",.02)
 ;;54569-1634-00
 ;;9002226.02101,"977,54569-2499-00 ",.01)
 ;;54569-2499-00
 ;;9002226.02101,"977,54569-2499-00 ",.02)
 ;;54569-2499-00
 ;;9002226.02101,"977,54569-3097-00 ",.01)
 ;;54569-3097-00
 ;;9002226.02101,"977,54569-3097-00 ",.02)
 ;;54569-3097-00
 ;;9002226.02101,"977,54569-3432-00 ",.01)
 ;;54569-3432-00
 ;;9002226.02101,"977,54569-3432-00 ",.02)
 ;;54569-3432-00
 ;;9002226.02101,"977,54569-3432-01 ",.01)
 ;;54569-3432-01
 ;;9002226.02101,"977,54569-3432-01 ",.02)
 ;;54569-3432-01
 ;;9002226.02101,"977,54569-3432-03 ",.01)
 ;;54569-3432-03
 ;;9002226.02101,"977,54569-3432-03 ",.02)
 ;;54569-3432-03
 ;;9002226.02101,"977,54569-3432-04 ",.01)
 ;;54569-3432-04
 ;;9002226.02101,"977,54569-3432-04 ",.02)
 ;;54569-3432-04
 ;;9002226.02101,"977,54569-3432-05 ",.01)
 ;;54569-3432-05
 ;;9002226.02101,"977,54569-3432-05 ",.02)
 ;;54569-3432-05
 ;;9002226.02101,"977,54569-3654-00 ",.01)
 ;;54569-3654-00
 ;;9002226.02101,"977,54569-3654-00 ",.02)
 ;;54569-3654-00
 ;;9002226.02101,"977,54569-3654-03 ",.01)
 ;;54569-3654-03
 ;;9002226.02101,"977,54569-3654-03 ",.02)
 ;;54569-3654-03
 ;;9002226.02101,"977,54569-3787-00 ",.01)
 ;;54569-3787-00
 ;;9002226.02101,"977,54569-3787-00 ",.02)
 ;;54569-3787-00
 ;;9002226.02101,"977,54569-3787-01 ",.01)
 ;;54569-3787-01
 ;;9002226.02101,"977,54569-3787-01 ",.02)
 ;;54569-3787-01
 ;;9002226.02101,"977,54569-3787-02 ",.01)
 ;;54569-3787-02
 ;;9002226.02101,"977,54569-3787-02 ",.02)
 ;;54569-3787-02
 ;;9002226.02101,"977,54569-3788-00 ",.01)
 ;;54569-3788-00
 ;;9002226.02101,"977,54569-3788-00 ",.02)
 ;;54569-3788-00
 ;;9002226.02101,"977,54569-3788-01 ",.01)
 ;;54569-3788-01
 ;;9002226.02101,"977,54569-3788-01 ",.02)
 ;;54569-3788-01
 ;;9002226.02101,"977,54569-3789-00 ",.01)
 ;;54569-3789-00
 ;;9002226.02101,"977,54569-3789-00 ",.02)
 ;;54569-3789-00
 ;;9002226.02101,"977,54569-3790-00 ",.01)
 ;;54569-3790-00
 ;;9002226.02101,"977,54569-3790-00 ",.02)
 ;;54569-3790-00
 ;;9002226.02101,"977,54569-3791-00 ",.01)
 ;;54569-3791-00
 ;;9002226.02101,"977,54569-3791-00 ",.02)
 ;;54569-3791-00
 ;;9002226.02101,"977,54569-3885-00 ",.01)
 ;;54569-3885-00
 ;;9002226.02101,"977,54569-3885-00 ",.02)
 ;;54569-3885-00
 ;;9002226.02101,"977,54569-3885-02 ",.01)
 ;;54569-3885-02
 ;;9002226.02101,"977,54569-3885-02 ",.02)
 ;;54569-3885-02
 ;;9002226.02101,"977,54569-4441-00 ",.01)
 ;;54569-4441-00
 ;;9002226.02101,"977,54569-4441-00 ",.02)
 ;;54569-4441-00
 ;;9002226.02101,"977,54569-4442-00 ",.01)
 ;;54569-4442-00
 ;;9002226.02101,"977,54569-4442-00 ",.02)
 ;;54569-4442-00
 ;;9002226.02101,"977,54569-4707-00 ",.01)
 ;;54569-4707-00
 ;;9002226.02101,"977,54569-4707-00 ",.02)
 ;;54569-4707-00
 ;;9002226.02101,"977,54569-4708-00 ",.01)
 ;;54569-4708-00
 ;;9002226.02101,"977,54569-4708-00 ",.02)
 ;;54569-4708-00
 ;;9002226.02101,"977,54569-4718-00 ",.01)
 ;;54569-4718-00
 ;;9002226.02101,"977,54569-4718-00 ",.02)
 ;;54569-4718-00
 ;;9002226.02101,"977,54569-4718-01 ",.01)
 ;;54569-4718-01
 ;;9002226.02101,"977,54569-4718-01 ",.02)
 ;;54569-4718-01
 ;;9002226.02101,"977,54569-5368-00 ",.01)
 ;;54569-5368-00
 ;;9002226.02101,"977,54569-5368-00 ",.02)
 ;;54569-5368-00
 ;;9002226.02101,"977,54569-5385-00 ",.01)
 ;;54569-5385-00
 ;;9002226.02101,"977,54569-5385-00 ",.02)
 ;;54569-5385-00
 ;;9002226.02101,"977,54569-5404-00 ",.01)
 ;;54569-5404-00
 ;;9002226.02101,"977,54569-5404-00 ",.02)
 ;;54569-5404-00
 ;;9002226.02101,"977,54569-5404-01 ",.01)
 ;;54569-5404-01
 ;;9002226.02101,"977,54569-5404-01 ",.02)
 ;;54569-5404-01
 ;;9002226.02101,"977,54569-5417-00 ",.01)
 ;;54569-5417-00
 ;;9002226.02101,"977,54569-5417-00 ",.02)
 ;;54569-5417-00
 ;;9002226.02101,"977,54569-5417-01 ",.01)
 ;;54569-5417-01
 ;;9002226.02101,"977,54569-5417-01 ",.02)
 ;;54569-5417-01
 ;;9002226.02101,"977,54569-5419-00 ",.01)
 ;;54569-5419-00
 ;;9002226.02101,"977,54569-5419-00 ",.02)
 ;;54569-5419-00
 ;;9002226.02101,"977,54569-5419-01 ",.01)
 ;;54569-5419-01
 ;;9002226.02101,"977,54569-5419-01 ",.02)
 ;;54569-5419-01
 ;;9002226.02101,"977,54569-5870-00 ",.01)
 ;;54569-5870-00
 ;;9002226.02101,"977,54569-5870-00 ",.02)
 ;;54569-5870-00
 ;;9002226.02101,"977,54569-5954-00 ",.01)
 ;;54569-5954-00
 ;;9002226.02101,"977,54569-5954-00 ",.02)
 ;;54569-5954-00
 ;;9002226.02101,"977,54569-5961-00 ",.01)
 ;;54569-5961-00
 ;;9002226.02101,"977,54569-5961-00 ",.02)
 ;;54569-5961-00
 ;;9002226.02101,"977,54569-8532-00 ",.01)
 ;;54569-8532-00
 ;;9002226.02101,"977,54569-8532-00 ",.02)
 ;;54569-8532-00
 ;;9002226.02101,"977,54569-8543-00 ",.01)
 ;;54569-8543-00
 ;;9002226.02101,"977,54569-8543-00 ",.02)
 ;;54569-8543-00
 ;;9002226.02101,"977,54569-8545-00 ",.01)
 ;;54569-8545-00
 ;;9002226.02101,"977,54569-8545-00 ",.02)
 ;;54569-8545-00
 ;;9002226.02101,"977,54569-8545-01 ",.01)
 ;;54569-8545-01
 ;;9002226.02101,"977,54569-8545-01 ",.02)
 ;;54569-8545-01
 ;;9002226.02101,"977,54569-8574-00 ",.01)
 ;;54569-8574-00
 ;;9002226.02101,"977,54569-8574-00 ",.02)
 ;;54569-8574-00
 ;;9002226.02101,"977,54569-8591-00 ",.01)
 ;;54569-8591-00
 ;;9002226.02101,"977,54569-8591-00 ",.02)
 ;;54569-8591-00
 ;;9002226.02101,"977,54738-0468-01 ",.01)
 ;;54738-0468-01
 ;;9002226.02101,"977,54738-0468-01 ",.02)
 ;;54738-0468-01
 ;;9002226.02101,"977,54738-0468-03 ",.01)
 ;;54738-0468-03
 ;;9002226.02101,"977,54738-0468-03 ",.02)
 ;;54738-0468-03
 ;;9002226.02101,"977,54738-0469-01 ",.01)
 ;;54738-0469-01
 ;;9002226.02101,"977,54738-0469-01 ",.02)
 ;;54738-0469-01
 ;;9002226.02101,"977,54738-0469-03 ",.01)
 ;;54738-0469-03
 ;;9002226.02101,"977,54738-0469-03 ",.02)
 ;;54738-0469-03
 ;;9002226.02101,"977,54868-0052-00 ",.01)
 ;;54868-0052-00
 ;;9002226.02101,"977,54868-0052-00 ",.02)
 ;;54868-0052-00
 ;;9002226.02101,"977,54868-0052-01 ",.01)
 ;;54868-0052-01
 ;;9002226.02101,"977,54868-0052-01 ",.02)
 ;;54868-0052-01
 ;;9002226.02101,"977,54868-0052-02 ",.01)
 ;;54868-0052-02
 ;;9002226.02101,"977,54868-0052-02 ",.02)
 ;;54868-0052-02
 ;;9002226.02101,"977,54868-0053-02 ",.01)
 ;;54868-0053-02
 ;;9002226.02101,"977,54868-0053-02 ",.02)
 ;;54868-0053-02
 ;;9002226.02101,"977,54868-0053-03 ",.01)
 ;;54868-0053-03
 ;;9002226.02101,"977,54868-0053-03 ",.02)
 ;;54868-0053-03
 ;;9002226.02101,"977,54868-0053-06 ",.01)
 ;;54868-0053-06
 ;;9002226.02101,"977,54868-0053-06 ",.02)
 ;;54868-0053-06
 ;;9002226.02101,"977,54868-0053-07 ",.01)
 ;;54868-0053-07
 ;;9002226.02101,"977,54868-0053-07 ",.02)
 ;;54868-0053-07
 ;;9002226.02101,"977,54868-0106-00 ",.01)
 ;;54868-0106-00
 ;;9002226.02101,"977,54868-0106-00 ",.02)
 ;;54868-0106-00
 ;;9002226.02101,"977,54868-0293-00 ",.01)
 ;;54868-0293-00
 ;;9002226.02101,"977,54868-0293-00 ",.02)
 ;;54868-0293-00
 ;;9002226.02101,"977,54868-0293-01 ",.01)
 ;;54868-0293-01
 ;;9002226.02101,"977,54868-0293-01 ",.02)
 ;;54868-0293-01
 ;;9002226.02101,"977,54868-0293-03 ",.01)
 ;;54868-0293-03
 ;;9002226.02101,"977,54868-0293-03 ",.02)
 ;;54868-0293-03
 ;;9002226.02101,"977,54868-0293-04 ",.01)
 ;;54868-0293-04
 ;;9002226.02101,"977,54868-0293-04 ",.02)
 ;;54868-0293-04
 ;;9002226.02101,"977,54868-0293-05 ",.01)
 ;;54868-0293-05
 ;;9002226.02101,"977,54868-0293-05 ",.02)
 ;;54868-0293-05
 ;;9002226.02101,"977,54868-0293-06 ",.01)
 ;;54868-0293-06
 ;;9002226.02101,"977,54868-0293-06 ",.02)
 ;;54868-0293-06
 ;;9002226.02101,"977,54868-0321-00 ",.01)
 ;;54868-0321-00
 ;;9002226.02101,"977,54868-0321-00 ",.02)
 ;;54868-0321-00
 ;;9002226.02101,"977,54868-0674-00 ",.01)
 ;;54868-0674-00
 ;;9002226.02101,"977,54868-0674-00 ",.02)
 ;;54868-0674-00
 ;;9002226.02101,"977,54868-0674-01 ",.01)
 ;;54868-0674-01
 ;;9002226.02101,"977,54868-0674-01 ",.02)
 ;;54868-0674-01
 ;;9002226.02101,"977,54868-0680-00 ",.01)
 ;;54868-0680-00
 ;;9002226.02101,"977,54868-0680-00 ",.02)
 ;;54868-0680-00
 ;;9002226.02101,"977,54868-0680-01 ",.01)
 ;;54868-0680-01
 ;;9002226.02101,"977,54868-0680-01 ",.02)
 ;;54868-0680-01
 ;;9002226.02101,"977,54868-0685-01 ",.01)
 ;;54868-0685-01
 ;;9002226.02101,"977,54868-0685-01 ",.02)
 ;;54868-0685-01
 ;;9002226.02101,"977,54868-0696-01 ",.01)
 ;;54868-0696-01
 ;;9002226.02101,"977,54868-0696-01 ",.02)
 ;;54868-0696-01
 ;;9002226.02101,"977,54868-0696-02 ",.01)
 ;;54868-0696-02
 ;;9002226.02101,"977,54868-0696-02 ",.02)
 ;;54868-0696-02
 ;;9002226.02101,"977,54868-0696-03 ",.01)
 ;;54868-0696-03
 ;;9002226.02101,"977,54868-0696-03 ",.02)
 ;;54868-0696-03
 ;;9002226.02101,"977,54868-0701-00 ",.01)
 ;;54868-0701-00
 ;;9002226.02101,"977,54868-0701-00 ",.02)
 ;;54868-0701-00
 ;;9002226.02101,"977,54868-0854-00 ",.01)
 ;;54868-0854-00
 ;;9002226.02101,"977,54868-0854-00 ",.02)
 ;;54868-0854-00
 ;;9002226.02101,"977,54868-0854-01 ",.01)
 ;;54868-0854-01
 ;;9002226.02101,"977,54868-0854-01 ",.02)
 ;;54868-0854-01
 ;;9002226.02101,"977,54868-1004-01 ",.01)
 ;;54868-1004-01
 ;;9002226.02101,"977,54868-1004-01 ",.02)
 ;;54868-1004-01
 ;;9002226.02101,"977,54868-1063-00 ",.01)
 ;;54868-1063-00
 ;;9002226.02101,"977,54868-1063-00 ",.02)
 ;;54868-1063-00
 ;;9002226.02101,"977,54868-1063-01 ",.01)
 ;;54868-1063-01
 ;;9002226.02101,"977,54868-1063-01 ",.02)
 ;;54868-1063-01
 ;;9002226.02101,"977,54868-1063-02 ",.01)
 ;;54868-1063-02
 ;;9002226.02101,"977,54868-1063-02 ",.02)
 ;;54868-1063-02
 ;;9002226.02101,"977,54868-1078-01 ",.01)
 ;;54868-1078-01
 ;;9002226.02101,"977,54868-1078-01 ",.02)
 ;;54868-1078-01
 ;;9002226.02101,"977,54868-1078-03 ",.01)
 ;;54868-1078-03
 ;;9002226.02101,"977,54868-1078-03 ",.02)
 ;;54868-1078-03
 ;;9002226.02101,"977,54868-1078-05 ",.01)
 ;;54868-1078-05
 ;;9002226.02101,"977,54868-1078-05 ",.02)
 ;;54868-1078-05
 ;;9002226.02101,"977,54868-1441-00 ",.01)
 ;;54868-1441-00
 ;;9002226.02101,"977,54868-1441-00 ",.02)
 ;;54868-1441-00
 ;;9002226.02101,"977,54868-1441-01 ",.01)
 ;;54868-1441-01
 ;;9002226.02101,"977,54868-1441-01 ",.02)
 ;;54868-1441-01
 ;;9002226.02101,"977,54868-1442-00 ",.01)
 ;;54868-1442-00
 ;;9002226.02101,"977,54868-1442-00 ",.02)
 ;;54868-1442-00
 ;;9002226.02101,"977,54868-1442-01 ",.01)
 ;;54868-1442-01
 ;;9002226.02101,"977,54868-1442-01 ",.02)
 ;;54868-1442-01
 ;;9002226.02101,"977,54868-1442-02 ",.01)
 ;;54868-1442-02
