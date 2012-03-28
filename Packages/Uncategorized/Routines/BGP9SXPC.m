BGP9SXPC ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON MAR 25, 2009 ;
 ;;9.0;IHS CLINICAL REPORTING;;JUL 1, 2009
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"722,00046-1102-81 ",.01)
 ;;00046-1102-81
 ;;9002226.02101,"722,00046-1102-81 ",.02)
 ;;00046-1102-81
 ;;9002226.02101,"722,00046-1102-91 ",.01)
 ;;00046-1102-91
 ;;9002226.02101,"722,00046-1102-91 ",.02)
 ;;00046-1102-91
 ;;9002226.02101,"722,00046-1103-81 ",.01)
 ;;00046-1103-81
 ;;9002226.02101,"722,00046-1103-81 ",.02)
 ;;00046-1103-81
 ;;9002226.02101,"722,00046-1104-81 ",.01)
 ;;00046-1104-81
 ;;9002226.02101,"722,00046-1104-81 ",.02)
 ;;00046-1104-81
 ;;9002226.02101,"722,00046-1104-91 ",.01)
 ;;00046-1104-91
 ;;9002226.02101,"722,00046-1104-91 ",.02)
 ;;00046-1104-91
 ;;9002226.02101,"722,00046-2572-01 ",.01)
 ;;00046-2572-01
 ;;9002226.02101,"722,00046-2572-01 ",.02)
 ;;00046-2572-01
 ;;9002226.02101,"722,00046-2573-01 ",.01)
 ;;00046-2573-01
 ;;9002226.02101,"722,00046-2573-01 ",.02)
 ;;00046-2573-01
 ;;9002226.02101,"722,00046-2573-05 ",.01)
 ;;00046-2573-05
 ;;9002226.02101,"722,00046-2573-05 ",.02)
 ;;00046-2573-05
 ;;9002226.02101,"722,00046-2573-06 ",.01)
 ;;00046-2573-06
 ;;9002226.02101,"722,00046-2573-06 ",.02)
 ;;00046-2573-06
 ;;9002226.02101,"722,00046-3867-81 ",.01)
 ;;00046-3867-81
 ;;9002226.02101,"722,00046-3867-81 ",.02)
 ;;00046-3867-81
 ;;9002226.02101,"722,00062-1800-01 ",.01)
 ;;00062-1800-01
 ;;9002226.02101,"722,00062-1800-01 ",.02)
 ;;00062-1800-01
 ;;9002226.02101,"722,00247-0249-00 ",.01)
 ;;00247-0249-00
 ;;9002226.02101,"722,00247-0249-00 ",.02)
 ;;00247-0249-00
 ;;9002226.02101,"722,00247-0249-30 ",.01)
 ;;00247-0249-30
 ;;9002226.02101,"722,00247-0249-30 ",.02)
 ;;00247-0249-30
 ;;9002226.02101,"722,00247-0250-00 ",.01)
 ;;00247-0250-00
 ;;9002226.02101,"722,00247-0250-00 ",.02)
 ;;00247-0250-00
 ;;9002226.02101,"722,00247-0250-28 ",.01)
 ;;00247-0250-28
 ;;9002226.02101,"722,00247-0250-28 ",.02)
 ;;00247-0250-28
 ;;9002226.02101,"722,00247-0250-30 ",.01)
 ;;00247-0250-30
 ;;9002226.02101,"722,00247-0250-30 ",.02)
 ;;00247-0250-30
 ;;9002226.02101,"722,00247-0251-00 ",.01)
 ;;00247-0251-00
 ;;9002226.02101,"722,00247-0251-00 ",.02)
 ;;00247-0251-00
 ;;9002226.02101,"722,00247-0251-30 ",.01)
 ;;00247-0251-30
 ;;9002226.02101,"722,00247-0251-30 ",.02)
 ;;00247-0251-30
 ;;9002226.02101,"722,00247-0251-60 ",.01)
 ;;00247-0251-60
 ;;9002226.02101,"722,00247-0251-60 ",.02)
 ;;00247-0251-60
 ;;9002226.02101,"722,00247-0251-90 ",.01)
 ;;00247-0251-90
 ;;9002226.02101,"722,00247-0251-90 ",.02)
 ;;00247-0251-90
 ;;9002226.02101,"722,00527-1409-01 ",.01)
 ;;00527-1409-01
 ;;9002226.02101,"722,00527-1409-01 ",.02)
 ;;00527-1409-01
 ;;9002226.02101,"722,00527-1410-01 ",.01)
 ;;00527-1410-01
 ;;9002226.02101,"722,00527-1410-01 ",.02)
 ;;00527-1410-01
 ;;9002226.02101,"722,10135-0469-01 ",.01)
 ;;10135-0469-01
 ;;9002226.02101,"722,10135-0469-01 ",.02)
 ;;10135-0469-01
 ;;9002226.02101,"722,10135-0470-01 ",.01)
 ;;10135-0470-01
 ;;9002226.02101,"722,10135-0470-01 ",.02)
 ;;10135-0470-01
 ;;9002226.02101,"722,12280-0039-00 ",.01)
 ;;12280-0039-00
 ;;9002226.02101,"722,12280-0039-00 ",.02)
 ;;12280-0039-00
 ;;9002226.02101,"722,51655-0452-25 ",.01)
 ;;51655-0452-25
 ;;9002226.02101,"722,51655-0452-25 ",.02)
 ;;51655-0452-25
 ;;9002226.02101,"722,51991-0078-01 ",.01)
 ;;51991-0078-01
 ;;9002226.02101,"722,51991-0078-01 ",.02)
 ;;51991-0078-01
 ;;9002226.02101,"722,51991-0079-01 ",.01)
 ;;51991-0079-01
 ;;9002226.02101,"722,51991-0079-01 ",.02)
 ;;51991-0079-01
 ;;9002226.02101,"722,52959-0222-00 ",.01)
 ;;52959-0222-00
 ;;9002226.02101,"722,52959-0222-00 ",.02)
 ;;52959-0222-00
 ;;9002226.02101,"722,52959-0223-00 ",.01)
 ;;52959-0223-00
 ;;9002226.02101,"722,52959-0223-00 ",.02)
 ;;52959-0223-00
 ;;9002226.02101,"722,52959-0223-30 ",.01)
 ;;52959-0223-30
 ;;9002226.02101,"722,52959-0223-30 ",.02)
 ;;52959-0223-30
 ;;9002226.02101,"722,54569-0811-01 ",.01)
 ;;54569-0811-01
 ;;9002226.02101,"722,54569-0811-01 ",.02)
 ;;54569-0811-01
 ;;9002226.02101,"722,54569-0812-00 ",.01)
 ;;54569-0812-00
 ;;9002226.02101,"722,54569-0812-00 ",.02)
 ;;54569-0812-00
 ;;9002226.02101,"722,54569-0812-01 ",.01)
 ;;54569-0812-01
 ;;9002226.02101,"722,54569-0812-01 ",.02)
 ;;54569-0812-01
 ;;9002226.02101,"722,54569-0812-02 ",.01)
 ;;54569-0812-02
 ;;9002226.02101,"722,54569-0812-02 ",.02)
 ;;54569-0812-02
 ;;9002226.02101,"722,54569-0812-05 ",.01)
 ;;54569-0812-05
 ;;9002226.02101,"722,54569-0812-05 ",.02)
 ;;54569-0812-05
 ;;9002226.02101,"722,54569-0813-00 ",.01)
 ;;54569-0813-00
 ;;9002226.02101,"722,54569-0813-00 ",.02)
 ;;54569-0813-00
 ;;9002226.02101,"722,54569-0813-01 ",.01)
 ;;54569-0813-01
 ;;9002226.02101,"722,54569-0813-01 ",.02)
 ;;54569-0813-01
 ;;9002226.02101,"722,54569-0849-00 ",.01)
 ;;54569-0849-00
 ;;9002226.02101,"722,54569-0849-00 ",.02)
 ;;54569-0849-00
 ;;9002226.02101,"722,54569-0849-01 ",.01)
 ;;54569-0849-01
 ;;9002226.02101,"722,54569-0849-01 ",.02)
 ;;54569-0849-01
 ;;9002226.02101,"722,54569-4354-01 ",.01)
 ;;54569-4354-01
 ;;9002226.02101,"722,54569-4354-01 ",.02)
 ;;54569-4354-01
 ;;9002226.02101,"722,54569-4618-00 ",.01)
 ;;54569-4618-00
 ;;9002226.02101,"722,54569-4618-00 ",.02)
 ;;54569-4618-00
 ;;9002226.02101,"722,54569-4673-00 ",.01)
 ;;54569-4673-00
 ;;9002226.02101,"722,54569-4673-00 ",.02)
 ;;54569-4673-00
