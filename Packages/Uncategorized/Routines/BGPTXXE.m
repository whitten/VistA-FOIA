BGPTXXE ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON MAR 19, 2004 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"781,12193-9 ",.02)
 ;;12193-9
 ;;9002226.02101,"781,12194-7 ",.01)
 ;;12194-7
 ;;9002226.02101,"781,12194-7 ",.02)
 ;;12194-7
 ;;9002226.02101,"781,12195-4 ",.01)
 ;;12195-4
 ;;9002226.02101,"781,12195-4 ",.02)
 ;;12195-4
 ;;9002226.02101,"781,12421-4 ",.01)
 ;;12421-4
 ;;9002226.02101,"781,12421-4 ",.02)
 ;;12421-4
 ;;9002226.02101,"781,12534-4 ",.01)
 ;;12534-4
 ;;9002226.02101,"781,12534-4 ",.02)
 ;;12534-4
 ;;9002226.02101,"781,12572-4 ",.01)
 ;;12572-4
 ;;9002226.02101,"781,12572-4 ",.02)
 ;;12572-4
 ;;9002226.02101,"781,12573-2 ",.01)
 ;;12573-2
 ;;9002226.02101,"781,12573-2 ",.02)
 ;;12573-2
 ;;9002226.02101,"781,12574-0 ",.01)
 ;;12574-0
 ;;9002226.02101,"781,12574-0 ",.02)
 ;;12574-0
 ;;9002226.02101,"781,12575-7 ",.01)
 ;;12575-7
 ;;9002226.02101,"781,12575-7 ",.02)
 ;;12575-7
 ;;9002226.02101,"781,12576-5 ",.01)
 ;;12576-5
 ;;9002226.02101,"781,12576-5 ",.02)
 ;;12576-5
 ;;9002226.02101,"781,12577-3 ",.01)
 ;;12577-3
 ;;9002226.02101,"781,12577-3 ",.02)
 ;;12577-3
 ;;9002226.02101,"781,12578-1 ",.01)
 ;;12578-1
 ;;9002226.02101,"781,12578-1 ",.02)
 ;;12578-1
 ;;9002226.02101,"781,12579-9 ",.01)
 ;;12579-9
 ;;9002226.02101,"781,12579-9 ",.02)
 ;;12579-9
 ;;9002226.02101,"781,12580-7 ",.01)
 ;;12580-7
 ;;9002226.02101,"781,12580-7 ",.02)
 ;;12580-7
 ;;9002226.02101,"781,12581-5 ",.01)
 ;;12581-5
 ;;9002226.02101,"781,12581-5 ",.02)
 ;;12581-5
 ;;9002226.02101,"781,12582-3 ",.01)
 ;;12582-3
 ;;9002226.02101,"781,12582-3 ",.02)
 ;;12582-3
 ;;9002226.02101,"781,12583-1 ",.01)
 ;;12583-1
 ;;9002226.02101,"781,12583-1 ",.02)
 ;;12583-1
 ;;9002226.02101,"781,12584-9 ",.01)
 ;;12584-9
 ;;9002226.02101,"781,12584-9 ",.02)
 ;;12584-9
 ;;9002226.02101,"781,12585-6 ",.01)
 ;;12585-6
 ;;9002226.02101,"781,12585-6 ",.02)
 ;;12585-6
 ;;9002226.02101,"781,12586-4 ",.01)
 ;;12586-4
 ;;9002226.02101,"781,12586-4 ",.02)
 ;;12586-4
 ;;9002226.02101,"781,12587-2 ",.01)
 ;;12587-2
 ;;9002226.02101,"781,12587-2 ",.02)
 ;;12587-2
 ;;9002226.02101,"781,12588-0 ",.01)
 ;;12588-0
 ;;9002226.02101,"781,12588-0 ",.02)
 ;;12588-0
 ;;9002226.02101,"781,12589-8 ",.01)
 ;;12589-8
 ;;9002226.02101,"781,12589-8 ",.02)
 ;;12589-8
 ;;9002226.02101,"781,13001-3 ",.01)
 ;;13001-3
 ;;9002226.02101,"781,13001-3 ",.02)
 ;;13001-3
 ;;9002226.02101,"781,13441-1 ",.01)
 ;;13441-1
 ;;9002226.02101,"781,13441-1 ",.02)
 ;;13441-1
 ;;9002226.02101,"781,13442-9 ",.01)
 ;;13442-9
 ;;9002226.02101,"781,13442-9 ",.02)
 ;;13442-9
 ;;9002226.02101,"781,13443-7 ",.01)
 ;;13443-7
 ;;9002226.02101,"781,13443-7 ",.02)
 ;;13443-7
 ;;9002226.02101,"781,13445-2 ",.01)
 ;;13445-2
 ;;9002226.02101,"781,13445-2 ",.02)
 ;;13445-2
 ;;9002226.02101,"781,13446-0 ",.01)
 ;;13446-0
 ;;9002226.02101,"781,13446-0 ",.02)
 ;;13446-0
 ;;9002226.02101,"781,13447-8 ",.01)
 ;;13447-8
 ;;9002226.02101,"781,13447-8 ",.02)
 ;;13447-8
 ;;9002226.02101,"781,13449-4 ",.01)
 ;;13449-4
 ;;9002226.02101,"781,13449-4 ",.02)
 ;;13449-4
 ;;9002226.02101,"781,13450-2 ",.01)
 ;;13450-2
 ;;9002226.02101,"781,13450-2 ",.02)
 ;;13450-2
 ;;9002226.02101,"781,13451-0 ",.01)
 ;;13451-0
 ;;9002226.02101,"781,13451-0 ",.02)
 ;;13451-0
 ;;9002226.02101,"781,13463-5 ",.01)
 ;;13463-5
 ;;9002226.02101,"781,13463-5 ",.02)
 ;;13463-5
 ;;9002226.02101,"781,13464-3 ",.01)
 ;;13464-3
 ;;9002226.02101,"781,13464-3 ",.02)
 ;;13464-3
 ;;9002226.02101,"781,13465-0 ",.01)
 ;;13465-0
 ;;9002226.02101,"781,13465-0 ",.02)
 ;;13465-0
 ;;9002226.02101,"781,13466-8 ",.01)
 ;;13466-8
 ;;9002226.02101,"781,13466-8 ",.02)
 ;;13466-8
 ;;9002226.02101,"781,13467-6 ",.01)
 ;;13467-6
 ;;9002226.02101,"781,13467-6 ",.02)
 ;;13467-6
 ;;9002226.02101,"781,13468-4 ",.01)
 ;;13468-4
 ;;9002226.02101,"781,13468-4 ",.02)
 ;;13468-4
 ;;9002226.02101,"781,13469-2 ",.01)
 ;;13469-2
 ;;9002226.02101,"781,13469-2 ",.02)
 ;;13469-2
 ;;9002226.02101,"781,13470-0 ",.01)
 ;;13470-0
 ;;9002226.02101,"781,13470-0 ",.02)
 ;;13470-0
 ;;9002226.02101,"781,13471-8 ",.01)
 ;;13471-8
 ;;9002226.02101,"781,13471-8 ",.02)
 ;;13471-8
 ;;9002226.02101,"781,13472-6 ",.01)
 ;;13472-6
 ;;9002226.02101,"781,13472-6 ",.02)
 ;;13472-6
 ;;9002226.02101,"781,13473-4 ",.01)
 ;;13473-4
 ;;9002226.02101,"781,13473-4 ",.02)
 ;;13473-4
 ;;9002226.02101,"781,13474-2 ",.01)
 ;;13474-2
 ;;9002226.02101,"781,13474-2 ",.02)
 ;;13474-2
 ;;9002226.02101,"781,13475-9 ",.01)
 ;;13475-9
 ;;9002226.02101,"781,13475-9 ",.02)
 ;;13475-9
 ;;9002226.02101,"781,13476-7 ",.01)
 ;;13476-7
 ;;9002226.02101,"781,13476-7 ",.02)
 ;;13476-7
 ;;9002226.02101,"781,13477-5 ",.01)
 ;;13477-5
 ;;9002226.02101,"781,13477-5 ",.02)
 ;;13477-5
 ;;9002226.02101,"781,13478-3 ",.01)
 ;;13478-3
 ;;9002226.02101,"781,13478-3 ",.02)
 ;;13478-3
 ;;9002226.02101,"781,13479-1 ",.01)
 ;;13479-1
 ;;9002226.02101,"781,13479-1 ",.02)
 ;;13479-1
 ;;9002226.02101,"781,13480-9 ",.01)
 ;;13480-9
 ;;9002226.02101,"781,13480-9 ",.02)
 ;;13480-9
 ;;9002226.02101,"781,13481-7 ",.01)
 ;;13481-7
 ;;9002226.02101,"781,13481-7 ",.02)
 ;;13481-7
 ;;9002226.02101,"781,13482-5 ",.01)
 ;;13482-5
 ;;9002226.02101,"781,13482-5 ",.02)
 ;;13482-5
