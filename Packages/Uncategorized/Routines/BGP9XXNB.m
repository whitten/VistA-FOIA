BGP9XXNB ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON MAR 25, 2009 ;
 ;;9.0;IHS CLINICAL REPORTING;;JUL 1, 2009
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"312,38237-4 ",.02)
 ;;38237-4
 ;;9002226.02101,"312,38238-2 ",.01)
 ;;38238-2
 ;;9002226.02101,"312,38238-2 ",.02)
 ;;38238-2
 ;;9002226.02101,"312,38239-0 ",.01)
 ;;38239-0
 ;;9002226.02101,"312,38239-0 ",.02)
 ;;38239-0
 ;;9002226.02101,"312,38240-8 ",.01)
 ;;38240-8
 ;;9002226.02101,"312,38240-8 ",.02)
 ;;38240-8
 ;;9002226.02101,"312,41994-5 ",.01)
 ;;41994-5
 ;;9002226.02101,"312,41994-5 ",.02)
 ;;41994-5
 ;;9002226.02101,"312,42737-7 ",.01)
 ;;42737-7
 ;;9002226.02101,"312,42737-7 ",.02)
 ;;42737-7
 ;;9002226.02101,"312,43944-8 ",.01)
 ;;43944-8
 ;;9002226.02101,"312,43944-8 ",.02)
 ;;43944-8
 ;;9002226.02101,"312,43952-1 ",.01)
 ;;43952-1
 ;;9002226.02101,"312,43952-1 ",.02)
 ;;43952-1
 ;;9002226.02101,"312,43962-0 ",.01)
 ;;43962-0
 ;;9002226.02101,"312,43962-0 ",.02)
 ;;43962-0
 ;;9002226.02101,"312,44056-0 ",.01)
 ;;44056-0
 ;;9002226.02101,"312,44056-0 ",.02)
 ;;44056-0
 ;;9002226.02101,"312,5472-6 ",.01)
 ;;5472-6
 ;;9002226.02101,"312,5472-6 ",.02)
 ;;5472-6
 ;;9002226.02101,"312,5473-4 ",.01)
 ;;5473-4
 ;;9002226.02101,"312,5473-4 ",.02)
 ;;5473-4
 ;;9002226.02101,"312,5474-2 ",.01)
 ;;5474-2
 ;;9002226.02101,"312,5474-2 ",.02)
 ;;5474-2
 ;;9002226.02101,"312,8123-2 ",.01)
 ;;8123-2
 ;;9002226.02101,"312,8123-2 ",.02)
 ;;8123-2
 ;;9002226.02101,"312,8127-3 ",.01)
 ;;8127-3
 ;;9002226.02101,"312,8127-3 ",.02)
 ;;8127-3
 ;;9002226.02101,"312,8128-1 ",.01)
 ;;8128-1
 ;;9002226.02101,"312,8128-1 ",.02)
 ;;8128-1
 ;;9002226.02101,"312,8129-9 ",.01)
 ;;8129-9
 ;;9002226.02101,"312,8129-9 ",.02)
 ;;8129-9
