BUD0ZA19 ;IHS/CMI/LAB-CREATED BY ^ATXSTX ON DEC 13, 2010;
 ;;5.0;IHS/RPMS UNIFORM DATA SYSTEM;;JAN 18, 2011;Build 12
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"1062,00247-2289-60 ",.02)
 ;;00247-2289-60
 ;;9002226.02101,"1062,00247-2289-77 ",.01)
 ;;00247-2289-77
 ;;9002226.02101,"1062,00247-2289-77 ",.02)
 ;;00247-2289-77
 ;;9002226.02101,"1062,00247-2289-90 ",.01)
 ;;00247-2289-90
 ;;9002226.02101,"1062,00247-2289-90 ",.02)
 ;;00247-2289-90
 ;;9002226.02101,"1062,00247-2290-30 ",.01)
 ;;00247-2290-30
 ;;9002226.02101,"1062,00247-2290-30 ",.02)
 ;;00247-2290-30
 ;;9002226.02101,"1062,00247-2290-60 ",.01)
 ;;00247-2290-60
 ;;9002226.02101,"1062,00247-2290-60 ",.02)
 ;;00247-2290-60
 ;;9002226.02101,"1062,00247-2290-77 ",.01)
 ;;00247-2290-77
 ;;9002226.02101,"1062,00247-2290-77 ",.02)
 ;;00247-2290-77
 ;;9002226.02101,"1062,00247-2290-90 ",.01)
 ;;00247-2290-90
 ;;9002226.02101,"1062,00247-2290-90 ",.02)
 ;;00247-2290-90
 ;;9002226.02101,"1062,00247-2291-30 ",.01)
 ;;00247-2291-30
 ;;9002226.02101,"1062,00247-2291-30 ",.02)
 ;;00247-2291-30
 ;;9002226.02101,"1062,00247-2291-60 ",.01)
 ;;00247-2291-60
 ;;9002226.02101,"1062,00247-2291-60 ",.02)
 ;;00247-2291-60
 ;;9002226.02101,"1062,00247-2292-30 ",.01)
 ;;00247-2292-30
 ;;9002226.02101,"1062,00247-2292-30 ",.02)
 ;;00247-2292-30
 ;;9002226.02101,"1062,00247-2292-60 ",.01)
 ;;00247-2292-60
 ;;9002226.02101,"1062,00247-2292-60 ",.02)
 ;;00247-2292-60
 ;;9002226.02101,"1062,00247-2293-30 ",.01)
 ;;00247-2293-30
 ;;9002226.02101,"1062,00247-2293-30 ",.02)
 ;;00247-2293-30
 ;;9002226.02101,"1062,00247-2293-60 ",.01)
 ;;00247-2293-60
 ;;9002226.02101,"1062,00247-2293-60 ",.02)
 ;;00247-2293-60
 ;;9002226.02101,"1062,00247-2294-30 ",.01)
 ;;00247-2294-30
 ;;9002226.02101,"1062,00247-2294-30 ",.02)
 ;;00247-2294-30
 ;;9002226.02101,"1062,00247-2295-30 ",.01)
 ;;00247-2295-30
 ;;9002226.02101,"1062,00247-2295-30 ",.02)
 ;;00247-2295-30
 ;;9002226.02101,"1062,00247-2296-30 ",.01)
 ;;00247-2296-30
 ;;9002226.02101,"1062,00247-2296-30 ",.02)
 ;;00247-2296-30
 ;;9002226.02101,"1062,00364-0722-90 ",.01)
 ;;00364-0722-90
 ;;9002226.02101,"1062,00364-0722-90 ",.02)
 ;;00364-0722-90
 ;;9002226.02101,"1062,00364-2605-05 ",.01)
 ;;00364-2605-05
 ;;9002226.02101,"1062,00364-2605-05 ",.02)
 ;;00364-2605-05
 ;;9002226.02101,"1062,00378-0197-01 ",.01)
 ;;00378-0197-01
 ;;9002226.02101,"1062,00378-0197-01 ",.02)
 ;;00378-0197-01
 ;;9002226.02101,"1062,00378-0197-05 ",.01)
 ;;00378-0197-05
 ;;9002226.02101,"1062,00378-0197-05 ",.02)
 ;;00378-0197-05
 ;;9002226.02101,"1062,00378-0210-01 ",.01)
 ;;00378-0210-01
 ;;9002226.02101,"1062,00378-0210-01 ",.02)
 ;;00378-0210-01
 ;;9002226.02101,"1062,00378-0210-10 ",.01)
 ;;00378-0210-10
 ;;9002226.02101,"1062,00378-0210-10 ",.02)
 ;;00378-0210-10
 ;;9002226.02101,"1062,00378-0215-01 ",.01)
 ;;00378-0215-01
 ;;9002226.02101,"1062,00378-0215-01 ",.02)
 ;;00378-0215-01
 ;;9002226.02101,"1062,00378-0215-05 ",.01)
 ;;00378-0215-05
 ;;9002226.02101,"1062,00378-0215-05 ",.02)
 ;;00378-0215-05
 ;;9002226.02101,"1062,00378-0217-01 ",.01)
 ;;00378-0217-01
 ;;9002226.02101,"1062,00378-0217-01 ",.02)
 ;;00378-0217-01
 ;;9002226.02101,"1062,00378-0551-01 ",.01)
 ;;00378-0551-01
 ;;9002226.02101,"1062,00378-0551-01 ",.02)
 ;;00378-0551-01
 ;;9002226.02101,"1062,00378-1105-01 ",.01)
 ;;00378-1105-01
 ;;9002226.02101,"1062,00378-1105-01 ",.02)
 ;;00378-1105-01
 ;;9002226.02101,"1062,00378-1105-05 ",.01)
 ;;00378-1105-05
 ;;9002226.02101,"1062,00378-1105-05 ",.02)
 ;;00378-1105-05
 ;;9002226.02101,"1062,00378-1110-01 ",.01)
 ;;00378-1110-01
 ;;9002226.02101,"1062,00378-1110-01 ",.02)
 ;;00378-1110-01
 ;;9002226.02101,"1062,00378-1110-05 ",.01)
 ;;00378-1110-05
 ;;9002226.02101,"1062,00378-1110-05 ",.02)
 ;;00378-1110-05
 ;;9002226.02101,"1062,00378-1113-01 ",.01)
 ;;00378-1113-01
 ;;9002226.02101,"1062,00378-1113-01 ",.02)
 ;;00378-1113-01
 ;;9002226.02101,"1062,00378-1125-01 ",.01)
 ;;00378-1125-01
 ;;9002226.02101,"1062,00378-1125-01 ",.02)
 ;;00378-1125-01
 ;;9002226.02101,"1062,00378-1125-10 ",.01)
 ;;00378-1125-10
 ;;9002226.02101,"1062,00378-1125-10 ",.02)
 ;;00378-1125-10
 ;;9002226.02101,"1062,00378-1142-01 ",.01)
 ;;00378-1142-01
 ;;9002226.02101,"1062,00378-1142-01 ",.02)
 ;;00378-1142-01
 ;;9002226.02101,"1062,00378-3131-01 ",.01)
 ;;00378-3131-01
 ;;9002226.02101,"1062,00378-3131-01 ",.02)
 ;;00378-3131-01
 ;;9002226.02101,"1062,00378-3132-01 ",.01)
 ;;00378-3132-01
 ;;9002226.02101,"1062,00378-3132-01 ",.02)
 ;;00378-3132-01
 ;;9002226.02101,"1062,00378-3133-01 ",.01)
 ;;00378-3133-01
 ;;9002226.02101,"1062,00378-3133-01 ",.02)
 ;;00378-3133-01
 ;;9002226.02101,"1062,00378-4011-01 ",.01)
 ;;00378-4011-01
 ;;9002226.02101,"1062,00378-4011-01 ",.02)
 ;;00378-4011-01
 ;;9002226.02101,"1062,00378-4012-01 ",.01)
 ;;00378-4012-01
 ;;9002226.02101,"1062,00378-4012-01 ",.02)
 ;;00378-4012-01
 ;;9002226.02101,"1062,00378-4013-01 ",.01)
 ;;00378-4013-01
 ;;9002226.02101,"1062,00378-4013-01 ",.02)
 ;;00378-4013-01
 ;;9002226.02101,"1062,00405-4024-01 ",.01)
 ;;00405-4024-01
 ;;9002226.02101,"1062,00405-4024-01 ",.02)
 ;;00405-4024-01
 ;;9002226.02101,"1062,00405-4025-01 ",.01)
 ;;00405-4025-01
 ;;9002226.02101,"1062,00405-4025-01 ",.02)
 ;;00405-4025-01
 ;;9002226.02101,"1062,00405-4205-01 ",.01)
 ;;00405-4205-01
 ;;9002226.02101,"1062,00405-4205-01 ",.02)
 ;;00405-4205-01
