BGP9VXDW ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON MAR 25, 2009 ;
 ;;9.0;IHS CLINICAL REPORTING;;JUL 1, 2009
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"858,380.21 ",.02)
 ;;380.21 
 ;;9002226.02101,"858,380.22 ",.01)
 ;;380.22 
 ;;9002226.02101,"858,380.22 ",.02)
 ;;380.22 
 ;;9002226.02101,"858,380.23 ",.01)
 ;;380.23 
 ;;9002226.02101,"858,380.23 ",.02)
 ;;380.23 
 ;;9002226.02101,"858,382.00 ",.01)
 ;;382.00 
 ;;9002226.02101,"858,382.00 ",.02)
 ;;382.00 
 ;;9002226.02101,"858,382.01 ",.01)
 ;;382.01 
 ;;9002226.02101,"858,382.01 ",.02)
 ;;382.01 
 ;;9002226.02101,"858,382.02 ",.01)
 ;;382.02 
 ;;9002226.02101,"858,382.02 ",.02)
 ;;382.02 
 ;;9002226.02101,"858,382.1 ",.01)
 ;;382.1 
 ;;9002226.02101,"858,382.1 ",.02)
 ;;382.1 
 ;;9002226.02101,"858,382.2 ",.01)
 ;;382.2 
 ;;9002226.02101,"858,382.2 ",.02)
 ;;382.2 
 ;;9002226.02101,"858,421.0 ",.01)
 ;;421.0 
 ;;9002226.02101,"858,421.0 ",.02)
 ;;421.0 
 ;;9002226.02101,"858,421.1 ",.01)
 ;;421.1 
 ;;9002226.02101,"858,421.1 ",.02)
 ;;421.1 
 ;;9002226.02101,"858,421.9 ",.01)
 ;;421.9 
 ;;9002226.02101,"858,421.9 ",.02)
 ;;421.9 
 ;;9002226.02101,"858,422.0 ",.01)
 ;;422.0 
 ;;9002226.02101,"858,422.0 ",.02)
 ;;422.0 
 ;;9002226.02101,"858,422.90 ",.01)
 ;;422.90 
 ;;9002226.02101,"858,422.90 ",.02)
 ;;422.90 
 ;;9002226.02101,"858,422.91 ",.01)
 ;;422.91 
 ;;9002226.02101,"858,422.91 ",.02)
 ;;422.91 
 ;;9002226.02101,"858,422.92 ",.01)
 ;;422.92 
 ;;9002226.02101,"858,422.92 ",.02)
 ;;422.92 
 ;;9002226.02101,"858,422.93 ",.01)
 ;;422.93 
 ;;9002226.02101,"858,422.93 ",.02)
 ;;422.93 
 ;;9002226.02101,"858,422.99 ",.01)
 ;;422.99 
 ;;9002226.02101,"858,422.99 ",.02)
 ;;422.99 
 ;;9002226.02101,"858,460 ",.01)
 ;;460 
 ;;9002226.02101,"858,460 ",.02)
 ;;460 
 ;;9002226.02101,"858,461.0 ",.01)
 ;;461.0 
 ;;9002226.02101,"858,461.0 ",.02)
 ;;461.0 
 ;;9002226.02101,"858,461.1 ",.01)
 ;;461.1 
 ;;9002226.02101,"858,461.1 ",.02)
 ;;461.1 
 ;;9002226.02101,"858,461.2 ",.01)
 ;;461.2 
 ;;9002226.02101,"858,461.2 ",.02)
 ;;461.2 
 ;;9002226.02101,"858,461.3 ",.01)
 ;;461.3 
 ;;9002226.02101,"858,461.3 ",.02)
 ;;461.3 
 ;;9002226.02101,"858,461.8 ",.01)
 ;;461.8 
 ;;9002226.02101,"858,461.8 ",.02)
 ;;461.8 
 ;;9002226.02101,"858,461.9 ",.01)
 ;;461.9 
 ;;9002226.02101,"858,461.9 ",.02)
 ;;461.9 
 ;;9002226.02101,"858,462 ",.01)
 ;;462 
 ;;9002226.02101,"858,462 ",.02)
 ;;462 
 ;;9002226.02101,"858,463 ",.01)
 ;;463 
 ;;9002226.02101,"858,463 ",.02)
 ;;463 
 ;;9002226.02101,"858,464.00 ",.01)
 ;;464.00 
 ;;9002226.02101,"858,464.00 ",.02)
 ;;464.00 
 ;;9002226.02101,"858,464.01 ",.01)
 ;;464.01 
 ;;9002226.02101,"858,464.01 ",.02)
 ;;464.01 
 ;;9002226.02101,"858,464.10 ",.01)
 ;;464.10 
 ;;9002226.02101,"858,464.10 ",.02)
 ;;464.10 
 ;;9002226.02101,"858,464.11 ",.01)
 ;;464.11 
 ;;9002226.02101,"858,464.11 ",.02)
 ;;464.11 
 ;;9002226.02101,"858,464.20 ",.01)
 ;;464.20 
 ;;9002226.02101,"858,464.20 ",.02)
 ;;464.20 
 ;;9002226.02101,"858,464.21 ",.01)
 ;;464.21 
 ;;9002226.02101,"858,464.21 ",.02)
 ;;464.21 
 ;;9002226.02101,"858,464.30 ",.01)
 ;;464.30 
 ;;9002226.02101,"858,464.30 ",.02)
 ;;464.30 
 ;;9002226.02101,"858,464.31 ",.01)
 ;;464.31 
 ;;9002226.02101,"858,464.31 ",.02)
 ;;464.31 
 ;;9002226.02101,"858,464.4 ",.01)
 ;;464.4 
 ;;9002226.02101,"858,464.4 ",.02)
 ;;464.4 
 ;;9002226.02101,"858,464.50 ",.01)
 ;;464.50 
 ;;9002226.02101,"858,464.50 ",.02)
 ;;464.50 
 ;;9002226.02101,"858,464.51 ",.01)
 ;;464.51 
 ;;9002226.02101,"858,464.51 ",.02)
 ;;464.51 
 ;;9002226.02101,"858,465.0 ",.01)
 ;;465.0 
 ;;9002226.02101,"858,465.0 ",.02)
 ;;465.0 
 ;;9002226.02101,"858,465.8 ",.01)
 ;;465.8 
 ;;9002226.02101,"858,465.8 ",.02)
 ;;465.8 
 ;;9002226.02101,"858,465.9 ",.01)
 ;;465.9 
 ;;9002226.02101,"858,465.9 ",.02)
 ;;465.9 
 ;;9002226.02101,"858,466.0 ",.01)
 ;;466.0 
 ;;9002226.02101,"858,466.0 ",.02)
 ;;466.0 
 ;;9002226.02101,"858,466.19 ",.01)
 ;;466.19 
 ;;9002226.02101,"858,466.19 ",.02)
 ;;466.19 
 ;;9002226.02101,"858,472.0 ",.01)
 ;;472.0 
 ;;9002226.02101,"858,472.0 ",.02)
 ;;472.0 
 ;;9002226.02101,"858,472.1 ",.01)
 ;;472.1 
 ;;9002226.02101,"858,472.1 ",.02)
 ;;472.1 
 ;;9002226.02101,"858,472.2 ",.01)
 ;;472.2 
 ;;9002226.02101,"858,472.2 ",.02)
 ;;472.2 
 ;;9002226.02101,"858,473.0 ",.01)
 ;;473.0 
 ;;9002226.02101,"858,473.0 ",.02)
 ;;473.0 
 ;;9002226.02101,"858,473.1 ",.01)
 ;;473.1 
 ;;9002226.02101,"858,473.1 ",.02)
 ;;473.1 
 ;;9002226.02101,"858,473.2 ",.01)
 ;;473.2 
 ;;9002226.02101,"858,473.2 ",.02)
 ;;473.2 
 ;;9002226.02101,"858,473.3 ",.01)
 ;;473.3 
 ;;9002226.02101,"858,473.3 ",.02)
 ;;473.3 
 ;;9002226.02101,"858,473.8 ",.01)
 ;;473.8 
 ;;9002226.02101,"858,473.8 ",.02)
 ;;473.8 
 ;;9002226.02101,"858,473.9 ",.01)
 ;;473.9 
 ;;9002226.02101,"858,473.9 ",.02)
 ;;473.9 
 ;;9002226.02101,"858,474.00 ",.01)
 ;;474.00 
 ;;9002226.02101,"858,474.00 ",.02)
 ;;474.00 
 ;;9002226.02101,"858,474.01 ",.01)
 ;;474.01 
 ;;9002226.02101,"858,474.01 ",.02)
 ;;474.01 
 ;;9002226.02101,"858,474.02 ",.01)
 ;;474.02 
 ;;9002226.02101,"858,474.02 ",.02)
 ;;474.02 
 ;;9002226.02101,"858,474.10 ",.01)
 ;;474.10 
 ;;9002226.02101,"858,474.10 ",.02)
 ;;474.10 
 ;;9002226.02101,"858,474.11 ",.01)
 ;;474.11 
