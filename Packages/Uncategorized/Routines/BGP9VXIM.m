BGP9VXIM ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON MAR 25, 2009 ;
 ;;9.0;IHS CLINICAL REPORTING;;JUL 1, 2009
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"862,39.21 ",.02)
 ;;39.21 
 ;;9002226.02101,"862,39.22 ",.01)
 ;;39.22 
 ;;9002226.02101,"862,39.22 ",.02)
 ;;39.22 
 ;;9002226.02101,"862,39.23 ",.01)
 ;;39.23 
 ;;9002226.02101,"862,39.23 ",.02)
 ;;39.23 
 ;;9002226.02101,"862,39.24 ",.01)
 ;;39.24 
 ;;9002226.02101,"862,39.24 ",.02)
 ;;39.24 
 ;;9002226.02101,"862,39.25 ",.01)
 ;;39.25 
 ;;9002226.02101,"862,39.25 ",.02)
 ;;39.25 
 ;;9002226.02101,"862,39.26 ",.01)
 ;;39.26 
 ;;9002226.02101,"862,39.26 ",.02)
 ;;39.26 
 ;;9002226.02101,"862,39.27 ",.01)
 ;;39.27 
 ;;9002226.02101,"862,39.27 ",.02)
 ;;39.27 
 ;;9002226.02101,"862,39.28 ",.01)
 ;;39.28 
 ;;9002226.02101,"862,39.28 ",.02)
 ;;39.28 
 ;;9002226.02101,"862,39.29 ",.01)
 ;;39.29 
 ;;9002226.02101,"862,39.29 ",.02)
 ;;39.29 
 ;;9002226.02101,"862,39.30 ",.01)
 ;;39.30 
 ;;9002226.02101,"862,39.30 ",.02)
 ;;39.30 
 ;;9002226.02101,"862,39.31 ",.01)
 ;;39.31 
 ;;9002226.02101,"862,39.31 ",.02)
 ;;39.31 
 ;;9002226.02101,"862,39.32 ",.01)
 ;;39.32 
 ;;9002226.02101,"862,39.32 ",.02)
 ;;39.32 
 ;;9002226.02101,"862,39.41 ",.01)
 ;;39.41 
 ;;9002226.02101,"862,39.41 ",.02)
 ;;39.41 
 ;;9002226.02101,"862,39.42 ",.01)
 ;;39.42 
 ;;9002226.02101,"862,39.42 ",.02)
 ;;39.42 
 ;;9002226.02101,"862,39.43 ",.01)
 ;;39.43 
 ;;9002226.02101,"862,39.43 ",.02)
 ;;39.43 
 ;;9002226.02101,"862,39.49 ",.01)
 ;;39.49 
 ;;9002226.02101,"862,39.49 ",.02)
 ;;39.49 
 ;;9002226.02101,"862,39.51 ",.01)
 ;;39.51 
 ;;9002226.02101,"862,39.51 ",.02)
 ;;39.51 
 ;;9002226.02101,"862,39.52 ",.01)
 ;;39.52 
 ;;9002226.02101,"862,39.52 ",.02)
 ;;39.52 
 ;;9002226.02101,"862,39.53 ",.01)
 ;;39.53 
 ;;9002226.02101,"862,39.53 ",.02)
 ;;39.53 
 ;;9002226.02101,"862,39.54 ",.01)
 ;;39.54 
 ;;9002226.02101,"862,39.54 ",.02)
 ;;39.54 
 ;;9002226.02101,"862,39.55 ",.01)
 ;;39.55 
 ;;9002226.02101,"862,39.55 ",.02)
 ;;39.55 
 ;;9002226.02101,"862,39.56 ",.01)
 ;;39.56 
 ;;9002226.02101,"862,39.56 ",.02)
 ;;39.56 
 ;;9002226.02101,"862,39.57 ",.01)
 ;;39.57 
 ;;9002226.02101,"862,39.57 ",.02)
 ;;39.57 
 ;;9002226.02101,"862,39.58 ",.01)
 ;;39.58 
 ;;9002226.02101,"862,39.58 ",.02)
 ;;39.58 
 ;;9002226.02101,"862,39.59 ",.01)
 ;;39.59 
 ;;9002226.02101,"862,39.59 ",.02)
 ;;39.59 
 ;;9002226.02101,"862,39.71 ",.01)
 ;;39.71 
 ;;9002226.02101,"862,39.71 ",.02)
 ;;39.71 
 ;;9002226.02101,"862,39.72 ",.01)
 ;;39.72 
 ;;9002226.02101,"862,39.72 ",.02)
 ;;39.72 
 ;;9002226.02101,"862,39.73 ",.01)
 ;;39.73 
 ;;9002226.02101,"862,39.73 ",.02)
 ;;39.73 
 ;;9002226.02101,"862,39.79 ",.01)
 ;;39.79 
 ;;9002226.02101,"862,39.79 ",.02)
 ;;39.79 
 ;;9002226.02101,"862,39.8 ",.01)
 ;;39.8 
 ;;9002226.02101,"862,39.8 ",.02)
 ;;39.8 
 ;;9002226.02101,"862,39.91 ",.01)
 ;;39.91 
 ;;9002226.02101,"862,39.91 ",.02)
 ;;39.91 
 ;;9002226.02101,"862,39.98 ",.01)
 ;;39.98 
 ;;9002226.02101,"862,39.98 ",.02)
 ;;39.98 
 ;;9002226.02101,"862,39.99 ",.01)
 ;;39.99 
 ;;9002226.02101,"862,39.99 ",.02)
 ;;39.99 
 ;;9002226.02101,"862,40.22 ",.01)
 ;;40.22 
 ;;9002226.02101,"862,40.22 ",.02)
 ;;40.22 
 ;;9002226.02101,"862,40.40 ",.01)
 ;;40.40 
 ;;9002226.02101,"862,40.40 ",.02)
 ;;40.40 
 ;;9002226.02101,"862,40.41 ",.01)
 ;;40.41 
 ;;9002226.02101,"862,40.41 ",.02)
 ;;40.41 
 ;;9002226.02101,"862,40.42 ",.01)
 ;;40.42 
 ;;9002226.02101,"862,40.42 ",.02)
 ;;40.42 
 ;;9002226.02101,"862,40.50 ",.01)
 ;;40.50 
 ;;9002226.02101,"862,40.50 ",.02)
 ;;40.50 
 ;;9002226.02101,"862,40.51 ",.01)
 ;;40.51 
 ;;9002226.02101,"862,40.51 ",.02)
 ;;40.51 
 ;;9002226.02101,"862,40.52 ",.01)
 ;;40.52 
 ;;9002226.02101,"862,40.52 ",.02)
 ;;40.52 
 ;;9002226.02101,"862,40.53 ",.01)
 ;;40.53 
 ;;9002226.02101,"862,40.53 ",.02)
 ;;40.53 
 ;;9002226.02101,"862,40.54 ",.01)
 ;;40.54 
 ;;9002226.02101,"862,40.54 ",.02)
 ;;40.54 
 ;;9002226.02101,"862,40.59 ",.01)
 ;;40.59 
 ;;9002226.02101,"862,40.59 ",.02)
 ;;40.59 
 ;;9002226.02101,"862,40.62 ",.01)
 ;;40.62 
 ;;9002226.02101,"862,40.62 ",.02)
 ;;40.62 
 ;;9002226.02101,"862,40.63 ",.01)
 ;;40.63 
 ;;9002226.02101,"862,40.63 ",.02)
 ;;40.63 
 ;;9002226.02101,"862,41.2 ",.01)
 ;;41.2 
 ;;9002226.02101,"862,41.2 ",.02)
 ;;41.2 
 ;;9002226.02101,"862,41.5 ",.01)
 ;;41.5 
 ;;9002226.02101,"862,41.5 ",.02)
 ;;41.5 
 ;;9002226.02101,"862,42.01 ",.01)
 ;;42.01 
 ;;9002226.02101,"862,42.01 ",.02)
 ;;42.01 
 ;;9002226.02101,"862,42.09 ",.01)
 ;;42.09 
 ;;9002226.02101,"862,42.09 ",.02)
 ;;42.09 
 ;;9002226.02101,"862,42.10 ",.01)
 ;;42.10 
 ;;9002226.02101,"862,42.10 ",.02)
 ;;42.10 
 ;;9002226.02101,"862,42.11 ",.01)
 ;;42.11 
 ;;9002226.02101,"862,42.11 ",.02)
 ;;42.11 
 ;;9002226.02101,"862,42.12 ",.01)
 ;;42.12 
 ;;9002226.02101,"862,42.12 ",.02)
 ;;42.12 
 ;;9002226.02101,"862,42.19 ",.01)
 ;;42.19 
 ;;9002226.02101,"862,42.19 ",.02)
 ;;42.19 
 ;;9002226.02101,"862,42.40 ",.01)
 ;;42.40 
 ;;9002226.02101,"862,42.40 ",.02)
 ;;42.40 
 ;;9002226.02101,"862,42.41 ",.01)
 ;;42.41 
 ;;9002226.02101,"862,42.41 ",.02)
 ;;42.41 
 ;;9002226.02101,"862,42.42 ",.01)
 ;;42.42 
 ;;9002226.02101,"862,42.42 ",.02)
 ;;42.42 
