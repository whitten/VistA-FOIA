BGP9VXIH ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON MAR 25, 2009 ;
 ;;9.0;IHS CLINICAL REPORTING;;JUL 1, 2009
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"862,04.3 ",.02)
 ;;04.3 
 ;;9002226.02101,"862,04.41 ",.01)
 ;;04.41 
 ;;9002226.02101,"862,04.41 ",.02)
 ;;04.41 
 ;;9002226.02101,"862,04.42 ",.01)
 ;;04.42 
 ;;9002226.02101,"862,04.42 ",.02)
 ;;04.42 
 ;;9002226.02101,"862,05.0 ",.01)
 ;;05.0 
 ;;9002226.02101,"862,05.0 ",.02)
 ;;05.0 
 ;;9002226.02101,"862,05.21 ",.01)
 ;;05.21 
 ;;9002226.02101,"862,05.21 ",.02)
 ;;05.21 
 ;;9002226.02101,"862,05.22 ",.01)
 ;;05.22 
 ;;9002226.02101,"862,05.22 ",.02)
 ;;05.22 
 ;;9002226.02101,"862,05.23 ",.01)
 ;;05.23 
 ;;9002226.02101,"862,05.23 ",.02)
 ;;05.23 
 ;;9002226.02101,"862,05.24 ",.01)
 ;;05.24 
 ;;9002226.02101,"862,05.24 ",.02)
 ;;05.24 
 ;;9002226.02101,"862,05.25 ",.01)
 ;;05.25 
 ;;9002226.02101,"862,05.25 ",.02)
 ;;05.25 
 ;;9002226.02101,"862,05.29 ",.01)
 ;;05.29 
 ;;9002226.02101,"862,05.29 ",.02)
 ;;05.29 
 ;;9002226.02101,"862,06.02 ",.01)
 ;;06.02 
 ;;9002226.02101,"862,06.02 ",.02)
 ;;06.02 
 ;;9002226.02101,"862,06.09 ",.01)
 ;;06.09 
 ;;9002226.02101,"862,06.09 ",.02)
 ;;06.09 
 ;;9002226.02101,"862,06.2 ",.01)
 ;;06.2 
 ;;9002226.02101,"862,06.2 ",.02)
 ;;06.2 
 ;;9002226.02101,"862,06.31 ",.01)
 ;;06.31 
 ;;9002226.02101,"862,06.31 ",.02)
 ;;06.31 
 ;;9002226.02101,"862,06.39 ",.01)
 ;;06.39 
 ;;9002226.02101,"862,06.39 ",.02)
 ;;06.39 
 ;;9002226.02101,"862,06.4 ",.01)
 ;;06.4 
 ;;9002226.02101,"862,06.4 ",.02)
 ;;06.4 
 ;;9002226.02101,"862,06.50 ",.01)
 ;;06.50 
 ;;9002226.02101,"862,06.50 ",.02)
 ;;06.50 
 ;;9002226.02101,"862,06.51 ",.01)
 ;;06.51 
 ;;9002226.02101,"862,06.51 ",.02)
 ;;06.51 
 ;;9002226.02101,"862,06.52 ",.01)
 ;;06.52 
 ;;9002226.02101,"862,06.52 ",.02)
 ;;06.52 
 ;;9002226.02101,"862,06.6 ",.01)
 ;;06.6 
 ;;9002226.02101,"862,06.6 ",.02)
 ;;06.6 
 ;;9002226.02101,"862,06.7 ",.01)
 ;;06.7 
 ;;9002226.02101,"862,06.7 ",.02)
 ;;06.7 
 ;;9002226.02101,"862,06.81 ",.01)
 ;;06.81 
 ;;9002226.02101,"862,06.81 ",.02)
 ;;06.81 
 ;;9002226.02101,"862,06.89 ",.01)
 ;;06.89 
 ;;9002226.02101,"862,06.89 ",.02)
 ;;06.89 
 ;;9002226.02101,"862,06.91 ",.01)
 ;;06.91 
 ;;9002226.02101,"862,06.91 ",.02)
 ;;06.91 
 ;;9002226.02101,"862,06.92 ",.01)
 ;;06.92 
 ;;9002226.02101,"862,06.92 ",.02)
 ;;06.92 
 ;;9002226.02101,"862,06.93 ",.01)
 ;;06.93 
 ;;9002226.02101,"862,06.93 ",.02)
 ;;06.93 
 ;;9002226.02101,"862,06.94 ",.01)
 ;;06.94 
 ;;9002226.02101,"862,06.94 ",.02)
 ;;06.94 
 ;;9002226.02101,"862,06.95 ",.01)
 ;;06.95 
 ;;9002226.02101,"862,06.95 ",.02)
 ;;06.95 
 ;;9002226.02101,"862,06.98 ",.01)
 ;;06.98 
 ;;9002226.02101,"862,06.98 ",.02)
 ;;06.98 
 ;;9002226.02101,"862,06.99 ",.01)
 ;;06.99 
 ;;9002226.02101,"862,06.99 ",.02)
 ;;06.99 
 ;;9002226.02101,"862,07.00 ",.01)
 ;;07.00 
 ;;9002226.02101,"862,07.00 ",.02)
 ;;07.00 
 ;;9002226.02101,"862,07.01 ",.01)
 ;;07.01 
 ;;9002226.02101,"862,07.01 ",.02)
 ;;07.01 
 ;;9002226.02101,"862,07.02 ",.01)
 ;;07.02 
 ;;9002226.02101,"862,07.02 ",.02)
 ;;07.02 
 ;;9002226.02101,"862,07.21 ",.01)
 ;;07.21 
 ;;9002226.02101,"862,07.21 ",.02)
 ;;07.21 
 ;;9002226.02101,"862,07.22 ",.01)
 ;;07.22 
 ;;9002226.02101,"862,07.22 ",.02)
 ;;07.22 
 ;;9002226.02101,"862,07.29 ",.01)
 ;;07.29 
 ;;9002226.02101,"862,07.29 ",.02)
 ;;07.29 
 ;;9002226.02101,"862,07.3 ",.01)
 ;;07.3 
 ;;9002226.02101,"862,07.3 ",.02)
 ;;07.3 
 ;;9002226.02101,"862,07.41 ",.01)
 ;;07.41 
 ;;9002226.02101,"862,07.41 ",.02)
 ;;07.41 
 ;;9002226.02101,"862,07.42 ",.01)
 ;;07.42 
 ;;9002226.02101,"862,07.42 ",.02)
 ;;07.42 
 ;;9002226.02101,"862,07.43 ",.01)
 ;;07.43 
 ;;9002226.02101,"862,07.43 ",.02)
 ;;07.43 
 ;;9002226.02101,"862,07.44 ",.01)
 ;;07.44 
 ;;9002226.02101,"862,07.44 ",.02)
 ;;07.44 
 ;;9002226.02101,"862,07.45 ",.01)
 ;;07.45 
 ;;9002226.02101,"862,07.45 ",.02)
 ;;07.45 
 ;;9002226.02101,"862,07.49 ",.01)
 ;;07.49 
 ;;9002226.02101,"862,07.49 ",.02)
 ;;07.49 
 ;;9002226.02101,"862,07.51 ",.01)
 ;;07.51 
 ;;9002226.02101,"862,07.51 ",.02)
 ;;07.51 
 ;;9002226.02101,"862,07.52 ",.01)
 ;;07.52 
 ;;9002226.02101,"862,07.52 ",.02)
 ;;07.52 
 ;;9002226.02101,"862,07.53 ",.01)
 ;;07.53 
 ;;9002226.02101,"862,07.53 ",.02)
 ;;07.53 
 ;;9002226.02101,"862,07.54 ",.01)
 ;;07.54 
 ;;9002226.02101,"862,07.54 ",.02)
 ;;07.54 
 ;;9002226.02101,"862,07.59 ",.01)
 ;;07.59 
 ;;9002226.02101,"862,07.59 ",.02)
 ;;07.59 
 ;;9002226.02101,"862,07.61 ",.01)
 ;;07.61 
 ;;9002226.02101,"862,07.61 ",.02)
 ;;07.61 
 ;;9002226.02101,"862,07.62 ",.01)
 ;;07.62 
 ;;9002226.02101,"862,07.62 ",.02)
 ;;07.62 
 ;;9002226.02101,"862,07.63 ",.01)
 ;;07.63 
 ;;9002226.02101,"862,07.63 ",.02)
 ;;07.63 
 ;;9002226.02101,"862,07.64 ",.01)
 ;;07.64 
 ;;9002226.02101,"862,07.64 ",.02)
 ;;07.64 
 ;;9002226.02101,"862,07.65 ",.01)
 ;;07.65 
 ;;9002226.02101,"862,07.65 ",.02)
 ;;07.65 
 ;;9002226.02101,"862,07.68 ",.01)
 ;;07.68 
 ;;9002226.02101,"862,07.68 ",.02)
 ;;07.68 
 ;;9002226.02101,"862,07.69 ",.01)
 ;;07.69 
 ;;9002226.02101,"862,07.69 ",.02)
 ;;07.69 
 ;;9002226.02101,"862,07.71 ",.01)
 ;;07.71 
 ;;9002226.02101,"862,07.71 ",.02)
 ;;07.71 
 ;;9002226.02101,"862,07.72 ",.01)
 ;;07.72 
