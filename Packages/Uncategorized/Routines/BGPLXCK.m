BGPLXCK ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON NOV 18, 2003 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"781,22708-2 ",.02)
 ;;22708-2
 ;;9002226.02101,"781,22709-0 ",.01)
 ;;22709-0
 ;;9002226.02101,"781,22709-0 ",.02)
 ;;22709-0
 ;;9002226.02101,"781,22710-8 ",.01)
 ;;22710-8
 ;;9002226.02101,"781,22710-8 ",.02)
 ;;22710-8
 ;;9002226.02101,"781,22711-6 ",.01)
 ;;22711-6
 ;;9002226.02101,"781,22711-6 ",.02)
 ;;22711-6
 ;;9002226.02101,"781,22726-4 ",.01)
 ;;22726-4
 ;;9002226.02101,"781,22726-4 ",.02)
 ;;22726-4
 ;;9002226.02101,"781,23853-5 ",.01)
 ;;23853-5
 ;;9002226.02101,"781,23853-5 ",.02)
 ;;23853-5
 ;;9002226.02101,"781,24435-0 ",.01)
 ;;24435-0
 ;;9002226.02101,"781,24435-0 ",.02)
 ;;24435-0
 ;;9002226.02101,"781,24436-8 ",.01)
 ;;24436-8
 ;;9002226.02101,"781,24436-8 ",.02)
 ;;24436-8
 ;;9002226.02101,"781,24437-6 ",.01)
 ;;24437-6
 ;;9002226.02101,"781,24437-6 ",.02)
 ;;24437-6
 ;;9002226.02101,"781,24438-4 ",.01)
 ;;24438-4
 ;;9002226.02101,"781,24438-4 ",.02)
 ;;24438-4
 ;;9002226.02101,"781,24439-2 ",.01)
 ;;24439-2
 ;;9002226.02101,"781,24439-2 ",.02)
 ;;24439-2
 ;;9002226.02101,"781,24440-0 ",.01)
 ;;24440-0
 ;;9002226.02101,"781,24440-0 ",.02)
 ;;24440-0
 ;;9002226.02101,"781,24441-8 ",.01)
 ;;24441-8
 ;;9002226.02101,"781,24441-8 ",.02)
 ;;24441-8
 ;;9002226.02101,"781,24442-6 ",.01)
 ;;24442-6
 ;;9002226.02101,"781,24442-6 ",.02)
 ;;24442-6
 ;;9002226.02101,"781,24443-4 ",.01)
 ;;24443-4
 ;;9002226.02101,"781,24443-4 ",.02)
 ;;24443-4
 ;;9002226.02101,"781,24444-2 ",.01)
 ;;24444-2
 ;;9002226.02101,"781,24444-2 ",.02)
 ;;24444-2
 ;;9002226.02101,"781,24445-9 ",.01)
 ;;24445-9
 ;;9002226.02101,"781,24445-9 ",.02)
 ;;24445-9
 ;;9002226.02101,"781,24456-6 ",.01)
 ;;24456-6
 ;;9002226.02101,"781,24456-6 ",.02)
 ;;24456-6
 ;;9002226.02101,"781,24518-3 ",.01)
 ;;24518-3
 ;;9002226.02101,"781,24518-3 ",.02)
 ;;24518-3
 ;;9002226.02101,"781,24522-5 ",.01)
 ;;24522-5
 ;;9002226.02101,"781,24522-5 ",.02)
 ;;24522-5
 ;;9002226.02101,"781,24523-3 ",.01)
 ;;24523-3
 ;;9002226.02101,"781,24523-3 ",.02)
 ;;24523-3
 ;;9002226.02101,"781,24524-1 ",.01)
 ;;24524-1
 ;;9002226.02101,"781,24524-1 ",.02)
 ;;24524-1
 ;;9002226.02101,"781,25082-9 ",.01)
 ;;25082-9
 ;;9002226.02101,"781,25082-9 ",.02)
 ;;25082-9
 ;;9002226.02101,"781,25083-7 ",.01)
 ;;25083-7
 ;;9002226.02101,"781,25083-7 ",.02)
 ;;25083-7
 ;;9002226.02101,"781,25084-5 ",.01)
 ;;25084-5
 ;;9002226.02101,"781,25084-5 ",.02)
 ;;25084-5
 ;;9002226.02101,"781,25085-2 ",.01)
 ;;25085-2
 ;;9002226.02101,"781,25085-2 ",.02)
 ;;25085-2
 ;;9002226.02101,"781,25086-0 ",.01)
 ;;25086-0
 ;;9002226.02101,"781,25086-0 ",.02)
 ;;25086-0
 ;;9002226.02101,"781,25087-8 ",.01)
 ;;25087-8
 ;;9002226.02101,"781,25087-8 ",.02)
 ;;25087-8
 ;;9002226.02101,"781,25088-6 ",.01)
 ;;25088-6
 ;;9002226.02101,"781,25088-6 ",.02)
 ;;25088-6
 ;;9002226.02101,"781,25089-4 ",.01)
 ;;25089-4
 ;;9002226.02101,"781,25089-4 ",.02)
 ;;25089-4
 ;;9002226.02101,"781,25090-2 ",.01)
 ;;25090-2
 ;;9002226.02101,"781,25090-2 ",.02)
 ;;25090-2
 ;;9002226.02101,"781,25091-0 ",.01)
 ;;25091-0
 ;;9002226.02101,"781,25091-0 ",.02)
 ;;25091-0
 ;;9002226.02101,"781,25092-8 ",.01)
 ;;25092-8
 ;;9002226.02101,"781,25092-8 ",.02)
 ;;25092-8
 ;;9002226.02101,"781,25093-6 ",.01)
 ;;25093-6
 ;;9002226.02101,"781,25093-6 ",.02)
 ;;25093-6
 ;;9002226.02101,"781,25094-4 ",.01)
 ;;25094-4
 ;;9002226.02101,"781,25094-4 ",.02)
 ;;25094-4
 ;;9002226.02101,"781,25095-1 ",.01)
 ;;25095-1
 ;;9002226.02101,"781,25095-1 ",.02)
 ;;25095-1
 ;;9002226.02101,"781,25096-9 ",.01)
 ;;25096-9
 ;;9002226.02101,"781,25096-9 ",.02)
 ;;25096-9
 ;;9002226.02101,"781,25097-7 ",.01)
 ;;25097-7
 ;;9002226.02101,"781,25097-7 ",.02)
 ;;25097-7
 ;;9002226.02101,"781,25098-5 ",.01)
 ;;25098-5
 ;;9002226.02101,"781,25098-5 ",.02)
 ;;25098-5
 ;;9002226.02101,"781,25099-3 ",.01)
 ;;25099-3
 ;;9002226.02101,"781,25099-3 ",.02)
 ;;25099-3
 ;;9002226.02101,"781,25100-9 ",.01)
 ;;25100-9
 ;;9002226.02101,"781,25100-9 ",.02)
 ;;25100-9
 ;;9002226.02101,"781,25101-7 ",.01)
 ;;25101-7
 ;;9002226.02101,"781,25101-7 ",.02)
 ;;25101-7
 ;;9002226.02101,"781,25102-5 ",.01)
 ;;25102-5
 ;;9002226.02101,"781,25102-5 ",.02)
 ;;25102-5
 ;;9002226.02101,"781,25103-3 ",.01)
 ;;25103-3
 ;;9002226.02101,"781,25103-3 ",.02)
 ;;25103-3
 ;;9002226.02101,"781,25104-1 ",.01)
 ;;25104-1
 ;;9002226.02101,"781,25104-1 ",.02)
 ;;25104-1
 ;;9002226.02101,"781,25105-8 ",.01)
 ;;25105-8
 ;;9002226.02101,"781,25105-8 ",.02)
 ;;25105-8
 ;;9002226.02101,"781,25106-6 ",.01)
 ;;25106-6
 ;;9002226.02101,"781,25106-6 ",.02)
 ;;25106-6
 ;;9002226.02101,"781,25107-4 ",.01)
 ;;25107-4
 ;;9002226.02101,"781,25107-4 ",.02)
 ;;25107-4
 ;;9002226.02101,"781,25108-2 ",.01)
 ;;25108-2
 ;;9002226.02101,"781,25108-2 ",.02)
 ;;25108-2
 ;;9002226.02101,"781,25109-0 ",.01)
 ;;25109-0
 ;;9002226.02101,"781,25109-0 ",.02)
 ;;25109-0
 ;;9002226.02101,"781,25110-8 ",.01)
 ;;25110-8
 ;;9002226.02101,"781,25110-8 ",.02)
 ;;25110-8
 ;;9002226.02101,"781,25111-6 ",.01)
 ;;25111-6
 ;;9002226.02101,"781,25111-6 ",.02)
 ;;25111-6
 ;;9002226.02101,"781,25112-4 ",.01)
 ;;25112-4
 ;;9002226.02101,"781,25112-4 ",.02)
 ;;25112-4
