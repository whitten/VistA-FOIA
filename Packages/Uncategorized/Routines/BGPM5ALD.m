BGPM5ALD ;IHS/MSC/MMT-CREATED BY ^ATXSTX ON JUL 15, 2011;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"885,66553000301 ",.02)
 ;;66553000301
 ;;9002226.02101,"885,66715971001 ",.01)
 ;;66715971001
 ;;9002226.02101,"885,66715971001 ",.02)
 ;;66715971001
 ;;9002226.02101,"885,66715971002 ",.01)
 ;;66715971002
 ;;9002226.02101,"885,66715971002 ",.02)
 ;;66715971002
 ;;9002226.02101,"885,67046016230 ",.01)
 ;;67046016230
 ;;9002226.02101,"885,67046016230 ",.02)
 ;;67046016230
 ;;9002226.02101,"885,67046016430 ",.01)
 ;;67046016430
 ;;9002226.02101,"885,67046016430 ",.02)
 ;;67046016430
 ;;9002226.02101,"885,67046016530 ",.01)
 ;;67046016530
 ;;9002226.02101,"885,67046016530 ",.02)
 ;;67046016530
 ;;9002226.02101,"885,67046016830 ",.01)
 ;;67046016830
 ;;9002226.02101,"885,67046016830 ",.02)
 ;;67046016830
 ;;9002226.02101,"885,67046060430 ",.01)
 ;;67046060430
 ;;9002226.02101,"885,67046060430 ",.02)
 ;;67046060430
 ;;9002226.02101,"885,67216017101 ",.01)
 ;;67216017101
 ;;9002226.02101,"885,67216017101 ",.02)
 ;;67216017101
 ;;9002226.02101,"885,67216017103 ",.01)
 ;;67216017103
 ;;9002226.02101,"885,67216017103 ",.02)
 ;;67216017103
 ;;9002226.02101,"885,67216017105 ",.01)
 ;;67216017105
 ;;9002226.02101,"885,67216017105 ",.02)
 ;;67216017105
 ;;9002226.02101,"885,67216017106 ",.01)
 ;;67216017106
 ;;9002226.02101,"885,67216017106 ",.02)
 ;;67216017106
 ;;9002226.02101,"885,67544068531 ",.01)
 ;;67544068531
 ;;9002226.02101,"885,67544068531 ",.02)
 ;;67544068531
 ;;9002226.02101,"885,67754097571 ",.01)
 ;;67754097571
 ;;9002226.02101,"885,67754097571 ",.02)
 ;;67754097571
 ;;9002226.02101,"885,68115043320 ",.01)
 ;;68115043320
 ;;9002226.02101,"885,68115043320 ",.02)
 ;;68115043320
 ;;9002226.02101,"885,68115049948 ",.01)
 ;;68115049948
 ;;9002226.02101,"885,68115049948 ",.02)
 ;;68115049948
 ;;9002226.02101,"885,68752000336 ",.01)
 ;;68752000336
 ;;9002226.02101,"885,68752000336 ",.02)
 ;;68752000336
 ;;9002226.02101,"885,68788018602 ",.01)
 ;;68788018602
 ;;9002226.02101,"885,68788018602 ",.02)
 ;;68788018602
 ;;9002226.02101,"885,68788178501 ",.01)
 ;;68788178501
 ;;9002226.02101,"885,68788178501 ",.02)
 ;;68788178501
 ;;9002226.02101,"885,68788178503 ",.01)
 ;;68788178503
 ;;9002226.02101,"885,68788178503 ",.02)
 ;;68788178503
 ;;9002226.02101,"885,68788178506 ",.01)
 ;;68788178506
 ;;9002226.02101,"885,68788178506 ",.02)
 ;;68788178506
 ;;9002226.02101,"885,70030041190 ",.01)
 ;;70030041190
 ;;9002226.02101,"885,70030041190 ",.02)
 ;;70030041190
 ;;9002226.02101,"885,70253021807 ",.01)
 ;;70253021807
 ;;9002226.02101,"885,70253021807 ",.02)
 ;;70253021807
 ;;9002226.02101,"885,74684000710 ",.01)
 ;;74684000710
 ;;9002226.02101,"885,74684000710 ",.02)
 ;;74684000710
 ;;9002226.02101,"885,75942000110 ",.01)
 ;;75942000110
 ;;9002226.02101,"885,75942000110 ",.02)
 ;;75942000110
 ;;9002226.02101,"885,75942000112 ",.01)
 ;;75942000112
 ;;9002226.02101,"885,75942000112 ",.02)
 ;;75942000112
 ;;9002226.02101,"885,75942000136 ",.01)
 ;;75942000136
 ;;9002226.02101,"885,75942000136 ",.02)
 ;;75942000136
 ;;9002226.02101,"885,75942000150 ",.01)
 ;;75942000150
 ;;9002226.02101,"885,75942000150 ",.02)
 ;;75942000150
 ;;9002226.02101,"885,75942000175 ",.01)
 ;;75942000175
 ;;9002226.02101,"885,75942000175 ",.02)
 ;;75942000175
