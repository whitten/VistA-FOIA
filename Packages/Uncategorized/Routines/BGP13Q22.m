BGP13Q22 ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON APR 14, 2011 ;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;;JUN 27, 2011;Build 33
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"995,54868-5196-02 ",.01)
 ;;54868-5196-02
 ;;9002226.02101,"995,54868-5196-02 ",.02)
 ;;54868-5196-02
 ;;9002226.02101,"995,54868-5204-00 ",.01)
 ;;54868-5204-00
 ;;9002226.02101,"995,54868-5204-00 ",.02)
 ;;54868-5204-00
 ;;9002226.02101,"995,54868-5241-00 ",.01)
 ;;54868-5241-00
 ;;9002226.02101,"995,54868-5241-00 ",.02)
 ;;54868-5241-00
 ;;9002226.02101,"995,54868-5241-01 ",.01)
 ;;54868-5241-01
 ;;9002226.02101,"995,54868-5241-01 ",.02)
 ;;54868-5241-01
 ;;9002226.02101,"995,54868-5241-02 ",.01)
 ;;54868-5241-02
 ;;9002226.02101,"995,54868-5241-02 ",.02)
 ;;54868-5241-02
 ;;9002226.02101,"995,54868-5245-00 ",.01)
 ;;54868-5245-00
 ;;9002226.02101,"995,54868-5245-00 ",.02)
 ;;54868-5245-00
 ;;9002226.02101,"995,54868-5245-01 ",.01)
 ;;54868-5245-01
 ;;9002226.02101,"995,54868-5245-01 ",.02)
 ;;54868-5245-01
 ;;9002226.02101,"995,54868-5245-02 ",.01)
 ;;54868-5245-02
 ;;9002226.02101,"995,54868-5245-02 ",.02)
 ;;54868-5245-02
 ;;9002226.02101,"995,54868-5246-00 ",.01)
 ;;54868-5246-00
 ;;9002226.02101,"995,54868-5246-00 ",.02)
 ;;54868-5246-00
 ;;9002226.02101,"995,54868-5246-01 ",.01)
 ;;54868-5246-01
 ;;9002226.02101,"995,54868-5246-01 ",.02)
 ;;54868-5246-01
 ;;9002226.02101,"995,54868-5256-00 ",.01)
 ;;54868-5256-00
 ;;9002226.02101,"995,54868-5256-00 ",.02)
 ;;54868-5256-00
 ;;9002226.02101,"995,54868-5279-00 ",.01)
 ;;54868-5279-00
 ;;9002226.02101,"995,54868-5279-00 ",.02)
 ;;54868-5279-00
 ;;9002226.02101,"995,54868-5296-00 ",.01)
 ;;54868-5296-00
 ;;9002226.02101,"995,54868-5296-00 ",.02)
 ;;54868-5296-00
 ;;9002226.02101,"995,54868-5313-00 ",.01)
 ;;54868-5313-00
 ;;9002226.02101,"995,54868-5313-00 ",.02)
 ;;54868-5313-00
 ;;9002226.02101,"995,54868-5313-01 ",.01)
 ;;54868-5313-01
 ;;9002226.02101,"995,54868-5313-01 ",.02)
 ;;54868-5313-01
 ;;9002226.02101,"995,54868-5346-00 ",.01)
 ;;54868-5346-00
 ;;9002226.02101,"995,54868-5346-00 ",.02)
 ;;54868-5346-00
 ;;9002226.02101,"995,54868-5423-00 ",.01)
 ;;54868-5423-00
 ;;9002226.02101,"995,54868-5423-00 ",.02)
 ;;54868-5423-00
 ;;9002226.02101,"995,54868-5423-01 ",.01)
 ;;54868-5423-01
 ;;9002226.02101,"995,54868-5423-01 ",.02)
 ;;54868-5423-01
 ;;9002226.02101,"995,54868-5469-00 ",.01)
 ;;54868-5469-00
 ;;9002226.02101,"995,54868-5469-00 ",.02)
 ;;54868-5469-00
 ;;9002226.02101,"995,54868-5469-01 ",.01)
 ;;54868-5469-01
 ;;9002226.02101,"995,54868-5469-01 ",.02)
 ;;54868-5469-01
 ;;9002226.02101,"995,54868-5475-00 ",.01)
 ;;54868-5475-00
 ;;9002226.02101,"995,54868-5475-00 ",.02)
 ;;54868-5475-00
 ;;9002226.02101,"995,54868-5503-00 ",.01)
 ;;54868-5503-00
 ;;9002226.02101,"995,54868-5503-00 ",.02)
 ;;54868-5503-00
 ;;9002226.02101,"995,54868-5503-01 ",.01)
 ;;54868-5503-01
 ;;9002226.02101,"995,54868-5503-01 ",.02)
 ;;54868-5503-01
 ;;9002226.02101,"995,54868-5548-00 ",.01)
 ;;54868-5548-00
 ;;9002226.02101,"995,54868-5548-00 ",.02)
 ;;54868-5548-00
 ;;9002226.02101,"995,54868-5690-02 ",.01)
 ;;54868-5690-02
 ;;9002226.02101,"995,54868-5690-02 ",.02)
 ;;54868-5690-02
 ;;9002226.02101,"995,54868-5747-00 ",.01)
 ;;54868-5747-00
 ;;9002226.02101,"995,54868-5747-00 ",.02)
 ;;54868-5747-00
 ;;9002226.02101,"995,54868-5747-01 ",.01)
 ;;54868-5747-01
 ;;9002226.02101,"995,54868-5747-01 ",.02)
 ;;54868-5747-01
 ;;9002226.02101,"995,54868-5781-00 ",.01)
 ;;54868-5781-00
 ;;9002226.02101,"995,54868-5781-00 ",.02)
 ;;54868-5781-00
 ;;9002226.02101,"995,54868-5782-00 ",.01)
 ;;54868-5782-00
 ;;9002226.02101,"995,54868-5782-00 ",.02)
 ;;54868-5782-00
 ;;9002226.02101,"995,54868-5782-04 ",.01)
 ;;54868-5782-04
 ;;9002226.02101,"995,54868-5782-04 ",.02)
 ;;54868-5782-04
 ;;9002226.02101,"995,54868-5783-00 ",.01)
 ;;54868-5783-00
 ;;9002226.02101,"995,54868-5783-00 ",.02)
 ;;54868-5783-00
 ;;9002226.02101,"995,54868-5783-01 ",.01)
 ;;54868-5783-01
 ;;9002226.02101,"995,54868-5783-01 ",.02)
 ;;54868-5783-01
 ;;9002226.02101,"995,54868-5787-00 ",.01)
 ;;54868-5787-00
 ;;9002226.02101,"995,54868-5787-00 ",.02)
 ;;54868-5787-00
 ;;9002226.02101,"995,54868-5792-00 ",.01)
 ;;54868-5792-00
 ;;9002226.02101,"995,54868-5792-00 ",.02)
 ;;54868-5792-00
 ;;9002226.02101,"995,54868-5842-00 ",.01)
 ;;54868-5842-00
 ;;9002226.02101,"995,54868-5842-00 ",.02)
 ;;54868-5842-00
 ;;9002226.02101,"995,54868-5843-00 ",.01)
 ;;54868-5843-00
 ;;9002226.02101,"995,54868-5843-00 ",.02)
 ;;54868-5843-00
 ;;9002226.02101,"995,54868-5856-00 ",.01)
 ;;54868-5856-00
 ;;9002226.02101,"995,54868-5856-00 ",.02)
 ;;54868-5856-00
 ;;9002226.02101,"995,54868-5896-00 ",.01)
 ;;54868-5896-00
 ;;9002226.02101,"995,54868-5896-00 ",.02)
 ;;54868-5896-00
 ;;9002226.02101,"995,55045-2376-00 ",.01)
 ;;55045-2376-00
 ;;9002226.02101,"995,55045-2376-00 ",.02)
 ;;55045-2376-00
 ;;9002226.02101,"995,55045-2799-00 ",.01)
 ;;55045-2799-00
 ;;9002226.02101,"995,55045-2799-00 ",.02)
 ;;55045-2799-00
 ;;9002226.02101,"995,55045-2799-01 ",.01)
 ;;55045-2799-01
 ;;9002226.02101,"995,55045-2799-01 ",.02)
 ;;55045-2799-01
 ;;9002226.02101,"995,55045-2799-02 ",.01)
 ;;55045-2799-02
 ;;9002226.02101,"995,55045-2799-02 ",.02)
 ;;55045-2799-02
 ;;9002226.02101,"995,55045-2799-06 ",.01)
 ;;55045-2799-06
 ;;9002226.02101,"995,55045-2799-06 ",.02)
 ;;55045-2799-06
 ;;9002226.02101,"995,55045-2799-09 ",.01)
 ;;55045-2799-09
 ;;9002226.02101,"995,55045-2799-09 ",.02)
 ;;55045-2799-09
 ;;9002226.02101,"995,55045-2827-00 ",.01)
 ;;55045-2827-00
 ;;9002226.02101,"995,55045-2827-00 ",.02)
 ;;55045-2827-00
 ;;9002226.02101,"995,55045-2827-04 ",.01)
 ;;55045-2827-04
 ;;9002226.02101,"995,55045-2827-04 ",.02)
 ;;55045-2827-04
 ;;9002226.02101,"995,55045-2827-06 ",.01)
 ;;55045-2827-06
 ;;9002226.02101,"995,55045-2827-06 ",.02)
 ;;55045-2827-06
 ;;9002226.02101,"995,55045-2827-08 ",.01)
 ;;55045-2827-08
 ;;9002226.02101,"995,55045-2827-08 ",.02)
 ;;55045-2827-08
 ;;9002226.02101,"995,55045-2827-09 ",.01)
 ;;55045-2827-09
 ;;9002226.02101,"995,55045-2827-09 ",.02)
 ;;55045-2827-09
 ;;9002226.02101,"995,55045-2832-08 ",.01)
 ;;55045-2832-08
 ;;9002226.02101,"995,55045-2832-08 ",.02)
 ;;55045-2832-08
 ;;9002226.02101,"995,55045-2929-00 ",.01)
 ;;55045-2929-00
 ;;9002226.02101,"995,55045-2929-00 ",.02)
 ;;55045-2929-00
 ;;9002226.02101,"995,55045-2929-06 ",.01)
 ;;55045-2929-06
 ;;9002226.02101,"995,55045-2929-06 ",.02)
 ;;55045-2929-06
 ;;9002226.02101,"995,55045-2929-08 ",.01)
 ;;55045-2929-08
 ;;9002226.02101,"995,55045-2929-08 ",.02)
 ;;55045-2929-08
 ;;9002226.02101,"995,55045-2936-00 ",.01)
 ;;55045-2936-00
 ;;9002226.02101,"995,55045-2936-00 ",.02)
 ;;55045-2936-00
 ;;9002226.02101,"995,55045-2936-08 ",.01)
 ;;55045-2936-08
 ;;9002226.02101,"995,55045-2936-08 ",.02)
 ;;55045-2936-08
 ;;9002226.02101,"995,55045-2937-00 ",.01)
 ;;55045-2937-00
 ;;9002226.02101,"995,55045-2937-00 ",.02)
 ;;55045-2937-00
 ;;9002226.02101,"995,55045-2938-00 ",.01)
 ;;55045-2938-00
 ;;9002226.02101,"995,55045-2938-00 ",.02)
 ;;55045-2938-00
 ;;9002226.02101,"995,55045-2938-08 ",.01)
 ;;55045-2938-08
 ;;9002226.02101,"995,55045-2938-08 ",.02)
 ;;55045-2938-08
 ;;9002226.02101,"995,55045-2975-00 ",.01)
 ;;55045-2975-00
 ;;9002226.02101,"995,55045-2975-00 ",.02)
 ;;55045-2975-00
 ;;9002226.02101,"995,55045-2975-06 ",.01)
 ;;55045-2975-06
 ;;9002226.02101,"995,55045-2975-06 ",.02)
 ;;55045-2975-06
 ;;9002226.02101,"995,55045-2975-08 ",.01)
 ;;55045-2975-08
 ;;9002226.02101,"995,55045-2975-08 ",.02)
 ;;55045-2975-08
 ;;9002226.02101,"995,55045-3059-00 ",.01)
 ;;55045-3059-00
 ;;9002226.02101,"995,55045-3059-00 ",.02)
 ;;55045-3059-00
 ;;9002226.02101,"995,55045-3059-08 ",.01)
 ;;55045-3059-08
 ;;9002226.02101,"995,55045-3059-08 ",.02)
 ;;55045-3059-08
 ;;9002226.02101,"995,55045-3164-00 ",.01)
 ;;55045-3164-00
 ;;9002226.02101,"995,55045-3164-00 ",.02)
 ;;55045-3164-00
 ;;9002226.02101,"995,55045-3170-00 ",.01)
 ;;55045-3170-00
 ;;9002226.02101,"995,55045-3170-00 ",.02)
 ;;55045-3170-00
 ;;9002226.02101,"995,55045-3225-06 ",.01)
 ;;55045-3225-06
 ;;9002226.02101,"995,55045-3225-06 ",.02)
 ;;55045-3225-06
 ;;9002226.02101,"995,55045-3373-08 ",.01)
 ;;55045-3373-08
 ;;9002226.02101,"995,55045-3373-08 ",.02)
 ;;55045-3373-08
 ;;9002226.02101,"995,55045-3772-08 ",.01)
 ;;55045-3772-08
 ;;9002226.02101,"995,55045-3772-08 ",.02)
 ;;55045-3772-08
 ;;9002226.02101,"995,55111-0133-01 ",.01)
 ;;55111-0133-01
 ;;9002226.02101,"995,55111-0133-01 ",.02)
 ;;55111-0133-01
 ;;9002226.02101,"995,55111-0134-01 ",.01)
 ;;55111-0134-01
 ;;9002226.02101,"995,55111-0134-01 ",.02)
 ;;55111-0134-01
 ;;9002226.02101,"995,55111-0338-01 ",.01)
 ;;55111-0338-01
 ;;9002226.02101,"995,55111-0338-01 ",.02)
 ;;55111-0338-01
 ;;9002226.02101,"995,55111-0339-01 ",.01)
 ;;55111-0339-01
 ;;9002226.02101,"995,55111-0339-01 ",.02)
 ;;55111-0339-01
 ;;9002226.02101,"995,55111-0339-05 ",.01)
 ;;55111-0339-05
 ;;9002226.02101,"995,55111-0339-05 ",.02)
 ;;55111-0339-05
 ;;9002226.02101,"995,55111-0340-01 ",.01)
 ;;55111-0340-01
 ;;9002226.02101,"995,55111-0340-01 ",.02)
 ;;55111-0340-01
 ;;9002226.02101,"995,55111-0340-05 ",.01)
 ;;55111-0340-05
 ;;9002226.02101,"995,55111-0340-05 ",.02)
 ;;55111-0340-05
 ;;9002226.02101,"995,55111-0341-01 ",.01)
 ;;55111-0341-01
 ;;9002226.02101,"995,55111-0341-01 ",.02)
 ;;55111-0341-01
 ;;9002226.02101,"995,55111-0341-05 ",.01)
 ;;55111-0341-05
 ;;9002226.02101,"995,55111-0341-05 ",.02)
 ;;55111-0341-05
 ;;9002226.02101,"995,55111-0438-90 ",.01)
 ;;55111-0438-90
 ;;9002226.02101,"995,55111-0438-90 ",.02)
 ;;55111-0438-90
 ;;9002226.02101,"995,55111-0439-05 ",.01)
 ;;55111-0439-05
 ;;9002226.02101,"995,55111-0439-05 ",.02)
 ;;55111-0439-05
 ;;9002226.02101,"995,55111-0439-90 ",.01)
 ;;55111-0439-90
 ;;9002226.02101,"995,55111-0439-90 ",.02)
 ;;55111-0439-90
 ;;9002226.02101,"995,55111-0440-05 ",.01)
 ;;55111-0440-05
 ;;9002226.02101,"995,55111-0440-05 ",.02)
 ;;55111-0440-05
 ;;9002226.02101,"995,55111-0440-90 ",.01)
 ;;55111-0440-90
 ;;9002226.02101,"995,55111-0440-90 ",.02)
 ;;55111-0440-90
 ;;9002226.02101,"995,55111-0441-05 ",.01)
 ;;55111-0441-05
 ;;9002226.02101,"995,55111-0441-05 ",.02)
 ;;55111-0441-05
 ;;9002226.02101,"995,55111-0441-90 ",.01)
 ;;55111-0441-90
 ;;9002226.02101,"995,55111-0441-90 ",.02)
 ;;55111-0441-90
 ;;9002226.02101,"995,55111-0621-90 ",.01)
 ;;55111-0621-90
 ;;9002226.02101,"995,55111-0621-90 ",.02)
 ;;55111-0621-90
 ;;9002226.02101,"995,55111-0622-90 ",.01)
 ;;55111-0622-90
 ;;9002226.02101,"995,55111-0622-90 ",.02)
 ;;55111-0622-90
 ;;9002226.02101,"995,55111-0623-90 ",.01)
 ;;55111-0623-90
 ;;9002226.02101,"995,55111-0623-90 ",.02)
 ;;55111-0623-90
 ;;9002226.02101,"995,55111-0624-90 ",.01)
 ;;55111-0624-90
 ;;9002226.02101,"995,55111-0624-90 ",.02)
 ;;55111-0624-90
 ;;9002226.02101,"995,55175-3964-03 ",.01)
 ;;55175-3964-03
 ;;9002226.02101,"995,55175-3964-03 ",.02)
 ;;55175-3964-03
 ;;9002226.02101,"995,55289-0039-30 ",.01)
 ;;55289-0039-30
 ;;9002226.02101,"995,55289-0039-30 ",.02)
 ;;55289-0039-30
 ;;9002226.02101,"995,55289-0086-30 ",.01)
 ;;55289-0086-30
 ;;9002226.02101,"995,55289-0086-30 ",.02)
 ;;55289-0086-30
 ;;9002226.02101,"995,55289-0086-97 ",.01)
 ;;55289-0086-97
 ;;9002226.02101,"995,55289-0086-97 ",.02)
 ;;55289-0086-97
 ;;9002226.02101,"995,55289-0096-30 ",.01)
 ;;55289-0096-30
 ;;9002226.02101,"995,55289-0096-30 ",.02)
 ;;55289-0096-30
 ;;9002226.02101,"995,55289-0106-30 ",.01)
 ;;55289-0106-30
 ;;9002226.02101,"995,55289-0106-30 ",.02)
 ;;55289-0106-30
 ;;9002226.02101,"995,55289-0109-30 ",.01)
 ;;55289-0109-30
 ;;9002226.02101,"995,55289-0109-30 ",.02)
 ;;55289-0109-30
 ;;9002226.02101,"995,55289-0109-97 ",.01)
 ;;55289-0109-97
 ;;9002226.02101,"995,55289-0109-97 ",.02)
 ;;55289-0109-97
 ;;9002226.02101,"995,55289-0212-30 ",.01)
 ;;55289-0212-30
 ;;9002226.02101,"995,55289-0212-30 ",.02)
 ;;55289-0212-30
 ;;9002226.02101,"995,55289-0344-30 ",.01)
 ;;55289-0344-30
 ;;9002226.02101,"995,55289-0344-30 ",.02)
 ;;55289-0344-30
 ;;9002226.02101,"995,55289-0344-90 ",.01)
 ;;55289-0344-90
 ;;9002226.02101,"995,55289-0344-90 ",.02)
 ;;55289-0344-90
 ;;9002226.02101,"995,55289-0344-97 ",.01)
 ;;55289-0344-97
 ;;9002226.02101,"995,55289-0344-97 ",.02)
 ;;55289-0344-97
 ;;9002226.02101,"995,55289-0483-30 ",.01)
 ;;55289-0483-30
 ;;9002226.02101,"995,55289-0483-30 ",.02)
 ;;55289-0483-30
 ;;9002226.02101,"995,55289-0484-30 ",.01)
 ;;55289-0484-30
 ;;9002226.02101,"995,55289-0484-30 ",.02)
 ;;55289-0484-30
 ;;9002226.02101,"995,55289-0506-30 ",.01)
 ;;55289-0506-30
 ;;9002226.02101,"995,55289-0506-30 ",.02)
 ;;55289-0506-30
 ;;9002226.02101,"995,55289-0506-97 ",.01)
 ;;55289-0506-97
