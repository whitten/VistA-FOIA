BGPM5ACS ;IHS/MSC/MMT-CREATED BY ^ATXSTX ON JUL 15, 2011;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"846,21695046860 ",.02)
 ;;21695046860
 ;;9002226.02101,"846,21695046872 ",.01)
 ;;21695046872
 ;;9002226.02101,"846,21695046872 ",.02)
 ;;21695046872
 ;;9002226.02101,"846,21695046878 ",.01)
 ;;21695046878
 ;;9002226.02101,"846,21695046878 ",.02)
 ;;21695046878
 ;;9002226.02101,"846,21695046930 ",.01)
 ;;21695046930
 ;;9002226.02101,"846,21695046930 ",.02)
 ;;21695046930
 ;;9002226.02101,"846,21695046990 ",.01)
 ;;21695046990
 ;;9002226.02101,"846,21695046990 ",.02)
 ;;21695046990
 ;;9002226.02101,"846,21695047000 ",.01)
 ;;21695047000
 ;;9002226.02101,"846,21695047000 ",.02)
 ;;21695047000
 ;;9002226.02101,"846,21695047030 ",.01)
 ;;21695047030
 ;;9002226.02101,"846,21695047030 ",.02)
 ;;21695047030
 ;;9002226.02101,"846,21695047060 ",.01)
 ;;21695047060
 ;;9002226.02101,"846,21695047060 ",.02)
 ;;21695047060
 ;;9002226.02101,"846,21695047078 ",.01)
 ;;21695047078
 ;;9002226.02101,"846,21695047078 ",.02)
 ;;21695047078
 ;;9002226.02101,"846,21695047090 ",.01)
 ;;21695047090
 ;;9002226.02101,"846,21695047090 ",.02)
 ;;21695047090
 ;;9002226.02101,"846,21695056830 ",.01)
 ;;21695056830
 ;;9002226.02101,"846,21695056830 ",.02)
 ;;21695056830
 ;;9002226.02101,"846,21695074630 ",.01)
 ;;21695074630
 ;;9002226.02101,"846,21695074630 ",.02)
 ;;21695074630
 ;;9002226.02101,"846,21695074690 ",.01)
 ;;21695074690
 ;;9002226.02101,"846,21695074690 ",.02)
 ;;21695074690
 ;;9002226.02101,"846,21695074730 ",.01)
 ;;21695074730
 ;;9002226.02101,"846,21695074730 ",.02)
 ;;21695074730
 ;;9002226.02101,"846,21695074760 ",.01)
 ;;21695074760
 ;;9002226.02101,"846,21695074760 ",.02)
 ;;21695074760
 ;;9002226.02101,"846,21695074790 ",.01)
 ;;21695074790
 ;;9002226.02101,"846,21695074790 ",.02)
 ;;21695074790
 ;;9002226.02101,"846,21695089400 ",.01)
 ;;21695089400
 ;;9002226.02101,"846,21695089400 ",.02)
 ;;21695089400
 ;;9002226.02101,"846,23155005601 ",.01)
 ;;23155005601
 ;;9002226.02101,"846,23155005601 ",.02)
 ;;23155005601
 ;;9002226.02101,"846,23155005610 ",.01)
 ;;23155005610
 ;;9002226.02101,"846,23155005610 ",.02)
 ;;23155005610
 ;;9002226.02101,"846,23155005701 ",.01)
 ;;23155005701
 ;;9002226.02101,"846,23155005701 ",.02)
 ;;23155005701
 ;;9002226.02101,"846,23155005710 ",.01)
 ;;23155005710
 ;;9002226.02101,"846,23155005710 ",.02)
 ;;23155005710
 ;;9002226.02101,"846,23155005801 ",.01)
 ;;23155005801
 ;;9002226.02101,"846,23155005801 ",.02)
 ;;23155005801
 ;;9002226.02101,"846,23155005810 ",.01)
 ;;23155005810
 ;;9002226.02101,"846,23155005810 ",.02)
 ;;23155005810
 ;;9002226.02101,"846,23155011501 ",.01)
 ;;23155011501
 ;;9002226.02101,"846,23155011501 ",.02)
 ;;23155011501
 ;;9002226.02101,"846,23155011510 ",.01)
 ;;23155011510
 ;;9002226.02101,"846,23155011510 ",.02)
 ;;23155011510
 ;;9002226.02101,"846,23155011601 ",.01)
 ;;23155011601
 ;;9002226.02101,"846,23155011601 ",.02)
 ;;23155011601
 ;;9002226.02101,"846,23155011610 ",.01)
 ;;23155011610
 ;;9002226.02101,"846,23155011610 ",.02)
 ;;23155011610
 ;;9002226.02101,"846,23155011701 ",.01)
 ;;23155011701
 ;;9002226.02101,"846,23155011701 ",.02)
 ;;23155011701
 ;;9002226.02101,"846,23155011710 ",.01)
 ;;23155011710
 ;;9002226.02101,"846,23155011710 ",.02)
 ;;23155011710
 ;;9002226.02101,"846,23490046000 ",.01)
 ;;23490046000
 ;;9002226.02101,"846,23490046000 ",.02)
 ;;23490046000
 ;;9002226.02101,"846,23490046100 ",.01)
 ;;23490046100
 ;;9002226.02101,"846,23490046100 ",.02)
 ;;23490046100
 ;;9002226.02101,"846,23490064103 ",.01)
 ;;23490064103
 ;;9002226.02101,"846,23490064103 ",.02)
 ;;23490064103
 ;;9002226.02101,"846,23490064106 ",.01)
 ;;23490064106
 ;;9002226.02101,"846,23490064106 ",.02)
 ;;23490064106
 ;;9002226.02101,"846,23490064200 ",.01)
 ;;23490064200
 ;;9002226.02101,"846,23490064200 ",.02)
 ;;23490064200
 ;;9002226.02101,"846,23490064203 ",.01)
 ;;23490064203
 ;;9002226.02101,"846,23490064203 ",.02)
 ;;23490064203
 ;;9002226.02101,"846,23490064206 ",.01)
 ;;23490064206
 ;;9002226.02101,"846,23490064206 ",.02)
 ;;23490064206
 ;;9002226.02101,"846,23490064209 ",.01)
 ;;23490064209
 ;;9002226.02101,"846,23490064209 ",.02)
 ;;23490064209
 ;;9002226.02101,"846,23490112807 ",.01)
 ;;23490112807
 ;;9002226.02101,"846,23490112807 ",.02)
 ;;23490112807
 ;;9002226.02101,"846,23490563201 ",.01)
 ;;23490563201
 ;;9002226.02101,"846,23490563201 ",.02)
 ;;23490563201
 ;;9002226.02101,"846,23490563202 ",.01)
 ;;23490563202
 ;;9002226.02101,"846,23490563202 ",.02)
 ;;23490563202
 ;;9002226.02101,"846,23490563203 ",.01)
 ;;23490563203
 ;;9002226.02101,"846,23490563203 ",.02)
 ;;23490563203
 ;;9002226.02101,"846,23490563301 ",.01)
 ;;23490563301
 ;;9002226.02101,"846,23490563301 ",.02)
 ;;23490563301
 ;;9002226.02101,"846,23490563403 ",.01)
 ;;23490563403
 ;;9002226.02101,"846,23490563403 ",.02)
 ;;23490563403
 ;;9002226.02101,"846,23490563503 ",.01)
 ;;23490563503
 ;;9002226.02101,"846,23490563503 ",.02)
 ;;23490563503
 ;;9002226.02101,"846,23490563801 ",.01)
 ;;23490563801
 ;;9002226.02101,"846,23490563801 ",.02)
 ;;23490563801
 ;;9002226.02101,"846,23490563802 ",.01)
 ;;23490563802
 ;;9002226.02101,"846,23490563802 ",.02)
 ;;23490563802
 ;;9002226.02101,"846,23490563901 ",.01)
 ;;23490563901
 ;;9002226.02101,"846,23490563901 ",.02)
 ;;23490563901
 ;;9002226.02101,"846,23490563902 ",.01)
 ;;23490563902
 ;;9002226.02101,"846,23490563902 ",.02)
 ;;23490563902
 ;;9002226.02101,"846,23490744901 ",.01)
 ;;23490744901
 ;;9002226.02101,"846,23490744901 ",.02)
 ;;23490744901
 ;;9002226.02101,"846,24196054501 ",.01)
 ;;24196054501
 ;;9002226.02101,"846,24196054501 ",.02)
 ;;24196054501
 ;;9002226.02101,"846,24196054505 ",.01)
 ;;24196054505
 ;;9002226.02101,"846,24196054505 ",.02)
 ;;24196054505
 ;;9002226.02101,"846,24196054510 ",.01)
 ;;24196054510
 ;;9002226.02101,"846,24196054510 ",.02)
 ;;24196054510
 ;;9002226.02101,"846,24196054601 ",.01)
 ;;24196054601
 ;;9002226.02101,"846,24196054601 ",.02)
 ;;24196054601
 ;;9002226.02101,"846,24196054605 ",.01)
 ;;24196054605
 ;;9002226.02101,"846,24196054605 ",.02)
 ;;24196054605
 ;;9002226.02101,"846,24196054610 ",.01)
 ;;24196054610
 ;;9002226.02101,"846,24196054610 ",.02)
 ;;24196054610
 ;;9002226.02101,"846,24236006402 ",.01)
 ;;24236006402
 ;;9002226.02101,"846,24236006402 ",.02)
 ;;24236006402
 ;;9002226.02101,"846,24236017202 ",.01)
 ;;24236017202
 ;;9002226.02101,"846,24236017202 ",.02)
 ;;24236017202
 ;;9002226.02101,"846,24236018002 ",.01)
 ;;24236018002
 ;;9002226.02101,"846,24236018002 ",.02)
 ;;24236018002
 ;;9002226.02101,"846,24236021602 ",.01)
 ;;24236021602
 ;;9002226.02101,"846,24236021602 ",.02)
 ;;24236021602
 ;;9002226.02101,"846,24236025202 ",.01)
 ;;24236025202
 ;;9002226.02101,"846,24236025202 ",.02)
 ;;24236025202
 ;;9002226.02101,"846,24236025502 ",.01)
 ;;24236025502
 ;;9002226.02101,"846,24236025502 ",.02)
 ;;24236025502
 ;;9002226.02101,"846,24236026602 ",.01)
 ;;24236026602
 ;;9002226.02101,"846,24236026602 ",.02)
 ;;24236026602
 ;;9002226.02101,"846,24236027602 ",.01)
 ;;24236027602
 ;;9002226.02101,"846,24236027602 ",.02)
 ;;24236027602
 ;;9002226.02101,"846,24236036802 ",.01)
 ;;24236036802
 ;;9002226.02101,"846,24236036802 ",.02)
 ;;24236036802
 ;;9002226.02101,"846,24236038102 ",.01)
 ;;24236038102
 ;;9002226.02101,"846,24236038102 ",.02)
 ;;24236038102
 ;;9002226.02101,"846,24236043102 ",.01)
 ;;24236043102
 ;;9002226.02101,"846,24236043102 ",.02)
 ;;24236043102
 ;;9002226.02101,"846,24236048902 ",.01)
 ;;24236048902
 ;;9002226.02101,"846,24236048902 ",.02)
 ;;24236048902
 ;;9002226.02101,"846,24236063202 ",.01)
 ;;24236063202
 ;;9002226.02101,"846,24236063202 ",.02)
 ;;24236063202
 ;;9002226.02101,"846,24236079902 ",.01)
 ;;24236079902
 ;;9002226.02101,"846,24236079902 ",.02)
 ;;24236079902
 ;;9002226.02101,"846,26053006701 ",.01)
 ;;26053006701
 ;;9002226.02101,"846,26053006701 ",.02)
 ;;26053006701
 ;;9002226.02101,"846,26053015701 ",.01)
 ;;26053015701
 ;;9002226.02101,"846,26053015701 ",.02)
 ;;26053015701
 ;;9002226.02101,"846,26053015801 ",.01)
 ;;26053015801
 ;;9002226.02101,"846,26053015801 ",.02)
 ;;26053015801
 ;;9002226.02101,"846,26053015901 ",.01)
 ;;26053015901
 ;;9002226.02101,"846,26053015901 ",.02)
 ;;26053015901
 ;;9002226.02101,"846,26053016001 ",.01)
 ;;26053016001
 ;;9002226.02101,"846,26053016001 ",.02)
 ;;26053016001
 ;;9002226.02101,"846,26053016101 ",.01)
 ;;26053016101
 ;;9002226.02101,"846,26053016101 ",.02)
 ;;26053016101
 ;;9002226.02101,"846,26053016201 ",.01)
 ;;26053016201
 ;;9002226.02101,"846,26053016201 ",.02)
 ;;26053016201
 ;;9002226.02101,"846,26053016301 ",.01)
 ;;26053016301
 ;;9002226.02101,"846,26053016301 ",.02)
 ;;26053016301
 ;;9002226.02101,"846,26053016401 ",.01)
 ;;26053016401
 ;;9002226.02101,"846,26053016401 ",.02)
 ;;26053016401
 ;;9002226.02101,"846,33358015730 ",.01)
 ;;33358015730
 ;;9002226.02101,"846,33358015730 ",.02)
 ;;33358015730
 ;;9002226.02101,"846,33358015760 ",.01)
 ;;33358015760
 ;;9002226.02101,"846,33358015760 ",.02)
 ;;33358015760
 ;;9002226.02101,"846,33358015800 ",.01)
 ;;33358015800
 ;;9002226.02101,"846,33358015800 ",.02)
 ;;33358015800
 ;;9002226.02101,"846,33358015830 ",.01)
 ;;33358015830
 ;;9002226.02101,"846,33358015830 ",.02)
 ;;33358015830
 ;;9002226.02101,"846,33358015860 ",.01)
 ;;33358015860
 ;;9002226.02101,"846,33358015860 ",.02)
 ;;33358015860
 ;;9002226.02101,"846,33358016030 ",.01)
 ;;33358016030
 ;;9002226.02101,"846,33358016030 ",.02)
 ;;33358016030
 ;;9002226.02101,"846,33358016060 ",.01)
 ;;33358016060
 ;;9002226.02101,"846,33358016060 ",.02)
 ;;33358016060
 ;;9002226.02101,"846,33358016101 ",.01)
 ;;33358016101
 ;;9002226.02101,"846,33358016101 ",.02)
 ;;33358016101
 ;;9002226.02101,"846,33358016130 ",.01)
 ;;33358016130
 ;;9002226.02101,"846,33358016130 ",.02)
 ;;33358016130
 ;;9002226.02101,"846,33358016160 ",.01)
 ;;33358016160
 ;;9002226.02101,"846,33358016160 ",.02)
 ;;33358016160
 ;;9002226.02101,"846,35356011300 ",.01)
 ;;35356011300
 ;;9002226.02101,"846,35356011300 ",.02)
 ;;35356011300
 ;;9002226.02101,"846,35356011330 ",.01)
 ;;35356011330
 ;;9002226.02101,"846,35356011330 ",.02)
 ;;35356011330
 ;;9002226.02101,"846,35356036030 ",.01)
 ;;35356036030
 ;;9002226.02101,"846,35356036030 ",.02)
 ;;35356036030
 ;;9002226.02101,"846,42291030790 ",.01)
 ;;42291030790
 ;;9002226.02101,"846,42291030790 ",.02)
 ;;42291030790
 ;;9002226.02101,"846,42291030890 ",.01)
 ;;42291030890
 ;;9002226.02101,"846,42291030890 ",.02)
 ;;42291030890
 ;;9002226.02101,"846,42291030990 ",.01)
 ;;42291030990
 ;;9002226.02101,"846,42291030990 ",.02)
 ;;42291030990
 ;;9002226.02101,"846,43063003430 ",.01)
 ;;43063003430
 ;;9002226.02101,"846,43063003430 ",.02)
 ;;43063003430
 ;;9002226.02101,"846,43063003490 ",.01)
 ;;43063003490
 ;;9002226.02101,"846,43063003490 ",.02)
 ;;43063003490
 ;;9002226.02101,"846,43063011990 ",.01)
 ;;43063011990
 ;;9002226.02101,"846,43063011990 ",.02)
 ;;43063011990
 ;;9002226.02101,"846,43063012090 ",.01)
 ;;43063012090
 ;;9002226.02101,"846,43063012090 ",.02)
 ;;43063012090
 ;;9002226.02101,"846,43063012130 ",.01)
 ;;43063012130
 ;;9002226.02101,"846,43063012130 ",.02)
 ;;43063012130
 ;;9002226.02101,"846,43063012190 ",.01)
 ;;43063012190
 ;;9002226.02101,"846,43063012190 ",.02)
 ;;43063012190
 ;;9002226.02101,"846,43063012230 ",.01)
 ;;43063012230
 ;;9002226.02101,"846,43063012230 ",.02)
 ;;43063012230
 ;;9002226.02101,"846,43063012290 ",.01)
 ;;43063012290
 ;;9002226.02101,"846,43063012290 ",.02)
 ;;43063012290
 ;;9002226.02101,"846,43353026160 ",.01)
 ;;43353026160
