BGPM5BCV ;IHS/MSC/SAT-CREATED BY ^ATXSTX ON SEP 12, 2011;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"1044,55289012412 ",.02)
 ;;55289012412
 ;;9002226.02101,"1044,55289012430 ",.01)
 ;;55289012430
 ;;9002226.02101,"1044,55289012430 ",.02)
 ;;55289012430
 ;;9002226.02101,"1044,55289014414 ",.01)
 ;;55289014414
 ;;9002226.02101,"1044,55289014414 ",.02)
 ;;55289014414
 ;;9002226.02101,"1044,55289014430 ",.01)
 ;;55289014430
 ;;9002226.02101,"1044,55289014430 ",.02)
 ;;55289014430
 ;;9002226.02101,"1044,55289014490 ",.01)
 ;;55289014490
 ;;9002226.02101,"1044,55289014490 ",.02)
 ;;55289014490
 ;;9002226.02101,"1044,55289014930 ",.01)
 ;;55289014930
 ;;9002226.02101,"1044,55289014930 ",.02)
 ;;55289014930
 ;;9002226.02101,"1044,55289014960 ",.01)
 ;;55289014960
 ;;9002226.02101,"1044,55289014960 ",.02)
 ;;55289014960
 ;;9002226.02101,"1044,55289018517 ",.01)
 ;;55289018517
 ;;9002226.02101,"1044,55289018517 ",.02)
 ;;55289018517
 ;;9002226.02101,"1044,55289022330 ",.01)
 ;;55289022330
 ;;9002226.02101,"1044,55289022330 ",.02)
 ;;55289022330
 ;;9002226.02101,"1044,55289022360 ",.01)
 ;;55289022360
 ;;9002226.02101,"1044,55289022360 ",.02)
 ;;55289022360
 ;;9002226.02101,"1044,55289022390 ",.01)
 ;;55289022390
 ;;9002226.02101,"1044,55289022390 ",.02)
 ;;55289022390
 ;;9002226.02101,"1044,55289029114 ",.01)
 ;;55289029114
 ;;9002226.02101,"1044,55289029114 ",.02)
 ;;55289029114
 ;;9002226.02101,"1044,55289029130 ",.01)
 ;;55289029130
 ;;9002226.02101,"1044,55289029130 ",.02)
 ;;55289029130
 ;;9002226.02101,"1044,55289029160 ",.01)
 ;;55289029160
 ;;9002226.02101,"1044,55289029160 ",.02)
 ;;55289029160
 ;;9002226.02101,"1044,55289037830 ",.01)
 ;;55289037830
 ;;9002226.02101,"1044,55289037830 ",.02)
 ;;55289037830
 ;;9002226.02101,"1044,55289038130 ",.01)
 ;;55289038130
 ;;9002226.02101,"1044,55289038130 ",.02)
 ;;55289038130
 ;;9002226.02101,"1044,55289038145 ",.01)
 ;;55289038145
 ;;9002226.02101,"1044,55289038145 ",.02)
 ;;55289038145
 ;;9002226.02101,"1044,55289038160 ",.01)
 ;;55289038160
 ;;9002226.02101,"1044,55289038160 ",.02)
 ;;55289038160
 ;;9002226.02101,"1044,55289038190 ",.01)
 ;;55289038190
 ;;9002226.02101,"1044,55289038190 ",.02)
 ;;55289038190
 ;;9002226.02101,"1044,55289058630 ",.01)
 ;;55289058630
 ;;9002226.02101,"1044,55289058630 ",.02)
 ;;55289058630
 ;;9002226.02101,"1044,55289061014 ",.01)
 ;;55289061014
 ;;9002226.02101,"1044,55289061014 ",.02)
 ;;55289061014
 ;;9002226.02101,"1044,55289061028 ",.01)
 ;;55289061028
 ;;9002226.02101,"1044,55289061028 ",.02)
 ;;55289061028
 ;;9002226.02101,"1044,55289061030 ",.01)
 ;;55289061030
 ;;9002226.02101,"1044,55289061030 ",.02)
 ;;55289061030
 ;;9002226.02101,"1044,55289061060 ",.01)
 ;;55289061060
 ;;9002226.02101,"1044,55289061060 ",.02)
 ;;55289061060
 ;;9002226.02101,"1044,55289061090 ",.01)
 ;;55289061090
 ;;9002226.02101,"1044,55289061090 ",.02)
 ;;55289061090
 ;;9002226.02101,"1044,55289061314 ",.01)
 ;;55289061314
 ;;9002226.02101,"1044,55289061314 ",.02)
 ;;55289061314
 ;;9002226.02101,"1044,55289061330 ",.01)
 ;;55289061330
 ;;9002226.02101,"1044,55289061330 ",.02)
 ;;55289061330
 ;;9002226.02101,"1044,55289061360 ",.01)
 ;;55289061360
 ;;9002226.02101,"1044,55289061360 ",.02)
 ;;55289061360
 ;;9002226.02101,"1044,55289073001 ",.01)
 ;;55289073001
 ;;9002226.02101,"1044,55289073001 ",.02)
 ;;55289073001
 ;;9002226.02101,"1044,55289073012 ",.01)
 ;;55289073012
 ;;9002226.02101,"1044,55289073012 ",.02)
 ;;55289073012
 ;;9002226.02101,"1044,55289073025 ",.01)
 ;;55289073025
 ;;9002226.02101,"1044,55289073025 ",.02)
 ;;55289073025
 ;;9002226.02101,"1044,55289073030 ",.01)
 ;;55289073030
 ;;9002226.02101,"1044,55289073030 ",.02)
 ;;55289073030
 ;;9002226.02101,"1044,55289073060 ",.01)
 ;;55289073060
 ;;9002226.02101,"1044,55289073060 ",.02)
 ;;55289073060
 ;;9002226.02101,"1044,55289073090 ",.01)
 ;;55289073090
 ;;9002226.02101,"1044,55289073090 ",.02)
 ;;55289073090
 ;;9002226.02101,"1044,55289083930 ",.01)
 ;;55289083930
 ;;9002226.02101,"1044,55289083930 ",.02)
 ;;55289083930
 ;;9002226.02101,"1044,55289088314 ",.01)
 ;;55289088314
 ;;9002226.02101,"1044,55289088314 ",.02)
 ;;55289088314
 ;;9002226.02101,"1044,55289088330 ",.01)
 ;;55289088330
 ;;9002226.02101,"1044,55289088330 ",.02)
 ;;55289088330
 ;;9002226.02101,"1044,55289088360 ",.01)
 ;;55289088360
 ;;9002226.02101,"1044,55289088360 ",.02)
 ;;55289088360
 ;;9002226.02101,"1044,55289088390 ",.01)
 ;;55289088390
 ;;9002226.02101,"1044,55289088390 ",.02)
 ;;55289088390
 ;;9002226.02101,"1044,55289097230 ",.01)
 ;;55289097230
 ;;9002226.02101,"1044,55289097230 ",.02)
 ;;55289097230
 ;;9002226.02101,"1044,55289097260 ",.01)
 ;;55289097260
 ;;9002226.02101,"1044,55289097260 ",.02)
 ;;55289097260
 ;;9002226.02101,"1044,55289097290 ",.01)
 ;;55289097290
 ;;9002226.02101,"1044,55289097290 ",.02)
 ;;55289097290
 ;;9002226.02101,"1044,55567011206 ",.01)
 ;;55567011206
 ;;9002226.02101,"1044,55567011206 ",.02)
 ;;55567011206
 ;;9002226.02101,"1044,55567011215 ",.01)
 ;;55567011215
 ;;9002226.02101,"1044,55567011215 ",.02)
 ;;55567011215
 ;;9002226.02101,"1044,55567011225 ",.01)
 ;;55567011225
 ;;9002226.02101,"1044,55567011225 ",.02)
 ;;55567011225
 ;;9002226.02101,"1044,55567011306 ",.01)
 ;;55567011306
 ;;9002226.02101,"1044,55567011306 ",.02)
 ;;55567011306
 ;;9002226.02101,"1044,55567011315 ",.01)
 ;;55567011315
 ;;9002226.02101,"1044,55567011315 ",.02)
 ;;55567011315
 ;;9002226.02101,"1044,55567017013 ",.01)
 ;;55567017013
 ;;9002226.02101,"1044,55567017013 ",.02)
 ;;55567017013
 ;;9002226.02101,"1044,55567017035 ",.01)
 ;;55567017035
 ;;9002226.02101,"1044,55567017035 ",.02)
 ;;55567017035
 ;;9002226.02101,"1044,55567017037 ",.01)
 ;;55567017037
 ;;9002226.02101,"1044,55567017037 ",.02)
 ;;55567017037
 ;;9002226.02101,"1044,55567017113 ",.01)
 ;;55567017113
 ;;9002226.02101,"1044,55567017113 ",.02)
 ;;55567017113
 ;;9002226.02101,"1044,55567017135 ",.01)
 ;;55567017135
 ;;9002226.02101,"1044,55567017135 ",.02)
 ;;55567017135
 ;;9002226.02101,"1044,55567017136 ",.01)
 ;;55567017136
 ;;9002226.02101,"1044,55567017136 ",.02)
 ;;55567017136
 ;;9002226.02101,"1044,55567017213 ",.01)
 ;;55567017213
 ;;9002226.02101,"1044,55567017213 ",.02)
 ;;55567017213
 ;;9002226.02101,"1044,55567017234 ",.01)
 ;;55567017234
 ;;9002226.02101,"1044,55567017234 ",.02)
 ;;55567017234
 ;;9002226.02101,"1044,55567017235 ",.01)
 ;;55567017235
 ;;9002226.02101,"1044,55567017235 ",.02)
 ;;55567017235
 ;;9002226.02101,"1044,55567021413 ",.01)
 ;;55567021413
 ;;9002226.02101,"1044,55567021413 ",.02)
 ;;55567021413
 ;;9002226.02101,"1044,55567021435 ",.01)
 ;;55567021435
 ;;9002226.02101,"1044,55567021435 ",.02)
 ;;55567021435
 ;;9002226.02101,"1044,55567021513 ",.01)
 ;;55567021513
 ;;9002226.02101,"1044,55567021513 ",.02)
 ;;55567021513
 ;;9002226.02101,"1044,55567021518 ",.01)
 ;;55567021518
 ;;9002226.02101,"1044,55567021518 ",.02)
 ;;55567021518
 ;;9002226.02101,"1044,55567021526 ",.01)
 ;;55567021526
 ;;9002226.02101,"1044,55567021526 ",.02)
 ;;55567021526
 ;;9002226.02101,"1044,55567021613 ",.01)
 ;;55567021613
 ;;9002226.02101,"1044,55567021613 ",.02)
 ;;55567021613
 ;;9002226.02101,"1044,55567021635 ",.01)
 ;;55567021635
 ;;9002226.02101,"1044,55567021635 ",.02)
 ;;55567021635
 ;;9002226.02101,"1044,55567021713 ",.01)
 ;;55567021713
 ;;9002226.02101,"1044,55567021713 ",.02)
 ;;55567021713
 ;;9002226.02101,"1044,55567021735 ",.01)
 ;;55567021735
 ;;9002226.02101,"1044,55567021735 ",.02)
 ;;55567021735
 ;;9002226.02101,"1044,55648071601 ",.01)
 ;;55648071601
 ;;9002226.02101,"1044,55648071601 ",.02)
 ;;55648071601
 ;;9002226.02101,"1044,55648071602 ",.01)
 ;;55648071602
 ;;9002226.02101,"1044,55648071602 ",.02)
 ;;55648071602
 ;;9002226.02101,"1044,55648071603 ",.01)
 ;;55648071603
 ;;9002226.02101,"1044,55648071603 ",.02)
 ;;55648071603
 ;;9002226.02101,"1044,55648071604 ",.01)
 ;;55648071604
 ;;9002226.02101,"1044,55648071604 ",.02)
 ;;55648071604
 ;;9002226.02101,"1044,55648071605 ",.01)
 ;;55648071605
 ;;9002226.02101,"1044,55648071605 ",.02)
 ;;55648071605
 ;;9002226.02101,"1044,55648071701 ",.01)
 ;;55648071701
 ;;9002226.02101,"1044,55648071701 ",.02)
 ;;55648071701
 ;;9002226.02101,"1044,55648071702 ",.01)
 ;;55648071702
 ;;9002226.02101,"1044,55648071702 ",.02)
 ;;55648071702
 ;;9002226.02101,"1044,55648071703 ",.01)
 ;;55648071703
 ;;9002226.02101,"1044,55648071703 ",.02)
 ;;55648071703
 ;;9002226.02101,"1044,55648071704 ",.01)
 ;;55648071704
 ;;9002226.02101,"1044,55648071704 ",.02)
 ;;55648071704
 ;;9002226.02101,"1044,55648071705 ",.01)
 ;;55648071705
 ;;9002226.02101,"1044,55648071705 ",.02)
 ;;55648071705
 ;;9002226.02101,"1044,55648071801 ",.01)
 ;;55648071801
 ;;9002226.02101,"1044,55648071801 ",.02)
 ;;55648071801
 ;;9002226.02101,"1044,55648071802 ",.01)
 ;;55648071802
 ;;9002226.02101,"1044,55648071802 ",.02)
 ;;55648071802
 ;;9002226.02101,"1044,55648071803 ",.01)
 ;;55648071803
 ;;9002226.02101,"1044,55648071803 ",.02)
 ;;55648071803
 ;;9002226.02101,"1044,55648071804 ",.01)
 ;;55648071804
 ;;9002226.02101,"1044,55648071804 ",.02)
 ;;55648071804
 ;;9002226.02101,"1044,55648071805 ",.01)
 ;;55648071805
 ;;9002226.02101,"1044,55648071805 ",.02)
 ;;55648071805
 ;;9002226.02101,"1044,55648075101 ",.01)
 ;;55648075101
 ;;9002226.02101,"1044,55648075101 ",.02)
 ;;55648075101
 ;;9002226.02101,"1044,55648075201 ",.01)
 ;;55648075201
 ;;9002226.02101,"1044,55648075201 ",.02)
 ;;55648075201
 ;;9002226.02101,"1044,55648075202 ",.01)
 ;;55648075202
 ;;9002226.02101,"1044,55648075202 ",.02)
 ;;55648075202
 ;;9002226.02101,"1044,55648075203 ",.01)
 ;;55648075203
 ;;9002226.02101,"1044,55648075203 ",.02)
 ;;55648075203
 ;;9002226.02101,"1044,55648075204 ",.01)
 ;;55648075204
 ;;9002226.02101,"1044,55648075204 ",.02)
 ;;55648075204
 ;;9002226.02101,"1044,55648075207 ",.01)
 ;;55648075207
 ;;9002226.02101,"1044,55648075207 ",.02)
 ;;55648075207
 ;;9002226.02101,"1044,55648075301 ",.01)
 ;;55648075301
 ;;9002226.02101,"1044,55648075301 ",.02)
 ;;55648075301
 ;;9002226.02101,"1044,55648075302 ",.01)
 ;;55648075302
 ;;9002226.02101,"1044,55648075302 ",.02)
 ;;55648075302
 ;;9002226.02101,"1044,55648075303 ",.01)
 ;;55648075303
 ;;9002226.02101,"1044,55648075303 ",.02)
 ;;55648075303
 ;;9002226.02101,"1044,55648075304 ",.01)
 ;;55648075304
 ;;9002226.02101,"1044,55648075304 ",.02)
 ;;55648075304
 ;;9002226.02101,"1044,55648075307 ",.01)
 ;;55648075307
 ;;9002226.02101,"1044,55648075307 ",.02)
 ;;55648075307
 ;;9002226.02101,"1044,55864070830 ",.01)
 ;;55864070830
 ;;9002226.02101,"1044,55864070830 ",.02)
 ;;55864070830
 ;;9002226.02101,"1044,55887007530 ",.01)
 ;;55887007530
 ;;9002226.02101,"1044,55887007530 ",.02)
 ;;55887007530
 ;;9002226.02101,"1044,55887012530 ",.01)
 ;;55887012530
 ;;9002226.02101,"1044,55887012530 ",.02)
 ;;55887012530
 ;;9002226.02101,"1044,55887012560 ",.01)
 ;;55887012560
 ;;9002226.02101,"1044,55887012560 ",.02)
 ;;55887012560
 ;;9002226.02101,"1044,55887016015 ",.01)
 ;;55887016015
 ;;9002226.02101,"1044,55887016015 ",.02)
 ;;55887016015
 ;;9002226.02101,"1044,55887016030 ",.01)
 ;;55887016030
 ;;9002226.02101,"1044,55887016030 ",.02)
 ;;55887016030
 ;;9002226.02101,"1044,55887016060 ",.01)
 ;;55887016060
 ;;9002226.02101,"1044,55887016060 ",.02)
 ;;55887016060
 ;;9002226.02101,"1044,55887016090 ",.01)
 ;;55887016090
 ;;9002226.02101,"1044,55887016090 ",.02)
 ;;55887016090
 ;;9002226.02101,"1044,55887016830 ",.01)
 ;;55887016830
 ;;9002226.02101,"1044,55887016830 ",.02)
 ;;55887016830
 ;;9002226.02101,"1044,55887016860 ",.01)
 ;;55887016860
 ;;9002226.02101,"1044,55887016860 ",.02)
 ;;55887016860
