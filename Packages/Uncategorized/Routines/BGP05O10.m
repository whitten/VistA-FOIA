BGP05O10 ;IHS/CMI/LAB-CREATED BY ^ATXSTX ON MAY 23, 2010;
 ;;10.0;IHS CLINICAL REPORTING;;JUN 18, 2010
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;
 ;;2195
 ;;
 ;;2196
 ;;
 ;;2197
 ;;
 ;;2610
 ;;
 ;;2613
 ;;
 ;;2612
 ;;
 ;;2611
 ;;
 ;;2599
 ;;
 ;;2600
 ;;
 ;;2601
 ;;
 ;;2602
 ;;
 ;;2603
 ;;
 ;;2604
 ;;
 ;;2605
 ;;
 ;;2606
 ;;
 ;;2607
 ;;
 ;;2608
 ;;
 ;;2609
 ;;
 ;;2629
 ;;
 ;;2631
 ;;
 ;;2630
 ;;
 ;;2614
 ;;
 ;;2615
 ;;
 ;;2616
 ;;
 ;;2617
 ;;
 ;;2618
 ;;
 ;;2619
 ;;
 ;;2620
 ;;
 ;;2621
 ;;
 ;;2622
 ;;
 ;;2623
 ;;
 ;;2624
 ;;
 ;;2625
 ;;
 ;;2626
 ;;
 ;;2627
 ;;
 ;;2628
 ;;
 ;;110
 ;;
 ;;111
 ;;
 ;;97
 ;;
 ;;98
 ;;
 ;;99
 ;;
 ;;100
 ;;
 ;;101
 ;;
 ;;102
 ;;
 ;;103
 ;;
 ;;104
 ;;
 ;;105
 ;;
 ;;106
 ;;
 ;;107
 ;;
 ;;112
 ;;
 ;;113
 ;;
 ;;114
 ;;
 ;;108
 ;;
 ;;109
 ;;
 ;;3023
 ;;
 ;;3007
 ;;
 ;;3008
 ;;
 ;;3009
 ;;
 ;;3010
 ;;
 ;;3011
 ;;
 ;;3012
 ;;
 ;;3013
 ;;
 ;;3014
 ;;
 ;;3015
 ;;
 ;;3016
 ;;
 ;;3017
 ;;
 ;;3018
 ;;
 ;;3019
 ;;
 ;;3020
 ;;
 ;;3021
 ;;
 ;;3022
 ;;
 ;;1587
 ;;
 ;;1588
 ;;
 ;;1589
 ;;
 ;;974
 ;;
 ;;975
 ;;
 ;;964
 ;;
 ;;965
 ;;
 ;;966
 ;;
 ;;967
 ;;
 ;;968
 ;;
 ;;969
 ;;
 ;;970
 ;;
 ;;971
 ;;
 ;;972
 ;;
 ;;973
 ;;
 ;;3253
 ;;
 ;;3254
 ;;
 ;;3243
 ;;
 ;;3244
 ;;
 ;;3245
 ;;
 ;;3246
 ;;
 ;;3247
 ;;
 ;;3248
 ;;
 ;;3249
 ;;
 ;;3250
 ;;
 ;;3251
 ;;
 ;;3252
 ;;
 ;;1403
 ;;
 ;;1406
 ;;
 ;;1404
 ;;
 ;;1386
 ;;
 ;;1405
 ;;
 ;;1387
 ;;
 ;;1388
 ;;
 ;;1389
 ;;
 ;;1390
 ;;
 ;;1391
 ;;
 ;;1392
 ;;
 ;;1393
 ;;
 ;;1394
 ;;
 ;;1395
 ;;
 ;;1396
 ;;
 ;;1397
 ;;
 ;;1398
 ;;
 ;;1399
 ;;
 ;;1400
 ;;
 ;;1401
 ;;
 ;;1402
 ;;
 ;;681
 ;;
 ;;682
 ;;
 ;;674
 ;;
 ;;675
 ;;
 ;;676
 ;;
 ;;677
 ;;
 ;;678
 ;;
 ;;679
 ;;
 ;;680
 ;;
 ;;696
 ;;
 ;;683
 ;;
 ;;684
 ;;
 ;;685
 ;;
 ;;686
 ;;
 ;;687
 ;;
 ;;688
 ;;
 ;;689
 ;;
 ;;690
 ;;
 ;;691
 ;;
 ;;692
 ;;
 ;;693
 ;;
 ;;694
 ;;
 ;;695
 ;;
 ;;416
 ;;
 ;;417
 ;;
 ;;405
 ;;
 ;;406
 ;;
 ;;407
 ;;
 ;;408
 ;;
 ;;409
 ;;
 ;;410
 ;;
 ;;411
 ;;
 ;;412
 ;;
 ;;413
 ;;
 ;;414
 ;;
 ;;415
 ;;
 ;;428
 ;;
 ;;429
 ;;
 ;;418
 ;;
 ;;419
 ;;
 ;;420
 ;;
 ;;421
 ;;
 ;;422
 ;;
 ;;423
 ;;
 ;;424
 ;;
 ;;425
 ;;
 ;;426
 ;;
 ;;430
 ;;
 ;;427
 ;;
 ;;1547
 ;;
 ;;1544
 ;;
 ;;1545
 ;;
 ;;1546
 ;;
 ;;1548
 ;;
 ;;2382
 ;;
 ;;2383
 ;;
 ;;129
 ;;
 ;;134
 ;;
 ;;130
 ;;
 ;;116
 ;;
 ;;117
 ;;
 ;;118
 ;;
 ;;119
 ;;
 ;;120
 ;;
 ;;121
 ;;
 ;;122
 ;;
 ;;123
 ;;
 ;;124
 ;;
 ;;125
 ;;
 ;;126
 ;;
 ;;131
 ;;
 ;;132
 ;;
 ;;133
 ;;
 ;;127
 ;;
 ;;128
 ;;
 ;;962
 ;;
 ;;963
 ;;
 ;;958
 ;;
 ;;959
 ;;
 ;;960
 ;;
 ;;961
 ;;
 ;;855
 ;;
 ;;856
 ;;
 ;;848
 ;;
 ;;849
 ;;
 ;;850
 ;;
 ;;851
 ;;
 ;;852
 ;;
 ;;853
 ;;
 ;;854
 ;;
 ;;3490
 ;;
 ;;3478
 ;;
 ;;3479
 ;;
 ;;3480
 ;;
 ;;3481
 ;;
 ;;3482
 ;;
 ;;3483
 ;;
 ;;3484
 ;;
 ;;3485
 ;;
 ;;3486
 ;;
 ;;3487
 ;;
 ;;3488
 ;;
 ;;3489
 ;;
 ;;3477
 ;;
 ;;3466
 ;;
 ;;3467
 ;;
 ;;3468
 ;;
 ;;3469
 ;;
 ;;3470
 ;;
 ;;3471
 ;;
 ;;3472
 ;;
 ;;3473
 ;;
 ;;3474
 ;;
 ;;3475
 ;;
 ;;3476
 ;;
 ;;1136
 ;;
 ;;1126
 ;;
 ;;1127
 ;;
 ;;1128
 ;;
 ;;1129
 ;;
 ;;1130
 ;;
 ;;1131
 ;;
 ;;1132
 ;;
 ;;1133
 ;;
 ;;1134
 ;;
 ;;1135
 ;;
 ;;1239
 ;;
 ;;1240
 ;;
 ;;2918
 ;;
 ;;2919
 ;;
 ;;2920
 ;;
 ;;2921
 ;;
 ;;2922
 ;;
 ;;2923
 ;;
 ;;2924
 ;;
 ;;2925
 ;;
 ;;141
 ;;
 ;;135
 ;;
 ;;136
 ;;
 ;;137
 ;;
 ;;138
 ;;
 ;;139
 ;;
 ;;140
 ;;
 ;;3535
 ;;
 ;;3533
 ;;
 ;;3534
 ;;
 ;;3538
 ;;
 ;;3536
 ;;
 ;;3537
 ;;
 ;;14
 ;;
 ;;13
 ;;
 ;;177
 ;;
 ;;176
 ;;
 ;;179
 ;;
 ;;178
 ;;
 ;;2058
 ;;
 ;;2056
 ;;
 ;;2057
 ;;
 ;;2053
 ;;
 ;;2052
 ;;
 ;;2055
 ;;
 ;;2054
 ;;
 ;;2740
 ;;
 ;;2739
 ;;
 ;;1459
 ;;
 ;;1457
 ;;
 ;;1458
 ;;
 ;;586
 ;;
 ;;2452
 ;;
 ;;2454
 ;;
 ;;2455
 ;;
 ;;2456
 ;;
 ;;2458
 ;;
 ;;2460
 ;;
 ;;2459
 ;;
 ;;1653
 ;;
 ;;1654
 ;;
 ;;1655
 ;;
 ;;1656
 ;;
 ;;1657
 ;;
 ;;1658
 ;;
 ;;2453
 ;;
 ;;658
 ;;
 ;;1691
 ;;
 ;;1689
 ;;
 ;;1690
 ;;
 ;;3454
 ;;
 ;;3455
 ;;
 ;;303
 ;;
 ;;1226
 ;;
 ;;1225
 ;;
 ;;2021
 ;;
 ;;2022
 ;;
 ;;2023
 ;;
 ;;2024
 ;;
 ;;2025
 ;;
 ;;2026
 ;;
 ;;2027
 ;;
 ;;2028
 ;;
 ;;2029
 ;;
 ;;2030
 ;;
 ;;2031
 ;;
 ;;2032
 ;;
 ;;2034
 ;;
 ;;2035
 ;;
 ;;2036
 ;;
 ;;2037
 ;;
 ;;2038
 ;;
 ;;2039
 ;;
 ;;2040
 ;;
 ;;2041
 ;;
 ;;2033
 ;;
 ;;3539
 ;;
 ;;3540
 ;;
 ;;3541
 ;;
 ;;3542
 ;;
 ;;3543
 ;;
 ;;3544
 ;;
 ;;3545
 ;;
 ;;3546
 ;;
 ;;3328
 ;;
 ;;3329
 ;;
 ;;3330
 ;;
 ;;185
 ;;
 ;;186
 ;;
 ;;482
 ;;
 ;;483
 ;;
 ;;484
 ;;
 ;;485
 ;;
 ;;727
 ;;
 ;;728
 ;;
 ;;729
 ;;
 ;;731
 ;;
 ;;732
 ;;
 ;;733
 ;;
 ;;2397
 ;;
 ;;2398
 ;;
 ;;2744
 ;;
 ;;2745
 ;;
 ;;2746
 ;;
 ;;2747
 ;;
 ;;2748
 ;;
 ;;2749
 ;;
 ;;2750
 ;;
 ;;2751
 ;;
 ;;2752
 ;;
 ;;2753
 ;;
 ;;2754
 ;;
 ;;2755
 ;;
 ;;2756
 ;;
 ;;2757
 ;;
 ;;2758
 ;;
 ;;2759
 ;;
 ;;2760
 ;;
 ;;2761
 ;;
 ;;3082
 ;;
 ;;3083
 ;;
 ;;3084
 ;;
 ;;3085
 ;;
 ;;1463
 ;;
 ;;1464
 ;;
 ;;1465
 ;;
 ;;1466
 ;;
 ;;1467
 ;;
 ;;1468
 ;;
 ;;1249
 ;;
 ;;1250
 ;;
 ;;1251
 ;;
 ;;1252
 ;;
 ;;1006
 ;;
 ;;1007
 ;;
 ;;1008
 ;;
 ;;1009
 ;;
 ;;900
 ;;
 ;;901
 ;;
 ;;1005
 ;;
 ;;182
 ;;
 ;;183
 ;;
 ;;184
 ;;
 ;;3274
 ;;
 ;;3275
 ;;
 ;;3276
 ;;
 ;;3277
 ;;
 ;;869
 ;;
 ;;1165
 ;;
 ;;1166
 ;;
 ;;1167
 ;;
 ;;1607
 ;;
 ;;486
 ;;
 ;;1168
 ;;
 ;;1169
 ;;
 ;;730
 ;;
 ;;2291
 ;;
 ;;2292
 ;;
 ;;2293
 ;;
 ;;2294
 ;;
 ;;2295
 ;;
 ;;2296
 ;;
 ;;2297
 ;;
 ;;2298
 ;;
 ;;164
 ;;
 ;;165
 ;;
 ;;1153
 ;;
 ;;2278
 ;;
 ;;2279
 ;;
 ;;2280
 ;;
 ;;2281
 ;;
 ;;863
 ;;
 ;;864
 ;;
 ;;166
 ;;
 ;;167
 ;;
 ;;2282
 ;;
 ;;2283
 ;;
 ;;2284
 ;;
 ;;2285
 ;;
 ;;2286
 ;;
 ;;2287
 ;;
 ;;2288
 ;;
 ;;2289
 ;;
 ;;2290
 ;;
 ;;2392
 ;;
 ;;2393
 ;;
 ;;1424
 ;;
 ;;1425
 ;;
 ;;1426
 ;;
 ;;1427
 ;;
 ;;716
 ;;
 ;;982
 ;;
 ;;983
 ;;
 ;;3325
 ;;
 ;;3521
 ;;
 ;;3522
 ;;
 ;;3523
 ;;
 ;;453
 ;;
 ;;454
 ;;
 ;;2677
 ;;
 ;;2678
 ;;
 ;;2679
 ;;
 ;;2681
 ;;
 ;;2682
 ;;
 ;;2680
 ;;
 ;;2683
 ;;
 ;;2684
 ;;
 ;;2672
 ;;
 ;;2673
 ;;
 ;;2674
 ;;
 ;;2675
 ;;
 ;;2676
 ;;
 ;;3051
 ;;
 ;;3052
 ;;
 ;;3053
 ;;
 ;;3054
 ;;
 ;;3049
 ;;
 ;;3050
 ;;
 ;;450
 ;;
 ;;451
 ;;
 ;;452
 ;;
 ;;2541
 ;;
 ;;2542
 ;;
 ;;2545
 ;;
 ;;2546
 ;;
 ;;2547
 ;;
 ;;2549
 ;;
 ;;2551
 ;;
 ;;2552
 ;;
 ;;3513
 ;;
 ;;3514
 ;;
 ;;3515
 ;;
 ;;3520
 ;;
 ;;3516
 ;;
 ;;3517
 ;;
 ;;3518
 ;;
 ;;3519
 ;;
 ;;8
 ;;
 ;;9
 ;;
 ;;154
 ;;
 ;;155
 ;;
 ;;156
 ;;
 ;;157
 ;;
 ;;158
 ;;
 ;;159
 ;;
 ;;160
 ;;
 ;;161
 ;;
 ;;447
 ;;
 ;;448
 ;;
 ;;449
 ;;
 ;;1549
 ;;
 ;;2253
 ;;
 ;;2254
 ;;
 ;;2255
 ;;
 ;;2256
 ;;
 ;;2257
 ;;
 ;;2258
 ;;
 ;;2265
 ;;
 ;;2259
 ;;
 ;;2260
 ;;
 ;;2261
 ;;
 ;;2262
 ;;
 ;;2263
 ;;
 ;;2264
 ;;
 ;;2267
 ;;
 ;;2268
 ;;
 ;;2269
 ;;
 ;;2270
 ;;
 ;;2271
 ;;
 ;;2272
 ;;
 ;;2273
 ;;
 ;;2274
 ;;
 ;;2275
 ;;
 ;;2277
 ;;
 ;;710
 ;;
 ;;711
 ;;
 ;;712
 ;;
 ;;713
 ;;
 ;;714
 ;;
 ;;715
 ;;
 ;;2389
 ;;
 ;;2390
 ;;
 ;;2391
 ;;
 ;;861
 ;;
 ;;862
 ;;
 ;;1148
 ;;
 ;;1145
 ;;
 ;;1146
 ;;
 ;;1147
 ;;
 ;;1149
 ;;
 ;;1150
 ;;
 ;;1151
 ;;
 ;;1152
 ;;
 ;;2655
 ;;
 ;;2656
 ;;
 ;;2657
 ;;
 ;;2658
 ;;
 ;;2659
 ;;
 ;;2660
 ;;
 ;;2661
 ;;
 ;;2662
 ;;
 ;;2663
 ;;
 ;;2664
 ;;
 ;;2665
 ;;
 ;;2666
 ;;
 ;;2667
 ;;
 ;;2668
 ;;
 ;;2669
 ;;
 ;;2670
 ;;
 ;;2671
 ;;
 ;;3044
 ;;
 ;;3045
 ;;
 ;;3046
 ;;
 ;;3047
 ;;
 ;;3048
 ;;
 ;;3264
 ;;
 ;;3265
 ;;
 ;;1418
 ;;
 ;;1419
 ;;
 ;;1420
 ;;
 ;;1421
 ;;
 ;;1422
 ;;
 ;;1423
 ;;
 ;;980
 ;;
 ;;981
 ;;
 ;;3407
 ;;
 ;;1416
 ;;
 ;;1417
 ;;
 ;;3373
 ;;
 ;;3374
 ;;
 ;;3375
 ;;
 ;;1301
 ;;
 ;;1304
 ;;
 ;;1302
 ;;
 ;;163
 ;;
 ;;3376
 ;;
 ;;3408
 ;;
 ;;1303
 ;;
 ;;2276
 ;;
 ;;2266
 ;;
 ;;479
 ;;
 ;;480
 ;;
 ;;481
 ;;
 ;;3272
 ;;
 ;;3273
 ;;
 ;;2051
 ;;
 ;;2048
 ;;
 ;;2049
 ;;
 ;;2050
 ;;
 ;;180
 ;;
 ;;181
 ;;
 ;;1163
 ;;
 ;;1164
 ;;
 ;;15
 ;;
 ;;868
 ;;
 ;;1460
 ;;
 ;;1461
 ;;
 ;;1462
 ;;
 ;;2741
 ;;
 ;;2742
 ;;
 ;;2743
