BGP09W4 ;IHS/CMI/LAB-CREATED BY ^ATXSTX ON MAY 23, 2010;
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
 ;;1879
 ;;
 ;;1880
 ;;
 ;;1881
 ;;
 ;;1882
 ;;
 ;;1883
 ;;
 ;;1884
 ;;
 ;;1834
 ;;
 ;;1885
 ;;
 ;;1886
 ;;
 ;;1887
 ;;
 ;;1555
 ;;
 ;;1556
 ;;
 ;;1557
 ;;
 ;;1558
 ;;
 ;;1559
 ;;
 ;;839
 ;;
 ;;892
 ;;
 ;;893
 ;;
 ;;833
 ;;
 ;;834
 ;;
 ;;835
 ;;
 ;;836
 ;;
 ;;837
 ;;
 ;;838
 ;;
 ;;891
 ;;
 ;;895
 ;;
 ;;894
 ;;
 ;;2362
 ;;
 ;;2417
 ;;
 ;;2356
 ;;
 ;;2357
 ;;
 ;;2358
 ;;
 ;;2359
 ;;
 ;;2360
 ;;
 ;;2361
 ;;
 ;;2876
 ;;
 ;;2877
 ;;
 ;;2878
 ;;
 ;;2863
 ;;
 ;;2864
 ;;
 ;;2865
 ;;
 ;;2866
 ;;
 ;;2867
 ;;
 ;;2868
 ;;
 ;;2869
 ;;
 ;;2870
 ;;
 ;;2871
 ;;
 ;;2872
 ;;
 ;;2873
 ;;
 ;;2874
 ;;
 ;;2880
 ;;
 ;;2879
 ;;
 ;;2875
 ;;
 ;;2119
 ;;
 ;;2896
 ;;
 ;;2897
 ;;
 ;;2899
 ;;
 ;;2881
 ;;
 ;;2882
 ;;
 ;;2883
 ;;
 ;;2884
 ;;
 ;;2885
 ;;
 ;;2886
 ;;
 ;;2887
 ;;
 ;;2888
 ;;
 ;;2889
 ;;
 ;;2890
 ;;
 ;;2891
 ;;
 ;;2892
 ;;
 ;;2893
 ;;
 ;;2894
 ;;
 ;;2898
 ;;
 ;;2901
 ;;
 ;;2900
 ;;
 ;;2895
 ;;
 ;;1045
 ;;
 ;;1046
 ;;
 ;;1047
 ;;
 ;;1037
 ;;
 ;;1038
 ;;
 ;;1039
 ;;
 ;;1040
 ;;
 ;;1041
 ;;
 ;;1042
 ;;
 ;;1043
 ;;
 ;;1049
 ;;
 ;;1048
 ;;
 ;;1044
 ;;
 ;;2858
 ;;
 ;;2859
 ;;
 ;;2860
 ;;
 ;;2847
 ;;
 ;;2848
 ;;
 ;;2849
 ;;
 ;;2850
 ;;
 ;;2851
 ;;
 ;;2852
 ;;
 ;;2853
 ;;
 ;;2854
 ;;
 ;;2855
 ;;
 ;;2856
 ;;
 ;;2857
 ;;
 ;;2862
 ;;
 ;;2861
 ;;
 ;;3143
 ;;
 ;;3144
 ;;
 ;;3145
 ;;
 ;;3130
 ;;
 ;;3131
 ;;
 ;;3132
 ;;
 ;;3133
 ;;
 ;;3134
 ;;
 ;;3135
 ;;
 ;;3136
 ;;
 ;;3137
 ;;
 ;;3138
 ;;
 ;;3139
 ;;
 ;;3140
 ;;
 ;;3141
 ;;
 ;;3147
 ;;
 ;;3146
 ;;
 ;;3142
 ;;
 ;;1620
 ;;
 ;;1621
 ;;
 ;;1622
 ;;
 ;;1614
 ;;
 ;;1615
 ;;
 ;;1616
 ;;
 ;;1617
 ;;
 ;;1618
 ;;
 ;;1619
 ;;
 ;;1624
 ;;
 ;;1623
 ;;
 ;;2955
 ;;
 ;;2949
 ;;
 ;;2950
 ;;
 ;;2951
 ;;
 ;;2952
 ;;
 ;;2953
 ;;
 ;;2954
 ;;
 ;;527
 ;;
 ;;524
 ;;
 ;;525
 ;;
 ;;374
 ;;
 ;;375
 ;;
 ;;376
 ;;
 ;;377
 ;;
 ;;378
 ;;
 ;;526
 ;;
 ;;2431
 ;;
 ;;2418
 ;;
 ;;2419
 ;;
 ;;2420
 ;;
 ;;2421
 ;;
 ;;2422
 ;;
 ;;2423
 ;;
 ;;2424
 ;;
 ;;2425
 ;;
 ;;2426
 ;;
 ;;2427
 ;;
 ;;2428
 ;;
 ;;2429
 ;;
 ;;2430
 ;;
 ;;217
 ;;
 ;;218
 ;;
 ;;219
 ;;
 ;;68
 ;;
 ;;69
 ;;
 ;;70
 ;;
 ;;213
 ;;
 ;;71
 ;;
 ;;214
 ;;
 ;;221
 ;;
 ;;215
 ;;
 ;;220
 ;;
 ;;216
 ;;
 ;;223
 ;;
 ;;224
 ;;
 ;;225
 ;;
 ;;72
 ;;
 ;;73
 ;;
 ;;74
 ;;
 ;;75
 ;;
 ;;76
 ;;
 ;;77
 ;;
 ;;78
 ;;
 ;;79
 ;;
 ;;227
 ;;
 ;;222
 ;;
 ;;226
 ;;
 ;;80
 ;;
 ;;27
 ;;
 ;;22
 ;;
 ;;23
 ;;
 ;;24
 ;;
 ;;25
 ;;
 ;;26
 ;;
 ;;239
 ;;
 ;;240
 ;;
 ;;241
 ;;
 ;;229
 ;;
 ;;230
 ;;
 ;;231
 ;;
 ;;232
 ;;
 ;;233
 ;;
 ;;234
 ;;
 ;;235
 ;;
 ;;236
 ;;
 ;;237
 ;;
 ;;243
 ;;
 ;;242
 ;;
 ;;238
 ;;
 ;;2438
 ;;
 ;;2432
 ;;
 ;;2433
 ;;
 ;;2434
 ;;
 ;;2435
 ;;
 ;;2436
 ;;
 ;;2437
 ;;
 ;;333
 ;;
 ;;330
 ;;
 ;;331
 ;;
 ;;332
 ;;
 ;;3306
 ;;
 ;;3307
 ;;
 ;;3308
 ;;
 ;;3296
 ;;
 ;;3297
 ;;
 ;;3298
 ;;
 ;;3299
 ;;
 ;;3300
 ;;
 ;;3301
 ;;
 ;;3302
 ;;
 ;;3303
 ;;
 ;;3304
 ;;
 ;;3310
 ;;
 ;;3309
 ;;
 ;;3305
 ;;
 ;;325
 ;;
 ;;326
 ;;
 ;;327
 ;;
 ;;322
 ;;
 ;;323
 ;;
 ;;329
 ;;
 ;;328
 ;;
 ;;324
 ;;
 ;;1329
 ;;
 ;;1324
 ;;
 ;;1325
 ;;
 ;;1326
 ;;
 ;;1327
 ;;
 ;;1328
 ;;
 ;;1075
 ;;
 ;;1072
 ;;
 ;;1073
 ;;
 ;;1074
 ;;
 ;;1211
 ;;
 ;;1212
 ;;
 ;;1198
 ;;
 ;;1199
 ;;
 ;;1200
 ;;
 ;;1201
 ;;
 ;;1202
 ;;
 ;;1203
 ;;
 ;;1204
 ;;
 ;;1205
 ;;
 ;;1206
 ;;
 ;;1207
 ;;
 ;;1208
 ;;
 ;;1209
 ;;
 ;;1210
 ;;
 ;;1217
 ;;
 ;;1218
 ;;
 ;;1219
 ;;
 ;;1213
 ;;
 ;;1214
 ;;
 ;;1215
 ;;
 ;;1221
 ;;
 ;;1220
 ;;
 ;;1216
 ;;
 ;;1335
 ;;
 ;;1336
 ;;
 ;;1330
 ;;
 ;;1331
 ;;
 ;;1334
 ;;
 ;;531
 ;;
 ;;532
 ;;
 ;;528
 ;;
 ;;529
 ;;
 ;;530
 ;;
 ;;1516
 ;;
 ;;1517
 ;;
 ;;1519
 ;;
 ;;1503
 ;;
 ;;1504
 ;;
 ;;1505
 ;;
 ;;1506
 ;;
 ;;1507
 ;;
 ;;1508
 ;;
 ;;1509
 ;;
 ;;1510
 ;;
 ;;1511
 ;;
 ;;1512
 ;;
 ;;1513
 ;;
 ;;1514
 ;;
 ;;1521
 ;;
 ;;1520
 ;;
 ;;1515
 ;;
 ;;1518
 ;;
 ;;3125
 ;;
 ;;3126
 ;;
 ;;3127
 ;;
 ;;3119
 ;;
 ;;3120
 ;;
 ;;3121
 ;;
 ;;3122
 ;;
 ;;3123
 ;;
 ;;3129
 ;;
 ;;3128
 ;;
 ;;3124
 ;;
 ;;1501
 ;;
 ;;1489
 ;;
 ;;1490
 ;;
 ;;1491
 ;;
 ;;1492
 ;;
 ;;1493
 ;;
 ;;1494
 ;;
 ;;1495
 ;;
 ;;1496
 ;;
 ;;1497
 ;;
 ;;1498
 ;;
 ;;1499
 ;;
 ;;1500
 ;;
 ;;1502
 ;;
 ;;1032
 ;;
 ;;1033
 ;;
 ;;1034
 ;;
 ;;1026
 ;;
 ;;1027
 ;;
 ;;1028
 ;;
 ;;1029
 ;;
 ;;1030
 ;;
 ;;1036
 ;;
 ;;1035
 ;;
 ;;1031
 ;;
 ;;1633
 ;;
 ;;1625
 ;;
 ;;1626
 ;;
 ;;1627
 ;;
 ;;1629
 ;;
 ;;1628
 ;;
 ;;1630
 ;;
 ;;1631
 ;;
 ;;1632
 ;;
 ;;2439
 ;;
 ;;228
 ;;
 ;;1332
 ;;
 ;;1333
 ;;
 ;;295
 ;;
 ;;294
 ;;
 ;;890
 ;;
 ;;823
 ;;
 ;;2933
 ;;
 ;;2934
 ;;
 ;;2825
 ;;
 ;;264
 ;;
 ;;265
 ;;
 ;;2111
 ;;
 ;;2018
 ;;
 ;;2019
 ;;
 ;;737
 ;;
 ;;738
 ;;
 ;;739
 ;;
 ;;2772
 ;;
 ;;2770
 ;;
 ;;2771
 ;;
 ;;2782
 ;;
 ;;2783
 ;;
 ;;2788
 ;;
 ;;2791
 ;;
 ;;2795
 ;;
 ;;2798
 ;;
 ;;2763
 ;;
 ;;1015
 ;;
 ;;1477
 ;;
 ;;1478
 ;;
 ;;3549
 ;;
 ;;3554
 ;;
 ;;3090
 ;;
 ;;3098
 ;;
 ;;1474
 ;;
 ;;953
 ;;
 ;;952
 ;;
 ;;3403
 ;;
 ;;1099
 ;;
 ;;1569
 ;;
 ;;1570
 ;;
 ;;3227
 ;;
 ;;3228
 ;;
 ;;1713
 ;;
 ;;1714
 ;;
 ;;1715
 ;;
 ;;1716
 ;;
 ;;1717
 ;;
 ;;1712
 ;;
 ;;3236
 ;;
 ;;3237
 ;;
 ;;3362
 ;;
 ;;3445
 ;;
 ;;2844
 ;;
 ;;3189
 ;;
 ;;772
 ;;
 ;;773
 ;;
 ;;775
 ;;
 ;;780
 ;;
 ;;3355
 ;;
 ;;922
 ;;
 ;;1718
 ;;
 ;;1719
 ;;
 ;;1720
 ;;
 ;;1721
 ;;
 ;;1722
 ;;
 ;;1723
 ;;
 ;;1726
 ;;
 ;;1724
 ;;
 ;;1725
 ;;
 ;;605
 ;;
 ;;2472
 ;;
 ;;2473
 ;;
 ;;2469
 ;;
 ;;2470
 ;;
 ;;2471
 ;;
 ;;2474
 ;;
 ;;2476
 ;;
 ;;2475
 ;;
 ;;339
 ;;
 ;;340
 ;;
 ;;341
 ;;
 ;;342
 ;;
 ;;3219
 ;;
 ;;3220
 ;;
 ;;807
 ;;
 ;;808
 ;;
 ;;809
 ;;
 ;;1222
 ;;
 ;;1522
 ;;
 ;;1523
 ;;
 ;;1337
 ;;
 ;;1829
 ;;
 ;;1827
 ;;
 ;;1828
 ;;
 ;;1832
 ;;
 ;;1830
 ;;
 ;;1831
 ;;
 ;;2902
 ;;
 ;;2903
 ;;
 ;;896
 ;;
 ;;3311
 ;;
 ;;3312
 ;;
 ;;3313
 ;;
 ;;2904
 ;;
 ;;2905
 ;;
 ;;2906
 ;;
 ;;3596
 ;;
 ;;3597
 ;;
 ;;3117
 ;;
 ;;3118
 ;;
 ;;533
 ;;
 ;;534
 ;;
 ;;244
 ;;
 ;;245
 ;;
 ;;3409
 ;;
 ;;3412
 ;;
 ;;3411
 ;;
 ;;3410
 ;;
 ;;3413
 ;;
 ;;3416
 ;;
 ;;3417
 ;;
 ;;3418
 ;;
 ;;3414
 ;;
 ;;3415
 ;;
 ;;3419
 ;;
 ;;1597
 ;;
 ;;1599
 ;;
 ;;1600
 ;;
 ;;1598
 ;;
 ;;1601
 ;;
 ;;1604
 ;;
 ;;1605
 ;;
 ;;1602
 ;;
 ;;1603
 ;;
 ;;1606
 ;;
 ;;2703
 ;;
 ;;2704
 ;;
 ;;2706
 ;;
 ;;2707
 ;;
 ;;2705
 ;;
 ;;2708
 ;;
 ;;2711
 ;;
 ;;2712
 ;;
 ;;2713
 ;;
 ;;2709
 ;;
 ;;2710
 ;;
 ;;2714
 ;;
 ;;2715
 ;;
 ;;2716
 ;;
 ;;2718
 ;;
 ;;2719
 ;;
 ;;2717
 ;;
 ;;2720
 ;;
 ;;2723
 ;;
 ;;2724
 ;;
 ;;2725
 ;;
 ;;2721
 ;;
 ;;2722
 ;;
 ;;2726
 ;;
 ;;2727
 ;;
 ;;2728
 ;;
 ;;2730
 ;;
 ;;2731
 ;;
 ;;2729
 ;;
 ;;2732
 ;;
 ;;2735
 ;;
 ;;2736
 ;;
 ;;2737
 ;;
 ;;2733
 ;;
 ;;2734
 ;;
 ;;2738
 ;;
 ;;3060
 ;;
 ;;3062
 ;;
 ;;3063
 ;;
 ;;3061
 ;;
 ;;3064
 ;;
 ;;3067
 ;;
 ;;3068
 ;;
 ;;3069
 ;;
 ;;3065
 ;;
 ;;3066
 ;;
 ;;3070
 ;;
 ;;3071
 ;;
 ;;3073
 ;;
 ;;3074
 ;;
 ;;3072
 ;;
 ;;3075
 ;;
 ;;3078
 ;;
 ;;3079
 ;;
 ;;3080
 ;;
 ;;3076
 ;;
 ;;3077
 ;;
 ;;3081
 ;;
 ;;1435
 ;;
 ;;1437
 ;;
 ;;1438
 ;;
 ;;1436
 ;;
 ;;1439
 ;;
 ;;1442
 ;;
 ;;1443
 ;;
 ;;1444
 ;;
 ;;1440
 ;;
 ;;1441
 ;;
 ;;1445
 ;;
 ;;1446
 ;;
 ;;1448
 ;;
 ;;1449
 ;;
 ;;1447
 ;;
 ;;1450
 ;;
 ;;1454
 ;;
 ;;1453
 ;;
 ;;1455
 ;;
 ;;1451
 ;;
 ;;1452
 ;;
 ;;1456
 ;;
 ;;2059
 ;;
 ;;2061
 ;;
 ;;2062
 ;;
 ;;2060
 ;;
 ;;2063
 ;;
 ;;2066
 ;;
 ;;2067
 ;;
 ;;2064
 ;;
 ;;2065
 ;;
 ;;2068
 ;;
 ;;2069
 ;;
 ;;2071
 ;;
 ;;2072
 ;;
 ;;2070
 ;;
 ;;2073
 ;;
 ;;2076
 ;;
 ;;2077
 ;;
 ;;2074
 ;;
 ;;2075
 ;;
 ;;2078
 ;;
 ;;2079
 ;;
 ;;2081
 ;;
 ;;2082
 ;;
 ;;2080
 ;;
 ;;2083
 ;;
 ;;2086
 ;;
 ;;2087
 ;;
 ;;2084
 ;;
 ;;2085
 ;;
 ;;2088
 ;;
 ;;1056
 ;;
 ;;1058
 ;;
 ;;1059
 ;;
 ;;1057
 ;;
 ;;1060
 ;;
 ;;1063
 ;;
 ;;1064
 ;;
 ;;1065
 ;;
 ;;1061
 ;;
 ;;1062
 ;;
 ;;1066
 ;;
 ;;985
 ;;
 ;;987
 ;;
 ;;988
 ;;
 ;;986
 ;;
 ;;989
 ;;
 ;;992
 ;;
 ;;993
 ;;
 ;;990
 ;;
 ;;991
 ;;
 ;;994
 ;;
 ;;995
 ;;
 ;;997
 ;;
 ;;998
 ;;
 ;;996
 ;;
 ;;999
 ;;
 ;;1002
 ;;
 ;;1003
 ;;
 ;;1000
 ;;
 ;;1001
 ;;
 ;;1004
 ;;
 ;;2457
 ;;
 ;;2324
 ;;
 ;;2962
 ;;
 ;;2152
 ;;
 ;;2153
 ;;
 ;;2154
 ;;
 ;;2155
 ;;
 ;;2156
 ;;
 ;;2157
 ;;
 ;;21
 ;;
 ;;20
 ;;
 ;;1768
 ;;
 ;;1769
 ;;
 ;;1770
 ;;
 ;;1771
 ;;
 ;;2339
 ;;
 ;;2340
 ;;
 ;;2341
 ;;
 ;;2342
 ;;
 ;;2343
 ;;
 ;;2344
 ;;
 ;;1350
 ;;
 ;;1351
 ;;
 ;;1352
 ;;
 ;;623
 ;;
 ;;622
 ;;
 ;;2502
 ;;
 ;;38
 ;;
 ;;39
 ;;
 ;;2158
 ;;
 ;;2159
 ;;
 ;;2379
 ;;
 ;;2380
 ;;
 ;;2381
 ;;
 ;;1564
 ;;
 ;;1560
 ;;
 ;;1368
 ;;
 ;;1369
 ;;
 ;;1370
 ;;
 ;;352
 ;;
 ;;353
 ;;
 ;;810
 ;;
 ;;811
 ;;
 ;;812
 ;;
 ;;30
 ;;
 ;;31
 ;;
 ;;32
 ;;
 ;;1634
 ;;
 ;;1635
 ;;
 ;;1636
 ;;
 ;;1637
 ;;
 ;;1727
 ;;
 ;;1728
 ;;
 ;;1729
 ;;
 ;;606
 ;;
 ;;607
 ;;
 ;;608
 ;;
 ;;609
 ;;
 ;;610
 ;;
 ;;2330
 ;;
 ;;2331
 ;;
 ;;2332
 ;;
 ;;2333
 ;;
 ;;2334
 ;;
 ;;2329
 ;;
 ;;2477
 ;;
 ;;2478
 ;;
 ;;2479
 ;;
 ;;2480
 ;;
 ;;1340
 ;;
 ;;1341
 ;;
 ;;1342
 ;;
 ;;1343
 ;;
 ;;2042
 ;;
 ;;2043
 ;;
 ;;2044
 ;;
 ;;2045
 ;;
 ;;2046
 ;;
 ;;2047
 ;;
 ;;2597
 ;;
 ;;2595
 ;;
 ;;2596
 ;;
 ;;2598
 ;;
 ;;3441
 ;;
 ;;3438
 ;;
 ;;3439
 ;;
 ;;3440
 ;;
 ;;671
 ;;
 ;;672
 ;;
 ;;659
 ;;
 ;;673
 ;;
 ;;660
 ;;
 ;;661
 ;;
 ;;662
 ;;
 ;;663
 ;;
 ;;664
 ;;
 ;;665
 ;;
 ;;666
 ;;
 ;;667
 ;;
 ;;668
 ;;
 ;;669
 ;;
 ;;670
 ;;
 ;;2177
 ;;
 ;;2178
 ;;
 ;;2160
 ;;
 ;;2161
 ;;
 ;;2162
 ;;
 ;;2163
 ;;
 ;;2164
 ;;
 ;;2165
 ;;
 ;;2166
 ;;
 ;;2167
 ;;
 ;;2168
 ;;
 ;;2169
 ;;
 ;;2170
 ;;
 ;;2171
 ;;
 ;;2172
 ;;
 ;;2173
 ;;
 ;;2174
 ;;
 ;;2175
 ;;
 ;;2176
 ;;
 ;;2220
 ;;
 ;;2222
 ;;
 ;;2221
 ;;
 ;;2201
 ;;
 ;;2202
 ;;
 ;;2203
 ;;
 ;;2204
 ;;
 ;;2205
 ;;
 ;;2206
 ;;
 ;;2207
 ;;
 ;;2208
 ;;
 ;;2209
 ;;
 ;;2210
 ;;
 ;;2211
 ;;
 ;;2212
 ;;
 ;;2213
 ;;
 ;;2214
 ;;
 ;;2215
 ;;
 ;;2216
 ;;
 ;;2217
 ;;
 ;;2218
 ;;
 ;;2219
 ;;
 ;;2198
 ;;
 ;;2200
 ;;
 ;;2199
 ;;
 ;;2179
 ;;
 ;;2180
 ;;
 ;;2181
 ;;
 ;;2182
 ;;
 ;;2183
 ;;
 ;;2184
 ;;
 ;;2185
 ;;
 ;;2186
 ;;
 ;;2187
 ;;
 ;;2188
 ;;
 ;;2189
 ;;
 ;;2190
 ;;
 ;;2191
 ;;
 ;;2192
 ;;
 ;;2193
 ;;
 ;;2194
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
