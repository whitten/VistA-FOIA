BGP05K ;IHS/CMI/LAB-CREATED BY ^ATXSTX ON MAY 23, 2010;
 ;;10.0;IHS CLINICAL REPORTING;;JUN 18, 2010
 ;;BGP FRACTURE CPTS
 ;
 ; This routine loads Taxonomy BGP FRACTURE CPTS
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 D OTHER
 I $O(^TMP("ATX",$J,3.6,0)) D BULL^ATXSTX2
 I $O(^TMP("ATX",$J,9002226,0)) D TAX^ATXSTX2
 D KILL^ATXSTX2
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;21,"21800 ")
 ;;1
 ;;21,"22305 ")
 ;;2
 ;;21,"22318 ")
 ;;3
 ;;21,"22520 ")
 ;;4
 ;;21,"22521 ")
 ;;5
 ;;21,"22523 ")
 ;;6
 ;;21,"22524 ")
 ;;7
 ;;21,"23500 ")
 ;;8
 ;;21,"23570 ")
 ;;9
 ;;21,"23665 ")
 ;;10
 ;;21,"24500 ")
 ;;11
 ;;21,"24620 ")
 ;;12
 ;;21,"24635 ")
 ;;13
 ;;21,"24650 ")
 ;;14
 ;;21,"25500 ")
 ;;15
 ;;21,"25611 ")
 ;;16
 ;;21,"25620 ")
 ;;17
 ;;21,"25622 ")
 ;;18
 ;;21,"25680 ")
 ;;19
 ;;21,"25685 ")
 ;;20
 ;;21,"27193 ")
 ;;21
 ;;21,"27254 ")
 ;;22
 ;;21,"27500 ")
 ;;23
 ;;21,"27520 ")
 ;;24
 ;;21,"27750 ")
 ;;25
 ;;21,"S2360 ")
 ;;26
 ;;21,"S2362 ")
 ;;27
 ;;9002226,364,.01)
 ;;BGP FRACTURE CPTS
 ;;9002226,364,.02)
 ;;@
 ;;9002226,364,.04)
 ;;@
 ;;9002226,364,.06)
 ;;@
 ;;9002226,364,.08)
 ;;0
 ;;9002226,364,.09)
 ;;3100418
 ;;9002226,364,.11)
 ;;@
 ;;9002226,364,.12)
 ;;455
 ;;9002226,364,.13)
 ;;1
 ;;9002226,364,.14)
 ;;@
 ;;9002226,364,.15)
 ;;81
 ;;9002226,364,.16)
 ;;@
 ;;9002226,364,.17)
 ;;@
 ;;9002226,364,3101)
 ;;@
 ;;9002226.02101,"364,21800 ",.01)
 ;;21800 
 ;;9002226.02101,"364,21800 ",.02)
 ;;21825 
 ;;9002226.02101,"364,22305 ",.01)
 ;;22305 
 ;;9002226.02101,"364,22305 ",.02)
 ;;22310 
 ;;9002226.02101,"364,22318 ",.01)
 ;;22318 
 ;;9002226.02101,"364,22318 ",.02)
 ;;22320 
 ;;9002226.02101,"364,22520 ",.01)
 ;;22520 
 ;;9002226.02101,"364,22520 ",.02)
 ;;22520 
 ;;9002226.02101,"364,22521 ",.01)
 ;;22521 
 ;;9002226.02101,"364,22521 ",.02)
 ;;22521 
 ;;9002226.02101,"364,22523 ",.01)
 ;;22523 
 ;;9002226.02101,"364,22523 ",.02)
 ;;22523 
 ;;9002226.02101,"364,22524 ",.01)
 ;;22524 
 ;;9002226.02101,"364,22524 ",.02)
 ;;22524 
 ;;9002226.02101,"364,23500 ",.01)
 ;;23500 
 ;;9002226.02101,"364,23500 ",.02)
 ;;23515 
 ;;9002226.02101,"364,23570 ",.01)
 ;;23570 
 ;;9002226.02101,"364,23570 ",.02)
 ;;23630 
 ;;9002226.02101,"364,23665 ",.01)
 ;;23665 
 ;;9002226.02101,"364,23665 ",.02)
 ;;23680 
 ;;9002226.02101,"364,24500 ",.01)
 ;;24500 
 ;;9002226.02101,"364,24500 ",.02)
 ;;24585 
 ;;9002226.02101,"364,24620 ",.01)
 ;;24620 
 ;;9002226.02101,"364,24620 ",.02)
 ;;24620 
 ;;9002226.02101,"364,24635 ",.01)
 ;;24635 
 ;;9002226.02101,"364,24635 ",.02)
 ;;24635 
 ;;9002226.02101,"364,24650 ",.01)
 ;;24650 
 ;;9002226.02101,"364,24650 ",.02)
 ;;24685 
 ;;9002226.02101,"364,25500 ",.01)
 ;;25500 
 ;;9002226.02101,"364,25500 ",.02)
 ;;25609 
 ;;9002226.02101,"364,25611 ",.01)
 ;;25611 
 ;;9002226.02101,"364,25611 ",.02)
 ;;25611 
 ;;9002226.02101,"364,25620 ",.01)
 ;;25620 
 ;;9002226.02101,"364,25620 ",.02)
 ;;25620 
 ;;9002226.02101,"364,25622 ",.01)
 ;;25622 
 ;;9002226.02101,"364,25622 ",.02)
 ;;25652 
 ;;9002226.02101,"364,25680 ",.01)
 ;;25680 
 ;;9002226.02101,"364,25680 ",.02)
 ;;25680 
 ;;9002226.02101,"364,25685 ",.01)
 ;;25685 
 ;;9002226.02101,"364,25685 ",.02)
 ;;25685 
 ;;9002226.02101,"364,27193 ",.01)
 ;;27193 
 ;;9002226.02101,"364,27193 ",.02)
 ;;27248 
 ;;9002226.02101,"364,27254 ",.01)
 ;;27254 
 ;;9002226.02101,"364,27254 ",.02)
 ;;27254 
 ;;9002226.02101,"364,27500 ",.01)
 ;;27500 
 ;;9002226.02101,"364,27500 ",.02)
 ;;27514 
 ;;9002226.02101,"364,27520 ",.01)
 ;;27520 
 ;;9002226.02101,"364,27520 ",.02)
 ;;27540 
 ;;9002226.02101,"364,27750 ",.01)
 ;;27750 
 ;;9002226.02101,"364,27750 ",.02)
 ;;27828 
 ;;9002226.02101,"364,S2360 ",.01)
 ;;S2360 
 ;;9002226.02101,"364,S2360 ",.02)
 ;;S2360 
 ;;9002226.02101,"364,S2362 ",.01)
 ;;S2362 
 ;;9002226.02101,"364,S2362 ",.02)
 ;;S2362 
 ;;
 ;;3148
 ;;
 ;;897
 ;;
 ;;3191
 ;;
 ;;3196
 ;;
 ;;3197
 ;;
 ;;3149
 ;;
 ;;3150
 ;;
 ;;3151
 ;;
 ;;3208
 ;;
 ;;3209
 ;;
 ;;3435
 ;;
 ;;3436
 ;;
 ;;584
 ;;
 ;;583
 ;;
 ;;2440
 ;;
 ;;2091
 ;;
 ;;2092
 ;;
 ;;2093
 ;;
 ;;2094
 ;;
 ;;2096
 ;;
 ;;2097
 ;;
 ;;2098
 ;;
 ;;2095
 ;;
 ;;3314
 ;;
 ;;3315
 ;;
 ;;3316
 ;;
 ;;300
 ;;
 ;;298
 ;;
 ;;299
 ;;
 ;;301
 ;;
 ;;3446
 ;;
 ;;3447
 ;;
 ;;3448
 ;;
 ;;3449
 ;;
 ;;3450
 ;;
 ;;3451
 ;;
 ;;3453
 ;;
 ;;3452
 ;;
 ;;296
 ;;
 ;;261
 ;;
 ;;262
 ;;
 ;;263
 ;;
 ;;1223
 ;;
 ;;1224
 ;;
 ;;302
 ;;
 ;;1084
 ;;
 ;;2089
 ;;
 ;;2090
 ;;
 ;;535
 ;;
 ;;536
 ;;
 ;;571
 ;;
 ;;572
 ;;
 ;;2527
 ;;
 ;;58
 ;;
 ;;60
 ;;
 ;;61
 ;;
 ;;63
 ;;
 ;;59
 ;;
 ;;62
 ;;
 ;;1524
 ;;
 ;;1525
 ;;
 ;;826
 ;;
 ;;828
 ;;
 ;;824
 ;;
 ;;825
 ;;
 ;;827
 ;;
 ;;829
 ;;
 ;;2553
 ;;
 ;;2554
 ;;
 ;;2555
 ;;
 ;;2556
 ;;
 ;;2557
 ;;
 ;;2558
 ;;
 ;;2559
 ;;
 ;;2560
 ;;
 ;;844
 ;;
 ;;2985
 ;;
 ;;2986
 ;;
 ;;2987
 ;;
 ;;2988
 ;;
 ;;2989
 ;;
 ;;2990
 ;;
 ;;1581
 ;;
 ;;1582
 ;;
 ;;3401
 ;;
 ;;3402
 ;;
 ;;3400
 ;;
 ;;1374
 ;;
 ;;1375
 ;;
 ;;1376
 ;;
 ;;392
 ;;
 ;;3234
 ;;
 ;;3235
 ;;
 ;;3
 ;;
 ;;4
 ;;
 ;;2911
 ;;
 ;;2912
 ;;
 ;;2913
 ;;
 ;;2914
 ;;
 ;;1115
 ;;
 ;;1116
 ;;
 ;;83
 ;;
 ;;395
 ;;
 ;;393
 ;;
 ;;2371
 ;;
 ;;2372
 ;;
 ;;2373
 ;;
 ;;394
 ;;
 ;;391
 ;;
 ;;1053
 ;;
 ;;2996
 ;;
 ;;2997
 ;;
 ;;2995
 ;;
 ;;1754
 ;;
 ;;611
 ;;
 ;;613
 ;;
 ;;614
 ;;
 ;;616
 ;;
 ;;1109
 ;;
 ;;1110
 ;;
 ;;1111
 ;;
 ;;1112
 ;;
 ;;1113
 ;;
 ;;1114
 ;;
 ;;1538
 ;;
 ;;3229
 ;;
 ;;3230
 ;;
 ;;3231
 ;;
 ;;612
 ;;
 ;;615
 ;;
 ;;1752
 ;;
 ;;1753
 ;;
 ;;3364
 ;;
 ;;3363
 ;;
 ;;2490
 ;;
 ;;2491
 ;;
 ;;345
 ;;
 ;;346
 ;;
 ;;347
 ;;
 ;;343
 ;;
 ;;344
 ;;
 ;;3225
 ;;
 ;;3226
 ;;
 ;;1105
 ;;
 ;;1106
 ;;
 ;;1107
 ;;
 ;;1108
 ;;
 ;;600
 ;;
 ;;601
 ;;
 ;;599
 ;;
 ;;1698
 ;;
 ;;1699
 ;;
 ;;1700
 ;;
 ;;1701
 ;;
 ;;1702
 ;;
 ;;595
 ;;
 ;;596
 ;;
 ;;597
 ;;
 ;;598
 ;;
 ;;246
 ;;
 ;;247
 ;;
 ;;248
 ;;
 ;;249
 ;;
 ;;250
 ;;
 ;;251
 ;;
 ;;334
 ;;
 ;;335
 ;;
 ;;336
 ;;
 ;;337
 ;;
 ;;338
 ;;
 ;;28
 ;;
 ;;1574
 ;;
 ;;1575
 ;;
 ;;1576
 ;;
 ;;624
 ;;
 ;;625
 ;;
 ;;626
 ;;
 ;;627
 ;;
 ;;348
 ;;
 ;;1052
 ;;
 ;;40
 ;;
 ;;2503
 ;;
 ;;2504
 ;;
 ;;928
 ;;
 ;;2507
 ;;
 ;;2508
 ;;
 ;;1539
 ;;
 ;;929
 ;;
 ;;2505
 ;;
 ;;2506
 ;;
 ;;1353
 ;;
 ;;830
 ;;
 ;;349
 ;;
 ;;1354
 ;;
 ;;1355
 ;;
 ;;1090
 ;;
 ;;1
 ;;
 ;;1091
 ;;
 ;;1092
 ;;
 ;;2345
 ;;
 ;;1051
 ;;
 ;;2346
 ;;
 ;;2347
 ;;
 ;;1639
 ;;
 ;;1640
 ;;
 ;;1638
 ;;
 ;;1688
 ;;
 ;;1687
 ;;
 ;;85
 ;;
 ;;86
 ;;
 ;;84
 ;;
 ;;88
 ;;
 ;;89
 ;;
 ;;90
 ;;
 ;;87
 ;;
 ;;91
 ;;
 ;;2568
 ;;
 ;;2569
 ;;
 ;;2991
 ;;
 ;;2992
 ;;
 ;;2993
 ;;
 ;;2994
 ;;
 ;;2565
 ;;
 ;;2566
 ;;
 ;;2567
 ;;
 ;;2131
 ;;
 ;;2132
 ;;
 ;;2127
 ;;
 ;;2128
 ;;
 ;;2129
 ;;
 ;;2130
 ;;
 ;;950
 ;;
 ;;951
 ;;
 ;;948
 ;;
 ;;949
 ;;
 ;;1283
 ;;
 ;;1284
 ;;
 ;;1809
 ;;
 ;;1810
 ;;
 ;;1811
 ;;
 ;;1812
 ;;
 ;;1813
 ;;
 ;;1814
 ;;
 ;;3379
 ;;
 ;;3380
 ;;
 ;;3381
 ;;
 ;;2121
 ;;
 ;;2122
 ;;
 ;;2123
 ;;
 ;;2124
 ;;
 ;;2125
 ;;
 ;;2126
 ;;
 ;;943
 ;;
 ;;944
 ;;
 ;;942
 ;;
 ;;946
 ;;
 ;;947
 ;;
 ;;945
 ;;
 ;;1054
 ;;
 ;;2528
 ;;
 ;;2529
 ;;
 ;;2530
 ;;
 ;;2531
 ;;
 ;;2532
 ;;
 ;;2533
 ;;
 ;;2976
 ;;
 ;;2977
 ;;
 ;;2978
 ;;
 ;;2979
 ;;
 ;;67
 ;;
 ;;2909
 ;;
 ;;2910
 ;;
 ;;64
 ;;
 ;;65
 ;;
 ;;66
 ;;
 ;;2
 ;;
 ;;1757
 ;;
 ;;1755
 ;;
 ;;1756
 ;;
 ;;923
 ;;
 ;;1760
 ;;
 ;;1758
 ;;
 ;;1759
 ;;
 ;;1763
 ;;
 ;;1762
 ;;
 ;;1761
 ;;
 ;;2492
 ;;
 ;;2494
 ;;
 ;;2496
 ;;
 ;;2495
 ;;
 ;;2493
 ;;
 ;;2497
 ;;
 ;;813
 ;;
 ;;815
 ;;
 ;;818
 ;;
 ;;814
 ;;
 ;;816
 ;;
 ;;817
 ;;
 ;;1779
 ;;
 ;;1780
 ;;
 ;;3190
 ;;
 ;;1078
 ;;
 ;;1079
 ;;
 ;;617
 ;;
 ;;618
 ;;
 ;;619
 ;;
 ;;620
 ;;
 ;;2335
 ;;
 ;;2336
 ;;
 ;;2337
 ;;
 ;;2338
 ;;
 ;;621
 ;;
 ;;1087
 ;;
 ;;1088
 ;;
 ;;1089
 ;;
 ;;1764
 ;;
 ;;1765
 ;;
 ;;1766
 ;;
 ;;1767
 ;;
 ;;1347
 ;;
 ;;1348
 ;;
 ;;1349
 ;;
 ;;1571
 ;;
 ;;1572
 ;;
 ;;1573
 ;;
 ;;2964
 ;;
 ;;2965
 ;;
 ;;2966
 ;;
 ;;2499
 ;;
 ;;2500
 ;;
 ;;2501
 ;;
 ;;2498
 ;;
 ;;35
 ;;
 ;;36
 ;;
 ;;37
 ;;
 ;;81
 ;;
 ;;82
 ;;
 ;;3397
 ;;
 ;;3398
 ;;
 ;;3399
 ;;
 ;;1578
 ;;
 ;;1579
 ;;
 ;;1580
 ;;
 ;;1815
 ;;
 ;;1816
 ;;
 ;;1817
 ;;
 ;;1818
 ;;
 ;;1819
 ;;
 ;;641
 ;;
 ;;642
 ;;
 ;;643
 ;;
 ;;2363
 ;;
 ;;2365
 ;;
 ;;2367
 ;;
 ;;2369
 ;;
 ;;2364
 ;;
 ;;2366
 ;;
 ;;2368
 ;;
 ;;2370
 ;;
 ;;841
 ;;
 ;;842
 ;;
 ;;843
 ;;
 ;;840
 ;;
 ;;2538
 ;;
 ;;2534
 ;;
 ;;2535
 ;;
 ;;2536
 ;;
 ;;2537
 ;;
 ;;2984
 ;;
 ;;2981
 ;;
 ;;2982
 ;;
 ;;2980
 ;;
 ;;2983
 ;;
 ;;1371
 ;;
 ;;1372
 ;;
 ;;1373
 ;;
 ;;937
 ;;
 ;;938
 ;;
 ;;939
 ;;
 ;;1083
 ;;
 ;;1081
 ;;
 ;;1082
 ;;
 ;;350
 ;;
 ;;3232
 ;;
 ;;3233
 ;;
 ;;1772
 ;;
 ;;1773
 ;;
 ;;1774
 ;;
 ;;1775
 ;;
 ;;1776
 ;;
 ;;1777
 ;;
 ;;3593
 ;;
 ;;3594
 ;;
 ;;3595
 ;;
 ;;1894
 ;;
 ;;1895
 ;;
 ;;1896
 ;;
 ;;1897
 ;;
 ;;1898
 ;;
 ;;1899
 ;;
 ;;1900
 ;;
 ;;1901
 ;;
 ;;2840
 ;;
 ;;2841
 ;;
 ;;2842
 ;;
 ;;2843
 ;;
 ;;2845
 ;;
 ;;2846
 ;;
 ;;570
 ;;
 ;;515
 ;;
 ;;516
 ;;
 ;;517
 ;;
 ;;518
 ;;
 ;;1912
 ;;
 ;;1902
 ;;
 ;;1903
 ;;
 ;;1904
 ;;
 ;;1905
 ;;
 ;;1906
 ;;
 ;;1907
 ;;
 ;;1908
 ;;
 ;;1909
 ;;
 ;;1910
 ;;
 ;;1911
 ;;
 ;;2839
 ;;
 ;;2834
 ;;
 ;;2835
 ;;
 ;;2836
 ;;
 ;;2837
 ;;
 ;;2838
 ;;
 ;;3110
 ;;
 ;;3111
 ;;
 ;;3112
 ;;
 ;;3113
 ;;
 ;;3114
 ;;
 ;;3115
 ;;
 ;;3354
 ;;
 ;;1487
 ;;
 ;;1488
 ;;
 ;;209
 ;;
 ;;210
 ;;
 ;;211
 ;;
 ;;212
 ;;
 ;;1888
 ;;
 ;;1889
 ;;
 ;;1890
 ;;
 ;;1891
 ;;
 ;;1892
 ;;
 ;;1893
 ;;
 ;;776
 ;;
 ;;777
 ;;
 ;;778
 ;;
 ;;779
 ;;
 ;;1189
 ;;
 ;;1190
 ;;
 ;;1191
 ;;
 ;;1192
 ;;
 ;;1193
 ;;
 ;;1194
 ;;
 ;;1195
 ;;
 ;;1196
 ;;
 ;;1197
 ;;
 ;;3378
 ;;
 ;;1281
 ;;
 ;;1279
 ;;
 ;;1282
 ;;
 ;;1280
 ;;
 ;;885
 ;;
 ;;886
 ;;
 ;;887
 ;;
 ;;888
 ;;
 ;;889
 ;;
 ;;2415
 ;;
 ;;2416
 ;;
 ;;3116
 ;;
 ;;3109
 ;;
 ;;207
 ;;
 ;;208
 ;;
 ;;770
 ;;
 ;;771
 ;;
 ;;774
 ;;
 ;;1613
 ;;
 ;;293
 ;;
 ;;781
 ;;
 ;;3295
 ;;
 ;;3588
 ;;
 ;;3589
 ;;
 ;;3590
 ;;
 ;;3591
 ;;
 ;;3592
 ;;
 ;;884
 ;;
 ;;1323
 ;;
 ;;920
 ;;
 ;;206
 ;;
 ;;3366
 ;;
 ;;3367
 ;;
 ;;3368
 ;;
 ;;3369
 ;;
 ;;51
 ;;
 ;;52
 ;;
 ;;53
 ;;
 ;;50
 ;;
 ;;55
 ;;
 ;;56
 ;;
 ;;57
 ;;
 ;;54
 ;;
 ;;832
 ;;
 ;;2907
 ;;
 ;;2908
 ;;
 ;;2151
 ;;
 ;;644
 ;;
 ;;645
 ;;
 ;;646
 ;;
 ;;1820
 ;;
 ;;1821
 ;;
 ;;1822
 ;;
 ;;1824
 ;;
 ;;1823
 ;;
 ;;1825
 ;;
 ;;1826
 ;;
 ;;940
 ;;
 ;;941
 ;;
 ;;1377
 ;;
 ;;1378
 ;;
 ;;2561
 ;;
 ;;2562
 ;;
 ;;2563
 ;;
 ;;2564
 ;;
 ;;1583
 ;;
 ;;1117
 ;;
 ;;1076
 ;;
 ;;1077
 ;;
 ;;602
 ;;
 ;;603
 ;;
 ;;604
 ;;
 ;;2325
 ;;
 ;;2326
 ;;
 ;;2327
 ;;
 ;;2328
 ;;
 ;;1709
 ;;
 ;;1710
 ;;
 ;;1711
 ;;
 ;;2467
 ;;
 ;;2468
 ;;
 ;;2963
 ;;
 ;;1338
 ;;
 ;;1339
 ;;
 ;;29
 ;;
 ;;1568
 ;;
 ;;1649
 ;;
 ;;1651
 ;;
 ;;1652
 ;;
 ;;1650
 ;;
 ;;1645
 ;;
 ;;1647
 ;;
 ;;1648
 ;;
 ;;1646
 ;;
 ;;1641
 ;;
 ;;1643
 ;;
 ;;1644
 ;;
 ;;1642
 ;;
 ;;1957
 ;;
 ;;1959
 ;;
 ;;1960
 ;;
 ;;1958
 ;;
 ;;1956
 ;;
 ;;1961
 ;;
 ;;1962
 ;;
 ;;1963
 ;;
 ;;1964
 ;;
 ;;1965
 ;;
 ;;1966
 ;;
 ;;1967
 ;;
 ;;1969
 ;;
 ;;1970
 ;;
 ;;1971
 ;;
 ;;1968
 ;;
 ;;1972
 ;;
 ;;1973
 ;;
 ;;749
 ;;
 ;;748
 ;;
 ;;1067
 ;;
 ;;1479
 ;;
 ;;2801
 ;;
 ;;2800
 ;;
 ;;2804
 ;;
 ;;2803
 ;;
 ;;2802
 ;;
 ;;3101
 ;;
 ;;3100
 ;;
 ;;2799
 ;;
 ;;381
 ;;
 ;;382
 ;;
 ;;383
 ;;
 ;;384
 ;
OTHER ; OTHER ROUTINES
 D ^BGP05K2
 D ^BGP05K3
 D ^BGP05K4
 D ^BGP05K5
 Q
