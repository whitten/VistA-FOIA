BGP06P10 ;IHS/CMI/LAB-CREATED BY ^ATXSTX ON MAY 23, 2010;
 ;;10.0;IHS CLINICAL REPORTING;;JUN 18, 2010
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"700,66336-0628-12 ",.02)
 ;;66336-0628-12
 ;;9002226.02101,"700,66336-0628-15 ",.01)
 ;;66336-0628-15
 ;;9002226.02101,"700,66336-0628-15 ",.02)
 ;;66336-0628-15
 ;;9002226.02101,"700,66336-0628-16 ",.01)
 ;;66336-0628-16
 ;;9002226.02101,"700,66336-0628-16 ",.02)
 ;;66336-0628-16
 ;;9002226.02101,"700,66336-0628-20 ",.01)
 ;;66336-0628-20
 ;;9002226.02101,"700,66336-0628-20 ",.02)
 ;;66336-0628-20
 ;;9002226.02101,"700,66336-0628-30 ",.01)
 ;;66336-0628-30
 ;;9002226.02101,"700,66336-0628-30 ",.02)
 ;;66336-0628-30
 ;;9002226.02101,"700,66336-0628-60 ",.01)
 ;;66336-0628-60
 ;;9002226.02101,"700,66336-0628-60 ",.02)
 ;;66336-0628-60
 ;;9002226.02101,"700,66479-0510-10 ",.01)
 ;;66479-0510-10
 ;;9002226.02101,"700,66479-0510-10 ",.02)
 ;;66479-0510-10
 ;;9002226.02101,"700,66479-0512-10 ",.01)
 ;;66479-0512-10
 ;;9002226.02101,"700,66479-0512-10 ",.02)
 ;;66479-0512-10
 ;;9002226.02101,"700,66479-0513-10 ",.01)
 ;;66479-0513-10
 ;;9002226.02101,"700,66479-0513-10 ",.02)
 ;;66479-0513-10
 ;;9002226.02101,"700,66479-0514-10 ",.01)
 ;;66479-0514-10
 ;;9002226.02101,"700,66479-0514-10 ",.02)
 ;;66479-0514-10
 ;;9002226.02101,"700,66479-0515-10 ",.01)
 ;;66479-0515-10
 ;;9002226.02101,"700,66479-0515-10 ",.02)
 ;;66479-0515-10
 ;;9002226.02101,"700,66479-0515-50 ",.01)
 ;;66479-0515-50
 ;;9002226.02101,"700,66479-0515-50 ",.02)
 ;;66479-0515-50
 ;;9002226.02101,"700,66591-0612-41 ",.01)
 ;;66591-0612-41
 ;;9002226.02101,"700,66591-0612-41 ",.02)
 ;;66591-0612-41
 ;;9002226.02101,"700,66591-0622-41 ",.01)
 ;;66591-0622-41
 ;;9002226.02101,"700,66591-0622-41 ",.02)
 ;;66591-0622-41
 ;;9002226.02101,"700,66591-0631-41 ",.01)
 ;;66591-0631-41
 ;;9002226.02101,"700,66591-0631-41 ",.02)
 ;;66591-0631-41
 ;;9002226.02101,"700,66591-0631-51 ",.01)
 ;;66591-0631-51
 ;;9002226.02101,"700,66591-0631-51 ",.02)
 ;;66591-0631-51
 ;;9002226.02101,"700,66591-0641-41 ",.01)
 ;;66591-0641-41
 ;;9002226.02101,"700,66591-0641-41 ",.02)
 ;;66591-0641-41
 ;;9002226.02101,"700,66591-0641-51 ",.01)
 ;;66591-0641-51
 ;;9002226.02101,"700,66591-0641-51 ",.02)
 ;;66591-0641-51
 ;;9002226.02101,"700,66591-0651-41 ",.01)
 ;;66591-0651-41
 ;;9002226.02101,"700,66591-0651-41 ",.02)
 ;;66591-0651-41
 ;;9002226.02101,"700,66591-0691-41 ",.01)
 ;;66591-0691-41
 ;;9002226.02101,"700,66591-0691-41 ",.02)
 ;;66591-0691-41
 ;;9002226.02101,"700,66591-1622-41 ",.01)
 ;;66591-1622-41
 ;;9002226.02101,"700,66591-1622-41 ",.02)
 ;;66591-1622-41
 ;;9002226.02101,"700,68071-0760-30 ",.01)
 ;;68071-0760-30
 ;;9002226.02101,"700,68071-0760-30 ",.02)
 ;;68071-0760-30
 ;;9002226.02101,"700,68115-0305-00 ",.01)
 ;;68115-0305-00
 ;;9002226.02101,"700,68115-0305-00 ",.02)
 ;;68115-0305-00
 ;;9002226.02101,"700,68115-0305-20 ",.01)
 ;;68115-0305-20
 ;;9002226.02101,"700,68115-0305-20 ",.02)
 ;;68115-0305-20
 ;;9002226.02101,"700,68115-0305-25 ",.01)
 ;;68115-0305-25
 ;;9002226.02101,"700,68115-0305-25 ",.02)
 ;;68115-0305-25
 ;;9002226.02101,"700,68115-0305-30 ",.01)
 ;;68115-0305-30
 ;;9002226.02101,"700,68115-0305-30 ",.02)
 ;;68115-0305-30
 ;;9002226.02101,"700,68115-0305-40 ",.01)
 ;;68115-0305-40
 ;;9002226.02101,"700,68115-0305-40 ",.02)
 ;;68115-0305-40
 ;;9002226.02101,"700,68115-0305-60 ",.01)
 ;;68115-0305-60
 ;;9002226.02101,"700,68115-0305-60 ",.02)
 ;;68115-0305-60
 ;;9002226.02101,"700,68115-0305-90 ",.01)
 ;;68115-0305-90
 ;;9002226.02101,"700,68115-0305-90 ",.02)
 ;;68115-0305-90
 ;;9002226.02101,"700,68115-0305-99 ",.01)
 ;;68115-0305-99
 ;;9002226.02101,"700,68115-0305-99 ",.02)
 ;;68115-0305-99
 ;;9002226.02101,"700,68115-0306-12 ",.01)
 ;;68115-0306-12
 ;;9002226.02101,"700,68115-0306-12 ",.02)
 ;;68115-0306-12
 ;;9002226.02101,"700,68115-0306-30 ",.01)
 ;;68115-0306-30
 ;;9002226.02101,"700,68115-0306-30 ",.02)
 ;;68115-0306-30
 ;;9002226.02101,"700,68115-0306-60 ",.01)
 ;;68115-0306-60
 ;;9002226.02101,"700,68115-0306-60 ",.02)
 ;;68115-0306-60
 ;;9002226.02101,"700,68115-0462-30 ",.01)
 ;;68115-0462-30
 ;;9002226.02101,"700,68115-0462-30 ",.02)
 ;;68115-0462-30
 ;;9002226.02101,"700,68115-0462-60 ",.01)
 ;;68115-0462-60
 ;;9002226.02101,"700,68115-0462-60 ",.02)
 ;;68115-0462-60
 ;;9002226.02101,"700,68115-0605-00 ",.01)
 ;;68115-0605-00
 ;;9002226.02101,"700,68115-0605-00 ",.02)
 ;;68115-0605-00
 ;;9002226.02101,"700,68115-0743-00 ",.01)
 ;;68115-0743-00
 ;;9002226.02101,"700,68115-0743-00 ",.02)
 ;;68115-0743-00
 ;;9002226.02101,"700,68115-0815-00 ",.01)
 ;;68115-0815-00
 ;;9002226.02101,"700,68115-0815-00 ",.02)
 ;;68115-0815-00
 ;;9002226.02101,"700,68387-0100-01 ",.01)
 ;;68387-0100-01
 ;;9002226.02101,"700,68387-0100-01 ",.02)
 ;;68387-0100-01
 ;;9002226.02101,"700,68387-0100-12 ",.01)
 ;;68387-0100-12
 ;;9002226.02101,"700,68387-0100-12 ",.02)
 ;;68387-0100-12
 ;;9002226.02101,"700,68387-0100-15 ",.01)
 ;;68387-0100-15
 ;;9002226.02101,"700,68387-0100-15 ",.02)
 ;;68387-0100-15
 ;;9002226.02101,"700,68387-0100-30 ",.01)
 ;;68387-0100-30
 ;;9002226.02101,"700,68387-0100-30 ",.02)
 ;;68387-0100-30
 ;;9002226.02101,"700,68387-0100-40 ",.01)
 ;;68387-0100-40
 ;;9002226.02101,"700,68387-0100-40 ",.02)
 ;;68387-0100-40
 ;;9002226.02101,"700,68387-0100-50 ",.01)
 ;;68387-0100-50
 ;;9002226.02101,"700,68387-0100-50 ",.02)
 ;;68387-0100-50
 ;;9002226.02101,"700,68387-0100-60 ",.01)
 ;;68387-0100-60
 ;;9002226.02101,"700,68387-0100-60 ",.02)
 ;;68387-0100-60
 ;;9002226.02101,"700,68387-0100-90 ",.01)
 ;;68387-0100-90
 ;;9002226.02101,"700,68387-0100-90 ",.02)
 ;;68387-0100-90
 ;;9002226.02101,"700,68387-0531-12 ",.01)
 ;;68387-0531-12
 ;;9002226.02101,"700,68387-0531-12 ",.02)
 ;;68387-0531-12
 ;;9002226.02101,"700,68387-0531-60 ",.01)
 ;;68387-0531-60
 ;;9002226.02101,"700,68387-0531-60 ",.02)
 ;;68387-0531-60
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
