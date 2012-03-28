BGP0ZD6 ;IHS/CMI/LAB-CREATED BY ^ATXSTX ON MAY 23, 2010;
 ;;10.0;IHS CLINICAL REPORTING;;JUN 18, 2010
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;21,"55887-0862-30 ")
 ;;705
 ;;21,"55887-0880-10 ")
 ;;857
 ;;21,"55887-0880-20 ")
 ;;858
 ;;21,"55887-0894-10 ")
 ;;3320
 ;;21,"55887-0894-14 ")
 ;;3321
 ;;21,"55887-0894-28 ")
 ;;3322
 ;;21,"55887-0894-30 ")
 ;;3323
 ;;21,"55887-0896-30 ")
 ;;2223
 ;;21,"55887-0912-12 ")
 ;;2384
 ;;21,"55887-0912-21 ")
 ;;2385
 ;;21,"55887-0912-30 ")
 ;;2386
 ;;21,"55887-0915-20 ")
 ;;899
 ;;21,"55887-0917-20 ")
 ;;697
 ;;21,"55887-0917-21 ")
 ;;698
 ;;21,"55887-0917-30 ")
 ;;699
 ;;21,"55887-0923-10 ")
 ;;3024
 ;;21,"55887-0923-14 ")
 ;;3025
 ;;21,"55887-0923-20 ")
 ;;3026
 ;;21,"55887-0923-21 ")
 ;;3027
 ;;21,"55887-0923-30 ")
 ;;3028
 ;;21,"55887-0928-20 ")
 ;;700
 ;;21,"55887-0928-30 ")
 ;;701
 ;;21,"55887-0959-12 ")
 ;;2636
 ;;21,"55887-0959-14 ")
 ;;2637
 ;;21,"55887-0959-15 ")
 ;;2638
 ;;21,"55887-0959-20 ")
 ;;2639
 ;;21,"55887-0959-28 ")
 ;;2640
 ;;21,"55887-0959-30 ")
 ;;2641
 ;;21,"55887-0959-40 ")
 ;;2642
 ;;21,"55887-0959-60 ")
 ;;2643
 ;;21,"55887-0959-82 ")
 ;;2645
 ;;21,"55887-0959-90 ")
 ;;2644
 ;;21,"55887-0976-01 ")
 ;;2231
 ;;21,"55887-0976-15 ")
 ;;2224
 ;;21,"55887-0976-21 ")
 ;;2225
 ;;21,"55887-0976-30 ")
 ;;2226
 ;;21,"55887-0976-42 ")
 ;;2227
 ;;21,"55887-0976-45 ")
 ;;2228
 ;;21,"55887-0976-60 ")
 ;;2229
 ;;21,"55887-0976-82 ")
 ;;2232
 ;;21,"55887-0976-90 ")
 ;;2230
 ;;21,"57664-0164-08 ")
 ;;1565
 ;;21,"57664-0165-08 ")
 ;;1566
 ;;21,"57664-0165-13 ")
 ;;1567
 ;;21,"57664-0219-08 ")
 ;;3359
 ;;21,"57664-0220-08 ")
 ;;3360
 ;;21,"57664-0221-08 ")
 ;;3361
 ;;21,"57664-0391-08 ")
 ;;3221
 ;;21,"57664-0391-13 ")
 ;;3222
 ;;21,"57866-0262-01 ")
 ;;444
 ;;21,"57866-0262-03 ")
 ;;445
 ;;21,"57866-0264-01 ")
 ;;443
 ;;21,"57866-0264-02 ")
 ;;442
 ;;21,"57866-1259-01 ")
 ;;3372
 ;;21,"57866-2899-01 ")
 ;;3260
 ;;21,"57866-2899-02 ")
 ;;3261
 ;;21,"57866-2899-03 ")
 ;;3262
 ;;21,"57866-2899-04 ")
 ;;3263
 ;;21,"57866-2999-03 ")
 ;;3324
 ;;21,"57866-3067-02 ")
 ;;151
 ;;21,"57866-3612-02 ")
 ;;3034
 ;;21,"57866-3612-03 ")
 ;;3035
 ;;21,"57866-3981-01 ")
 ;;706
 ;;21,"57866-3981-02 ")
 ;;707
 ;;21,"57866-3982-01 ")
 ;;708
 ;;21,"57866-4604-01 ")
 ;;2234
 ;;21,"57866-4604-02 ")
 ;;2235
 ;;21,"57866-4604-05 ")
 ;;2236
 ;;21,"57866-4604-08 ")
 ;;2233
 ;;21,"57866-4604-09 ")
 ;;2237
 ;;21,"57866-4605-00 ")
 ;;2238
 ;;21,"57866-4605-01 ")
 ;;2241
 ;;21,"57866-4605-02 ")
 ;;2242
 ;;21,"57866-4605-05 ")
 ;;2239
 ;;21,"57866-4605-09 ")
 ;;2240
 ;;21,"57866-4606-01 ")
 ;;2248
 ;;21,"57866-4606-02 ")
 ;;2250
 ;;21,"57866-4606-03 ")
 ;;2247
 ;;21,"57866-4606-05 ")
 ;;2246
 ;;21,"57866-4606-06 ")
 ;;2249
 ;;21,"57866-4606-07 ")
 ;;2245
 ;;21,"57866-4606-08 ")
 ;;2252
 ;;21,"57866-4606-09 ")
 ;;2251
 ;;21,"57866-4607-01 ")
 ;;2244
 ;;21,"57866-4607-02 ")
 ;;2243
 ;;21,"57866-4621-03 ")
 ;;976
 ;;21,"57866-4622-02 ")
 ;;978
 ;;21,"57866-4622-03 ")
 ;;977
 ;;21,"57866-4622-05 ")
 ;;979
 ;;21,"57866-4639-01 ")
 ;;2388
 ;;21,"57866-4639-02 ")
 ;;2387
 ;;21,"57866-4939-02 ")
 ;;709
 ;;21,"57866-5559-01 ")
 ;;1415
 ;;21,"57866-6011-01 ")
 ;;1594
 ;;21,"57866-6335-01 ")
 ;;1244
 ;;21,"57866-6602-02 ")
 ;;2935
 ;;21,"57866-6602-03 ")
 ;;2936
 ;;21,"57866-6607-02 ")
 ;;2646
 ;;21,"57866-6608-01 ")
 ;;2649
 ;;21,"57866-6608-03 ")
 ;;2648
 ;;21,"57866-6608-04 ")
 ;;2647
 ;;21,"57866-6609-01 ")
 ;;2653
 ;;21,"57866-6609-02 ")
 ;;2650
 ;;21,"57866-6609-03 ")
 ;;2654
 ;;21,"57866-6609-06 ")
 ;;2652
 ;;21,"57866-6609-07 ")
 ;;2651
 ;;21,"57866-6613-00 ")
 ;;3040
 ;;21,"57866-6613-01 ")
 ;;3042
 ;;21,"57866-6613-02 ")
 ;;3038
 ;;21,"57866-6613-03 ")
 ;;3036
 ;;21,"57866-6613-04 ")
 ;;3037
 ;;21,"57866-6613-05 ")
 ;;3039
 ;;21,"57866-6613-06 ")
 ;;3041
 ;;21,"57866-6613-07 ")
 ;;3043
 ;;21,"57866-6920-04 ")
 ;;7
 ;;21,"57866-6924-02 ")
 ;;152
 ;;21,"57866-6925-01 ")
 ;;153
 ;;21,"57866-7137-01 ")
 ;;446
 ;;21,"57866-7382-01 ")
 ;;859
 ;;21,"57866-7382-03 ")
 ;;860
 ;;21,"57866-7602-01 ")
 ;;3508
 ;;21,"57866-7602-02 ")
 ;;3510
 ;;21,"57866-7602-05 ")
 ;;3509
 ;;21,"57866-7602-06 ")
 ;;3511
 ;;21,"57866-7602-07 ")
 ;;3512
 ;;21,"57866-7801-01 ")
 ;;3507
 ;;21,"58016-0169-00 ")
 ;;3605
 ;;21,"58016-0169-10 ")
 ;;3598
 ;;21,"58016-0169-12 ")
 ;;3599
 ;;21,"58016-0169-20 ")
 ;;3600
 ;;21,"58016-0169-21 ")
 ;;3601
 ;;21,"58016-0169-28 ")
 ;;3602
 ;;21,"58016-0169-30 ")
 ;;3603
 ;;21,"58016-0169-60 ")
 ;;3604
 ;;21,"58016-0194-00 ")
 ;;3430
 ;;21,"58016-0194-02 ")
 ;;3431
 ;;21,"58016-0194-03 ")
 ;;3432
 ;;21,"58016-0194-10 ")
 ;;3386
 ;;21,"58016-0194-12 ")
 ;;3387
 ;;21,"58016-0194-14 ")
 ;;3388
 ;;21,"58016-0194-15 ")
 ;;3389
 ;;21,"58016-0194-20 ")
 ;;3390
 ;;21,"58016-0194-21 ")
 ;;3391
 ;;21,"58016-0194-24 ")
 ;;3392
 ;;21,"58016-0194-28 ")
 ;;3393
 ;;21,"58016-0194-30 ")
 ;;3394
 ;;21,"58016-0194-40 ")
 ;;3395
 ;;21,"58016-0194-50 ")
 ;;3426
 ;;21,"58016-0194-60 ")
 ;;3396
 ;;21,"58016-0194-70 ")
 ;;3427
 ;;21,"58016-0194-73 ")
 ;;3434
 ;;21,"58016-0194-80 ")
 ;;3428
 ;;21,"58016-0194-89 ")
 ;;3433
 ;;21,"58016-0194-90 ")
 ;;3429
 ;;21,"58016-0206-00 ")
 ;;519
 ;;21,"58016-0206-14 ")
 ;;354
 ;;21,"58016-0206-15 ")
 ;;355
 ;;21,"58016-0206-20 ")
 ;;356
 ;;21,"58016-0206-21 ")
 ;;357
 ;;21,"58016-0206-30 ")
 ;;358
 ;;21,"58016-0206-42 ")
 ;;359
 ;;21,"58016-0208-00 ")
 ;;523
 ;;21,"58016-0208-14 ")
 ;;367
 ;;21,"58016-0208-15 ")
 ;;368
 ;;21,"58016-0208-20 ")
 ;;369
 ;;21,"58016-0208-21 ")
 ;;370
 ;;21,"58016-0208-30 ")
 ;;371
 ;;21,"58016-0208-42 ")
 ;;372
 ;;21,"58016-0208-60 ")
 ;;373
 ;;21,"58016-0208-90 ")
 ;;522
 ;;21,"58016-0209-00 ")
 ;;521
 ;;21,"58016-0209-14 ")
 ;;360
 ;;21,"58016-0209-15 ")
 ;;361
 ;;21,"58016-0209-20 ")
 ;;362
 ;;21,"58016-0209-21 ")
 ;;363
 ;;21,"58016-0209-30 ")
 ;;364
 ;;21,"58016-0209-42 ")
 ;;365
 ;;21,"58016-0209-60 ")
 ;;366
 ;;21,"58016-0209-90 ")
 ;;520
 ;;21,"58016-0223-00 ")
 ;;3615
 ;;21,"58016-0223-10 ")
 ;;3606
 ;;21,"58016-0223-12 ")
 ;;3607
 ;;21,"58016-0223-14 ")
 ;;3608
 ;;21,"58016-0223-15 ")
 ;;3609
 ;;21,"58016-0223-20 ")
 ;;3610
 ;;21,"58016-0223-21 ")
 ;;3611
 ;;21,"58016-0223-28 ")
 ;;3612
 ;;21,"58016-0223-30 ")
 ;;3613
 ;;21,"58016-0223-60 ")
 ;;3614
 ;;21,"58016-0235-00 ")
 ;;787
 ;;21,"58016-0235-08 ")
 ;;782
 ;;21,"58016-0235-20 ")
 ;;783
 ;;21,"58016-0235-30 ")
 ;;784
 ;;21,"58016-0235-60 ")
 ;;785
 ;;21,"58016-0235-75 ")
 ;;786
 ;;21,"58016-0236-00 ")
 ;;794
 ;;21,"58016-0236-14 ")
 ;;788
 ;;21,"58016-0236-20 ")
 ;;789
 ;;21,"58016-0236-21 ")
 ;;790
 ;;21,"58016-0236-30 ")
 ;;791
 ;;21,"58016-0236-40 ")
 ;;792
 ;;21,"58016-0236-60 ")
 ;;793
 ;;21,"58016-0237-07 ")
 ;;795
 ;;21,"58016-0237-10 ")
 ;;796
 ;;21,"58016-0237-12 ")
 ;;797
 ;;21,"58016-0237-14 ")
 ;;798
 ;;21,"58016-0237-15 ")
 ;;799
 ;;21,"58016-0237-20 ")
 ;;800
 ;;21,"58016-0237-21 ")
 ;;801
 ;;21,"58016-0237-24 ")
 ;;802
 ;;21,"58016-0237-28 ")
 ;;803
 ;;21,"58016-0237-30 ")
 ;;804
 ;;21,"58016-0237-40 ")
 ;;805
 ;;21,"58016-0237-60 ")
 ;;806
 ;;21,"58016-0241-00 ")
 ;;1850
 ;;21,"58016-0241-02 ")
 ;;1851
 ;;21,"58016-0241-08 ")
 ;;1837
 ;;21,"58016-0241-12 ")
 ;;1838
 ;;21,"58016-0241-14 ")
 ;;1839
 ;;21,"58016-0241-15 ")
 ;;1840
 ;;21,"58016-0241-16 ")
 ;;1841
 ;;21,"58016-0241-20 ")
 ;;1842
 ;;21,"58016-0241-21 ")
 ;;1843
 ;;21,"58016-0241-30 ")
 ;;1844
 ;;21,"58016-0241-40 ")
 ;;1845
 ;;21,"58016-0241-50 ")
 ;;1846
 ;;21,"58016-0241-56 ")
 ;;1847
 ;;21,"58016-0241-60 ")
 ;;1848
 ;;21,"58016-0241-90 ")
 ;;1849
 ;;21,"58016-0242-00 ")
 ;;1867
 ;;21,"58016-0242-02 ")
 ;;1868
 ;;21,"58016-0242-06 ")
 ;;1852
 ;;21,"58016-0242-08 ")
 ;;1853
 ;;21,"58016-0242-10 ")
 ;;1854
 ;;21,"58016-0242-12 ")
 ;;1855
 ;;21,"58016-0242-15 ")
 ;;1856
 ;;21,"58016-0242-20 ")
 ;;1857
 ;;21,"58016-0242-21 ")
 ;;1858
 ;;21,"58016-0242-28 ")
 ;;1859
 ;;21,"58016-0242-30 ")
 ;;1860
 ;;21,"58016-0242-40 ")
 ;;1861
 ;;21,"58016-0242-42 ")
 ;;1862
 ;;21,"58016-0242-50 ")
 ;;1863
 ;;21,"58016-0242-60 ")
 ;;1864
 ;;21,"58016-0242-67 ")
 ;;1869
 ;;21,"58016-0242-80 ")
 ;;1865
 ;;21,"58016-0242-90 ")
 ;;1866
 ;;21,"58016-0243-00 ")
 ;;1836
 ;;21,"58016-0243-02 ")
 ;;1835
 ;;21,"58016-0243-03 ")
 ;;1833
 ;;21,"58016-0243-12 ")
 ;;1870
 ;;21,"58016-0243-14 ")
 ;;1871
 ;;21,"58016-0243-15 ")
 ;;1872
 ;;21,"58016-0243-16 ")
 ;;1873
 ;;21,"58016-0243-20 ")
 ;;1874
 ;;21,"58016-0243-21 ")
 ;;1875
 ;;21,"58016-0243-24 ")
 ;;1876
 ;;21,"58016-0243-28 ")
 ;;1877
 ;;21,"58016-0243-30 ")
 ;;1878
 ;;21,"58016-0243-40 ")
 ;;1879
 ;;21,"58016-0243-42 ")
 ;;1880
 ;;21,"58016-0243-45 ")
 ;;1881
 ;;21,"58016-0243-50 ")
 ;;1882
 ;;21,"58016-0243-56 ")
 ;;1883
 ;;21,"58016-0243-60 ")
 ;;1884
 ;;21,"58016-0243-67 ")
 ;;1834
 ;;21,"58016-0243-80 ")
 ;;1885
 ;;21,"58016-0243-84 ")
 ;;1886
 ;;21,"58016-0243-90 ")
 ;;1887
 ;;21,"58016-0244-06 ")
 ;;1555
 ;;21,"58016-0244-20 ")
 ;;1556
 ;;21,"58016-0244-21 ")
 ;;1557
 ;;21,"58016-0244-30 ")
 ;;1558
 ;;21,"58016-0244-60 ")
 ;;1559
 ;;21,"58016-0247-00 ")
 ;;839
 ;;21,"58016-0247-02 ")
 ;;892
 ;;21,"58016-0247-03 ")
 ;;893
 ;;21,"58016-0247-10 ")
 ;;833
 ;;21,"58016-0247-12 ")
 ;;834
 ;;21,"58016-0247-15 ")
 ;;835
 ;;21,"58016-0247-20 ")
 ;;836
 ;;21,"58016-0247-21 ")
 ;;837
 ;;21,"58016-0247-30 ")
 ;;838
 ;;21,"58016-0247-40 ")
 ;;891
 ;;21,"58016-0247-73 ")
 ;;895
 ;;21,"58016-0247-89 ")
 ;;894
 ;;21,"58016-0262-00 ")
 ;;2362
 ;;21,"58016-0262-02 ")
 ;;2417
 ;;21,"58016-0262-10 ")
 ;;2356
 ;;21,"58016-0262-15 ")
 ;;2357
 ;;21,"58016-0262-20 ")
 ;;2358
 ;;21,"58016-0262-28 ")
 ;;2359
 ;;21,"58016-0262-30 ")
 ;;2360
 ;;21,"58016-0262-60 ")
 ;;2361
 ;;21,"58016-0267-00 ")
 ;;2876
 ;;21,"58016-0267-02 ")
 ;;2877
 ;;21,"58016-0267-03 ")
 ;;2878
 ;;21,"58016-0267-10 ")
 ;;2863
 ;;21,"58016-0267-12 ")
 ;;2864
 ;;21,"58016-0267-14 ")
 ;;2865
 ;;21,"58016-0267-15 ")
 ;;2866
 ;;21,"58016-0267-20 ")
 ;;2867
 ;;21,"58016-0267-21 ")
 ;;2868
 ;;21,"58016-0267-24 ")
 ;;2869
 ;;21,"58016-0267-28 ")
 ;;2870
 ;;21,"58016-0267-30 ")
 ;;2871
 ;;21,"58016-0267-40 ")
 ;;2872
 ;;21,"58016-0267-42 ")
 ;;2873
 ;;21,"58016-0267-60 ")
 ;;2874
 ;;21,"58016-0267-73 ")
 ;;2880
 ;;21,"58016-0267-89 ")
 ;;2879
 ;;21,"58016-0267-90 ")
 ;;2875
 ;;21,"58016-0270-04 ")
 ;;2119
 ;;21,"58016-0289-00 ")
 ;;2896
 ;;21,"58016-0289-02 ")
 ;;2897
 ;;21,"58016-0289-03 ")
 ;;2899
 ;;21,"58016-0289-10 ")
 ;;2881
 ;;21,"58016-0289-12 ")
 ;;2882
 ;;21,"58016-0289-14 ")
 ;;2883
 ;;21,"58016-0289-15 ")
 ;;2884
 ;;21,"58016-0289-18 ")
 ;;2885
 ;;21,"58016-0289-20 ")
 ;;2886
 ;;21,"58016-0289-21 ")
 ;;2887
 ;;21,"58016-0289-24 ")
 ;;2888
 ;;21,"58016-0289-28 ")
 ;;2889
 ;;21,"58016-0289-30 ")
 ;;2890
 ;;21,"58016-0289-40 ")
 ;;2891
 ;;21,"58016-0289-42 ")
 ;;2892
 ;;21,"58016-0289-50 ")
 ;;2893
 ;;21,"58016-0289-60 ")
 ;;2894
 ;;21,"58016-0289-67 ")
 ;;2898
 ;;21,"58016-0289-73 ")
 ;;2901
 ;;21,"58016-0289-89 ")
 ;;2900
 ;;21,"58016-0289-90 ")
 ;;2895
 ;;21,"58016-0294-00 ")
 ;;1045
 ;;21,"58016-0294-02 ")
 ;;1046
 ;;21,"58016-0294-03 ")
 ;;1047
 ;;21,"58016-0294-12 ")
 ;;1037
 ;;21,"58016-0294-15 ")
 ;;1038
 ;;21,"58016-0294-20 ")
 ;;1039
 ;;21,"58016-0294-28 ")
 ;;1040
 ;;21,"58016-0294-30 ")
 ;;1041
 ;;21,"58016-0294-40 ")
 ;;1042
 ;;21,"58016-0294-60 ")
 ;;1043
 ;;21,"58016-0294-73 ")
 ;;1049
 ;;21,"58016-0294-89 ")
 ;;1048
 ;;21,"58016-0294-90 ")
 ;;1044
 ;;21,"58016-0314-00 ")
 ;;2858
 ;;21,"58016-0314-02 ")
 ;;2859
 ;;21,"58016-0314-03 ")
 ;;2860
 ;;21,"58016-0314-10 ")
 ;;2847
 ;;21,"58016-0314-12 ")
 ;;2848
 ;;21,"58016-0314-14 ")
 ;;2849
 ;;21,"58016-0314-15 ")
 ;;2850
 ;;21,"58016-0314-20 ")
 ;;2851
 ;;21,"58016-0314-21 ")
 ;;2852
 ;;21,"58016-0314-24 ")
 ;;2853
 ;;21,"58016-0314-28 ")
 ;;2854
 ;;21,"58016-0314-30 ")
 ;;2855
 ;;21,"58016-0314-40 ")
 ;;2856
 ;;21,"58016-0314-60 ")
 ;;2857
 ;;21,"58016-0314-73 ")
 ;;2862
 ;;21,"58016-0314-89 ")
 ;;2861
 ;;21,"58016-0321-00 ")
 ;;3143
 ;;21,"58016-0321-02 ")
 ;;3144
 ;;21,"58016-0321-03 ")
 ;;3145
 ;;21,"58016-0321-10 ")
 ;;3130
 ;;21,"58016-0321-12 ")
 ;;3131
 ;;21,"58016-0321-14 ")
 ;;3132
 ;;21,"58016-0321-15 ")
 ;;3133
 ;;21,"58016-0321-20 ")
 ;;3134
 ;;21,"58016-0321-24 ")
 ;;3135
 ;;21,"58016-0321-28 ")
 ;;3136
 ;;21,"58016-0321-30 ")
 ;;3137
 ;;21,"58016-0321-40 ")
 ;;3138
 ;;21,"58016-0321-42 ")
 ;;3139
 ;;21,"58016-0321-56 ")
 ;;3140
 ;;21,"58016-0321-60 ")
 ;;3141
 ;;21,"58016-0321-73 ")
 ;;3147
 ;;21,"58016-0321-89 ")
 ;;3146
 ;;21,"58016-0321-90 ")
 ;;3142
 ;;21,"58016-0340-00 ")
 ;;1620
 ;;21,"58016-0340-02 ")
 ;;1621
 ;;21,"58016-0340-03 ")
 ;;1622
 ;;21,"58016-0340-10 ")
 ;;1614
 ;;21,"58016-0340-15 ")
 ;;1615
 ;;21,"58016-0340-20 ")
 ;;1616
 ;;21,"58016-0340-28 ")
 ;;1617
