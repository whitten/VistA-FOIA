BGP06J7 ;IHS/CMI/LAB-CREATED BY ^ATXSTX ON MAY 23, 2010;
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
