BGP13Q2 ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON APR 14, 2011 ;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;;JUN 27, 2011;Build 33
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;21,"00185-0173-01 ")
 ;;1172
 ;;21,"00185-0173-10 ")
 ;;1173
 ;;21,"00185-0204-01 ")
 ;;315
 ;;21,"00185-0211-01 ")
 ;;316
 ;;21,"00185-0214-01 ")
 ;;666
 ;;21,"00185-0214-10 ")
 ;;667
 ;;21,"00185-0214-50 ")
 ;;668
 ;;21,"00185-0277-01 ")
 ;;317
 ;;21,"00185-0341-01 ")
 ;;1131
 ;;21,"00185-0342-01 ")
 ;;1132
 ;;21,"00185-0505-01 ")
 ;;177
 ;;21,"00185-0505-05 ")
 ;;178
 ;;21,"00185-0820-01 ")
 ;;179
 ;;21,"00185-0820-05 ")
 ;;180
 ;;21,"00185-5400-01 ")
 ;;1413
 ;;21,"00185-5400-10 ")
 ;;1414
 ;;21,"00185-7100-01 ")
 ;;1174
 ;;21,"00185-7100-10 ")
 ;;1175
 ;;21,"00228-2658-11 ")
 ;;669
 ;;21,"00228-2658-96 ")
 ;;670
 ;;21,"00228-2659-11 ")
 ;;671
 ;;21,"00228-2659-96 ")
 ;;672
 ;;21,"00228-2660-11 ")
 ;;673
 ;;21,"00228-2660-96 ")
 ;;674
 ;;21,"00228-2661-11 ")
 ;;675
 ;;21,"00228-2661-96 ")
 ;;676
 ;;21,"00228-2695-11 ")
 ;;2301
 ;;21,"00228-2695-50 ")
 ;;2302
 ;;21,"00228-2696-11 ")
 ;;2303
 ;;21,"00228-2696-50 ")
 ;;2304
 ;;21,"00228-2697-11 ")
 ;;2305
 ;;21,"00228-2697-50 ")
 ;;2306
 ;;21,"00228-2706-03 ")
 ;;1176
 ;;21,"00228-2706-11 ")
 ;;1177
 ;;21,"00228-2707-03 ")
 ;;1178
 ;;21,"00228-2707-11 ")
 ;;1179
 ;;21,"00228-2708-03 ")
 ;;1180
 ;;21,"00228-2708-11 ")
 ;;1181
 ;;21,"00247-0578-02 ")
 ;;2451
 ;;21,"00247-0578-03 ")
 ;;2452
 ;;21,"00247-0578-30 ")
 ;;2453
 ;;21,"00247-0578-45 ")
 ;;2454
 ;;21,"00247-0579-03 ")
 ;;2455
 ;;21,"00247-0579-30 ")
 ;;2456
 ;;21,"00247-1023-00 ")
 ;;2516
 ;;21,"00247-1023-30 ")
 ;;2517
 ;;21,"00247-1023-59 ")
 ;;2518
 ;;21,"00247-1023-60 ")
 ;;2519
 ;;21,"00247-1023-77 ")
 ;;2520
 ;;21,"00247-1023-79 ")
 ;;2521
 ;;21,"00247-1023-95 ")
 ;;2522
 ;;21,"00247-1090-30 ")
 ;;72
 ;;21,"00247-1090-60 ")
 ;;73
 ;;21,"00247-1091-03 ")
 ;;74
 ;;21,"00247-1091-30 ")
 ;;75
 ;;21,"00247-1091-60 ")
 ;;76
 ;;21,"00247-1124-30 ")
 ;;1996
 ;;21,"00247-1125-10 ")
 ;;1997
 ;;21,"00247-1125-30 ")
 ;;1998
 ;;21,"00247-1125-60 ")
 ;;1999
 ;;21,"00247-1126-10 ")
 ;;2000
 ;;21,"00247-1126-30 ")
 ;;2001
 ;;21,"00247-1126-60 ")
 ;;2002
 ;;21,"00247-1127-10 ")
 ;;2003
 ;;21,"00247-1127-30 ")
 ;;2004
 ;;21,"00247-1131-30 ")
 ;;2130
 ;;21,"00247-1131-60 ")
 ;;2131
 ;;21,"00247-1132-30 ")
 ;;2132
 ;;21,"00247-1132-60 ")
 ;;2133
 ;;21,"00247-1147-10 ")
 ;;2523
 ;;21,"00247-1147-30 ")
 ;;2524
 ;;21,"00247-1147-60 ")
 ;;2525
 ;;21,"00247-1148-10 ")
 ;;2526
 ;;21,"00247-1149-30 ")
 ;;2503
 ;;21,"00247-1149-60 ")
 ;;2504
 ;;21,"00247-1150-30 ")
 ;;2505
 ;;21,"00247-1150-60 ")
 ;;2506
 ;;21,"00247-1222-00 ")
 ;;370
 ;;21,"00247-1277-00 ")
 ;;2116
 ;;21,"00247-1380-14 ")
 ;;2457
 ;;21,"00247-1381-14 ")
 ;;371
 ;;21,"00247-1381-30 ")
 ;;372
 ;;21,"00247-1381-60 ")
 ;;373
 ;;21,"00247-1381-90 ")
 ;;374
 ;;21,"00247-1396-30 ")
 ;;2527
 ;;21,"00247-1439-04 ")
 ;;2449
 ;;21,"00247-1440-04 ")
 ;;2444
 ;;21,"00247-1525-30 ")
 ;;8
 ;;21,"00247-1525-59 ")
 ;;9
 ;;21,"00247-1525-90 ")
 ;;10
 ;;21,"00247-1635-30 ")
 ;;431
 ;;21,"00247-1636-00 ")
 ;;432
 ;;21,"00247-1636-01 ")
 ;;433
 ;;21,"00247-1636-14 ")
 ;;434
 ;;21,"00247-1636-30 ")
 ;;435
 ;;21,"00247-1636-60 ")
 ;;436
 ;;21,"00247-1636-90 ")
 ;;437
 ;;21,"00247-1636-99 ")
 ;;438
 ;;21,"00247-1637-00 ")
 ;;439
 ;;21,"00247-1637-30 ")
 ;;440
 ;;21,"00247-1637-60 ")
 ;;441
 ;;21,"00247-1881-02 ")
 ;;677
 ;;21,"00247-1881-05 ")
 ;;678
 ;;21,"00247-1881-30 ")
 ;;679
 ;;21,"00247-1881-60 ")
 ;;680
 ;;21,"00247-1882-30 ")
 ;;681
 ;;21,"00247-1882-60 ")
 ;;682
 ;;21,"00247-1882-99 ")
 ;;683
 ;;21,"00247-1919-30 ")
 ;;2173
 ;;21,"00247-1920-30 ")
 ;;2174
 ;;21,"00247-1921-30 ")
 ;;2175
 ;;21,"00247-1955-30 ")
 ;;11
 ;;21,"00247-1955-60 ")
 ;;12
 ;;21,"00247-1955-90 ")
 ;;13
 ;;21,"00247-1956-30 ")
 ;;14
 ;;21,"00247-1956-60 ")
 ;;15
 ;;21,"00247-1956-90 ")
 ;;16
 ;;21,"00247-1957-30 ")
 ;;17
 ;;21,"00247-1957-60 ")
 ;;18
 ;;21,"00247-1957-90 ")
 ;;19
 ;;21,"00247-2001-06 ")
 ;;1415
 ;;21,"00247-2001-30 ")
 ;;1416
 ;;21,"00247-2021-00 ")
 ;;1417
 ;;21,"00247-2021-30 ")
 ;;1418
 ;;21,"00247-2021-60 ")
 ;;1419
 ;;21,"00247-2058-30 ")
 ;;1054
 ;;21,"00247-2107-00 ")
 ;;1182
 ;;21,"00247-2107-30 ")
 ;;1183
 ;;21,"00247-2107-60 ")
 ;;1184
 ;;21,"00247-2107-79 ")
 ;;1185
 ;;21,"00247-2107-90 ")
 ;;1186
 ;;21,"00247-2136-30 ")
 ;;181
 ;;21,"00247-2136-60 ")
 ;;182
 ;;21,"00247-2137-30 ")
 ;;183
 ;;21,"00247-2151-30 ")
 ;;184
 ;;21,"00247-2151-60 ")
 ;;185
 ;;21,"00247-2155-30 ")
 ;;684
 ;;21,"00247-2155-60 ")
 ;;685
 ;;21,"00247-2155-90 ")
 ;;686
 ;;21,"00247-2171-30 ")
 ;;1055
 ;;21,"00247-2237-00 ")
 ;;1420
 ;;21,"00247-2237-30 ")
 ;;1421
 ;;21,"00247-2305-30 ")
 ;;1056
 ;;21,"00247-2305-60 ")
 ;;1057
 ;;21,"00247-2305-90 ")
 ;;1058
 ;;21,"00247-2306-30 ")
 ;;687
 ;;21,"00247-2306-60 ")
 ;;688
 ;;21,"00247-2306-90 ")
 ;;689
 ;;21,"00247-2318-30 ")
 ;;1422
 ;;21,"00247-2318-60 ")
 ;;1423
 ;;21,"00247-2318-90 ")
 ;;1424
 ;;21,"00247-2320-30 ")
 ;;2227
 ;;21,"00247-2320-60 ")
 ;;2228
 ;;21,"00247-2320-90 ")
 ;;2229
 ;;21,"00310-0130-10 ")
 ;;2528
 ;;21,"00310-0130-11 ")
 ;;2529
 ;;21,"00310-0130-34 ")
 ;;2530
 ;;21,"00310-0130-39 ")
 ;;2531
 ;;21,"00310-0131-10 ")
 ;;2532
 ;;21,"00310-0131-11 ")
 ;;2533
 ;;21,"00310-0131-34 ")
 ;;2534
 ;;21,"00310-0131-39 ")
 ;;2535
 ;;21,"00310-0132-10 ")
 ;;2536
 ;;21,"00310-0132-11 ")
 ;;2537
 ;;21,"00310-0132-39 ")
 ;;2538
 ;;21,"00310-0133-10 ")
 ;;2539
 ;;21,"00310-0133-11 ")
 ;;2540
 ;;21,"00310-0134-10 ")
 ;;2541
 ;;21,"00310-0134-11 ")
 ;;2542
 ;;21,"00310-0135-10 ")
 ;;2543
 ;;21,"00310-0141-10 ")
 ;;2507
 ;;21,"00310-0141-11 ")
 ;;2508
 ;;21,"00310-0142-10 ")
 ;;2509
 ;;21,"00310-0145-10 ")
 ;;2510
 ;;21,"00310-0145-11 ")
 ;;2511
 ;;21,"00364-2698-01 ")
 ;;690
 ;;21,"00364-2698-02 ")
 ;;691
 ;;21,"00364-2701-01 ")
 ;;692
 ;;21,"00364-2701-02 ")
 ;;693
 ;;21,"00364-2727-01 ")
 ;;694
 ;;21,"00364-2727-02 ")
 ;;695
 ;;21,"00364-2734-01 ")
 ;;696
 ;;21,"00364-2734-02 ")
 ;;697
 ;;21,"00378-0017-77 ")
 ;;2230
 ;;21,"00378-0081-01 ")
 ;;620
 ;;21,"00378-0083-01 ")
 ;;621
 ;;21,"00378-0084-01 ")
 ;;622
 ;;21,"00378-0086-01 ")
 ;;623
 ;;21,"00378-0226-77 ")
 ;;2231
 ;;21,"00378-0254-77 ")
 ;;2232
 ;;21,"00378-0272-77 ")
 ;;2233
 ;;21,"00378-0441-01 ")
 ;;186
 ;;21,"00378-0443-01 ")
 ;;187
 ;;21,"00378-0444-01 ")
 ;;188
 ;;21,"00378-0447-01 ")
 ;;189
 ;;21,"00378-0542-77 ")
 ;;1326
 ;;21,"00378-0543-77 ")
 ;;1327
 ;;21,"00378-0544-77 ")
 ;;1328
 ;;21,"00378-0712-01 ")
 ;;1012
 ;;21,"00378-0723-01 ")
 ;;1013
 ;;21,"00378-1012-01 ")
 ;;1187
 ;;21,"00378-1051-01 ")
 ;;698
 ;;21,"00378-1051-05 ")
 ;;699
 ;;21,"00378-1052-01 ")
 ;;700
 ;;21,"00378-1052-10 ")
 ;;701
 ;;21,"00378-1053-01 ")
 ;;702
 ;;21,"00378-1053-10 ")
 ;;703
 ;;21,"00378-1054-01 ")
 ;;704
 ;;21,"00378-1054-05 ")
 ;;705
 ;;21,"00378-1117-77 ")
 ;;2234
 ;;21,"00378-2012-01 ")
 ;;1188
 ;;21,"00378-2025-01 ")
 ;;1189
 ;;21,"00378-2072-01 ")
 ;;1425
 ;;21,"00378-2073-01 ")
 ;;1426
 ;;21,"00378-2073-10 ")
 ;;1427
 ;;21,"00378-2074-01 ")
 ;;1428
 ;;21,"00378-2074-10 ")
 ;;1429
 ;;21,"00378-2075-01 ")
 ;;1430
 ;;21,"00378-2075-10 ")
 ;;1431
 ;;21,"00378-2076-01 ")
 ;;1432
 ;;21,"00378-2076-05 ")
 ;;1433
 ;;21,"00378-2077-01 ")
 ;;1434
 ;;21,"00378-3007-01 ")
 ;;442
 ;;21,"00378-3007-10 ")
 ;;443
 ;;21,"00378-3012-01 ")
 ;;444
 ;;21,"00378-3012-10 ")
 ;;445
 ;;21,"00378-3017-01 ")
 ;;446
 ;;21,"00378-3017-10 ")
 ;;447
 ;;21,"00378-3022-01 ")
 ;;448
 ;;21,"00378-3241-01 ")
 ;;2398
 ;;21,"00378-3242-01 ")
 ;;2399
 ;;21,"00378-3243-01 ")
 ;;2400
 ;;21,"00378-4725-01 ")
 ;;318
 ;;21,"00378-4735-01 ")
 ;;319
 ;;21,"00378-4745-01 ")
 ;;320
 ;;21,"00378-4775-01 ")
 ;;321
 ;;21,"00440-7231-60 ")
 ;;449
 ;;21,"00440-7232-60 ")
 ;;450
 ;;21,"00440-7674-90 ")
 ;;1435
 ;;21,"00440-7675-90 ")
 ;;1436
 ;;21,"00440-7676-90 ")
 ;;1437
 ;;21,"00490-0067-00 ")
 ;;1014
 ;;21,"00490-0067-30 ")
 ;;1015
 ;;21,"00490-0067-60 ")
 ;;1016
 ;;21,"00490-0067-90 ")
 ;;1017
 ;;21,"00490-7030-00 ")
 ;;1018
 ;;21,"00490-7030-30 ")
 ;;1019
 ;;21,"00490-7030-60 ")
 ;;1020
 ;;21,"00490-7030-90 ")
 ;;1021
 ;;21,"00536-3471-01 ")
 ;;451
 ;;21,"00536-3473-01 ")
 ;;452
 ;;21,"00536-3474-01 ")
 ;;453
 ;;21,"00574-0110-01 ")
 ;;2117
 ;;21,"00574-0112-15 ")
 ;;2118
 ;;21,"00574-0133-01 ")
 ;;1313
 ;;21,"00574-0134-01 ")
 ;;1314
 ;;21,"00574-0135-01 ")
 ;;1315
 ;;21,"00591-0405-01 ")
 ;;1438
 ;;21,"00591-0405-05 ")
 ;;1439
 ;;21,"00591-0406-01 ")
 ;;1440
 ;;21,"00591-0406-10 ")
 ;;1441
 ;;21,"00591-0407-01 ")
 ;;1442
 ;;21,"00591-0407-10 ")
 ;;1443
 ;;21,"00591-0408-01 ")
 ;;1444
 ;;21,"00591-0408-10 ")
 ;;1445
 ;;21,"00591-0409-01 ")
 ;;1446
 ;;21,"00591-0409-05 ")
 ;;1447
 ;;21,"00591-0409-75 ")
 ;;1448
 ;;21,"00591-0668-01 ")
 ;;706
 ;;21,"00591-0668-05 ")
 ;;707
 ;;21,"00591-0669-01 ")
 ;;708
 ;;21,"00591-0669-05 ")
 ;;709
 ;;21,"00591-0670-01 ")
 ;;710
 ;;21,"00591-0670-05 ")
 ;;711
 ;;21,"00591-0671-01 ")
 ;;712
 ;;21,"00591-0671-05 ")
 ;;713
 ;;21,"00591-0671-10 ")
 ;;714
 ;;21,"00591-0860-01 ")
 ;;1190
 ;;21,"00591-0860-05 ")
 ;;1191
 ;;21,"00591-0861-01 ")
 ;;1192
 ;;21,"00591-0861-05 ")
 ;;1193
 ;;21,"00591-0862-01 ")
 ;;1194
 ;;21,"00591-0862-05 ")
 ;;1195
 ;;21,"00591-0885-01 ")
 ;;1449
 ;;21,"00603-4209-21 ")
 ;;1450
 ;;21,"00603-4209-28 ")
 ;;1451
 ;;21,"00603-4210-02 ")
 ;;1452
 ;;21,"00603-4210-16 ")
 ;;1453
 ;;21,"00603-4210-21 ")
 ;;1454
 ;;21,"00603-4210-28 ")
 ;;1455
 ;;21,"00603-4210-32 ")
 ;;1456
 ;;21,"00603-4210-60 ")
 ;;1457
 ;;21,"00603-4211-02 ")
 ;;1458
 ;;21,"00603-4211-21 ")
 ;;1459
 ;;21,"00603-4211-28 ")
 ;;1460
 ;;21,"00603-4211-32 ")
 ;;1461
 ;;21,"00603-4211-34 ")
 ;;1462
 ;;21,"00603-4211-60 ")
 ;;1463
 ;;21,"00603-4212-02 ")
 ;;1464
 ;;21,"00603-4212-21 ")
 ;;1465
 ;;21,"00603-4212-28 ")
 ;;1466
 ;;21,"00603-4212-32 ")
 ;;1467
 ;;21,"00603-4212-60 ")
 ;;1468
 ;;21,"00603-4214-02 ")
 ;;1469
 ;;21,"00603-4214-21 ")
 ;;1470
 ;;21,"00603-4214-30 ")
 ;;1471
 ;;21,"00603-4214-32 ")
 ;;1472
 ;;21,"00603-4214-60 ")
 ;;1473
 ;;21,"00615-4519-53 ")
 ;;454
 ;;21,"00615-4519-63 ")
 ;;455
 ;;21,"00615-4520-53 ")
 ;;456
 ;;21,"00615-4520-63 ")
 ;;457
 ;;21,"00615-4521-65 ")
 ;;458
 ;;21,"00781-1176-01 ")
 ;;1196
 ;;21,"00781-1178-01 ")
 ;;1197
 ;;21,"00781-1229-01 ")
 ;;715
 ;;21,"00781-1229-10 ")
 ;;716
 ;;21,"00781-1229-13 ")
 ;;717
 ;;21,"00781-1231-01 ")
 ;;718
 ;;21,"00781-1231-13 ")
 ;;719
 ;;21,"00781-1232-10 ")
 ;;720
 ;;21,"00781-1232-13 ")
 ;;721
 ;;21,"00781-1233-10 ")
 ;;722
 ;;21,"00781-1665-01 ")
 ;;1474
 ;;21,"00781-1666-01 ")
 ;;1475
 ;;21,"00781-1667-01 ")
 ;;1476
 ;;21,"00781-1668-01 ")
 ;;1477
 ;;21,"00781-1669-01 ")
 ;;1478
 ;;21,"00781-1673-01 ")
 ;;1479
 ;;21,"00781-1828-01 ")
 ;;459
 ;;21,"00781-1839-01 ")
 ;;460
 ;;21,"00781-1848-01 ")
 ;;1198
 ;;21,"00781-1891-01 ")
 ;;190
 ;;21,"00781-1892-01 ")
 ;;191
 ;;21,"00781-1893-01 ")
 ;;192
 ;;21,"00781-1894-01 ")
 ;;193
 ;;21,"00781-2126-01 ")
 ;;2307
 ;;21,"00781-2127-01 ")
 ;;2308
 ;;21,"00781-2127-05 ")
 ;;2309
 ;;21,"00781-2128-01 ")
 ;;2310
 ;;21,"00781-2128-05 ")
 ;;2311
 ;;21,"00781-2129-01 ")
 ;;2312
 ;;21,"00781-2129-05 ")
 ;;2313
 ;;21,"00781-2271-01 ")
 ;;124
 ;;21,"00781-2272-01 ")
 ;;125
 ;;21,"00781-2272-10 ")
 ;;126
 ;;21,"00781-2273-01 ")
 ;;127
 ;;21,"00781-2273-10 ")
 ;;128
 ;;21,"00781-2274-01 ")
 ;;129
 ;;21,"00781-2274-10 ")
 ;;130
 ;;21,"00781-5083-10 ")
 ;;1059
 ;;21,"00781-5083-92 ")
 ;;1060
 ;;21,"00781-5084-10 ")
 ;;1061
 ;;21,"00781-5084-92 ")
 ;;1062
 ;;21,"00781-5085-92 ")
 ;;1063
 ;;21,"00781-5131-01 ")
 ;;322
 ;;21,"00781-5132-01 ")
 ;;323
 ;;21,"00781-5133-01 ")
 ;;324
 ;;21,"00781-5134-01 ")
 ;;325
 ;;21,"00781-5320-01 ")
 ;;2401
 ;;21,"00781-5321-01 ")
 ;;2402
 ;;21,"00781-5322-01 ")
 ;;2403
 ;;21,"00781-5441-01 ")
 ;;723
 ;;21,"00781-5441-10 ")
 ;;724
