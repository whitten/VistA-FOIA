BGPM3AAU ;IHS/MSC/SAT-CREATED BY ^ATXSTX ON APR 21, 2011;
 ;;11.0;IHS CLINICAL REPORTING;**4**;JAN 06, 2011;Build 84
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;21,"00603016902 ")
 ;;194
 ;;21,"00603016921 ")
 ;;195
 ;;21,"00603016932 ")
 ;;196
 ;;21,"00603806511 ")
 ;;53
 ;;21,"00603806611 ")
 ;;685
 ;;21,"00615052210 ")
 ;;547
 ;;21,"00615052213 ")
 ;;548
 ;;21,"00615052214 ")
 ;;549
 ;;21,"00615052263 ")
 ;;550
 ;;21,"00615052310 ")
 ;;551
 ;;21,"00615056410 ")
 ;;197
 ;;21,"00615056439 ")
 ;;198
 ;;21,"00615056463 ")
 ;;199
 ;;21,"00615150905 ")
 ;;1788
 ;;21,"00615150929 ")
 ;;1789
 ;;21,"00615150939 ")
 ;;1790
 ;;21,"00615151005 ")
 ;;1920
 ;;21,"00615151029 ")
 ;;1921
 ;;21,"00615151039 ")
 ;;1922
 ;;21,"00615151205 ")
 ;;2389
 ;;21,"00615151229 ")
 ;;2390
 ;;21,"00615151239 ")
 ;;2391
 ;;21,"00615350863 ")
 ;;796
 ;;21,"00615454729 ")
 ;;1539
 ;;21,"00615454739 ")
 ;;1540
 ;;21,"00615454805 ")
 ;;2009
 ;;21,"00615454829 ")
 ;;2010
 ;;21,"00615454831 ")
 ;;2011
 ;;21,"00615454839 ")
 ;;2012
 ;;21,"00615454905 ")
 ;;2151
 ;;21,"00615454929 ")
 ;;2152
 ;;21,"00615454939 ")
 ;;2153
 ;;21,"00615455029 ")
 ;;2458
 ;;21,"00615455031 ")
 ;;2459
 ;;21,"00615455039 ")
 ;;2460
 ;;21,"00615455129 ")
 ;;2529
 ;;21,"00641039102 ")
 ;;1112
 ;;21,"00641039125 ")
 ;;1113
 ;;21,"00641039164 ")
 ;;1114
 ;;21,"00641040002 ")
 ;;1328
 ;;21,"00641040021 ")
 ;;1329
 ;;21,"00641040025 ")
 ;;1330
 ;;21,"00641040064 ")
 ;;1331
 ;;21,"00641040125 ")
 ;;1017
 ;;21,"00641041002 ")
 ;;1192
 ;;21,"00641041025 ")
 ;;1193
 ;;21,"00641041064 ")
 ;;1194
 ;;21,"00641244041 ")
 ;;1115
 ;;21,"00641244045 ")
 ;;1116
 ;;21,"00641245041 ")
 ;;1117
 ;;21,"00641245045 ")
 ;;1118
 ;;21,"00641246041 ")
 ;;1332
 ;;21,"00641246045 ")
 ;;1333
 ;;21,"00641247041 ")
 ;;1195
 ;;21,"00641247045 ")
 ;;1196
 ;;21,"00641326209 ")
 ;;1018
 ;;21,"00641326403 ")
 ;;1119
 ;;21,"00641326503 ")
 ;;1120
 ;;21,"00641326603 ")
 ;;1197
 ;;21,"00641326703 ")
 ;;1334
 ;;21,"00641326803 ")
 ;;1198
 ;;21,"00677001710 ")
 ;;39
 ;;21,"00677002001 ")
 ;;552
 ;;21,"00677002010 ")
 ;;553
 ;;21,"00677027521 ")
 ;;1335
 ;;21,"00677077101 ")
 ;;200
 ;;21,"00677077110 ")
 ;;201
 ;;21,"00677077201 ")
 ;;712
 ;;21,"00677077812 ")
 ;;54
 ;;21,"00677077912 ")
 ;;686
 ;;21,"00677079301 ")
 ;;1923
 ;;21,"00677079401 ")
 ;;2392
 ;;21,"00677085001 ")
 ;;554
 ;;21,"00677085101 ")
 ;;663
 ;;21,"00677117201 ")
 ;;731
 ;;21,"00677134701 ")
 ;;879
 ;;21,"00677179901 ")
 ;;732
 ;;21,"00677185301 ")
 ;;202
 ;;21,"00677185310 ")
 ;;203
 ;;21,"00677185408 ")
 ;;797
 ;;21,"00677185510 ")
 ;;868
 ;;21,"00677185567 ")
 ;;869
 ;;21,"00677197101 ")
 ;;204
 ;;21,"00677197110 ")
 ;;205
 ;;21,"00677197210 ")
 ;;870
 ;;21,"00677197708 ")
 ;;798
 ;;21,"00686000520 ")
 ;;555
 ;;21,"00686001101 ")
 ;;1019
 ;;21,"00686001105 ")
 ;;1020
 ;;21,"00686001910 ")
 ;;1121
 ;;21,"00713010401 ")
 ;;20
 ;;21,"00713010412 ")
 ;;21
 ;;21,"00713010450 ")
 ;;22
 ;;21,"00713010501 ")
 ;;55
 ;;21,"00713010512 ")
 ;;56
 ;;21,"00713010550 ")
 ;;57
 ;;21,"00713010601 ")
 ;;687
 ;;21,"00713010612 ")
 ;;688
 ;;21,"00713010650 ")
 ;;689
 ;;21,"00719199310 ")
 ;;2393
 ;;21,"00719199313 ")
 ;;2394
 ;;21,"00725004401 ")
 ;;1924
 ;;21,"00725004410 ")
 ;;1925
 ;;21,"00725004501 ")
 ;;2395
 ;;21,"00725004510 ")
 ;;2396
 ;;21,"00725004601 ")
 ;;1605
 ;;21,"00725004610 ")
 ;;1606
 ;;21,"00725004701 ")
 ;;1607
 ;;21,"00725004710 ")
 ;;1608
 ;;21,"00725005001 ")
 ;;1791
 ;;21,"00725005010 ")
 ;;1792
 ;;21,"00781035207 ")
 ;;1541
 ;;21,"00781035208 ")
 ;;1542
 ;;21,"00781036306 ")
 ;;1793
 ;;21,"00781036307 ")
 ;;1794
 ;;21,"00781036308 ")
 ;;1795
 ;;21,"00781036406 ")
 ;;1926
 ;;21,"00781036407 ")
 ;;1927
 ;;21,"00781036408 ")
 ;;1928
 ;;21,"00781036607 ")
 ;;2013
 ;;21,"00781036608 ")
 ;;2014
 ;;21,"00781036907 ")
 ;;2154
 ;;21,"00781036908 ")
 ;;2155
 ;;21,"00781037704 ")
 ;;2397
 ;;21,"00781037706 ")
 ;;2398
 ;;21,"00781037707 ")
 ;;2399
 ;;21,"00781037708 ")
 ;;2400
 ;;21,"00781038107 ")
 ;;2461
 ;;21,"00781038108 ")
 ;;2462
 ;;21,"00781038607 ")
 ;;2530
 ;;21,"00781038608 ")
 ;;2531
 ;;21,"00781038707 ")
 ;;1609
 ;;21,"00781038708 ")
 ;;1610
 ;;21,"00781113601 ")
 ;;556
 ;;21,"00781113610 ")
 ;;557
 ;;21,"00781120001 ")
 ;;558
 ;;21,"00781120010 ")
 ;;559
 ;;21,"00781151101 ")
 ;;560
 ;;21,"00781151405 ")
 ;;1389
 ;;21,"00781151431 ")
 ;;1390
 ;;21,"00781151460 ")
 ;;1391
 ;;21,"00781161001 ")
 ;;701
 ;;21,"00781180201 ")
 ;;733
 ;;21,"00781311963 ")
 ;;922
 ;;21,"00781311964 ")
 ;;923
 ;;21,"00781311966 ")
 ;;924
 ;;21,"00781311968 ")
 ;;925
 ;;21,"00781311969 ")
 ;;926
 ;;21,"00781312168 ")
 ;;928
 ;;21,"00781312169 ")
 ;;927
 ;;21,"00814090614 ")
 ;;561
 ;;21,"00814090622 ")
 ;;562
 ;;21,"00814090630 ")
 ;;563
 ;;21,"00814091420 ")
 ;;564
 ;;21,"00814091430 ")
 ;;565
 ;;21,"00814091814 ")
 ;;566
 ;;21,"00814091820 ")
 ;;567
 ;;21,"00814091830 ")
 ;;568
 ;;21,"00814091920 ")
 ;;713
 ;;21,"00814091930 ")
 ;;714
 ;;21,"00814365046 ")
 ;;1122
 ;;21,"00814365540 ")
 ;;1336
 ;;21,"00814852114 ")
 ;;1929
 ;;21,"00814852214 ")
 ;;2401
 ;;21,"00814852230 ")
 ;;2402
 ;;21,"00820013401 ")
 ;;569
 ;;21,"00820013410 ")
 ;;570
 ;;21,"00820013701 ")
 ;;571
 ;;21,"00820013710 ")
 ;;572
 ;;21,"00820014301 ")
 ;;573
 ;;21,"00820014304 ")
 ;;574
 ;;21,"00820014310 ")
 ;;575
 ;;21,"00832062500 ")
 ;;1930
 ;;21,"00832062600 ")
 ;;1931
 ;;21,"00832062700 ")
 ;;2403
 ;;21,"00832062710 ")
 ;;2404
 ;;21,"00832062725 ")
 ;;2405
 ;;21,"00839505806 ")
 ;;576
 ;;21,"00839505809 ")
 ;;577
 ;;21,"00839505816 ")
 ;;578
 ;;21,"00839517530 ")
 ;;1337
 ;;21,"00839552992 ")
 ;;2
 ;;21,"00839553092 ")
 ;;37
 ;;21,"00839553192 ")
 ;;58
 ;;21,"00839553292 ")
 ;;690
 ;;21,"00839558930 ")
 ;;1199
 ;;21,"00839559025 ")
 ;;1226
 ;;21,"00839662606 ")
 ;;1796
 ;;21,"00839662706 ")
 ;;1932
 ;;21,"00839662806 ")
 ;;2406
 ;;21,"00839662906 ")
 ;;2532
 ;;21,"00839663006 ")
 ;;1611
 ;;21,"00839675906 ")
 ;;579
 ;;21,"00839675912 ")
 ;;580
 ;;21,"00839676406 ")
 ;;581
 ;;21,"00839676409 ")
 ;;582
 ;;21,"00839676414 ")
 ;;583
 ;;21,"00839726906 ")
 ;;734
 ;;21,"00839740406 ")
 ;;885
 ;;21,"00839746802 ")
 ;;799
 ;;21,"00839760106 ")
 ;;584
 ;;21,"00839760116 ")
 ;;585
 ;;21,"00839806712 ")
 ;;586
 ;;21,"00869151310 ")
 ;;587
 ;;21,"00869152410 ")
 ;;588
 ;;21,"00869153009 ")
 ;;589
 ;;21,"00869153109 ")
 ;;590
 ;;21,"00869153209 ")
 ;;591
 ;;21,"00869154410 ")
 ;;592
 ;;21,"00869155410 ")
 ;;593
 ;;21,"00869155510 ")
 ;;594
 ;;21,"00869155529 ")
 ;;595
 ;;21,"00869155610 ")
 ;;596
 ;;21,"00869155710 ")
 ;;597
 ;;21,"00869155910 ")
 ;;598
 ;;21,"00879009301 ")
 ;;599
 ;;21,"00879027662 ")
 ;;3
 ;;21,"00879027762 ")
 ;;629
 ;;21,"00879027862 ")
 ;;721
 ;;21,"00879048301 ")
 ;;206
 ;;21,"00879048310 ")
 ;;207
 ;;21,"00894556001 ")
 ;;1797
 ;;21,"00894556002 ")
 ;;1798
 ;;21,"00894556501 ")
 ;;1933
 ;;21,"00894556502 ")
 ;;1934
 ;;21,"00894557001 ")
 ;;2407
 ;;21,"00894557002 ")
 ;;2408
 ;;21,"00894557601 ")
 ;;2533
 ;;21,"00894557602 ")
 ;;2534
 ;;21,"00894558001 ")
 ;;1612
 ;;21,"00894558002 ")
 ;;1613
 ;;21,"00904015312 ")
 ;;23
 ;;21,"00904015412 ")
 ;;59
 ;;21,"00904015512 ")
 ;;691
 ;;21,"00904034759 ")
 ;;664
 ;;21,"00904058260 ")
 ;;886
 ;;21,"00904058560 ")
 ;;735
 ;;21,"00904200940 ")
 ;;600
 ;;21,"00904200951 ")
 ;;601
 ;;21,"00904200960 ")
 ;;602
 ;;21,"00904200970 ")
 ;;603
 ;;21,"00904200980 ")
 ;;604
 ;;21,"00904201060 ")
 ;;605
 ;;21,"00904201080 ")
 ;;606
 ;;21,"00904201159 ")
 ;;208
 ;;21,"00904201360 ")
 ;;209
 ;;21,"00904201380 ")
 ;;210
 ;;21,"00904201559 ")
 ;;607
 ;;21,"00904201560 ")
 ;;608
 ;;21,"00904201959 ")
 ;;609
 ;;21,"00904201960 ")
 ;;610
 ;;21,"00904201961 ")
 ;;611
 ;;21,"00904201970 ")
 ;;612
 ;;21,"00904201980 ")
 ;;613
 ;;21,"00904256060 ")
 ;;1799
 ;;21,"00904256160 ")
 ;;1935
 ;;21,"00904256260 ")
 ;;2409
 ;;21,"00904256270 ")
 ;;2410
 ;;21,"00904256360 ")
 ;;1614
 ;;21,"00904256460 ")
 ;;2535
 ;;21,"00904404061 ")
 ;;800
 ;;21,"00904404073 ")
 ;;801
 ;;21,"00904521260 ")
 ;;880
 ;;21,"00904537840 ")
 ;;1392
 ;;21,"00904537846 ")
 ;;1393
 ;;21,"00904537852 ")
 ;;1394
 ;;21,"00904537860 ")
 ;;1395
 ;;21,"00904537861 ")
 ;;1396
 ;;21,"04116709325 ")
 ;;614
 ;;21,"04850100500 ")
 ;;615
 ;;21,"04850103000 ")
 ;;211
 ;;21,"04850109500 ")
 ;;616
 ;;21,"04850118000 ")
 ;;617
 ;;21,"04850125600 ")
 ;;618
 ;;21,"05307610230 ")
 ;;30
 ;;21,"06030008521 ")
 ;;619
 ;;21,"08290037003 ")
 ;;1021
 ;;21,"08290038003 ")
 ;;1022
 ;;21,"08290038005 ")
 ;;1023
 ;;21,"08290039003 ")
 ;;1024
 ;;21,"08290039005 ")
 ;;1025
 ;;21,"08290039105 ")
 ;;1026
 ;;21,"10009008401 ")
 ;;2156
 ;;21,"10135012601 ")
 ;;60
 ;;21,"10135012603 ")
 ;;61
 ;;21,"10135012610 ")
 ;;62
 ;;21,"10135012613 ")
 ;;63
 ;;21,"10135012620 ")
 ;;64
 ;;21,"10135012630 ")
 ;;65
 ;;21,"10135012650 ")
 ;;66
 ;;21,"10135012657 ")
 ;;67
 ;;21,"10135012660 ")
 ;;68
 ;;21,"10135012669 ")
 ;;69
 ;;21,"10135012675 ")
 ;;70
 ;;21,"10135012690 ")
 ;;71
 ;;21,"10135012699 ")
 ;;72
 ;;21,"10135015001 ")
 ;;213
 ;;21,"10135015005 ")
 ;;214
 ;;21,"10135015010 ")
 ;;215
 ;;21,"10135015013 ")
 ;;216
 ;;21,"10135015020 ")
 ;;217
 ;;21,"10135015024 ")
 ;;218
 ;;21,"10135015030 ")
 ;;219
 ;;21,"10135015057 ")
 ;;220
 ;;21,"10135015069 ")
 ;;221
 ;;21,"10135017201 ")
 ;;692
 ;;21,"10135017301 ")
 ;;802
 ;;21,"10135017303 ")
 ;;803
 ;;21,"10135017305 ")
 ;;804
 ;;21,"10135017310 ")
 ;;805
 ;;21,"10135017336 ")
 ;;806
 ;;21,"10135017353 ")
 ;;807
 ;;21,"10135017362 ")
 ;;808
 ;;21,"10135017369 ")
 ;;809
 ;;21,"10135017401 ")
 ;;871
 ;;21,"10135017410 ")
 ;;872
 ;;21,"10135024036 ")
 ;;736
 ;;21,"10244051063 ")
 ;;634
 ;;21,"10244051163 ")
 ;;222
 ;;21,"10244056210 ")
 ;;223
 ;;21,"10244056211 ")
 ;;224
 ;;21,"10244056261 ")
 ;;225
 ;;21,"10244056262 ")
 ;;226
 ;;21,"10244056263 ")
 ;;227
 ;;21,"10244056265 ")
 ;;228
 ;;21,"10244056663 ")
 ;;229
 ;;21,"10244056665 ")
 ;;230
 ;;21,"10244057163 ")
 ;;635
 ;;21,"10244057263 ")
 ;;636
 ;;21,"10244057663 ")
 ;;637
 ;;21,"10244057665 ")
 ;;638
 ;;21,"10244058263 ")
 ;;231
 ;;21,"10244059768 ")
 ;;232
 ;;21,"10544011830 ")
 ;;1936
 ;;21,"10647017202 ")
 ;;2157
 ;;21,"10719004542 ")
 ;;1027
 ;;21,"10719004642 ")
 ;;1276
 ;;21,"10719004731 ")
 ;;1123
 ;;21,"10719004742 ")
 ;;1124
 ;;21,"10916001501 ")
 ;;73
 ;;21,"10916021301 ")
 ;;233
 ;;21,"10916021436 ")
 ;;737
 ;;21,"10974000204 ")
 ;;1241
 ;;21,"10974000205 ")
 ;;1242
 ;;21,"10974000405 ")
 ;;1203
 ;;21,"10974000805 ")
 ;;1125
 ;;21,"10974001010 ")
 ;;1277
 ;;21,"10974030605 ")
 ;;1028
 ;;21,"11383005023 ")
 ;;234
 ;;21,"11383007225 ")
 ;;705
 ;;21,"11383014825 ")
 ;;235
 ;;21,"11722092199 ")
 ;;1397
 ;;21,"11722092299 ")
 ;;1615
 ;;21,"11722092399 ")
 ;;1800
 ;;21,"11722092499 ")
 ;;1937
 ;;21,"11722092599 ")
 ;;2015
 ;;21,"11722092699 ")
 ;;2158
 ;;21,"11722092799 ")
 ;;2411
