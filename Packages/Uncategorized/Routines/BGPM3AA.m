BGPM3AA ;IHS/MSC/SAT-CREATED BY ^ATXSTX ON APR 21, 2011;
 ;;11.0;IHS CLINICAL REPORTING;**4**;JAN 06, 2011;Build 84
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;21,"00418435141 ")
 ;;260
 ;;21,"00418446105 ")
 ;;335
 ;;21,"00436022070 ")
 ;;172
 ;;21,"00436022145 ")
 ;;261
 ;;21,"00436022170 ")
 ;;262
 ;;21,"00436022175 ")
 ;;263
 ;;21,"00436027570 ")
 ;;402
 ;;21,"00454091330 ")
 ;;173
 ;;21,"00456077826 ")
 ;;174
 ;;21,"00463103410 ")
 ;;175
 ;;21,"00467103325 ")
 ;;176
 ;;21,"00467126225 ")
 ;;403
 ;;21,"00467203325 ")
 ;;177
 ;;21,"00467215225 ")
 ;;264
 ;;21,"00467750710 ")
 ;;178
 ;;21,"00469003115 ")
 ;;92
 ;;21,"00469003325 ")
 ;;179
 ;;21,"00469015225 ")
 ;;265
 ;;21,"00469081330 ")
 ;;180
 ;;21,"00469081350 ")
 ;;181
 ;;21,"00469083300 ")
 ;;266
 ;;21,"00469083303 ")
 ;;267
 ;;21,"00469083370 ")
 ;;268
 ;;21,"00469091300 ")
 ;;182
 ;;21,"00469091330 ")
 ;;183
 ;;21,"00469091350 ")
 ;;184
 ;;21,"00469092300 ")
 ;;404
 ;;21,"00469092303 ")
 ;;405
 ;;21,"00469092330 ")
 ;;406
 ;;21,"00469093300 ")
 ;;269
 ;;21,"00469093320 ")
 ;;270
 ;;21,"00469103315 ")
 ;;185
 ;;21,"00469115515 ")
 ;;306
 ;;21,"00469126215 ")
 ;;407
 ;;21,"00469190300 ")
 ;;93
 ;;21,"00469190303 ")
 ;;94
 ;;21,"00469203325 ")
 ;;186
 ;;21,"00469215215 ")
 ;;271
 ;;21,"00469276010 ")
 ;;187
 ;;21,"00469276025 ")
 ;;188
 ;;21,"00469310115 ")
 ;;95
 ;;21,"00469310525 ")
 ;;96
 ;;21,"00469504001 ")
 ;;189
 ;;21,"00469504501 ")
 ;;97
 ;;21,"00469504505 ")
 ;;98
 ;;21,"00517430101 ")
 ;;190
 ;;21,"00527013955 ")
 ;;191
 ;;21,"00527014855 ")
 ;;408
 ;;21,"00527017455 ")
 ;;272
 ;;21,"00527100301 ")
 ;;1409
 ;;21,"00527100310 ")
 ;;1410
 ;;21,"00527106401 ")
 ;;808
 ;;21,"00527106410 ")
 ;;809
 ;;21,"00527107201 ")
 ;;940
 ;;21,"00527107210 ")
 ;;941
 ;;21,"00536486070 ")
 ;;192
 ;;21,"00536486075 ")
 ;;193
 ;;21,"00536490070 ")
 ;;409
 ;;21,"00536492570 ")
 ;;273
 ;;21,"00536495065 ")
 ;;307
 ;;21,"00555083102 ")
 ;;561
 ;;21,"00555083104 ")
 ;;562
 ;;21,"00555083105 ")
 ;;563
 ;;21,"00555083156 ")
 ;;564
 ;;21,"00555083202 ")
 ;;942
 ;;21,"00555083204 ")
 ;;943
 ;;21,"00555083205 ")
 ;;944
 ;;21,"00555083256 ")
 ;;945
 ;;21,"00555083302 ")
 ;;1411
 ;;21,"00555083304 ")
 ;;1412
 ;;21,"00555083305 ")
 ;;1413
 ;;21,"00555083356 ")
 ;;1414
 ;;21,"00555083402 ")
 ;;1552
 ;;21,"00555083405 ")
 ;;1553
 ;;21,"00555083456 ")
 ;;1554
 ;;21,"00555083502 ")
 ;;628
 ;;21,"00555083504 ")
 ;;629
 ;;21,"00555083556 ")
 ;;630
 ;;21,"00555086902 ")
 ;;810
 ;;21,"00555086904 ")
 ;;811
 ;;21,"00555086905 ")
 ;;812
 ;;21,"00555086956 ")
 ;;813
 ;;21,"00555087402 ")
 ;;1174
 ;;21,"00555087405 ")
 ;;1175
 ;;21,"00555087456 ")
 ;;1176
 ;;21,"00555092501 ")
 ;;1030
 ;;21,"00555092502 ")
 ;;1031
 ;;21,"00555092504 ")
 ;;1032
 ;;21,"00555092505 ")
 ;;1033
 ;;21,"00555092556 ")
 ;;1034
 ;;21,"00555092601 ")
 ;;1479
 ;;21,"00555092602 ")
 ;;1480
 ;;21,"00555092604 ")
 ;;1481
 ;;21,"00555092605 ")
 ;;1482
 ;;21,"00555092656 ")
 ;;1483
 ;;21,"00615150905 ")
 ;;814
 ;;21,"00615150929 ")
 ;;815
 ;;21,"00615150939 ")
 ;;816
 ;;21,"00615151005 ")
 ;;946
 ;;21,"00615151029 ")
 ;;947
 ;;21,"00615151039 ")
 ;;948
 ;;21,"00615151205 ")
 ;;1415
 ;;21,"00615151229 ")
 ;;1416
 ;;21,"00615151239 ")
 ;;1417
 ;;21,"00615454729 ")
 ;;565
 ;;21,"00615454739 ")
 ;;566
 ;;21,"00615454805 ")
 ;;1035
 ;;21,"00615454829 ")
 ;;1036
 ;;21,"00615454831 ")
 ;;1037
 ;;21,"00615454839 ")
 ;;1038
 ;;21,"00615454905 ")
 ;;1177
 ;;21,"00615454929 ")
 ;;1178
 ;;21,"00615454939 ")
 ;;1179
 ;;21,"00615455029 ")
 ;;1484
 ;;21,"00615455031 ")
 ;;1485
 ;;21,"00615455039 ")
 ;;1486
 ;;21,"00615455129 ")
 ;;1555
 ;;21,"00641039102 ")
 ;;194
 ;;21,"00641039125 ")
 ;;195
 ;;21,"00641039164 ")
 ;;196
 ;;21,"00641040002 ")
 ;;410
 ;;21,"00641040021 ")
 ;;411
 ;;21,"00641040025 ")
 ;;412
 ;;21,"00641040064 ")
 ;;413
 ;;21,"00641040125 ")
 ;;99
 ;;21,"00641041002 ")
 ;;274
 ;;21,"00641041025 ")
 ;;275
 ;;21,"00641041064 ")
 ;;276
 ;;21,"00641244041 ")
 ;;197
 ;;21,"00641244045 ")
 ;;198
 ;;21,"00641245041 ")
 ;;199
 ;;21,"00641245045 ")
 ;;200
 ;;21,"00641246041 ")
 ;;414
 ;;21,"00641246045 ")
 ;;415
 ;;21,"00641247041 ")
 ;;277
 ;;21,"00641247045 ")
 ;;278
 ;;21,"00641326209 ")
 ;;100
 ;;21,"00641326403 ")
 ;;201
 ;;21,"00641326503 ")
 ;;202
 ;;21,"00641326603 ")
 ;;279
 ;;21,"00641326703 ")
 ;;416
 ;;21,"00641326803 ")
 ;;280
 ;;21,"00677027521 ")
 ;;417
 ;;21,"00677079301 ")
 ;;949
 ;;21,"00677079401 ")
 ;;1418
 ;;21,"00686001101 ")
 ;;101
 ;;21,"00686001105 ")
 ;;102
 ;;21,"00686001910 ")
 ;;203
 ;;21,"00719199310 ")
 ;;1419
 ;;21,"00719199313 ")
 ;;1420
 ;;21,"00725004401 ")
 ;;950
 ;;21,"00725004410 ")
 ;;951
 ;;21,"00725004501 ")
 ;;1421
 ;;21,"00725004510 ")
 ;;1422
 ;;21,"00725004601 ")
 ;;631
 ;;21,"00725004610 ")
 ;;632
 ;;21,"00725004701 ")
 ;;633
 ;;21,"00725004710 ")
 ;;634
 ;;21,"00725005001 ")
 ;;817
 ;;21,"00725005010 ")
 ;;818
 ;;21,"00781035207 ")
 ;;567
 ;;21,"00781035208 ")
 ;;568
 ;;21,"00781036306 ")
 ;;819
 ;;21,"00781036307 ")
 ;;820
 ;;21,"00781036308 ")
 ;;821
 ;;21,"00781036406 ")
 ;;952
 ;;21,"00781036407 ")
 ;;953
 ;;21,"00781036408 ")
 ;;954
 ;;21,"00781036607 ")
 ;;1039
 ;;21,"00781036608 ")
 ;;1040
 ;;21,"00781036907 ")
 ;;1180
 ;;21,"00781036908 ")
 ;;1181
 ;;21,"00781037704 ")
 ;;1423
 ;;21,"00781037706 ")
 ;;1424
 ;;21,"00781037707 ")
 ;;1425
 ;;21,"00781037708 ")
 ;;1426
 ;;21,"00781038107 ")
 ;;1487
 ;;21,"00781038108 ")
 ;;1488
 ;;21,"00781038607 ")
 ;;1556
 ;;21,"00781038608 ")
 ;;1557
 ;;21,"00781038707 ")
 ;;635
 ;;21,"00781038708 ")
 ;;636
 ;;21,"00781311963 ")
 ;;3
 ;;21,"00781311964 ")
 ;;4
 ;;21,"00781311966 ")
 ;;5
 ;;21,"00781311968 ")
 ;;6
 ;;21,"00781311969 ")
 ;;7
 ;;21,"00781312168 ")
 ;;9
 ;;21,"00781312169 ")
 ;;8
 ;;21,"00814365046 ")
 ;;204
 ;;21,"00814365540 ")
 ;;418
 ;;21,"00814852114 ")
 ;;955
 ;;21,"00814852214 ")
 ;;1427
 ;;21,"00814852230 ")
 ;;1428
 ;;21,"00832062500 ")
 ;;956
 ;;21,"00832062600 ")
 ;;957
 ;;21,"00832062700 ")
 ;;1429
 ;;21,"00832062710 ")
 ;;1430
 ;;21,"00832062725 ")
 ;;1431
 ;;21,"00839517530 ")
 ;;419
 ;;21,"00839558930 ")
 ;;281
 ;;21,"00839559025 ")
 ;;308
 ;;21,"00839662606 ")
 ;;822
 ;;21,"00839662706 ")
 ;;958
 ;;21,"00839662806 ")
 ;;1432
 ;;21,"00839662906 ")
 ;;1558
 ;;21,"00839663006 ")
 ;;637
 ;;21,"00894556001 ")
 ;;823
 ;;21,"00894556002 ")
 ;;824
 ;;21,"00894556501 ")
 ;;959
 ;;21,"00894556502 ")
 ;;960
 ;;21,"00894557001 ")
 ;;1433
 ;;21,"00894557002 ")
 ;;1434
 ;;21,"00894557601 ")
 ;;1559
 ;;21,"00894557602 ")
 ;;1560
 ;;21,"00894558001 ")
 ;;638
 ;;21,"00894558002 ")
 ;;639
 ;;21,"00904256060 ")
 ;;825
 ;;21,"00904256160 ")
 ;;961
 ;;21,"00904256260 ")
 ;;1435
 ;;21,"00904256270 ")
 ;;1436
 ;;21,"00904256360 ")
 ;;640
 ;;21,"00904256460 ")
 ;;1561
 ;;21,"08290037003 ")
 ;;103
 ;;21,"08290038003 ")
 ;;104
 ;;21,"08290038005 ")
 ;;105
 ;;21,"08290039003 ")
 ;;106
 ;;21,"08290039005 ")
 ;;107
 ;;21,"08290039105 ")
 ;;108
 ;;21,"10009008401 ")
 ;;1182
 ;;21,"10544011830 ")
 ;;962
 ;;21,"10647017202 ")
 ;;1183
 ;;21,"10719004542 ")
 ;;109
 ;;21,"10719004642 ")
 ;;358
 ;;21,"10719004731 ")
 ;;205
 ;;21,"10719004742 ")
 ;;206
 ;;21,"10974000204 ")
 ;;323
 ;;21,"10974000205 ")
 ;;324
 ;;21,"10974000405 ")
 ;;285
 ;;21,"10974000805 ")
 ;;207
 ;;21,"10974001010 ")
 ;;359
 ;;21,"10974030605 ")
 ;;110
 ;;21,"11722092199 ")
 ;;423
 ;;21,"11722092299 ")
 ;;641
 ;;21,"11722092399 ")
 ;;826
 ;;21,"11722092499 ")
 ;;963
 ;;21,"11722092599 ")
 ;;1041
 ;;21,"11722092699 ")
 ;;1184
 ;;21,"11722092799 ")
 ;;1437
 ;;21,"11722092999 ")
 ;;569
 ;;21,"11743021002 ")
 ;;111
 ;;21,"12671002403 ")
 ;;112
 ;;21,"12671002407 ")
 ;;113
 ;;21,"12671002503 ")
 ;;360
 ;;21,"12671002601 ")
 ;;208
 ;;21,"12671002603 ")
 ;;209
 ;;21,"12671006303 ")
 ;;114
 ;;21,"12671006307 ")
 ;;115
 ;;21,"12671006403 ")
 ;;361
 ;;21,"12671006501 ")
 ;;210
 ;;21,"12671006515 ")
 ;;211
 ;;21,"13411033903 ")
 ;;1185
 ;;21,"15330010001 ")
 ;;424
 ;;21,"15330010010 ")
 ;;425
 ;;21,"15330010101 ")
 ;;642
 ;;21,"15330010110 ")
 ;;643
 ;;21,"15330010201 ")
 ;;827
 ;;21,"15330010210 ")
 ;;828
 ;;21,"15330010601 ")
 ;;1438
 ;;21,"15330010701 ")
 ;;1489
 ;;21,"15330010801 ")
 ;;570
 ;;21,"15330026601 ")
 ;;964
 ;;21,"15330026701 ")
 ;;1042
 ;;21,"15330026801 ")
 ;;1186
 ;;21,"15330026810 ")
 ;;1187
 ;;21,"17856402902 ")
 ;;829
 ;;21,"19458044301 ")
 ;;830
 ;;21,"19458047101 ")
 ;;1188
 ;;21,"19458059801 ")
 ;;644
 ;;21,"21695067330 ")
 ;;645
 ;;21,"21695067730 ")
 ;;1189
 ;;21,"23490083305 ")
 ;;1190
 ;;21,"23490141903 ")
 ;;646
 ;;21,"23490141909 ")
 ;;647
 ;;21,"23490142003 ")
 ;;831
 ;;21,"23490142009 ")
 ;;832
 ;;21,"23490142203 ")
 ;;1191
 ;;21,"23490142206 ")
 ;;1192
 ;;21,"23490142209 ")
 ;;1193
 ;;21,"23490647801 ")
 ;;426
 ;;21,"23490647802 ")
 ;;427
 ;;21,"23490647803 ")
 ;;428
 ;;21,"23490648001 ")
 ;;648
 ;;21,"23490648002 ")
 ;;649
 ;;21,"23490648003 ")
 ;;650
 ;;21,"23490648101 ")
 ;;833
 ;;21,"23490648102 ")
 ;;834
 ;;21,"23490648103 ")
 ;;835
 ;;21,"23490648201 ")
 ;;1043
 ;;21,"23490648202 ")
 ;;1044
 ;;21,"23490648203 ")
 ;;1045
 ;;21,"23490648301 ")
 ;;1194
 ;;21,"23490648302 ")
 ;;1195
 ;;21,"23490648303 ")
 ;;1196
 ;;21,"23490648401 ")
 ;;1490
 ;;21,"23490648402 ")
 ;;1491
 ;;21,"23490648403 ")
 ;;1492
 ;;21,"24236022802 ")
 ;;1197
 ;;21,"24236026002 ")
 ;;429
 ;;21,"24236032102 ")
 ;;651
 ;;21,"24236034802 ")
 ;;1198
 ;;21,"24236035302 ")
 ;;652
 ;;21,"24236056402 ")
 ;;430
 ;;21,"24236069302 ")
 ;;836
 ;;21,"24236084702 ")
 ;;965
 ;;21,"24236087202 ")
 ;;1046
 ;;21,"25021040001 ")
 ;;116
 ;;21,"25021040010 ")
 ;;117
 ;;21,"25021040030 ")
 ;;118
 ;;21,"25021040102 ")
 ;;119
 ;;21,"25021040201 ")
 ;;362
 ;;21,"25021040210 ")
 ;;363
 ;;21,"25021040301 ")
 ;;212
 ;;21,"25021040304 ")
 ;;213
 ;;21,"25021040401 ")
 ;;286
 ;;21,"35470053501 ")
 ;;1199
 ;;21,"39769001101 ")
 ;;19
 ;;21,"39769001102 ")
 ;;20
 ;;21,"39769001105 ")
 ;;21
 ;;21,"39769001171 ")
 ;;22
 ;;21,"39769001173 ")
 ;;23
 ;;21,"39769001175 ")
 ;;24
 ;;21,"39769001910 ")
 ;;120
 ;;21,"39769001930 ")
 ;;121
 ;;21,"39769002601 ")
 ;;122
 ;;21,"39769002701 ")
 ;;364
 ;;21,"39769002801 ")
 ;;214
 ;;21,"39769002905 ")
 ;;215
 ;;21,"39769003005 ")
 ;;287
 ;;21,"39769003610 ")
 ;;25
 ;;21,"39769003630 ")
 ;;26
 ;;21,"39769003671 ")
 ;;27
 ;;21,"39769010771 ")
 ;;28
 ;;21,"39769010773 ")
 ;;29
 ;;21,"39769010775 ")
 ;;30
 ;;21,"39769011871 ")
 ;;31
 ;;21,"39769011873 ")
 ;;32
 ;;21,"39769011875 ")
 ;;33
 ;;21,"43683011730 ")
 ;;1200
 ;;21,"43683011830 ")
 ;;966
 ;;21,"49072029130 ")
 ;;123
 ;;21,"49072029710 ")
 ;;365
 ;;21,"49910000606 ")
 ;;366
 ;;21,"49999057600 ")
 ;;1201
 ;;21,"49999057610 ")
 ;;1202
 ;;21,"49999057620 ")
 ;;1203
 ;;21,"49999082900 ")
 ;;1493
 ;;21,"49999092310 ")
 ;;1047
 ;;21,"50111092201 ")
 ;;653
 ;;21,"50111092203 ")
 ;;654
