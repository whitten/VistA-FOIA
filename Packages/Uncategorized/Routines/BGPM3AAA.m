BGPM3AAA ;IHS/MSC/SAT-CREATED BY ^ATXSTX ON APR 21, 2011;
 ;;11.0;IHS CLINICAL REPORTING;**4**;JAN 06, 2011;Build 84
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;21,"50111092210 ")
 ;;655
 ;;21,"50111092258 ")
 ;;656
 ;;21,"50111092401 ")
 ;;967
 ;;21,"50111092403 ")
 ;;968
 ;;21,"50111092410 ")
 ;;969
 ;;21,"50111092458 ")
 ;;970
 ;;21,"50111092501 ")
 ;;1048
 ;;21,"50111092503 ")
 ;;1049
 ;;21,"50111092510 ")
 ;;1050
 ;;21,"50111092558 ")
 ;;1051
 ;;21,"50111092601 ")
 ;;1204
 ;;21,"50111092603 ")
 ;;1205
 ;;21,"50111092610 ")
 ;;1206
 ;;21,"50111092658 ")
 ;;1207
 ;;21,"50111092701 ")
 ;;1439
 ;;21,"50111092703 ")
 ;;1440
 ;;21,"50111092710 ")
 ;;1441
 ;;21,"50111092758 ")
 ;;1442
 ;;21,"50111092801 ")
 ;;1494
 ;;21,"50111092803 ")
 ;;1495
 ;;21,"50111092810 ")
 ;;1496
 ;;21,"50111092858 ")
 ;;1497
 ;;21,"50111092901 ")
 ;;571
 ;;21,"50111092903 ")
 ;;572
 ;;21,"50111092910 ")
 ;;573
 ;;21,"50111092958 ")
 ;;574
 ;;21,"51079090820 ")
 ;;431
 ;;21,"51079090920 ")
 ;;657
 ;;21,"51079091020 ")
 ;;837
 ;;21,"51079091320 ")
 ;;1208
 ;;21,"51079091520 ")
 ;;1498
 ;;21,"51129181201 ")
 ;;1052
 ;;21,"51129201801 ")
 ;;1209
 ;;21,"51129247001 ")
 ;;1210
 ;;21,"51129254801 ")
 ;;658
 ;;21,"51129257101 ")
 ;;432
 ;;21,"51129267801 ")
 ;;838
 ;;21,"51129276101 ")
 ;;659
 ;;21,"51129282501 ")
 ;;433
 ;;21,"51129282502 ")
 ;;434
 ;;21,"51129284001 ")
 ;;839
 ;;21,"51129285101 ")
 ;;840
 ;;21,"51129285102 ")
 ;;841
 ;;21,"51129292001 ")
 ;;575
 ;;21,"51129294501 ")
 ;;971
 ;;21,"51129294502 ")
 ;;972
 ;;21,"51129294601 ")
 ;;1053
 ;;21,"51129294602 ")
 ;;1054
 ;;21,"51129302601 ")
 ;;660
 ;;21,"51129302602 ")
 ;;661
 ;;21,"51129323301 ")
 ;;1211
 ;;21,"51129323302 ")
 ;;1212
 ;;21,"51129324801 ")
 ;;1499
 ;;21,"51129324802 ")
 ;;1500
 ;;21,"51129382601 ")
 ;;662
 ;;21,"51129382602 ")
 ;;663
 ;;21,"51129382701 ")
 ;;1213
 ;;21,"51129382702 ")
 ;;1214
 ;;21,"51129382901 ")
 ;;435
 ;;21,"51129382902 ")
 ;;436
 ;;21,"51129385901 ")
 ;;1215
 ;;21,"51129385902 ")
 ;;1216
 ;;21,"51129395401 ")
 ;;1501
 ;;21,"51129395402 ")
 ;;1502
 ;;21,"51129396801 ")
 ;;842
 ;;21,"51129396802 ")
 ;;843
 ;;21,"51129396901 ")
 ;;576
 ;;21,"51129396902 ")
 ;;577
 ;;21,"51432090003 ")
 ;;844
 ;;21,"51432090103 ")
 ;;845
 ;;21,"51432090203 ")
 ;;1217
 ;;21,"51432090303 ")
 ;;1503
 ;;21,"51432090403 ")
 ;;578
 ;;21,"51655025124 ")
 ;;664
 ;;21,"51655025152 ")
 ;;665
 ;;21,"51655025153 ")
 ;;666
 ;;21,"51655078324 ")
 ;;1218
 ;;21,"51655078352 ")
 ;;1219
 ;;21,"51655078353 ")
 ;;1220
 ;;21,"51655078399 ")
 ;;1221
 ;;21,"51672402700 ")
 ;;437
 ;;21,"51672402701 ")
 ;;438
 ;;21,"51672402703 ")
 ;;439
 ;;21,"51672402707 ")
 ;;440
 ;;21,"51672402800 ")
 ;;667
 ;;21,"51672402801 ")
 ;;668
 ;;21,"51672402803 ")
 ;;669
 ;;21,"51672402807 ")
 ;;670
 ;;21,"51672402900 ")
 ;;846
 ;;21,"51672402901 ")
 ;;847
 ;;21,"51672402903 ")
 ;;848
 ;;21,"51672402907 ")
 ;;849
 ;;21,"51672403000 ")
 ;;973
 ;;21,"51672403001 ")
 ;;974
 ;;21,"51672403003 ")
 ;;975
 ;;21,"51672403007 ")
 ;;976
 ;;21,"51672403100 ")
 ;;1055
 ;;21,"51672403101 ")
 ;;1056
 ;;21,"51672403103 ")
 ;;1057
 ;;21,"51672403107 ")
 ;;1058
 ;;21,"51672403200 ")
 ;;1222
 ;;21,"51672403201 ")
 ;;1223
 ;;21,"51672403203 ")
 ;;1224
 ;;21,"51672403207 ")
 ;;1225
 ;;21,"51672403300 ")
 ;;1443
 ;;21,"51672403301 ")
 ;;1444
 ;;21,"51672403303 ")
 ;;1445
 ;;21,"51672403307 ")
 ;;1446
 ;;21,"51672403400 ")
 ;;1504
 ;;21,"51672403401 ")
 ;;1505
 ;;21,"51672403403 ")
 ;;1506
 ;;21,"51672403500 ")
 ;;579
 ;;21,"51672403501 ")
 ;;580
 ;;21,"51672403503 ")
 ;;581
 ;;21,"51698004910 ")
 ;;216
 ;;21,"51728054501 ")
 ;;1226
 ;;21,"51728054510 ")
 ;;1227
 ;;21,"51813003730 ")
 ;;850
 ;;21,"51813003799 ")
 ;;851
 ;;21,"51813003830 ")
 ;;1228
 ;;21,"51813003899 ")
 ;;1229
 ;;21,"51813027830 ")
 ;;582
 ;;21,"51813027899 ")
 ;;583
 ;;21,"52549402701 ")
 ;;441
 ;;21,"52549402703 ")
 ;;442
 ;;21,"52549402707 ")
 ;;443
 ;;21,"52549402801 ")
 ;;671
 ;;21,"52549402803 ")
 ;;672
 ;;21,"52549402807 ")
 ;;673
 ;;21,"52549402901 ")
 ;;852
 ;;21,"52549402903 ")
 ;;853
 ;;21,"52549402907 ")
 ;;854
 ;;21,"52549403001 ")
 ;;977
 ;;21,"52549403003 ")
 ;;978
 ;;21,"52549403007 ")
 ;;979
 ;;21,"52549403101 ")
 ;;1059
 ;;21,"52549403103 ")
 ;;1060
 ;;21,"52549403107 ")
 ;;1061
 ;;21,"52549403201 ")
 ;;1230
 ;;21,"52549403203 ")
 ;;1231
 ;;21,"52549403207 ")
 ;;1232
 ;;21,"52549403301 ")
 ;;1447
 ;;21,"52549403303 ")
 ;;1448
 ;;21,"52549403401 ")
 ;;1507
 ;;21,"52549403403 ")
 ;;1508
 ;;21,"52549403501 ")
 ;;584
 ;;21,"52549403503 ")
 ;;585
 ;;21,"52584004401 ")
 ;;855
 ;;21,"52584004410 ")
 ;;856
 ;;21,"52584004501 ")
 ;;1233
 ;;21,"52584004510 ")
 ;;1234
 ;;21,"52584005001 ")
 ;;674
 ;;21,"52584005010 ")
 ;;675
 ;;21,"52584015270 ")
 ;;34
 ;;21,"52584026201 ")
 ;;367
 ;;21,"52584054001 ")
 ;;124
 ;;21,"52584054201 ")
 ;;217
 ;;21,"52584058102 ")
 ;;282
 ;;21,"52953003801 ")
 ;;857
 ;;21,"52953003901 ")
 ;;1235
 ;;21,"52958032004 ")
 ;;35
 ;;21,"52958033008 ")
 ;;125
 ;;21,"52958033012 ")
 ;;126
 ;;21,"52958034012 ")
 ;;368
 ;;21,"52959092430 ")
 ;;444
 ;;21,"52959092530 ")
 ;;676
 ;;21,"52959092630 ")
 ;;1236
 ;;21,"52985007101 ")
 ;;1237
 ;;21,"53467017200 ")
 ;;1238
 ;;21,"53467064000 ")
 ;;1239
 ;;21,"53467064003 ")
 ;;1240
 ;;21,"53792014004 ")
 ;;325
 ;;21,"53808033401 ")
 ;;980
 ;;21,"53808033501 ")
 ;;677
 ;;21,"53808097001 ")
 ;;1241
 ;;21,"53808098501 ")
 ;;445
 ;;21,"53808098901 ")
 ;;586
 ;;21,"53808099401 ")
 ;;858
 ;;21,"53808099701 ")
 ;;1509
 ;;21,"53978031403 ")
 ;;1242
 ;;21,"53978205207 ")
 ;;369
 ;;21,"53978330103 ")
 ;;859
 ;;21,"53978330203 ")
 ;;446
 ;;21,"54441032401 ")
 ;;1243
 ;;21,"54441032405 ")
 ;;1244
 ;;21,"54441032410 ")
 ;;1245
 ;;21,"54441032425 ")
 ;;1246
 ;;21,"54441032450 ")
 ;;1247
 ;;21,"54569015850 ")
 ;;678
 ;;21,"54569015950 ")
 ;;1248
 ;;21,"54569493400 ")
 ;;1249
 ;;21,"54569493401 ")
 ;;1250
 ;;21,"54569586800 ")
 ;;860
 ;;21,"54569586900 ")
 ;;1062
 ;;21,"54868428600 ")
 ;;1251
 ;;21,"54868428601 ")
 ;;1252
 ;;21,"54868428602 ")
 ;;1253
 ;;21,"54868428603 ")
 ;;1254
 ;;21,"54868428604 ")
 ;;1255
 ;;21,"54868434900 ")
 ;;447
 ;;21,"54868434901 ")
 ;;448
 ;;21,"54868434902 ")
 ;;449
 ;;21,"54868434903 ")
 ;;450
 ;;21,"54868434904 ")
 ;;451
 ;;21,"54868434905 ")
 ;;452
 ;;21,"54868440000 ")
 ;;861
 ;;21,"54868440001 ")
 ;;862
 ;;21,"54868440002 ")
 ;;863
 ;;21,"54868440003 ")
 ;;864
 ;;21,"54868440004 ")
 ;;865
 ;;21,"54868440200 ")
 ;;1063
 ;;21,"54868440201 ")
 ;;1064
 ;;21,"54868440202 ")
 ;;1065
 ;;21,"54868442200 ")
 ;;679
 ;;21,"54868442201 ")
 ;;680
 ;;21,"54868442202 ")
 ;;681
 ;;21,"54868487100 ")
 ;;981
 ;;21,"54868487101 ")
 ;;982
 ;;21,"54868487102 ")
 ;;983
 ;;21,"54868487300 ")
 ;;1449
 ;;21,"54868487301 ")
 ;;1450
 ;;21,"54868487302 ")
 ;;1451
 ;;21,"54868495000 ")
 ;;1510
 ;;21,"54868495001 ")
 ;;1511
 ;;21,"54868495002 ")
 ;;1512
 ;;21,"54868525800 ")
 ;;587
 ;;21,"55045288108 ")
 ;;1256
 ;;21,"55045290100 ")
 ;;866
 ;;21,"55045290108 ")
 ;;867
 ;;21,"55045290200 ")
 ;;682
 ;;21,"55045290202 ")
 ;;683
 ;;21,"55045290208 ")
 ;;684
 ;;21,"55045290300 ")
 ;;1066
 ;;21,"55045329201 ")
 ;;588
 ;;21,"55154014105 ")
 ;;218
 ;;21,"55154087609 ")
 ;;685
 ;;21,"55154087709 ")
 ;;868
 ;;21,"55154087809 ")
 ;;984
 ;;21,"55154087909 ")
 ;;1067
 ;;21,"55154088009 ")
 ;;1257
 ;;21,"55154088309 ")
 ;;453
 ;;21,"55154088409 ")
 ;;1452
 ;;21,"55154236205 ")
 ;;36
 ;;21,"55154239705 ")
 ;;219
 ;;21,"55154420507 ")
 ;;220
 ;;21,"55154510105 ")
 ;;221
 ;;21,"55154510107 ")
 ;;222
 ;;21,"55154510305 ")
 ;;370
 ;;21,"55154510307 ")
 ;;371
 ;;21,"55154510905 ")
 ;;37
 ;;21,"55154512205 ")
 ;;127
 ;;21,"55154512207 ")
 ;;128
 ;;21,"55154512605 ")
 ;;129
 ;;21,"55154870107 ")
 ;;372
 ;;21,"55154935105 ")
 ;;288
 ;;21,"55154935305 ")
 ;;38
 ;;21,"55154935405 ")
 ;;373
 ;;21,"55154935505 ")
 ;;39
 ;;21,"55154935905 ")
 ;;223
 ;;21,"55154936005 ")
 ;;130
 ;;21,"55154937305 ")
 ;;131
 ;;21,"55154937705 ")
 ;;132
 ;;21,"55289034030 ")
 ;;454
 ;;21,"55289077314 ")
 ;;1258
 ;;21,"55289077330 ")
 ;;1259
 ;;21,"55289077360 ")
 ;;1260
 ;;21,"55289077390 ")
 ;;1261
 ;;21,"55567010000 ")
 ;;455
 ;;21,"55567010001 ")
 ;;456
 ;;21,"55567010010 ")
 ;;457
 ;;21,"55567010100 ")
 ;;686
 ;;21,"55567010101 ")
 ;;687
 ;;21,"55567010110 ")
 ;;688
 ;;21,"55567010118 ")
 ;;689
 ;;21,"55567010135 ")
 ;;690
 ;;21,"55567010200 ")
 ;;869
 ;;21,"55567010201 ")
 ;;870
 ;;21,"55567010210 ")
 ;;871
 ;;21,"55567010218 ")
 ;;872
 ;;21,"55567010235 ")
 ;;873
 ;;21,"55567010600 ")
 ;;1453
 ;;21,"55567010601 ")
 ;;1454
 ;;21,"55567010610 ")
 ;;1455
 ;;21,"55567010618 ")
 ;;1456
 ;;21,"55567010635 ")
 ;;1457
 ;;21,"55567010700 ")
 ;;1513
 ;;21,"55567010701 ")
 ;;1514
 ;;21,"55567010710 ")
 ;;1515
 ;;21,"55567010718 ")
 ;;1516
 ;;21,"55567010735 ")
 ;;1517
 ;;21,"55567010800 ")
 ;;589
 ;;21,"55567010801 ")
 ;;590
 ;;21,"55567010810 ")
 ;;591
 ;;21,"55567010818 ")
 ;;592
 ;;21,"55567010835 ")
 ;;593
 ;;21,"55567026600 ")
 ;;985
 ;;21,"55567026601 ")
 ;;986
 ;;21,"55567026610 ")
 ;;987
 ;;21,"55567026700 ")
 ;;1068
 ;;21,"55567026701 ")
 ;;1069
 ;;21,"55567026710 ")
 ;;1070
 ;;21,"55567026800 ")
 ;;1262
 ;;21,"55567026801 ")
 ;;1263
 ;;21,"55567026810 ")
 ;;1264
 ;;21,"55587057810 ")
 ;;1265
 ;;21,"55587057830 ")
 ;;1266
 ;;21,"55587057845 ")
 ;;1267
 ;;21,"55587057860 ")
 ;;1268
 ;;21,"55587057886 ")
 ;;1269
 ;;21,"55587057890 ")
 ;;1270
 ;;21,"55887004401 ")
 ;;988
 ;;21,"55887004430 ")
 ;;989
 ;;21,"55887004460 ")
 ;;990
 ;;21,"55887004490 ")
 ;;991
 ;;21,"55887026401 ")
 ;;458
 ;;21,"55887026430 ")
 ;;459
 ;;21,"55887026490 ")
 ;;460
 ;;21,"55887046430 ")
 ;;1071
 ;;21,"55887046490 ")
 ;;1072
 ;;21,"55887057710 ")
 ;;874
 ;;21,"55887057730 ")
 ;;875
 ;;21,"55887057760 ")
 ;;876
 ;;21,"55887057790 ")
 ;;877
 ;;21,"55887057810 ")
 ;;1271
 ;;21,"55887057830 ")
 ;;1272
 ;;21,"55887057860 ")
 ;;1273
 ;;21,"55887057886 ")
 ;;1274
 ;;21,"55887057890 ")
 ;;1275
 ;;21,"55887092630 ")
 ;;691
 ;;21,"55887092690 ")
 ;;692
 ;;21,"57866994601 ")
 ;;693
 ;;21,"57866994602 ")
 ;;694
 ;;21,"58016008300 ")
 ;;1073
 ;;21,"58016008301 ")
 ;;1074
 ;;21,"58016008302 ")
 ;;1075
 ;;21,"58016008303 ")
 ;;1076
 ;;21,"58016008304 ")
 ;;1077
 ;;21,"58016008305 ")
 ;;1078
 ;;21,"58016008306 ")
 ;;1079
 ;;21,"58016008307 ")
 ;;1080
 ;;21,"58016008308 ")
 ;;1081
 ;;21,"58016008309 ")
 ;;1082
 ;;21,"58016008310 ")
 ;;1083
 ;;21,"58016008312 ")
 ;;1084
 ;;21,"58016008314 ")
 ;;1085
 ;;21,"58016008315 ")
 ;;1086
 ;;21,"58016008316 ")
 ;;1087
 ;;21,"58016008318 ")
 ;;1088
 ;;21,"58016008320 ")
 ;;1089
 ;;21,"58016008321 ")
 ;;1090
 ;;21,"58016008324 ")
 ;;1091
 ;;21,"58016008325 ")
 ;;1092
 ;;21,"58016008326 ")
 ;;1093
 ;;21,"58016008327 ")
 ;;1094
 ;;21,"58016008328 ")
 ;;1095
 ;;21,"58016008330 ")
 ;;1096
 ;;21,"58016008332 ")
 ;;1097
