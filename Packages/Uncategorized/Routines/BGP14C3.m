BGP14C3 ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON APR 14, 2011 ;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;;JUN 27, 2011;Build 33
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;21,"54569-5834-03 ")
 ;;1186
 ;;21,"54569-5834-04 ")
 ;;1187
 ;;21,"54569-5881-00 ")
 ;;53
 ;;21,"54569-5951-00 ")
 ;;57
 ;;21,"54569-6099-00 ")
 ;;61
 ;;21,"54569-6113-00 ")
 ;;1393
 ;;21,"54868-0686-01 ")
 ;;373
 ;;21,"54868-0686-02 ")
 ;;374
 ;;21,"54868-0686-03 ")
 ;;375
 ;;21,"54868-0686-04 ")
 ;;376
 ;;21,"54868-1087-00 ")
 ;;475
 ;;21,"54868-1087-01 ")
 ;;476
 ;;21,"54868-1207-00 ")
 ;;79
 ;;21,"54868-1207-01 ")
 ;;80
 ;;21,"54868-1890-00 ")
 ;;847
 ;;21,"54868-1890-01 ")
 ;;848
 ;;21,"54868-1968-00 ")
 ;;282
 ;;21,"54868-2287-01 ")
 ;;575
 ;;21,"54868-2287-02 ")
 ;;576
 ;;21,"54868-2288-00 ")
 ;;641
 ;;21,"54868-2288-01 ")
 ;;642
 ;;21,"54868-2288-02 ")
 ;;643
 ;;21,"54868-2639-00 ")
 ;;921
 ;;21,"54868-2639-01 ")
 ;;922
 ;;21,"54868-3104-00 ")
 ;;1046
 ;;21,"54868-3104-01 ")
 ;;1047
 ;;21,"54868-3270-00 ")
 ;;727
 ;;21,"54868-3270-01 ")
 ;;728
 ;;21,"54868-3270-02 ")
 ;;729
 ;;21,"54868-3287-00 ")
 ;;76
 ;;21,"54868-3287-01 ")
 ;;77
 ;;21,"54868-3329-00 ")
 ;;227
 ;;21,"54868-3934-00 ")
 ;;103
 ;;21,"54868-3934-01 ")
 ;;104
 ;;21,"54868-3934-02 ")
 ;;105
 ;;21,"54868-3934-03 ")
 ;;106
 ;;21,"54868-3934-04 ")
 ;;125
 ;;21,"54868-3946-00 ")
 ;;140
 ;;21,"54868-3946-01 ")
 ;;141
 ;;21,"54868-3946-02 ")
 ;;142
 ;;21,"54868-3946-03 ")
 ;;143
 ;;21,"54868-4157-00 ")
 ;;1188
 ;;21,"54868-4157-01 ")
 ;;1189
 ;;21,"54868-4157-02 ")
 ;;1190
 ;;21,"54868-4181-00 ")
 ;;1394
 ;;21,"54868-4181-01 ")
 ;;1395
 ;;21,"54868-4224-00 ")
 ;;238
 ;;21,"54868-4224-01 ")
 ;;239
 ;;21,"54868-4229-00 ")
 ;;173
 ;;21,"54868-4229-01 ")
 ;;174
 ;;21,"54868-4229-02 ")
 ;;175
 ;;21,"54868-4229-03 ")
 ;;176
 ;;21,"54868-4585-00 ")
 ;;377
 ;;21,"54868-4585-01 ")
 ;;378
 ;;21,"54868-4585-02 ")
 ;;379
 ;;21,"54868-4585-03 ")
 ;;380
 ;;21,"54868-4593-00 ")
 ;;283
 ;;21,"54868-4593-01 ")
 ;;284
 ;;21,"54868-4593-02 ")
 ;;285
 ;;21,"54868-4601-00 ")
 ;;245
 ;;21,"54868-4634-00 ")
 ;;780
 ;;21,"54868-4774-00 ")
 ;;477
 ;;21,"54868-4774-01 ")
 ;;478
 ;;21,"54868-4774-02 ")
 ;;479
 ;;21,"54868-4774-03 ")
 ;;480
 ;;21,"54868-4807-00 ")
 ;;1465
 ;;21,"54868-4807-01 ")
 ;;1466
 ;;21,"54868-4807-02 ")
 ;;1467
 ;;21,"54868-4934-00 ")
 ;;202
 ;;21,"54868-4934-01 ")
 ;;203
 ;;21,"54868-4963-00 ")
 ;;804
 ;;21,"54868-4963-01 ")
 ;;805
 ;;21,"54868-4963-02 ")
 ;;806
 ;;21,"54868-4963-03 ")
 ;;807
 ;;21,"54868-4999-00 ")
 ;;1470
 ;;21,"54868-4999-01 ")
 ;;1471
 ;;21,"54868-5085-00 ")
 ;;827
 ;;21,"54868-5085-01 ")
 ;;828
 ;;21,"54868-5085-02 ")
 ;;829
 ;;21,"54868-5085-03 ")
 ;;839
 ;;21,"54868-5087-00 ")
 ;;1459
 ;;21,"54868-5179-00 ")
 ;;82
 ;;21,"54868-5187-00 ")
 ;;13
 ;;21,"54868-5187-01 ")
 ;;14
 ;;21,"54868-5187-02 ")
 ;;15
 ;;21,"54868-5189-00 ")
 ;;27
 ;;21,"54868-5189-01 ")
 ;;28
 ;;21,"54868-5200-00 ")
 ;;62
 ;;21,"54868-5200-01 ")
 ;;63
 ;;21,"54868-5209-00 ")
 ;;58
 ;;21,"54868-5209-01 ")
 ;;59
 ;;21,"54868-5250-00 ")
 ;;2
 ;;21,"54868-5259-00 ")
 ;;36
 ;;21,"54868-5259-01 ")
 ;;37
 ;;21,"54868-5341-00 ")
 ;;861
 ;;21,"54868-5341-01 ")
 ;;862
 ;;21,"54868-5358-00 ")
 ;;543
 ;;21,"54868-5420-00 ")
 ;;84
 ;;21,"54868-5513-00 ")
 ;;539
 ;;21,"54868-5523-00 ")
 ;;65
 ;;21,"54868-5523-01 ")
 ;;66
 ;;21,"54868-5567-00 ")
 ;;54
 ;;21,"54868-5576-00 ")
 ;;577
 ;;21,"54868-5576-01 ")
 ;;578
 ;;21,"54868-5577-00 ")
 ;;644
 ;;21,"54868-5577-01 ")
 ;;645
 ;;21,"54868-5578-00 ")
 ;;730
 ;;21,"54868-5578-01 ")
 ;;731
 ;;21,"54868-5578-02 ")
 ;;732
 ;;21,"54868-5579-00 ")
 ;;781
 ;;21,"54868-5579-01 ")
 ;;793
 ;;21,"54868-5627-00 ")
 ;;923
 ;;21,"54868-5627-01 ")
 ;;924
 ;;21,"54868-5628-00 ")
 ;;1048
 ;;21,"54868-5628-01 ")
 ;;1049
 ;;21,"54868-5628-02 ")
 ;;1050
 ;;21,"54868-5629-00 ")
 ;;1191
 ;;21,"54868-5629-01 ")
 ;;1192
 ;;21,"54868-5629-02 ")
 ;;1193
 ;;21,"54868-5629-03 ")
 ;;1194
 ;;21,"54868-5629-04 ")
 ;;1195
 ;;21,"54868-5630-00 ")
 ;;1396
 ;;21,"54868-5630-01 ")
 ;;1397
 ;;21,"54868-5653-00 ")
 ;;1462
 ;;21,"54868-5672-00 ")
 ;;69
 ;;21,"54868-5699-00 ")
 ;;71
 ;;21,"54868-5886-00 ")
 ;;1480
 ;;21,"54868-5886-01 ")
 ;;1481
 ;;21,"54868-5904-00 ")
 ;;1476
 ;;21,"54868-5904-01 ")
 ;;1477
 ;;21,"54868-5907-00 ")
 ;;1483
 ;;21,"54868-5907-01 ")
 ;;1484
 ;;21,"54868-6066-00 ")
 ;;1345
 ;;21,"55045-3014-08 ")
 ;;424
 ;;21,"55045-3015-08 ")
 ;;481
 ;;21,"55045-3655-08 ")
 ;;972
 ;;21,"55111-0197-05 ")
 ;;1319
 ;;21,"55111-0197-30 ")
 ;;1320
 ;;21,"55111-0197-90 ")
 ;;1321
 ;;21,"55111-0198-05 ")
 ;;925
 ;;21,"55111-0198-30 ")
 ;;926
 ;;21,"55111-0198-90 ")
 ;;927
 ;;21,"55111-0199-05 ")
 ;;1051
 ;;21,"55111-0199-30 ")
 ;;1052
 ;;21,"55111-0199-90 ")
 ;;1053
 ;;21,"55111-0200-05 ")
 ;;1196
 ;;21,"55111-0200-30 ")
 ;;1197
 ;;21,"55111-0200-90 ")
 ;;1198
 ;;21,"55111-0229-05 ")
 ;;579
 ;;21,"55111-0229-90 ")
 ;;580
 ;;21,"55111-0230-05 ")
 ;;646
 ;;21,"55111-0230-90 ")
 ;;647
 ;;21,"55111-0231-05 ")
 ;;733
 ;;21,"55111-0231-90 ")
 ;;734
 ;;21,"55111-0268-05 ")
 ;;1398
 ;;21,"55111-0268-30 ")
 ;;1399
 ;;21,"55111-0268-90 ")
 ;;1400
 ;;21,"55111-0274-05 ")
 ;;782
 ;;21,"55111-0274-90 ")
 ;;783
 ;;21,"55111-0726-10 ")
 ;;1322
 ;;21,"55111-0726-30 ")
 ;;1323
 ;;21,"55111-0726-90 ")
 ;;1324
 ;;21,"55111-0735-10 ")
 ;;928
 ;;21,"55111-0735-30 ")
 ;;929
 ;;21,"55111-0735-90 ")
 ;;930
 ;;21,"55111-0740-10 ")
 ;;1054
 ;;21,"55111-0740-30 ")
 ;;1055
 ;;21,"55111-0740-90 ")
 ;;1056
 ;;21,"55111-0749-10 ")
 ;;1199
 ;;21,"55111-0749-30 ")
 ;;1200
 ;;21,"55111-0749-90 ")
 ;;1201
 ;;21,"55111-0750-10 ")
 ;;1401
 ;;21,"55111-0750-30 ")
 ;;1402
 ;;21,"55111-0750-90 ")
 ;;1403
 ;;21,"55289-0104-30 ")
 ;;581
 ;;21,"55289-0280-30 ")
 ;;29
 ;;21,"55289-0293-14 ")
 ;;1057
 ;;21,"55289-0293-30 ")
 ;;1058
 ;;21,"55289-0293-90 ")
 ;;1059
 ;;21,"55289-0338-14 ")
 ;;931
 ;;21,"55289-0338-30 ")
 ;;932
 ;;21,"55289-0338-90 ")
 ;;933
 ;;21,"55289-0395-30 ")
 ;;1202
 ;;21,"55289-0395-90 ")
 ;;1203
 ;;21,"55289-0400-30 ")
 ;;381
 ;;21,"55289-0476-30 ")
 ;;240
 ;;21,"55289-0520-30 ")
 ;;38
 ;;21,"55289-0548-30 ")
 ;;482
 ;;21,"55289-0692-14 ")
 ;;483
 ;;21,"55289-0692-30 ")
 ;;484
 ;;21,"55289-0740-60 ")
 ;;228
 ;;21,"55289-0800-30 ")
 ;;144
 ;;21,"55289-0861-30 ")
 ;;177
 ;;21,"55289-0870-30 ")
 ;;107
 ;;21,"55289-0871-30 ")
 ;;648
 ;;21,"55289-0873-30 ")
 ;;735
 ;;21,"55289-0874-30 ")
 ;;1204
 ;;21,"55289-0881-30 ")
 ;;382
 ;;21,"55289-0932-30 ")
 ;;830
 ;;21,"55289-0935-30 ")
 ;;808
 ;;21,"55289-0980-21 ")
 ;;16
 ;;21,"55887-0192-90 ")
 ;;736
 ;;21,"55887-0203-30 ")
 ;;649
 ;;21,"55887-0203-90 ")
 ;;650
 ;;21,"55887-0350-30 ")
 ;;286
 ;;21,"55887-0350-60 ")
 ;;287
 ;;21,"55887-0350-90 ")
 ;;288
 ;;21,"55887-0369-30 ")
 ;;485
 ;;21,"55887-0369-60 ")
 ;;486
 ;;21,"55887-0369-90 ")
 ;;487
 ;;21,"55887-0624-20 ")
 ;;108
 ;;21,"55887-0624-30 ")
 ;;109
 ;;21,"55887-0624-40 ")
 ;;110
 ;;21,"55887-0624-60 ")
 ;;111
 ;;21,"55887-0624-82 ")
 ;;112
 ;;21,"55887-0624-90 ")
 ;;113
 ;;21,"55887-0858-10 ")
 ;;1205
 ;;21,"55887-0858-30 ")
 ;;1206
 ;;21,"55887-0858-60 ")
 ;;1207
 ;;21,"55887-0858-90 ")
 ;;1208
 ;;21,"55887-0929-90 ")
 ;;178
 ;;21,"55887-0974-30 ")
 ;;383
 ;;21,"57866-3932-01 ")
 ;;737
 ;;21,"57866-6400-01 ")
 ;;289
 ;;21,"57866-6500-01 ")
 ;;488
 ;;21,"57866-6601-01 ")
 ;;384
 ;;21,"57866-7982-01 ")
 ;;1060
 ;;21,"57866-7983-01 ")
 ;;1209
 ;;21,"57866-7986-01 ")
 ;;934
 ;;21,"57866-8615-01 ")
 ;;114
 ;;21,"58016-0006-00 ")
 ;;1210
 ;;21,"58016-0006-30 ")
 ;;1211
 ;;21,"58016-0006-60 ")
 ;;1212
 ;;21,"58016-0006-90 ")
 ;;1213
 ;;21,"58016-0007-00 ")
 ;;1061
 ;;21,"58016-0007-30 ")
 ;;1062
 ;;21,"58016-0007-60 ")
 ;;1063
 ;;21,"58016-0007-90 ")
 ;;1064
 ;;21,"58016-0008-00 ")
 ;;935
 ;;21,"58016-0008-30 ")
 ;;936
 ;;21,"58016-0008-60 ")
 ;;937
 ;;21,"58016-0008-90 ")
 ;;938
 ;;21,"58016-0012-00 ")
 ;;738
 ;;21,"58016-0012-30 ")
 ;;739
 ;;21,"58016-0012-60 ")
 ;;740
 ;;21,"58016-0012-90 ")
 ;;741
 ;;21,"58016-0013-00 ")
 ;;651
 ;;21,"58016-0013-30 ")
 ;;652
 ;;21,"58016-0013-60 ")
 ;;653
 ;;21,"58016-0013-90 ")
 ;;654
 ;;21,"58016-0037-00 ")
 ;;809
 ;;21,"58016-0037-30 ")
 ;;810
 ;;21,"58016-0037-60 ")
 ;;811
 ;;21,"58016-0037-90 ")
 ;;812
 ;;21,"58016-0051-00 ")
 ;;204
 ;;21,"58016-0051-30 ")
 ;;205
 ;;21,"58016-0051-60 ")
 ;;206
 ;;21,"58016-0051-90 ")
 ;;207
 ;;21,"58016-0052-00 ")
 ;;831
 ;;21,"58016-0052-30 ")
 ;;832
 ;;21,"58016-0052-60 ")
 ;;833
 ;;21,"58016-0052-90 ")
 ;;834
 ;;21,"58016-0071-00 ")
 ;;849
 ;;21,"58016-0071-30 ")
 ;;850
 ;;21,"58016-0071-60 ")
 ;;851
 ;;21,"58016-0071-90 ")
 ;;852
 ;;21,"58016-0364-00 ")
 ;;939
 ;;21,"58016-0364-30 ")
 ;;940
 ;;21,"58016-0364-60 ")
 ;;941
 ;;21,"58016-0364-90 ")
 ;;942
 ;;21,"58016-0365-00 ")
 ;;1214
 ;;21,"58016-0365-30 ")
 ;;1215
 ;;21,"58016-0365-60 ")
 ;;1216
 ;;21,"58016-0365-90 ")
 ;;1217
 ;;21,"58016-0385-00 ")
 ;;1065
 ;;21,"58016-0385-30 ")
 ;;1066
 ;;21,"58016-0385-60 ")
 ;;1067
 ;;21,"58016-0385-90 ")
 ;;1068
 ;;21,"58016-0425-00 ")
 ;;655
 ;;21,"58016-0425-30 ")
 ;;656
 ;;21,"58016-0425-60 ")
 ;;657
 ;;21,"58016-0425-90 ")
 ;;658
 ;;21,"58016-0546-00 ")
 ;;385
 ;;21,"58016-0900-00 ")
 ;;386
 ;;21,"58016-0900-02 ")
 ;;387
 ;;21,"58016-0900-30 ")
 ;;388
 ;;21,"58016-0900-60 ")
 ;;389
 ;;21,"58016-0900-90 ")
 ;;390
 ;;21,"58016-0922-00 ")
 ;;489
 ;;21,"58016-0922-02 ")
 ;;490
 ;;21,"58016-0922-30 ")
 ;;491
 ;;21,"58016-0922-60 ")
 ;;492
 ;;21,"58016-0922-90 ")
 ;;493
 ;;21,"58016-0979-00 ")
 ;;290
 ;;21,"58016-0979-02 ")
 ;;291
 ;;21,"58016-0979-20 ")
 ;;292
 ;;21,"58016-0979-30 ")
 ;;293
 ;;21,"58016-0979-60 ")
 ;;294
 ;;21,"58016-0979-90 ")
 ;;295
 ;;21,"58864-0608-30 ")
 ;;115
 ;;21,"58864-0623-15 ")
 ;;179
 ;;21,"58864-0623-30 ")
 ;;180
 ;;21,"58864-0653-30 ")
 ;;582
 ;;21,"58864-0682-30 ")
 ;;1218
 ;;21,"58864-0685-30 ")
 ;;145
 ;;21,"58864-0739-30 ")
 ;;1325
 ;;21,"58864-0743-15 ")
 ;;742
 ;;21,"58864-0743-30 ")
 ;;743
 ;;21,"58864-0760-30 ")
 ;;1069
 ;;21,"58864-0780-30 ")
 ;;391
 ;;21,"58864-0780-60 ")
 ;;392
 ;;21,"58864-0781-30 ")
 ;;296
 ;;21,"58864-0834-30 ")
 ;;208
 ;;21,"59630-0628-30 ")
 ;;536
 ;;21,"59630-0629-30 ")
 ;;540
 ;;21,"59630-0630-30 ")
 ;;544
 ;;21,"60429-0248-10 ")
 ;;318
 ;;21,"60429-0248-60 ")
 ;;319
 ;;21,"60429-0249-10 ")
 ;;425
 ;;21,"60429-0249-60 ")
 ;;426
 ;;21,"60429-0250-10 ")
 ;;525
 ;;21,"60429-0250-60 ")
 ;;526
 ;;21,"60429-0250-90 ")
 ;;527
 ;;21,"60505-0168-05 ")
 ;;583
 ;;21,"60505-0168-09 ")
 ;;584
 ;;21,"60505-0169-07 ")
 ;;659
 ;;21,"60505-0169-09 ")
 ;;660
 ;;21,"60505-0170-07 ")
 ;;744
 ;;21,"60505-0170-08 ")
 ;;745
 ;;21,"60505-0170-09 ")
 ;;746
 ;;21,"60505-0177-00 ")
 ;;297
 ;;21,"60505-0178-00 ")
 ;;393
 ;;21,"60505-0179-00 ")
 ;;494
 ;;21,"60505-1323-05 ")
 ;;784
 ;;21,"60505-1323-09 ")
 ;;785
 ;;21,"60598-0006-90 ")
 ;;1468
 ;;21,"60598-0007-90 ")
 ;;1472
 ;;21,"60598-0008-90 ")
 ;;1460
 ;;21,"60598-0009-90 ")
 ;;1463
 ;;21,"61442-0141-01 ")
 ;;298
 ;;21,"61442-0141-10 ")
 ;;299
 ;;21,"61442-0141-60 ")
 ;;300
 ;;21,"61442-0142-01 ")
 ;;394
 ;;21,"61442-0142-05 ")
 ;;395
 ;;21,"61442-0142-10 ")
 ;;396
 ;;21,"61442-0142-60 ")
 ;;397
 ;;21,"61442-0143-01 ")
 ;;495
 ;;21,"61442-0143-05 ")
 ;;496
 ;;21,"61442-0143-10 ")
 ;;497
 ;;21,"61442-0143-60 ")
 ;;498
 ;;21,"62022-0627-30 ")
 ;;534
 ;;21,"62022-0628-30 ")
 ;;537
 ;;21,"62022-0629-30 ")
 ;;541
 ;;21,"62022-0630-30 ")
 ;;545
 ;;21,"62022-0760-30 ")
 ;;535
 ;;21,"62022-0770-30 ")
 ;;538
 ;;21,"62022-0780-30 ")
 ;;542
 ;;21,"62022-0781-30 ")
 ;;546
 ;;21,"62037-0791-01 ")
 ;;301
 ;;21,"62037-0791-60 ")
 ;;302
 ;;21,"62037-0792-01 ")
 ;;398
 ;;21,"62037-0792-60 ")
 ;;399
 ;;21,"62037-0793-01 ")
 ;;499
 ;;21,"62037-0793-60 ")
 ;;500
 ;;21,"63304-0595-90 ")
 ;;585
 ;;21,"63304-0596-90 ")
 ;;661
 ;;21,"63304-0597-90 ")
 ;;747
