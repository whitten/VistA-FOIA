BGP8EXWD ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON SEP 28, 2007 ;
 ;;8.0;IHS CLINICAL REPORTING;;MAR 12, 2008
 ;;;BGP6;;SEP 28, 2007
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;21,"61570-0110-56 ")
 ;;1359
 ;;21,"61570-0111-01 ")
 ;;1368
 ;;21,"61570-0111-05 ")
 ;;1369
 ;;21,"61570-0111-56 ")
 ;;1370
 ;;21,"61570-0112-01 ")
 ;;1375
 ;;21,"61570-0112-05 ")
 ;;1376
 ;;21,"61570-0112-56 ")
 ;;1377
 ;;21,"61570-0120-01 ")
 ;;1362
 ;;21,"61570-0120-05 ")
 ;;1363
 ;;21,"62033-0101-20 ")
 ;;362
 ;;21,"62037-0516-01 ")
 ;;130
 ;;21,"62037-0517-01 ")
 ;;20
 ;;21,"62037-0518-01 ")
 ;;64
 ;;21,"62037-0519-01 ")
 ;;100
 ;;21,"62037-0756-01 ")
 ;;192
 ;;21,"62037-0757-01 ")
 ;;151
 ;;21,"62037-0758-01 ")
 ;;169
 ;;21,"62037-0759-01 ")
 ;;182
 ;;21,"62175-0171-37 ")
 ;;1277
 ;;21,"62175-0177-37 ")
 ;;1271
 ;;21,"62682-6044-03 ")
 ;;141
 ;;21,"63304-0403-01 ")
 ;;822
 ;;21,"63304-0404-01 ")
 ;;825
 ;;21,"63304-0522-01 ")
 ;;594
 ;;21,"63304-0522-10 ")
 ;;595
 ;;21,"63304-0523-01 ")
 ;;720
 ;;21,"63304-0523-10 ")
 ;;721
 ;;21,"63304-0524-01 ")
 ;;525
 ;;21,"63304-0524-10 ")
 ;;526
 ;;21,"63304-0525-01 ")
 ;;649
 ;;21,"63304-0525-10 ")
 ;;650
 ;;21,"63304-0531-01 ")
 ;;1032
 ;;21,"63304-0532-01 ")
 ;;1231
 ;;21,"63304-0532-10 ")
 ;;1232
 ;;21,"63304-0533-01 ")
 ;;962
 ;;21,"63304-0533-10 ")
 ;;963
 ;;21,"63304-0534-01 ")
 ;;1078
 ;;21,"63304-0534-10 ")
 ;;1079
 ;;21,"63304-0535-01 ")
 ;;1184
 ;;21,"63304-0535-10 ")
 ;;1185
 ;;21,"63304-0536-01 ")
 ;;844
 ;;21,"63304-0537-01 ")
 ;;876
 ;;21,"63304-0538-01 ")
 ;;902
 ;;21,"63304-0736-00 ")
 ;;131
 ;;21,"63304-0736-01 ")
 ;;132
 ;;21,"63304-0736-05 ")
 ;;133
 ;;21,"63304-0736-90 ")
 ;;134
 ;;21,"63304-0737-01 ")
 ;;21
 ;;21,"63304-0737-05 ")
 ;;22
 ;;21,"63304-0737-90 ")
 ;;23
 ;;21,"63304-0738-01 ")
 ;;65
 ;;21,"63304-0738-05 ")
 ;;66
 ;;21,"63304-0738-90 ")
 ;;67
 ;;21,"63304-0739-01 ")
 ;;101
 ;;21,"63304-0739-05 ")
 ;;102
 ;;21,"63304-0739-90 ")
 ;;103
 ;;21,"63304-0775-10 ")
 ;;781
 ;;21,"63304-0775-90 ")
 ;;782
 ;;21,"63304-0776-10 ")
 ;;800
 ;;21,"63304-0776-90 ")
 ;;801
 ;;21,"63304-0777-10 ")
 ;;816
 ;;21,"63304-0777-90 ")
 ;;817
 ;;21,"63304-0834-01 ")
 ;;596
 ;;21,"63304-0834-10 ")
 ;;597
 ;;21,"63304-0835-01 ")
 ;;722
 ;;21,"63304-0835-10 ")
 ;;723
 ;;21,"63304-0836-01 ")
 ;;527
 ;;21,"63304-0836-10 ")
 ;;528
 ;;21,"63304-0837-01 ")
 ;;651
 ;;21,"63304-0837-10 ")
 ;;652
 ;;21,"63739-0042-01 ")
 ;;281
 ;;21,"63739-0042-03 ")
 ;;282
 ;;21,"63739-0042-15 ")
 ;;283
 ;;21,"63739-0043-01 ")
 ;;363
 ;;21,"63739-0043-03 ")
 ;;364
 ;;21,"63739-0043-15 ")
 ;;365
 ;;21,"63739-0302-15 ")
 ;;529
 ;;21,"63739-0322-15 ")
 ;;598
 ;;21,"63739-0323-15 ")
 ;;724
 ;;21,"63739-0348-15 ")
 ;;1233
 ;;21,"63739-0349-15 ")
 ;;964
 ;;21,"63739-0350-15 ")
 ;;1080
 ;;21,"63874-0058-10 ")
 ;;1335
 ;;21,"63874-0058-15 ")
 ;;1336
 ;;21,"63874-0058-30 ")
 ;;1337
 ;;21,"63874-0058-90 ")
 ;;1338
 ;;21,"63874-0347-01 ")
 ;;366
 ;;21,"63874-0347-02 ")
 ;;367
 ;;21,"63874-0347-05 ")
 ;;368
 ;;21,"63874-0347-20 ")
 ;;369
 ;;21,"63874-0347-30 ")
 ;;370
 ;;21,"63874-0347-40 ")
 ;;371
 ;;21,"63874-0347-42 ")
 ;;372
 ;;21,"63874-0347-45 ")
 ;;373
 ;;21,"63874-0347-90 ")
 ;;374
 ;;21,"63874-0348-01 ")
 ;;448
 ;;21,"63874-0348-10 ")
 ;;449
 ;;21,"63874-0348-14 ")
 ;;450
 ;;21,"63874-0348-20 ")
 ;;451
 ;;21,"63874-0348-30 ")
 ;;452
 ;;21,"63874-0348-60 ")
 ;;453
 ;;21,"63874-0348-90 ")
 ;;454
 ;;21,"63874-0379-10 ")
 ;;1351
 ;;21,"63874-0379-30 ")
 ;;1352
 ;;21,"63874-0423-01 ")
 ;;725
 ;;21,"63874-0423-10 ")
 ;;726
 ;;21,"63874-0423-20 ")
 ;;727
 ;;21,"63874-0423-30 ")
 ;;728
 ;;21,"63874-0514-01 ")
 ;;1081
 ;;21,"63874-0514-10 ")
 ;;1082
 ;;21,"63874-0514-14 ")
 ;;1083
 ;;21,"63874-0514-15 ")
 ;;1084
 ;;21,"63874-0514-16 ")
 ;;1085
 ;;21,"63874-0514-20 ")
 ;;1086
 ;;21,"63874-0514-28 ")
 ;;1087
 ;;21,"63874-0514-30 ")
 ;;1088
 ;;21,"63874-0514-60 ")
 ;;1089
 ;;21,"63874-0514-90 ")
 ;;1090
 ;;21,"63874-0522-01 ")
 ;;965
 ;;21,"63874-0522-15 ")
 ;;966
 ;;21,"63874-0522-30 ")
 ;;967
 ;;21,"63874-0522-60 ")
 ;;968
 ;;21,"63874-0558-30 ")
 ;;1234
 ;;21,"63874-0581-10 ")
 ;;1297
 ;;21,"63874-0581-30 ")
 ;;1298
 ;;21,"63874-0581-90 ")
 ;;1299
 ;;21,"63874-0655-20 ")
 ;;530
 ;;21,"63874-0655-30 ")
 ;;531
 ;;21,"63874-0655-60 ")
 ;;532
 ;;21,"63874-0987-01 ")
 ;;653
 ;;21,"63874-0987-20 ")
 ;;654
 ;;21,"63874-0987-30 ")
 ;;655
 ;;21,"63874-0987-60 ")
 ;;656
 ;;21,"64455-0140-10 ")
 ;;611
 ;;21,"64455-0140-90 ")
 ;;612
 ;;21,"64455-0141-10 ")
 ;;754
 ;;21,"64455-0141-90 ")
 ;;755
 ;;21,"64455-0142-10 ")
 ;;559
 ;;21,"64455-0142-90 ")
 ;;560
 ;;21,"64455-0143-10 ")
 ;;677
 ;;21,"64455-0143-90 ")
 ;;678
 ;;21,"64455-0145-01 ")
 ;;772
 ;;21,"64455-0146-01 ")
 ;;765
 ;;21,"64679-0902-01 ")
 ;;284
 ;;21,"64679-0902-02 ")
 ;;285
 ;;21,"64679-0903-01 ")
 ;;375
 ;;21,"64679-0903-02 ")
 ;;376
 ;;21,"64679-0904-01 ")
 ;;455
 ;;21,"64679-0904-02 ")
 ;;456
 ;;21,"64679-0905-01 ")
 ;;220
 ;;21,"64679-0923-02 ")
 ;;599
 ;;21,"64679-0923-03 ")
 ;;600
 ;;21,"64679-0924-02 ")
 ;;729
 ;;21,"64679-0924-03 ")
 ;;730
 ;;21,"64679-0925-01 ")
 ;;533
 ;;21,"64679-0925-02 ")
 ;;534
 ;;21,"64679-0925-03 ")
 ;;535
 ;;21,"64679-0926-02 ")
 ;;657
 ;;21,"64679-0926-03 ")
 ;;658
 ;;21,"66267-1009-00 ")
 ;;536
 ;;21,"66336-0366-30 ")
 ;;45
 ;;21,"66336-0773-30 ")
 ;;24
 ;;21,"66636-0691-30 ")
 ;;68
 ;;21,"66685-0301-00 ")
 ;;601
 ;;21,"66685-0301-02 ")
 ;;602
 ;;21,"66685-0302-00 ")
 ;;731
 ;;21,"66685-0302-02 ")
 ;;732
 ;;21,"66685-0303-00 ")
 ;;537
 ;;21,"66685-0303-02 ")
 ;;538
 ;;21,"66685-0304-00 ")
 ;;659
 ;;21,"66685-0304-02 ")
 ;;660
 ;;21,"66685-0701-01 ")
 ;;1033
 ;;21,"66685-0701-02 ")
 ;;1034
 ;;21,"66685-0702-01 ")
 ;;1235
 ;;21,"66685-0702-02 ")
 ;;1236
 ;;21,"66685-0703-01 ")
 ;;969
 ;;21,"66685-0703-02 ")
 ;;970
 ;;21,"66685-0704-01 ")
 ;;1091
 ;;21,"66685-0704-02 ")
 ;;1092
 ;;21,"66685-0705-01 ")
 ;;1152
 ;;21,"66685-0705-02 ")
 ;;1153
 ;;21,"66685-0706-01 ")
 ;;1186
 ;;21,"66685-0706-03 ")
 ;;1187
 ;;21,"68115-0059-00 ")
 ;;377
 ;;21,"68115-0059-30 ")
 ;;378
 ;;21,"68115-0059-90 ")
 ;;379
 ;;21,"68115-0060-30 ")
 ;;457
 ;;21,"68115-0127-00 ")
 ;;539
 ;;21,"68115-0127-15 ")
 ;;540
 ;;21,"68115-0127-30 ")
 ;;541
 ;;21,"68115-0127-60 ")
 ;;542
 ;;21,"68115-0128-00 ")
 ;;661
 ;;21,"68115-0128-20 ")
 ;;662
 ;;21,"68115-0128-30 ")
 ;;663
 ;;21,"68115-0128-60 ")
 ;;664
 ;;21,"68115-0129-30 ")
 ;;733
 ;;21,"68115-0207-30 ")
 ;;971
 ;;21,"68115-0207-60 ")
 ;;972
 ;;21,"68115-0208-30 ")
 ;;1093
 ;;21,"68115-0209-30 ")
 ;;1188
 ;;21,"68115-0213-00 ")
 ;;25
 ;;21,"68115-0215-30 ")
 ;;46
 ;;21,"68115-0216-30 ")
 ;;86
 ;;21,"68115-0217-30 ")
 ;;118
 ;;21,"68115-0361-30 ")
 ;;1017
 ;;21,"68115-0362-00 ")
 ;;1133
 ;;21,"68115-0378-30 ")
 ;;1364
 ;;21,"68115-0396-30 ")
 ;;1237
 ;;21,"68115-0425-90 ")
 ;;221
 ;;21,"68115-0490-60 ")
 ;;26
 ;;21,"68115-0530-00 ")
 ;;1371
 ;;21,"68115-0597-00 ")
 ;;104
 ;;21,"68115-0615-00 ")
 ;;27
 ;;21,"68115-0621-00 ")
 ;;135
 ;;21,"68115-0650-00 ")
 ;;1409
 ;;21,"68115-0654-00 ")
 ;;69
 ;;21,"68115-0673-00 ")
 ;;1405
 ;;21,"68115-0677-00 ")
 ;;877
 ;;21,"68115-0677-30 ")
 ;;878
 ;;21,"68115-0778-00 ")
 ;;1401
 ;;21,"68115-0812-00 ")
 ;;1378
 ;;21,"68115-0824-00 ")
 ;;142
 ;;21,"68115-0889-90 ")
 ;;1318
 ;;21,"68180-0512-01 ")
 ;;1035
 ;;21,"68180-0512-02 ")
 ;;1036
 ;;21,"68180-0513-01 ")
 ;;1238
 ;;21,"68180-0513-03 ")
 ;;1239
 ;;21,"68180-0514-00 ")
 ;;973
 ;;21,"68180-0514-01 ")
 ;;974
 ;;21,"68180-0514-03 ")
 ;;975
 ;;21,"68180-0515-01 ")
 ;;1094
 ;;21,"68180-0515-03 ")
 ;;1095
 ;;21,"68180-0516-01 ")
 ;;1154
 ;;21,"68180-0516-02 ")
 ;;1155
 ;;21,"68180-0517-01 ")
 ;;1189
 ;;21,"68180-0517-03 ")
 ;;1190
 ;;9002226,635,.01)
 ;;BGP HEDIS ACEI NDC
 ;;9002226,635,.02)
 ;;@
 ;;9002226,635,.04)
 ;;n
 ;;9002226,635,.06)
 ;;@
 ;;9002226,635,.08)
 ;;@
 ;;9002226,635,.09)
 ;;@
 ;;9002226,635,.11)
 ;;@
 ;;9002226,635,.12)
 ;;@
 ;;9002226,635,.13)
 ;;1
 ;;9002226,635,.14)
 ;;@
 ;;9002226,635,.15)
 ;;@
 ;;9002226,635,.16)
 ;;@
 ;;9002226,635,.17)
 ;;@
 ;;9002226,635,3101)
 ;;@
 ;;9002226.02101,"635,00003-0338-50 ",.01)
 ;;00003-0338-50
 ;;9002226.02101,"635,00003-0338-50 ",.02)
 ;;00003-0338-50
 ;;9002226.02101,"635,00003-0349-50 ",.01)
 ;;00003-0349-50
 ;;9002226.02101,"635,00003-0349-50 ",.02)
 ;;00003-0349-50
 ;;9002226.02101,"635,00003-0384-50 ",.01)
 ;;00003-0384-50
 ;;9002226.02101,"635,00003-0384-50 ",.02)
 ;;00003-0384-50
 ;;9002226.02101,"635,00003-0390-50 ",.01)
 ;;00003-0390-50
 ;;9002226.02101,"635,00003-0390-50 ",.02)
 ;;00003-0390-50
 ;;9002226.02101,"635,00003-0450-51 ",.01)
 ;;00003-0450-51
 ;;9002226.02101,"635,00003-0450-51 ",.02)
 ;;00003-0450-51
 ;;9002226.02101,"635,00003-0450-54 ",.01)
 ;;00003-0450-54
 ;;9002226.02101,"635,00003-0450-54 ",.02)
 ;;00003-0450-54
 ;;9002226.02101,"635,00003-0450-75 ",.01)
 ;;00003-0450-75
 ;;9002226.02101,"635,00003-0450-75 ",.02)
 ;;00003-0450-75
 ;;9002226.02101,"635,00003-0452-50 ",.01)
 ;;00003-0452-50
 ;;9002226.02101,"635,00003-0452-50 ",.02)
 ;;00003-0452-50
 ;;9002226.02101,"635,00003-0452-51 ",.01)
 ;;00003-0452-51
 ;;9002226.02101,"635,00003-0452-51 ",.02)
 ;;00003-0452-51
 ;;9002226.02101,"635,00003-0452-75 ",.01)
 ;;00003-0452-75
 ;;9002226.02101,"635,00003-0452-75 ",.02)
 ;;00003-0452-75
 ;;9002226.02101,"635,00003-0482-50 ",.01)
 ;;00003-0482-50
 ;;9002226.02101,"635,00003-0482-50 ",.02)
 ;;00003-0482-50
 ;;9002226.02101,"635,00003-0482-51 ",.01)
 ;;00003-0482-51
 ;;9002226.02101,"635,00003-0482-51 ",.02)
 ;;00003-0482-51
 ;;9002226.02101,"635,00003-0482-75 ",.01)
 ;;00003-0482-75
 ;;9002226.02101,"635,00003-0482-75 ",.02)
 ;;00003-0482-75
 ;;9002226.02101,"635,00003-0485-50 ",.01)
 ;;00003-0485-50
 ;;9002226.02101,"635,00003-0485-50 ",.02)
 ;;00003-0485-50
 ;;9002226.02101,"635,00006-0014-28 ",.01)
 ;;00006-0014-28
 ;;9002226.02101,"635,00006-0014-28 ",.02)
 ;;00006-0014-28
 ;;9002226.02101,"635,00006-0014-68 ",.01)
 ;;00006-0014-68
 ;;9002226.02101,"635,00006-0014-68 ",.02)
 ;;00006-0014-68
 ;;9002226.02101,"635,00006-0014-82 ",.01)
 ;;00006-0014-82
 ;;9002226.02101,"635,00006-0014-82 ",.02)
 ;;00006-0014-82
 ;;9002226.02101,"635,00006-0014-87 ",.01)
 ;;00006-0014-87
 ;;9002226.02101,"635,00006-0014-87 ",.02)
 ;;00006-0014-87
 ;;9002226.02101,"635,00006-0014-94 ",.01)
 ;;00006-0014-94
 ;;9002226.02101,"635,00006-0014-94 ",.02)
 ;;00006-0014-94
 ;;9002226.02101,"635,00006-0015-28 ",.01)
 ;;00006-0015-28
 ;;9002226.02101,"635,00006-0015-28 ",.02)
 ;;00006-0015-28
 ;;9002226.02101,"635,00006-0015-31 ",.01)
 ;;00006-0015-31
 ;;9002226.02101,"635,00006-0015-31 ",.02)
 ;;00006-0015-31
 ;;9002226.02101,"635,00006-0015-58 ",.01)
 ;;00006-0015-58
 ;;9002226.02101,"635,00006-0015-58 ",.02)
 ;;00006-0015-58
 ;;9002226.02101,"635,00006-0015-72 ",.01)
 ;;00006-0015-72
 ;;9002226.02101,"635,00006-0015-72 ",.02)
 ;;00006-0015-72
 ;;9002226.02101,"635,00006-0019-28 ",.01)
 ;;00006-0019-28
 ;;9002226.02101,"635,00006-0019-28 ",.02)
 ;;00006-0019-28
 ;;9002226.02101,"635,00006-0019-54 ",.01)
 ;;00006-0019-54
 ;;9002226.02101,"635,00006-0019-54 ",.02)
 ;;00006-0019-54
 ;;9002226.02101,"635,00006-0019-58 ",.01)
 ;;00006-0019-58
 ;;9002226.02101,"635,00006-0019-58 ",.02)
 ;;00006-0019-58
 ;;9002226.02101,"635,00006-0019-72 ",.01)
 ;;00006-0019-72
 ;;9002226.02101,"635,00006-0019-72 ",.02)
 ;;00006-0019-72
 ;;9002226.02101,"635,00006-0019-82 ",.01)
 ;;00006-0019-82
 ;;9002226.02101,"635,00006-0019-82 ",.02)
 ;;00006-0019-82
 ;;9002226.02101,"635,00006-0019-86 ",.01)
 ;;00006-0019-86
 ;;9002226.02101,"635,00006-0019-86 ",.02)
 ;;00006-0019-86
 ;;9002226.02101,"635,00006-0019-87 ",.01)
 ;;00006-0019-87
 ;;9002226.02101,"635,00006-0019-87 ",.02)
 ;;00006-0019-87
 ;;9002226.02101,"635,00006-0019-94 ",.01)
 ;;00006-0019-94
 ;;9002226.02101,"635,00006-0019-94 ",.02)
 ;;00006-0019-94
 ;;9002226.02101,"635,00006-0106-28 ",.01)
 ;;00006-0106-28
 ;;9002226.02101,"635,00006-0106-28 ",.02)
 ;;00006-0106-28
 ;;9002226.02101,"635,00006-0106-31 ",.01)
 ;;00006-0106-31
 ;;9002226.02101,"635,00006-0106-31 ",.02)
 ;;00006-0106-31
 ;;9002226.02101,"635,00006-0106-54 ",.01)
 ;;00006-0106-54
 ;;9002226.02101,"635,00006-0106-54 ",.02)
 ;;00006-0106-54
 ;;9002226.02101,"635,00006-0106-58 ",.01)
 ;;00006-0106-58
 ;;9002226.02101,"635,00006-0106-58 ",.02)
 ;;00006-0106-58
 ;;9002226.02101,"635,00006-0106-72 ",.01)
 ;;00006-0106-72
 ;;9002226.02101,"635,00006-0106-72 ",.02)
 ;;00006-0106-72
 ;;9002226.02101,"635,00006-0106-82 ",.01)
 ;;00006-0106-82
 ;;9002226.02101,"635,00006-0106-82 ",.02)
 ;;00006-0106-82
 ;;9002226.02101,"635,00006-0106-86 ",.01)
 ;;00006-0106-86
 ;;9002226.02101,"635,00006-0106-86 ",.02)
 ;;00006-0106-86
 ;;9002226.02101,"635,00006-0106-87 ",.01)
 ;;00006-0106-87
 ;;9002226.02101,"635,00006-0106-87 ",.02)
 ;;00006-0106-87
 ;;9002226.02101,"635,00006-0106-94 ",.01)
 ;;00006-0106-94
 ;;9002226.02101,"635,00006-0106-94 ",.02)
 ;;00006-0106-94
 ;;9002226.02101,"635,00006-0140-31 ",.01)
 ;;00006-0140-31
 ;;9002226.02101,"635,00006-0140-31 ",.02)
 ;;00006-0140-31
 ;;9002226.02101,"635,00006-0140-58 ",.01)
 ;;00006-0140-58
