BGP6FXU ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON DEC 12, 2005 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;;;BGP6;;DEC 12, 2005
 ;;BGP HEDIS OSTEOPOROSIS NDC
 ;
 ; This routine loads Taxonomy BGP HEDIS OSTEOPOROSIS NDC
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
 ;;21,"00002-4165-02 ")
 ;;174
 ;;21,"00002-4165-07 ")
 ;;175
 ;;21,"00002-4165-30 ")
 ;;176
 ;;21,"00002-8971-01 ")
 ;;187
 ;;21,"00004-0185-23 ")
 ;;141
 ;;21,"00004-0186-82 ")
 ;;570
 ;;21,"00006-0031-21 ")
 ;;162
 ;;21,"00006-0031-44 ")
 ;;163
 ;;21,"00006-0077-21 ")
 ;;166
 ;;21,"00006-0077-44 ")
 ;;167
 ;;21,"00006-0212-31 ")
 ;;161
 ;;21,"00006-0710-21 ")
 ;;568
 ;;21,"00006-0710-44 ")
 ;;569
 ;;21,"00006-0925-31 ")
 ;;148
 ;;21,"00006-0925-58 ")
 ;;149
 ;;21,"00006-0936-28 ")
 ;;150
 ;;21,"00006-0936-31 ")
 ;;151
 ;;21,"00006-0936-58 ")
 ;;152
 ;;21,"00006-0936-72 ")
 ;;153
 ;;21,"00006-0936-82 ")
 ;;154
 ;;21,"00006-3833-34 ")
 ;;168
 ;;21,"00009-3772-01 ")
 ;;336
 ;;21,"00009-3772-17 ")
 ;;337
 ;;21,"00009-3773-01 ")
 ;;315
 ;;21,"00009-3773-17 ")
 ;;316
 ;;21,"00009-3774-01 ")
 ;;305
 ;;21,"00009-3774-17 ")
 ;;306
 ;;21,"00009-5174-01 ")
 ;;557
 ;;21,"00009-5174-02 ")
 ;;558
 ;;21,"00046-0864-81 ")
 ;;202
 ;;21,"00046-0865-81 ")
 ;;211
 ;;21,"00046-0865-91 ")
 ;;212
 ;;21,"00046-0866-81 ")
 ;;214
 ;;21,"00046-0866-91 ")
 ;;215
 ;;21,"00046-0866-95 ")
 ;;216
 ;;21,"00046-0866-99 ")
 ;;217
 ;;21,"00046-0867-81 ")
 ;;248
 ;;21,"00046-0867-91 ")
 ;;249
 ;;21,"00046-0867-95 ")
 ;;250
 ;;21,"00046-0867-99 ")
 ;;251
 ;;21,"00046-0868-81 ")
 ;;188
 ;;21,"00046-0868-91 ")
 ;;189
 ;;21,"00046-0875-01 ")
 ;;362
 ;;21,"00046-0875-02 ")
 ;;363
 ;;21,"00046-0875-05 ")
 ;;364
 ;;21,"00046-0875-06 ")
 ;;365
 ;;21,"00046-0936-81 ")
 ;;296
 ;;21,"00046-0936-99 ")
 ;;571
 ;;21,"00046-0937-04 ")
 ;;572
 ;;21,"00046-0937-09 ")
 ;;376
 ;;21,"00046-0938-08 ")
 ;;377
 ;;21,"00046-0938-09 ")
 ;;378
 ;;21,"00046-0975-02 ")
 ;;358
 ;;21,"00046-0975-05 ")
 ;;359
 ;;21,"00046-0975-06 ")
 ;;360
 ;;21,"00046-1104-81 ")
 ;;218
 ;;21,"00046-1104-91 ")
 ;;219
 ;;21,"00046-2572-01 ")
 ;;366
 ;;21,"00046-2573-01 ")
 ;;370
 ;;21,"00046-2573-02 ")
 ;;371
 ;;21,"00046-2573-05 ")
 ;;372
 ;;21,"00046-2573-06 ")
 ;;373
 ;;21,"00046-3867-81 ")
 ;;252
 ;;21,"00062-1800-01 ")
 ;;317
 ;;21,"00062-1801-01 ")
 ;;338
 ;;21,"00062-1840-01 ")
 ;;560
 ;;21,"00062-1840-15 ")
 ;;561
 ;;21,"00070-4492-01 ")
 ;;169
 ;;21,"00071-0144-23 ")
 ;;564
 ;;21,"00071-0144-45 ")
 ;;565
 ;;21,"00071-3006-03 ")
 ;;538
 ;;21,"00075-0514-08 ")
 ;;549
 ;;21,"00075-0525-08 ")
 ;;553
 ;;21,"00075-1306-01 ")
 ;;170
 ;;21,"00078-0149-23 ")
 ;;171
 ;;21,"00078-0311-54 ")
 ;;172
 ;;21,"00078-0311-90 ")
 ;;173
 ;;21,"00078-0343-42 ")
 ;;504
 ;;21,"00078-0343-45 ")
 ;;505
 ;;21,"00078-0344-42 ")
 ;;477
 ;;21,"00078-0344-45 ")
 ;;478
 ;;21,"00078-0345-42 ")
 ;;510
 ;;21,"00078-0345-45 ")
 ;;511
 ;;21,"00078-0346-42 ")
 ;;521
 ;;21,"00078-0346-45 ")
 ;;522
 ;;21,"00078-0348-42 ")
 ;;497
 ;;21,"00078-0348-44 ")
 ;;498
 ;;21,"00078-0348-62 ")
 ;;499
 ;;21,"00078-0365-42 ")
 ;;500
 ;;21,"00078-0365-45 ")
 ;;501
 ;;21,"00078-0377-42 ")
 ;;550
 ;;21,"00078-0377-45 ")
 ;;551
 ;;21,"00078-0377-62 ")
 ;;552
 ;;21,"00078-0378-42 ")
 ;;554
 ;;21,"00078-0378-45 ")
 ;;555
 ;;21,"00078-0378-62 ")
 ;;556
 ;;21,"00083-2310-01 ")
 ;;479
 ;;21,"00083-2310-08 ")
 ;;480
 ;;21,"00083-2310-62 ")
 ;;481
 ;;21,"00083-2320-01 ")
 ;;523
 ;;21,"00083-2320-08 ")
 ;;524
 ;;21,"00083-2320-62 ")
 ;;525
 ;;21,"00083-2325-01 ")
 ;;506
 ;;21,"00083-2325-08 ")
 ;;507
 ;;21,"00083-2325-62 ")
 ;;508
 ;;21,"00083-2326-01 ")
 ;;482
 ;;21,"00083-2326-08 ")
 ;;483
 ;;21,"00083-2326-25 ")
 ;;484
 ;;21,"00083-2326-62 ")
 ;;485
 ;;21,"00083-2327-01 ")
 ;;512
 ;;21,"00083-2327-08 ")
 ;;513
 ;;21,"00083-2327-25 ")
 ;;514
 ;;21,"00083-2327-62 ")
 ;;515
 ;;21,"00083-2328-01 ")
 ;;526
 ;;21,"00083-2328-08 ")
 ;;527
 ;;21,"00083-2328-25 ")
 ;;528
 ;;21,"00083-2328-62 ")
 ;;529
 ;;21,"00085-0070-03 ")
 ;;303
 ;;21,"00085-0070-06 ")
 ;;304
 ;;21,"00085-0150-03 ")
 ;;298
 ;;21,"00085-0298-03 ")
 ;;299
 ;;21,"00085-0298-06 ")
 ;;300
 ;;21,"00087-0021-41 ")
 ;;452
 ;;21,"00087-0755-01 ")
 ;;379
 ;;21,"00087-0755-48 ")
 ;;380
 ;;21,"00087-0756-01 ")
 ;;416
 ;;21,"00087-0756-48 ")
 ;;417
 ;;21,"00093-1057-01 ")
 ;;453
 ;;21,"00093-1058-01 ")
 ;;381
 ;;21,"00093-1059-01 ")
 ;;418
 ;;21,"00149-0470-01 ")
 ;;178
 ;;21,"00149-0470-03 ")
 ;;179
 ;;21,"00149-0471-01 ")
 ;;180
 ;;21,"00149-0471-03 ")
 ;;181
 ;;21,"00149-0472-01 ")
 ;;183
 ;;21,"00149-0491-01 ")
 ;;486
 ;;21,"00149-0491-03 ")
 ;;487
 ;;21,"00149-0491-04 ")
 ;;488
 ;;21,"00149-0491-05 ")
 ;;489
 ;;21,"00149-0491-07 ")
 ;;490
 ;;21,"00149-0492-01 ")
 ;;516
 ;;21,"00149-0492-04 ")
 ;;517
 ;;21,"00149-0493-01 ")
 ;;530
 ;;21,"00149-0493-04 ")
 ;;531
 ;;21,"00169-5174-01 ")
 ;;559
 ;;21,"00182-1976-01 ")
 ;;339
 ;;21,"00182-1977-01 ")
 ;;318
 ;;21,"00182-1978-01 ")
 ;;307
 ;;21,"00182-2648-01 ")
 ;;454
 ;;21,"00182-2649-01 ")
 ;;382
 ;;21,"00182-2650-01 ")
 ;;419
 ;;21,"00245-0008-35 ")
 ;;142
 ;;21,"00247-0242-30 ")
 ;;383
 ;;21,"00247-0243-21 ")
 ;;420
 ;;21,"00247-0243-30 ")
 ;;421
 ;;21,"00247-0249-00 ")
 ;;203
 ;;21,"00247-0249-30 ")
 ;;204
 ;;21,"00247-0250-00 ")
 ;;220
 ;;21,"00247-0250-28 ")
 ;;221
 ;;21,"00247-0250-30 ")
 ;;222
 ;;21,"00247-0251-00 ")
 ;;253
 ;;21,"00247-0251-30 ")
 ;;254
 ;;21,"00247-0251-60 ")
 ;;255
 ;;21,"00247-0251-90 ")
 ;;256
 ;;21,"00247-1226-30 ")
 ;;573
 ;;21,"00339-5981-12 ")
 ;;340
 ;;21,"00339-5983-12 ")
 ;;319
 ;;21,"00339-5985-12 ")
 ;;308
 ;;21,"00339-6073-12 ")
 ;;455
 ;;21,"00339-6074-12 ")
 ;;384
 ;;21,"00339-6075-12 ")
 ;;422
 ;;21,"00364-2600-01 ")
 ;;341
 ;;21,"00364-2601-01 ")
 ;;320
 ;;21,"00378-1452-01 ")
 ;;456
 ;;21,"00378-1452-05 ")
 ;;457
 ;;21,"00378-1454-01 ")
 ;;385
 ;;21,"00378-1454-05 ")
 ;;386
 ;;21,"00378-1458-01 ")
 ;;423
 ;;21,"00378-1458-05 ")
 ;;424
 ;;21,"00378-2252-99 ")
 ;;574
 ;;21,"00378-3349-99 ")
 ;;539
 ;;21,"00378-3350-99 ")
 ;;535
 ;;21,"00378-3351-99 ")
 ;;541
 ;;21,"00378-3352-99 ")
 ;;543
 ;;21,"00378-4551-01 ")
 ;;342
 ;;21,"00378-4553-01 ")
 ;;321
 ;;21,"00378-4555-01 ")
 ;;309
 ;;21,"00405-4413-01 ")
 ;;343
 ;;21,"00405-4414-01 ")
 ;;322
 ;;21,"00421-0158-01 ")
 ;;474
 ;;21,"00421-0748-01 ")
 ;;425
 ;;21,"00421-0768-01 ")
 ;;458
 ;;21,"00421-1259-01 ")
 ;;387
 ;;21,"00430-0021-24 ")
 ;;459
 ;;21,"00430-0023-24 ")
 ;;388
 ;;21,"00430-0023-30 ")
 ;;389
 ;;21,"00430-0024-24 ")
 ;;426
 ;;21,"00430-0024-30 ")
 ;;427
 ;;21,"00430-0544-14 ")
 ;;566
 ;;21,"00430-0544-23 ")
 ;;567
 ;;21,"00440-8170-30 ")
 ;;575
 ;;21,"00440-8170-60 ")
 ;;576
 ;;21,"00440-8170-90 ")
 ;;577
 ;;21,"00440-8171-30 ")
 ;;578
 ;;21,"00440-8171-60 ")
 ;;1
 ;;21,"00440-8171-90 ")
 ;;2
 ;;21,"00440-8172-30 ")
 ;;3
 ;;21,"00440-8172-60 ")
 ;;4
 ;;21,"00440-8172-90 ")
 ;;5
 ;;21,"00555-0727-02 ")
 ;;344
 ;;21,"00555-0728-02 ")
 ;;323
 ;;21,"00555-0729-02 ")
 ;;310
 ;;21,"00555-0886-02 ")
 ;;390
 ;;21,"00555-0886-04 ")
 ;;391
 ;;21,"00555-0887-02 ")
 ;;428
 ;;21,"00555-0887-04 ")
 ;;429
 ;;21,"00555-0899-02 ")
 ;;460
 ;;21,"00591-0414-01 ")
 ;;345
 ;;21,"00591-0415-01 ")
 ;;324
 ;;21,"00591-0416-01 ")
 ;;311
 ;;21,"00591-0487-01 ")
 ;;392
 ;;21,"00591-0487-05 ")
 ;;393
 ;;21,"00591-0488-01 ")
 ;;430
 ;;21,"00591-0488-05 ")
 ;;431
 ;;21,"00591-0528-01 ")
 ;;461
 ;;21,"00603-3556-21 ")
 ;;462
 ;;21,"00603-3557-21 ")
 ;;394
 ;;21,"00603-3558-21 ")
 ;;432
 ;;21,"00603-3559-21 ")
 ;;346
 ;;21,"00603-3560-21 ")
 ;;325
 ;;21,"00603-3561-21 ")
 ;;312
 ;;21,"00677-1508-01 ")
 ;;347
 ;;21,"00677-1509-01 ")
 ;;326
 ;;21,"00781-1543-01 ")
 ;;348
 ;;21,"00781-1553-01 ")
 ;;327
 ;;21,"00781-1563-01 ")
 ;;313
 ;;21,"00781-1897-01 ")
 ;;463
 ;;21,"00781-1898-01 ")
 ;;395
 ;;21,"00781-1899-01 ")
 ;;433
 ;;21,"00839-8076-06 ")
 ;;464
 ;;21,"00839-8077-06 ")
 ;;396
 ;;21,"00839-8078-06 ")
 ;;434
 ;;21,"00904-5177-60 ")
 ;;465
 ;;21,"00904-5178-60 ")
 ;;397
 ;;21,"00904-5179-60 ")
 ;;435
 ;;21,"12280-0039-00 ")
 ;;257
 ;;21,"12280-0110-00 ")
 ;;466
 ;;21,"22222-2000-01 ")
 ;;6
 ;;21,"44514-0493-18 ")
 ;;7
 ;;21,"49999-0083-30 ")
 ;;8
 ;;21,"49999-0109-30 ")
 ;;9
 ;;21,"49999-0109-90 ")
 ;;10
 ;;21,"50419-0451-04 ")
 ;;536
 ;;21,"50419-0452-04 ")
 ;;544
 ;;21,"50419-0453-04 ")
 ;;542
 ;;21,"50419-0454-04 ")
 ;;540
 ;;21,"50419-0455-04 ")
 ;;548
 ;;21,"50419-0456-04 ")
 ;;476
 ;;21,"50419-0459-04 ")
 ;;547
 ;;21,"51285-0010-01 ")
 ;;349
 ;;21,"51285-0010-02 ")
 ;;350
 ;;21,"51285-0011-02 ")
 ;;328
 ;;21,"51285-0088-90 ")
 ;;562
 ;;21,"51285-0441-02 ")
 ;;295
 ;;21,"51285-0441-05 ")
 ;;11
 ;;21,"51285-0441-30 ")
 ;;12
 ;;21,"51285-0442-02 ")
 ;;291
 ;;21,"51285-0442-05 ")
 ;;292
 ;;21,"51285-0442-30 ")
 ;;13
 ;;21,"51285-0443-02 ")
 ;;289
 ;;21,"51285-0443-05 ")
 ;;290
 ;;21,"51285-0443-30 ")
 ;;14
 ;;21,"51285-0444-02 ")
 ;;293
 ;;21,"51285-0444-05 ")
 ;;294
 ;;21,"51285-0444-30 ")
 ;;15
 ;;21,"51285-0446-02 ")
 ;;297
 ;;21,"51285-0501-02 ")
 ;;467
 ;;21,"51285-0502-02 ")
 ;;398
 ;;21,"51285-0502-04 ")
 ;;399
 ;;21,"51285-0504-02 ")
 ;;436
 ;;21,"51285-0504-04 ")
 ;;437
 ;;21,"51285-0875-02 ")
 ;;351
 ;;21,"51285-0876-02 ")
 ;;329
 ;;21,"51655-0452-25 ")
 ;;258
 ;;21,"52544-0414-01 ")
 ;;352
 ;;21,"52544-0415-01 ")
 ;;330
 ;;21,"52544-0416-01 ")
 ;;314
 ;;21,"52544-0471-08 ")
 ;;491
 ;;21,"52544-0471-23 ")
 ;;492
 ;;21,"52544-0472-08 ")
 ;;518
 ;;21,"52544-0473-08 ")
 ;;532
 ;;21,"52544-0487-01 ")
 ;;400
 ;;21,"52544-0487-05 ")
 ;;401
 ;;21,"52544-0488-01 ")
 ;;438
 ;;21,"52544-0488-05 ")
 ;;439
 ;;21,"52544-0528-01 ")
 ;;468
 ;;21,"52544-0884-08 ")
 ;;502
 ;;21,"52544-0884-23 ")
 ;;16
 ;;21,"52555-0649-01 ")
 ;;469
 ;;21,"52555-0650-01 ")
 ;;402
 ;;21,"52555-0650-05 ")
 ;;403
 ;;21,"52555-0651-01 ")
 ;;440
 ;;21,"52555-0716-01 ")
 ;;470
 ;;21,"52555-0717-01 ")
 ;;404
 ;;21,"52555-0717-05 ")
 ;;17
 ;;21,"52555-0718-01 ")
 ;;441
 ;;21,"52959-0222-00 ")
 ;;223
 ;;21,"52959-0223-00 ")
 ;;259
 ;;21,"52959-0223-30 ")
 ;;260
 ;;21,"52959-0323-00 ")
 ;;442
 ;;21,"52959-0326-01 ")
 ;;18
 ;;21,"52959-0326-10 ")
 ;;353
 ;;21,"54348-0645-07 ")
 ;;19
 ;;21,"54348-0645-21 ")
 ;;20
 ;;21,"54348-0714-21 ")
 ;;21
 ;;21,"54348-0719-30 ")
 ;;22
 ;;21,"54348-0723-30 ")
 ;;23
 ;;21,"54569-0802-01 ")
 ;;405
 ;;21,"54569-0804-00 ")
 ;;493
 ;;21,"54569-0811-01 ")
 ;;190
 ;;21,"54569-0811-03 ")
 ;;24
 ;;21,"54569-0812-00 ")
 ;;261
 ;;21,"54569-0812-01 ")
 ;;262
 ;;21,"54569-0812-02 ")
 ;;263
 ;;21,"54569-0812-05 ")
 ;;264
 ;;21,"54569-0812-06 ")
 ;;25
 ;;21,"54569-0813-00 ")
 ;;224
 ;;21,"54569-0813-01 ")
 ;;225
 ;;21,"54569-0849-00 ")
 ;;205
 ;;21,"54569-0849-01 ")
 ;;206
 ;;21,"54569-4365-00 ")
 ;;26
 ;;21,"54569-4366-00 ")
 ;;27
 ;;21,"54569-4367-00 ")
 ;;545
 ;;21,"54569-4367-01 ")
 ;;546
 ;;21,"54569-4618-00 ")
 ;;367
 ;;21,"54569-4628-00 ")
 ;;177
 ;;21,"54569-4673-00 ")
 ;;374
 ;;21,"54569-4711-00 ")
 ;;537
 ;;21,"54569-4866-00 ")
 ;;155
 ;;21,"54569-4907-00 ")
 ;;406
 ;;21,"54569-4908-00 ")
 ;;443
 ;;21,"54569-4925-00 ")
 ;;361
 ;;21,"54569-5152-00 ")
 ;;28
 ;;21,"54569-5153-00 ")
 ;;29
 ;;21,"54569-5164-00 ")
 ;;30
 ;;21,"54569-5218-00 ")
 ;;31
 ;;21,"54569-5380-00 ")
 ;;32
 ;;21,"54569-5399-00 ")
 ;;33
 ;;21,"54569-5462-00 ")
 ;;184
 ;;21,"54569-5581-00 ")
 ;;34
 ;;21,"54569-8006-00 ")
 ;;265
 ;;21,"54569-8006-01 ")
 ;;266
 ;;21,"54569-8006-02 ")
 ;;267
 ;;21,"54569-8014-00 ")
 ;;226
 ;;21,"54569-8500-00 ")
 ;;268
 ;;21,"54569-8500-01 ")
 ;;269
 ;;21,"54569-8500-02 ")
 ;;270
 ;;21,"54569-8505-00 ")
 ;;227
 ;;21,"54569-8505-01 ")
 ;;228
 ;;21,"54569-8505-02 ")
 ;;229
 ;;21,"54569-8517-00 ")
 ;;191
 ;;21,"54569-8517-01 ")
 ;;192
 ;;21,"54569-8518-00 ")
 ;;207
 ;;21,"54569-8518-01 ")
 ;;208
 ;;21,"54569-8525-00 ")
 ;;354
 ;;21,"54569-8528-00 ")
 ;;407
 ;;21,"54569-8551-00 ")
 ;;331
 ;;21,"54569-8577-00 ")
 ;;444
 ;;21,"54868-0365-00 ")
 ;;209
 ;;21,"54868-0365-02 ")
 ;;35
 ;;21,"54868-0365-03 ")
 ;;36
 ;;21,"54868-0451-00 ")
 ;;271
 ;;21,"54868-0451-01 ")
 ;;272
 ;;21,"54868-0451-02 ")
 ;;273
 ;;21,"54868-0451-03 ")
 ;;274
 ;;21,"54868-0451-06 ")
 ;;37
 ;;21,"54868-0452-03 ")
 ;;213
 ;;21,"54868-0453-00 ")
 ;;230
 ;;21,"54868-0453-01 ")
 ;;231
 ;;21,"54868-0453-02 ")
 ;;232
 ;;21,"54868-0453-04 ")
 ;;233
 ;;21,"54868-0453-05 ")
 ;;234
 ;;21,"54868-0494-00 ")
 ;;38
 ;;21,"54868-0494-01 ")
 ;;39
 ;;21,"54868-0494-02 ")
 ;;40
 ;;21,"54868-0495-00 ")
 ;;41
 ;;21,"54868-1261-00 ")
 ;;332
 ;;21,"54868-1262-00 ")
 ;;355
 ;;21,"54868-1262-01 ")
 ;;42
 ;;21,"54868-1262-02 ")
 ;;43
 ;;21,"54868-2702-00 ")
 ;;193
 ;;21,"54868-2702-01 ")
 ;;44
 ;
OTHER ; OTHER ROUTINES
 D ^BGP6FXUB
 D ^BGP6FXUC
 D ^BGP6FXUD
 D ^BGP6FXUE
 D ^BGP6FXUF
 D ^BGP6FXUG
 Q
