BGP8FXG ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON SEP 28, 2007 ;
 ;;8.0;IHS CLINICAL REPORTING;;MAR 12, 2008
 ;;;BGP6;;SEP 28, 2007
 ;;BGP HEDIS BENZODIAZEPINE NDC
 ;
 ; This routine loads Taxonomy BGP HEDIS BENZODIAZEPINE NDC
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
 ;;21,"00004-1932-09 ")
 ;;588
 ;;21,"00004-1933-06 ")
 ;;589
 ;;21,"00005-3129-23 ")
 ;;395
 ;;21,"00005-3129-31 ")
 ;;396
 ;;21,"00005-3130-23 ")
 ;;163
 ;;21,"00005-3130-31 ")
 ;;164
 ;;21,"00024-0376-02 ")
 ;;590
 ;;21,"00054-3185-44 ")
 ;;624
 ;;21,"00054-3188-63 ")
 ;;585
 ;;21,"00054-8207-16 ")
 ;;586
 ;;21,"00054-8208-16 ")
 ;;587
 ;;21,"00074-1273-02 ")
 ;;591
 ;;21,"00074-1273-12 ")
 ;;592
 ;;21,"00074-1273-22 ")
 ;;593
 ;;21,"00074-1273-32 ")
 ;;594
 ;;21,"00074-3210-01 ")
 ;;595
 ;;21,"00074-3210-32 ")
 ;;596
 ;;21,"00074-3213-01 ")
 ;;597
 ;;21,"00074-3213-02 ")
 ;;598
 ;;21,"00115-2758-01 ")
 ;;124
 ;;21,"00115-2758-03 ")
 ;;125
 ;;21,"00115-2760-01 ")
 ;;28
 ;;21,"00115-2760-03 ")
 ;;29
 ;;21,"00115-2762-01 ")
 ;;84
 ;;21,"00115-2762-03 ")
 ;;85
 ;;21,"00140-0001-01 ")
 ;;126
 ;;21,"00140-0001-49 ")
 ;;127
 ;;21,"00140-0001-50 ")
 ;;128
 ;;21,"00140-0002-01 ")
 ;;30
 ;;21,"00140-0002-14 ")
 ;;31
 ;;21,"00140-0002-49 ")
 ;;32
 ;;21,"00140-0002-50 ")
 ;;33
 ;;21,"00140-0003-01 ")
 ;;86
 ;;21,"00140-0003-14 ")
 ;;87
 ;;21,"00140-0003-49 ")
 ;;88
 ;;21,"00140-0003-50 ")
 ;;89
 ;;21,"00140-0004-01 ")
 ;;319
 ;;21,"00140-0004-14 ")
 ;;320
 ;;21,"00140-0004-49 ")
 ;;321
 ;;21,"00140-0004-50 ")
 ;;322
 ;;21,"00140-0005-01 ")
 ;;397
 ;;21,"00140-0005-14 ")
 ;;398
 ;;21,"00140-0005-49 ")
 ;;399
 ;;21,"00140-0005-50 ")
 ;;400
 ;;21,"00140-0006-01 ")
 ;;165
 ;;21,"00140-0006-14 ")
 ;;166
 ;;21,"00140-0006-49 ")
 ;;167
 ;;21,"00140-0006-50 ")
 ;;168
 ;;21,"00140-0013-01 ")
 ;;160
 ;;21,"00140-0014-01 ")
 ;;34
 ;;21,"00140-0015-01 ")
 ;;123
 ;;21,"00140-0065-01 ")
 ;;625
 ;;21,"00140-0065-14 ")
 ;;626
 ;;21,"00140-0066-01 ")
 ;;671
 ;;21,"00140-0066-14 ")
 ;;672
 ;;21,"00140-0070-01 ")
 ;;1
 ;;21,"00140-0070-49 ")
 ;;2
 ;;21,"00140-0071-01 ")
 ;;17
 ;;21,"00140-0071-49 ")
 ;;18
 ;;21,"00140-1931-06 ")
 ;;599
 ;;21,"00140-1932-06 ")
 ;;600
 ;;21,"00140-1933-06 ")
 ;;601
 ;;21,"00143-3367-01 ")
 ;;627
 ;;21,"00143-3367-05 ")
 ;;628
 ;;21,"00143-3370-01 ")
 ;;673
 ;;21,"00143-3370-05 ")
 ;;674
 ;;21,"00172-3925-60 ")
 ;;323
 ;;21,"00172-3925-70 ")
 ;;324
 ;;21,"00172-3925-80 ")
 ;;325
 ;;21,"00172-3926-60 ")
 ;;401
 ;;21,"00172-3926-70 ")
 ;;402
 ;;21,"00172-3926-80 ")
 ;;403
 ;;21,"00172-3927-60 ")
 ;;169
 ;;21,"00172-3927-70 ")
 ;;170
 ;;21,"00172-3927-80 ")
 ;;171
 ;;21,"00182-0977-01 ")
 ;;129
 ;;21,"00182-0977-89 ")
 ;;130
 ;;21,"00182-0978-01 ")
 ;;35
 ;;21,"00182-0978-05 ")
 ;;36
 ;;21,"00182-0978-10 ")
 ;;37
 ;;21,"00182-0978-89 ")
 ;;38
 ;;21,"00182-0979-01 ")
 ;;90
 ;;21,"00182-0979-89 ")
 ;;91
 ;;21,"00182-1757-01 ")
 ;;172
 ;;21,"00182-1757-05 ")
 ;;173
 ;;21,"00182-1757-10 ")
 ;;174
 ;;21,"00182-1757-89 ")
 ;;175
 ;;21,"00182-1817-01 ")
 ;;629
 ;;21,"00182-1817-05 ")
 ;;630
 ;;21,"00182-1818-01 ")
 ;;675
 ;;21,"00182-1818-05 ")
 ;;676
 ;;21,"00187-0658-20 ")
 ;;313
 ;;21,"00187-0659-20 ")
 ;;392
 ;;21,"00187-3750-10 ")
 ;;131
 ;;21,"00187-3751-10 ")
 ;;39
 ;;21,"00187-3755-74 ")
 ;;83
 ;;21,"00187-3758-10 ")
 ;;92
 ;;21,"00187-3805-10 ")
 ;;3
 ;;21,"00187-3806-10 ")
 ;;19
 ;;21,"00187-4051-10 ")
 ;;631
 ;;21,"00187-4052-10 ")
 ;;677
 ;;21,"00228-2051-10 ")
 ;;326
 ;;21,"00228-2051-50 ")
 ;;327
 ;;21,"00228-2052-10 ")
 ;;404
 ;;21,"00228-2052-50 ")
 ;;405
 ;;21,"00228-2052-96 ")
 ;;406
 ;;21,"00228-2053-10 ")
 ;;176
 ;;21,"00228-2053-50 ")
 ;;177
 ;;21,"00247-0187-00 ")
 ;;407
 ;;21,"00247-0187-02 ")
 ;;408
 ;;21,"00247-0187-03 ")
 ;;409
 ;;21,"00247-0187-04 ")
 ;;410
 ;;21,"00247-0187-06 ")
 ;;411
 ;;21,"00247-0187-07 ")
 ;;412
 ;;21,"00247-0187-10 ")
 ;;413
 ;;21,"00247-0187-12 ")
 ;;414
 ;;21,"00247-0187-14 ")
 ;;415
 ;;21,"00247-0187-15 ")
 ;;416
 ;;21,"00247-0187-20 ")
 ;;417
 ;;21,"00247-0187-21 ")
 ;;418
 ;;21,"00247-0187-28 ")
 ;;419
 ;;21,"00247-0187-30 ")
 ;;420
 ;;21,"00247-0187-50 ")
 ;;421
 ;;21,"00247-0187-60 ")
 ;;422
 ;;21,"00247-0187-98 ")
 ;;423
 ;;21,"00247-0294-02 ")
 ;;602
 ;;21,"00247-0294-20 ")
 ;;603
 ;;21,"00247-0345-00 ")
 ;;678
 ;;21,"00247-0345-02 ")
 ;;679
 ;;21,"00247-0345-03 ")
 ;;680
 ;;21,"00247-0345-07 ")
 ;;681
 ;;21,"00247-0345-10 ")
 ;;682
 ;;21,"00247-0345-15 ")
 ;;683
 ;;21,"00247-0345-20 ")
 ;;684
 ;;21,"00247-0345-30 ")
 ;;685
 ;;21,"00247-0345-98 ")
 ;;686
 ;;21,"00247-0493-00 ")
 ;;178
 ;;21,"00247-0493-02 ")
 ;;179
 ;;21,"00247-0493-03 ")
 ;;180
 ;;21,"00247-0493-06 ")
 ;;181
 ;;21,"00247-0493-07 ")
 ;;182
 ;;21,"00247-0493-10 ")
 ;;183
 ;;21,"00247-0493-12 ")
 ;;184
 ;;21,"00247-0493-14 ")
 ;;185
 ;;21,"00247-0493-20 ")
 ;;186
 ;;21,"00247-0493-30 ")
 ;;187
 ;;21,"00247-0939-03 ")
 ;;328
 ;;21,"00247-0939-06 ")
 ;;329
 ;;21,"00247-0939-15 ")
 ;;330
 ;;21,"00247-0939-30 ")
 ;;331
 ;;21,"00364-0774-01 ")
 ;;332
 ;;21,"00364-0774-02 ")
 ;;333
 ;;21,"00364-0774-05 ")
 ;;334
 ;;21,"00364-0775-01 ")
 ;;424
 ;;21,"00364-0775-02 ")
 ;;425
 ;;21,"00364-0775-05 ")
 ;;426
 ;;21,"00364-0776-01 ")
 ;;188
 ;;21,"00364-0776-02 ")
 ;;189
 ;;21,"00364-0776-05 ")
 ;;190
 ;;21,"00364-0801-05 ")
 ;;632
 ;;21,"00364-0825-48 ")
 ;;604
 ;;21,"00378-0211-01 ")
 ;;4
 ;;21,"00378-0211-05 ")
 ;;5
 ;;21,"00378-0271-01 ")
 ;;335
 ;;21,"00378-0271-05 ")
 ;;336
 ;;21,"00378-0277-01 ")
 ;;20
 ;;21,"00378-0277-05 ")
 ;;21
 ;;21,"00378-0345-01 ")
 ;;427
 ;;21,"00378-0345-05 ")
 ;;428
 ;;21,"00378-0477-01 ")
 ;;191
 ;;21,"00378-0477-05 ")
 ;;192
 ;;21,"00378-4415-01 ")
 ;;633
 ;;21,"00378-4415-05 ")
 ;;634
 ;;21,"00378-4430-01 ")
 ;;687
 ;;21,"00378-4430-05 ")
 ;;688
 ;;21,"00405-0040-01 ")
 ;;132
 ;;21,"00405-0040-02 ")
 ;;133
 ;;21,"00405-0041-01 ")
 ;;40
 ;;21,"00405-0041-03 ")
 ;;41
 ;;21,"00405-0042-01 ")
 ;;93
 ;;21,"00405-0042-02 ")
 ;;94
 ;;21,"00405-0068-01 ")
 ;;337
 ;;21,"00405-0068-02 ")
 ;;338
 ;;21,"00405-0068-03 ")
 ;;339
 ;;21,"00405-0069-01 ")
 ;;429
 ;;21,"00405-0069-02 ")
 ;;430
 ;;21,"00405-0069-03 ")
 ;;431
 ;;21,"00405-0070-01 ")
 ;;193
 ;;21,"00405-0070-02 ")
 ;;194
 ;;21,"00405-0085-01 ")
 ;;635
 ;;21,"00405-0085-02 ")
 ;;636
 ;;21,"00405-0086-01 ")
 ;;689
 ;;21,"00405-0086-02 ")
 ;;690
 ;;21,"00409-1273-32 ")
 ;;605
 ;;21,"00409-3213-02 ")
 ;;606
 ;;21,"00536-3487-05 ")
 ;;134
 ;;21,"00536-3489-05 ")
 ;;95
 ;;21,"00536-3796-01 ")
 ;;691
 ;;21,"00536-3796-05 ")
 ;;692
 ;;21,"00555-0033-02 ")
 ;;42
 ;;21,"00555-0033-05 ")
 ;;43
 ;;21,"00555-0158-02 ")
 ;;135
 ;;21,"00555-0158-04 ")
 ;;136
 ;;21,"00555-0159-02 ")
 ;;96
 ;;21,"00555-0159-04 ")
 ;;97
 ;;21,"00555-0163-02 ")
 ;;340
 ;;21,"00555-0163-05 ")
 ;;341
 ;;21,"00555-0164-02 ")
 ;;195
 ;;21,"00555-0164-05 ")
 ;;196
 ;;21,"00555-0363-02 ")
 ;;432
 ;;21,"00555-0363-05 ")
 ;;433
 ;;21,"00591-0785-01 ")
 ;;137
 ;;21,"00591-0785-05 ")
 ;;138
 ;;21,"00591-0786-01 ")
 ;;44
 ;;21,"00591-0786-05 ")
 ;;45
 ;;21,"00591-0786-10 ")
 ;;46
 ;;21,"00591-0787-01 ")
 ;;98
 ;;21,"00591-0787-05 ")
 ;;99
 ;;21,"00591-5619-01 ")
 ;;434
 ;;21,"00591-5619-05 ")
 ;;435
 ;;21,"00591-5619-10 ")
 ;;436
 ;;21,"00591-5620-01 ")
 ;;197
 ;;21,"00591-5620-05 ")
 ;;198
 ;;21,"00591-5620-10 ")
 ;;199
 ;;21,"00591-5621-01 ")
 ;;342
 ;;21,"00591-5621-05 ")
 ;;343
 ;;21,"00591-5621-10 ")
 ;;344
 ;;21,"00603-2666-32 ")
 ;;139
 ;;21,"00603-2667-21 ")
 ;;47
 ;;21,"00603-2690-21 ")
 ;;6
 ;;21,"00603-2690-28 ")
 ;;7
 ;;21,"00603-2691-21 ")
 ;;22
 ;;21,"00603-2691-28 ")
 ;;23
 ;;21,"00603-3213-21 ")
 ;;345
 ;;21,"00603-3213-28 ")
 ;;346
 ;;21,"00603-3214-21 ")
 ;;437
 ;;21,"00603-3214-28 ")
 ;;438
 ;;21,"00603-3214-32 ")
 ;;439
 ;;21,"00603-3215-21 ")
 ;;200
 ;;21,"00603-3215-28 ")
 ;;201
 ;;21,"00603-3215-32 ")
 ;;202
 ;;21,"00603-3216-28 ")
 ;;347
 ;;21,"00603-3217-28 ")
 ;;440
 ;;21,"00603-3218-28 ")
 ;;203
 ;;21,"00603-3691-21 ")
 ;;637
 ;;21,"00603-3692-28 ")
 ;;693
 ;;21,"00615-0435-05 ")
 ;;161
 ;;21,"00615-0435-10 ")
 ;;162
 ;;21,"00615-0436-01 ")
 ;;48
 ;;21,"00615-0436-10 ")
 ;;49
 ;;21,"00615-0437-01 ")
 ;;100
 ;;21,"00615-0437-05 ")
 ;;101
 ;;21,"00615-0437-10 ")
 ;;102
 ;;21,"00615-0461-47 ")
 ;;694
 ;;21,"00615-1532-53 ")
 ;;348
 ;;21,"00615-1532-63 ")
 ;;349
 ;;21,"00615-1533-01 ")
 ;;441
 ;;21,"00615-1533-10 ")
 ;;442
 ;;21,"00615-1533-32 ")
 ;;443
 ;;21,"00615-1533-53 ")
 ;;444
 ;;21,"00615-1533-63 ")
 ;;445
 ;;21,"00641-0371-25 ")
 ;;607
 ;;21,"00641-1408-33 ")
 ;;608
 ;;21,"00641-1408-35 ")
 ;;609
 ;;21,"00641-2289-41 ")
 ;;610
 ;;21,"00677-0457-01 ")
 ;;140
 ;;21,"00677-0459-01 ")
 ;;103
 ;;21,"00677-1088-21 ")
 ;;611
 ;;21,"00781-1482-01 ")
 ;;350
 ;;21,"00781-1482-05 ")
 ;;351
 ;;21,"00781-1482-13 ")
 ;;352
 ;;21,"00781-1483-01 ")
 ;;446
 ;;21,"00781-1483-05 ")
 ;;447
 ;;21,"00781-1483-13 ")
 ;;448
 ;;21,"00781-1484-01 ")
 ;;204
 ;;21,"00781-1484-05 ")
 ;;205
 ;;21,"00781-1484-13 ")
 ;;206
 ;;21,"00781-2080-01 ")
 ;;141
 ;;21,"00781-2080-05 ")
 ;;142
 ;;21,"00781-2080-10 ")
 ;;143
 ;;21,"00781-2082-05 ")
 ;;50
 ;;21,"00781-2082-10 ")
 ;;51
 ;;21,"00781-2084-01 ")
 ;;104
 ;;21,"00781-2084-05 ")
 ;;105
 ;;21,"00781-2084-10 ")
 ;;106
 ;;21,"00839-1130-06 ")
 ;;144
 ;;21,"00839-1130-16 ")
 ;;145
 ;;21,"00839-1131-06 ")
 ;;52
 ;;21,"00839-1131-16 ")
 ;;53
 ;;21,"00839-1132-06 ")
 ;;107
 ;;21,"00839-7131-06 ")
 ;;353
 ;;21,"00839-7131-16 ")
 ;;354
 ;;21,"00839-7132-06 ")
 ;;449
 ;;21,"00839-7132-16 ")
 ;;450
 ;;21,"00839-7133-06 ")
 ;;207
 ;;21,"00839-7133-12 ")
 ;;208
 ;;21,"00839-7133-16 ")
 ;;209
 ;;21,"00839-7154-06 ")
 ;;638
 ;;21,"00839-7154-12 ")
 ;;639
 ;;21,"00839-7155-06 ")
 ;;695
 ;;21,"00839-7155-12 ")
 ;;696
 ;;21,"00839-7190-30 ")
 ;;612
 ;;21,"00839-7279-06 ")
 ;;8
 ;;21,"00839-7279-12 ")
 ;;9
 ;;21,"00839-7280-06 ")
 ;;24
 ;;21,"00839-7280-12 ")
 ;;25
 ;;21,"00904-0090-40 ")
 ;;146
 ;;21,"00904-0090-60 ")
 ;;147
 ;;21,"00904-0090-80 ")
 ;;148
 ;;21,"00904-0091-40 ")
 ;;54
 ;;21,"00904-0091-60 ")
 ;;55
 ;;21,"00904-0091-80 ")
 ;;56
 ;;21,"00904-0092-40 ")
 ;;108
 ;;21,"00904-0092-60 ")
 ;;109
 ;;21,"00904-1700-40 ")
 ;;10
 ;;21,"00904-1700-60 ")
 ;;11
 ;;21,"00904-1701-40 ")
 ;;26
 ;;21,"00904-2800-40 ")
 ;;640
 ;;21,"00904-2800-60 ")
 ;;641
 ;;21,"00904-2800-61 ")
 ;;642
 ;;21,"00904-2801-40 ")
 ;;697
 ;;21,"00904-2801-60 ")
 ;;698
 ;;21,"00904-2801-61 ")
 ;;699
 ;;21,"00904-3901-40 ")
 ;;451
 ;;21,"00904-3901-60 ")
 ;;452
 ;;21,"00904-3901-61 ")
 ;;453
 ;;21,"00904-3901-80 ")
 ;;454
 ;;21,"00904-3902-40 ")
 ;;210
 ;;21,"00904-3902-60 ")
 ;;211
 ;;21,"00904-3902-61 ")
 ;;212
 ;;21,"00904-3902-80 ")
 ;;213
 ;;21,"00904-3903-40 ")
 ;;355
 ;;21,"00904-3903-60 ")
 ;;356
 ;;21,"00904-3903-80 ")
 ;;357
 ;;21,"10019-0004-44 ")
 ;;613
 ;;21,"10019-0005-42 ")
 ;;614
 ;;21,"10019-0005-67 ")
 ;;615
 ;;21,"12280-0006-00 ")
 ;;700
 ;;21,"12280-0219-00 ")
 ;;455
 ;;21,"49884-0958-01 ")
 ;;149
 ;;21,"49884-0959-01 ")
 ;;57
 ;;21,"49884-0960-01 ")
 ;;110
 ;;21,"49884-0961-01 ")
 ;;12
 ;;21,"49884-0962-01 ")
 ;;27
 ;;21,"51079-0141-20 ")
 ;;111
 ;;21,"51079-0284-20 ")
 ;;358
 ;;21,"51079-0284-21 ")
 ;;359
 ;;21,"51079-0285-20 ")
 ;;456
 ;;21,"51079-0285-21 ")
 ;;457
 ;;21,"51079-0286-20 ")
 ;;214
 ;;21,"51079-0286-21 ")
 ;;215
 ;;21,"51079-0302-20 ")
 ;;643
 ;;21,"51079-0302-21 ")
 ;;644
 ;;21,"51079-0303-20 ")
 ;;701
 ;;21,"51079-0303-21 ")
 ;;702
 ;;21,"51079-0374-20 ")
 ;;150
 ;;21,"51079-0374-21 ")
 ;;151
 ;;21,"51079-0375-20 ")
 ;;58
 ;;21,"51079-0375-21 ")
 ;;59
 ;;21,"51655-0801-24 ")
 ;;458
 ;;21,"51655-0801-25 ")
 ;;459
 ;;21,"51655-0801-26 ")
 ;;460
 ;;21,"51655-0801-82 ")
 ;;461
 ;;21,"51655-0833-24 ")
 ;;216
 ;;21,"51655-0833-25 ")
 ;;217
 ;;21,"51655-0833-26 ")
 ;;218
 ;;21,"51655-0833-82 ")
 ;;219
 ;;21,"52544-0785-01 ")
 ;;152
 ;;21,"52544-0785-05 ")
 ;;153
 ;;21,"52544-0786-01 ")
 ;;60
 ;;21,"52544-0786-05 ")
 ;;61
 ;;21,"52544-0786-10 ")
 ;;62
 ;;21,"52544-0787-01 ")
 ;;112
 ;;21,"52544-0787-05 ")
 ;;113
 ;;21,"52959-0047-03 ")
 ;;462
 ;;21,"52959-0047-05 ")
 ;;463
 ;;21,"52959-0047-06 ")
 ;;464
 ;;21,"52959-0047-10 ")
 ;;465
 ;;21,"52959-0047-12 ")
 ;;466
 ;;21,"52959-0047-15 ")
 ;;467
 ;;21,"52959-0047-20 ")
 ;;468
 ;;21,"52959-0047-21 ")
 ;;469
 ;;21,"52959-0047-25 ")
 ;;470
 ;;21,"52959-0047-30 ")
 ;;471
 ;;21,"52959-0047-45 ")
 ;;472
 ;;21,"52959-0047-50 ")
 ;;473
 ;;21,"52959-0047-60 ")
 ;;474
 ;;21,"52959-0236-60 ")
 ;;703
 ;;21,"52959-0295-30 ")
 ;;360
 ;;21,"52959-0295-50 ")
 ;;361
 ;;21,"52959-0306-06 ")
 ;;220
 ;;21,"52959-0306-20 ")
 ;;221
 ;;21,"52959-0306-30 ")
 ;;222
 ;
OTHER ; OTHER ROUTINES
 D ^BGP8FXGB
 D ^BGP8FXGC
 D ^BGP8FXGD
 D ^BGP8FXGE
 D ^BGP8FXGF
 D ^BGP8FXGG
 D ^BGP8FXGH
 D ^BGP8FXGI
 Q
