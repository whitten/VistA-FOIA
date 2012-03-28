BGP6BXB ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON AUG 21, 2005 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;;;BGP6;;AUG 21, 2005
 ;;BGP CMS BETA BLOCKER NDC
 ;
 ; This routine loads Taxonomy BGP CMS BETA BLOCKER NDC
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
 ;;21,"00005-3816-38 ")
 ;;1
 ;;21,"00005-3817-38 ")
 ;;2
 ;;21,"00007-4139-20 ")
 ;;3
 ;;21,"00007-4140-20 ")
 ;;4
 ;;21,"00007-4141-20 ")
 ;;5
 ;;21,"00007-4142-20 ")
 ;;6
 ;;21,"00008-4177-01 ")
 ;;7
 ;;21,"00008-4179-01 ")
 ;;8
 ;;21,"00025-5101-31 ")
 ;;9
 ;;21,"00025-5201-31 ")
 ;;10
 ;;21,"00028-0051-01 ")
 ;;11
 ;;21,"00028-0051-10 ")
 ;;12
 ;;21,"00028-0071-01 ")
 ;;13
 ;;21,"00028-0071-10 ")
 ;;14
 ;;21,"00046-0421-81 ")
 ;;15
 ;;21,"00046-0422-81 ")
 ;;16
 ;;21,"00046-0424-81 ")
 ;;17
 ;;21,"00046-0426-81 ")
 ;;18
 ;;21,"00046-0470-81 ")
 ;;19
 ;;21,"00046-0473-81 ")
 ;;20
 ;;21,"00046-0479-81 ")
 ;;21
 ;;21,"00046-0484-81 ")
 ;;22
 ;;21,"00074-1664-13 ")
 ;;23
 ;;21,"00083-0133-11 ")
 ;;24
 ;;21,"00085-0244-04 ")
 ;;25
 ;;21,"00085-0244-05 ")
 ;;26
 ;;21,"00085-0244-07 ")
 ;;27
 ;;21,"00085-0244-08 ")
 ;;28
 ;;21,"00085-0438-03 ")
 ;;29
 ;;21,"00085-0438-05 ")
 ;;30
 ;;21,"00085-0438-06 ")
 ;;31
 ;;21,"00085-0752-04 ")
 ;;32
 ;;21,"00085-0752-05 ")
 ;;33
 ;;21,"00085-0752-08 ")
 ;;34
 ;;21,"00091-4500-15 ")
 ;;35
 ;;21,"00093-0733-01 ")
 ;;36
 ;;21,"00093-0733-10 ")
 ;;37
 ;;21,"00093-0734-01 ")
 ;;38
 ;;21,"00093-0734-10 ")
 ;;39
 ;;21,"00093-0752-01 ")
 ;;40
 ;;21,"00093-0752-10 ")
 ;;41
 ;;21,"00093-0753-01 ")
 ;;42
 ;;21,"00093-1060-01 ")
 ;;43
 ;;21,"00093-1061-01 ")
 ;;44
 ;;21,"00093-1062-01 ")
 ;;45
 ;;21,"00093-1063-01 ")
 ;;46
 ;;21,"00093-5270-56 ")
 ;;47
 ;;21,"00115-2101-01 ")
 ;;48
 ;;21,"00115-2711-01 ")
 ;;49
 ;;21,"00115-2722-01 ")
 ;;50
 ;;21,"00115-2733-01 ")
 ;;51
 ;;21,"00115-2744-01 ")
 ;;52
 ;;21,"00158-0171-01 ")
 ;;53
 ;;21,"00172-4217-60 ")
 ;;54
 ;;21,"00172-4218-60 ")
 ;;55
 ;;21,"00172-4235-60 ")
 ;;56
 ;;21,"00172-4235-70 ")
 ;;57
 ;;21,"00172-4236-60 ")
 ;;58
 ;;21,"00172-4237-60 ")
 ;;59
 ;;21,"00172-4238-60 ")
 ;;60
 ;;21,"00172-4239-60 ")
 ;;61
 ;;21,"00172-4364-60 ")
 ;;62
 ;;21,"00172-4364-70 ")
 ;;63
 ;;21,"00172-4365-60 ")
 ;;64
 ;;21,"00172-4365-70 ")
 ;;65
 ;;21,"00172-4366-60 ")
 ;;66
 ;;21,"00182-1001-01 ")
 ;;67
 ;;21,"00182-1812-89 ")
 ;;68
 ;;21,"00182-1813-89 ")
 ;;69
 ;;21,"00182-1814-89 ")
 ;;70
 ;;21,"00182-1815-89 ")
 ;;71
 ;;21,"00182-2632-89 ")
 ;;72
 ;;21,"00185-0010-01 ")
 ;;73
 ;;21,"00185-0010-05 ")
 ;;74
 ;;21,"00185-0117-01 ")
 ;;75
 ;;21,"00185-0117-05 ")
 ;;76
 ;;21,"00185-0118-01 ")
 ;;77
 ;;21,"00185-0118-05 ")
 ;;78
 ;;21,"00185-0170-01 ")
 ;;79
 ;;21,"00185-0174-01 ")
 ;;80
 ;;21,"00185-0177-01 ")
 ;;81
 ;;21,"00185-0771-01 ")
 ;;82
 ;;21,"00185-0771-30 ")
 ;;83
 ;;21,"00185-0774-01 ")
 ;;84
 ;;21,"00185-0774-30 ")
 ;;85
 ;;21,"00223-2551-01 ")
 ;;86
 ;;21,"00223-2551-02 ")
 ;;87
 ;;21,"00223-2552-01 ")
 ;;88
 ;;21,"00223-2552-02 ")
 ;;89
 ;;21,"00223-2553-01 ")
 ;;90
 ;;21,"00223-2553-02 ")
 ;;91
 ;;21,"00223-2554-01 ")
 ;;92
 ;;21,"00223-2554-02 ")
 ;;93
 ;;21,"00364-2514-01 ")
 ;;94
 ;;21,"00378-0028-01 ")
 ;;95
 ;;21,"00378-0032-01 ")
 ;;96
 ;;21,"00378-0032-10 ")
 ;;97
 ;;21,"00378-0047-01 ")
 ;;98
 ;;21,"00378-0047-10 ")
 ;;99
 ;;21,"00378-0052-01 ")
 ;;100
 ;;21,"00378-0127-01 ")
 ;;101
 ;;21,"00378-0182-01 ")
 ;;102
 ;;21,"00378-0182-10 ")
 ;;103
 ;;21,"00378-0183-01 ")
 ;;104
 ;;21,"00378-0183-10 ")
 ;;105
 ;;21,"00378-0184-01 ")
 ;;106
 ;;21,"00378-0185-01 ")
 ;;107
 ;;21,"00378-0185-05 ")
 ;;108
 ;;21,"00378-0231-01 ")
 ;;109
 ;;21,"00378-0231-10 ")
 ;;110
 ;;21,"00378-0305-01 ")
 ;;111
 ;;21,"00378-0310-01 ")
 ;;112
 ;;21,"00378-0314-01 ")
 ;;113
 ;;21,"00378-0434-01 ")
 ;;114
 ;;21,"00378-0445-01 ")
 ;;115
 ;;21,"00378-0757-01 ")
 ;;116
 ;;21,"00378-1132-01 ")
 ;;117
 ;;21,"00378-1132-10 ")
 ;;118
 ;;21,"00378-1171-01 ")
 ;;119
 ;;21,"00378-1171-10 ")
 ;;120
 ;;21,"00378-1200-01 ")
 ;;121
 ;;21,"00378-1400-01 ")
 ;;122
 ;;21,"00591-0438-01 ")
 ;;123
 ;;21,"00591-0462-01 ")
 ;;124
 ;;21,"00591-0462-10 ")
 ;;125
 ;;21,"00591-0463-01 ")
 ;;126
 ;;21,"00591-0606-01 ")
 ;;127
 ;;21,"00591-0606-05 ")
 ;;128
 ;;21,"00591-0607-01 ")
 ;;129
 ;;21,"00591-5554-01 ")
 ;;130
 ;;21,"00591-5554-10 ")
 ;;131
 ;;21,"00591-5555-01 ")
 ;;132
 ;;21,"00591-5555-10 ")
 ;;133
 ;;21,"00591-5556-01 ")
 ;;134
 ;;21,"00591-5556-10 ")
 ;;135
 ;;21,"00591-5557-01 ")
 ;;136
 ;;21,"00591-5557-10 ")
 ;;137
 ;;21,"00591-5777-01 ")
 ;;138
 ;;21,"00603-4627-21 ")
 ;;139
 ;;21,"00603-4627-32 ")
 ;;140
 ;;21,"00603-4628-21 ")
 ;;141
 ;;21,"00603-4628-32 ")
 ;;142
 ;;21,"00603-5221-01 ")
 ;;143
 ;;21,"00615-2561-53 ")
 ;;144
 ;;21,"00615-2561-63 ")
 ;;145
 ;;21,"00615-2562-53 ")
 ;;146
 ;;21,"00615-2562-63 ")
 ;;147
 ;;21,"00615-3532-53 ")
 ;;148
 ;;21,"00615-3532-63 ")
 ;;149
 ;;21,"00615-3544-53 ")
 ;;150
 ;;21,"00615-3544-63 ")
 ;;151
 ;;21,"00615-3552-43 ")
 ;;152
 ;;21,"00615-3552-53 ")
 ;;153
 ;;21,"00615-3552-63 ")
 ;;154
 ;;21,"00615-3553-53 ")
 ;;155
 ;;21,"00615-3553-63 ")
 ;;156
 ;;21,"00677-1458-01 ")
 ;;157
 ;;21,"00677-1478-01 ")
 ;;158
 ;;21,"00677-1478-10 ")
 ;;159
 ;;21,"00677-1479-01 ")
 ;;160
 ;;21,"00677-1479-10 ")
 ;;161
 ;;21,"00677-1482-01 ")
 ;;162
 ;;21,"00677-1482-10 ")
 ;;163
 ;;21,"00677-1483-01 ")
 ;;164
 ;;21,"00677-1633-01 ")
 ;;165
 ;;21,"00677-1701-01 ")
 ;;166
 ;;21,"00677-1701-05 ")
 ;;167
 ;;21,"00677-1702-01 ")
 ;;168
 ;;21,"00677-1702-05 ")
 ;;169
 ;;21,"00677-1703-01 ")
 ;;170
 ;;21,"00677-1709-01 ")
 ;;171
 ;;21,"00677-1709-06 ")
 ;;172
 ;;21,"00677-1709-07 ")
 ;;173
 ;;21,"00677-1710-06 ")
 ;;174
 ;;21,"00677-1711-06 ")
 ;;175
 ;;21,"00677-1712-07 ")
 ;;176
 ;;21,"00677-1893-01 ")
 ;;177
 ;;21,"00677-1894-01 ")
 ;;178
 ;;21,"00677-1894-06 ")
 ;;179
 ;;21,"00677-1895-01 ")
 ;;180
 ;;21,"00677-1895-06 ")
 ;;181
 ;;21,"00781-1078-01 ")
 ;;182
 ;;21,"00781-1506-01 ")
 ;;183
 ;;21,"00781-1506-10 ")
 ;;184
 ;;21,"00781-1507-01 ")
 ;;185
 ;;21,"00781-1507-10 ")
 ;;186
 ;;21,"00891-0605-01 ")
 ;;187
 ;;21,"00891-0605-05 ")
 ;;188
 ;;21,"00904-0411-61 ")
 ;;189
 ;;21,"00904-0411-80 ")
 ;;190
 ;;21,"00904-0412-60 ")
 ;;191
 ;;21,"00904-0412-61 ")
 ;;192
 ;;21,"00904-0412-80 ")
 ;;193
 ;;21,"00904-0414-60 ")
 ;;194
 ;;21,"00904-0418-60 ")
 ;;195
 ;;21,"00904-5248-05 ")
 ;;196
 ;;21,"00904-5248-10 ")
 ;;197
 ;;21,"00904-5392-60 ")
 ;;198
 ;;21,"00904-7772-61 ")
 ;;199
 ;;21,"00904-7773-61 ")
 ;;200
 ;;21,"00904-7946-60 ")
 ;;201
 ;;21,"00904-7946-80 ")
 ;;202
 ;;21,"00904-7947-80 ")
 ;;203
 ;;21,"00904-7987-60 ")
 ;;204
 ;;21,"009047-772-61 ")
 ;;205
 ;;21,"17478-0288-10 ")
 ;;206
 ;;21,"17478-0288-11 ")
 ;;207
 ;;21,"17478-0710-10 ")
 ;;208
 ;;21,"17478-0710-11 ")
 ;;209
 ;;21,"20419-0105-11 ")
 ;;210
 ;;21,"24208-0324-10 ")
 ;;211
 ;;21,"24208-0330-05 ")
 ;;212
 ;;21,"24208-0330-10 ")
 ;;213
 ;;21,"24208-0357-15 ")
 ;;214
 ;;21,"24208-0367-05 ")
 ;;215
 ;;21,"24208-0367-10 ")
 ;;216
 ;;21,"38245-0724-10 ")
 ;;217
 ;;21,"38245-0731-10 ")
 ;;218
 ;;21,"49884-0555-01 ")
 ;;219
 ;;21,"49884-0582-01 ")
 ;;220
 ;;21,"49884-0582-10 ")
 ;;221
 ;;21,"49884-0583-01 ")
 ;;222
 ;;21,"49884-0583-10 ")
 ;;223
 ;;21,"49884-0584-01 ")
 ;;224
 ;;21,"49884-0584-10 ")
 ;;225
 ;;21,"49884-0585-01 ")
 ;;226
 ;;21,"49884-0585-10 ")
 ;;227
 ;;21,"49884-0587-01 ")
 ;;228
 ;;21,"50111-0467-01 ")
 ;;229
 ;;21,"50111-0467-03 ")
 ;;230
 ;;21,"50111-0468-01 ")
 ;;231
 ;;21,"50111-0468-03 ")
 ;;232
 ;;21,"50111-0470-01 ")
 ;;233
 ;;21,"50111-0471-01 ")
 ;;234
 ;;21,"50111-0471-02 ")
 ;;235
 ;;21,"50419-0105-10 ")
 ;;236
 ;;21,"50419-0106-10 ")
 ;;237
 ;;21,"50419-0106-11 ")
 ;;238
 ;;21,"50419-0107-10 ")
 ;;239
 ;;21,"50419-0107-11 ")
 ;;240
 ;;21,"50419-0109-10 ")
 ;;241
 ;;21,"50419-0109-11 ")
 ;;242
 ;;21,"50419-0115-06 ")
 ;;243
 ;;21,"50419-0115-11 ")
 ;;244
 ;;21,"50419-0116-06 ")
 ;;245
 ;;21,"50419-0116-11 ")
 ;;246
 ;;21,"50419-0119-06 ")
 ;;247
 ;;21,"50419-0119-11 ")
 ;;248
 ;;21,"51079-0277-19 ")
 ;;249
 ;;21,"51079-0277-20 ")
 ;;250
 ;;21,"51079-0278-20 ")
 ;;251
 ;;21,"51079-0279-20 ")
 ;;252
 ;;21,"51079-0280-20 ")
 ;;253
 ;;21,"51079-0684-19 ")
 ;;254
 ;;21,"51079-0684-20 ")
 ;;255
 ;;21,"51079-0685-20 ")
 ;;256
 ;;21,"51079-0759-19 ")
 ;;257
 ;;21,"51079-0801-19 ")
 ;;258
 ;;21,"51079-0801-20 ")
 ;;259
 ;;21,"51079-0801-24 ")
 ;;260
 ;;21,"51079-0802-19 ")
 ;;261
 ;;21,"51079-0802-20 ")
 ;;262
 ;;21,"51079-0812-20 ")
 ;;263
 ;;21,"51079-0813-20 ")
 ;;264
 ;;21,"51079-0814-20 ")
 ;;265
 ;;21,"51079-0815-57 ")
 ;;266
 ;;21,"51079-0928-20 ")
 ;;267
 ;;21,"51079-0929-20 ")
 ;;268
 ;;21,"51655-0349-24 ")
 ;;269
 ;;21,"51655-0532-24 ")
 ;;270
 ;;21,"52152-0179-02 ")
 ;;271
 ;;21,"52152-0180-02 ")
 ;;272
 ;;21,"52544-0305-01 ")
 ;;273
 ;;21,"52544-0305-10 ")
 ;;274
 ;;21,"52544-0306-01 ")
 ;;275
 ;;21,"52544-0307-01 ")
 ;;276
 ;;21,"52544-0307-10 ")
 ;;277
 ;;21,"52544-0308-01 ")
 ;;278
 ;;21,"52544-0308-10 ")
 ;;279
 ;;21,"52544-0352-01 ")
 ;;280
 ;;21,"52544-0462-01 ")
 ;;281
 ;;21,"52544-0462-10 ")
 ;;282
 ;;21,"52544-0463-01 ")
 ;;283
 ;;21,"52544-0463-10 ")
 ;;284
 ;;21,"52544-0603-01 ")
 ;;285
 ;;21,"52544-0605-01 ")
 ;;286
 ;;21,"52544-0605-05 ")
 ;;287
 ;;21,"52544-0654-01 ")
 ;;288
 ;;21,"52544-0655-01 ")
 ;;289
 ;;21,"52544-0656-01 ")
 ;;290
 ;;21,"52544-0665-01 ")
 ;;291
 ;;21,"52555-0454-01 ")
 ;;292
 ;;21,"52555-0455-01 ")
 ;;293
 ;;21,"52555-0673-10 ")
 ;;294
 ;;21,"52555-0674-01 ")
 ;;295
 ;;21,"52959-0212-10 ")
 ;;296
 ;;21,"52959-0212-20 ")
 ;;297
 ;;21,"52959-0253-00 ")
 ;;298
 ;;21,"52959-0253-30 ")
 ;;299
 ;;21,"52959-0253-40 ")
 ;;300
 ;;21,"52959-0463-01 ")
 ;;301
 ;;21,"53489-0354-01 ")
 ;;302
 ;;21,"53489-0354-05 ")
 ;;303
 ;;21,"53489-0355-05 ")
 ;;304
 ;;21,"53489-0356-01 ")
 ;;305
 ;;21,"53489-0356-05 ")
 ;;306
 ;;21,"53489-0366-01 ")
 ;;307
 ;;21,"53489-0367-10 ")
 ;;308
 ;;21,"53489-0430-01 ")
 ;;309
 ;;21,"53489-0431-01 ")
 ;;310
 ;;21,"53489-0529-01 ")
 ;;311
 ;;21,"53489-0529-10 ")
 ;;312
 ;;21,"53489-0530-01 ")
 ;;313
 ;;21,"53489-0530-10 ")
 ;;314
 ;;21,"54569-0442-00 ")
 ;;315
 ;;21,"54569-0557-00 ")
 ;;316
 ;;21,"54569-0557-01 ")
 ;;317
 ;;21,"54569-0557-03 ")
 ;;318
 ;;21,"54569-0559-00 ")
 ;;319
 ;;21,"54569-0559-01 ")
 ;;320
 ;;21,"54569-0559-03 ")
 ;;321
 ;;21,"54569-0561-01 ")
 ;;322
 ;;21,"54569-0561-02 ")
 ;;323
 ;;21,"54569-0561-03 ")
 ;;324
 ;;21,"54569-2499-00 ")
 ;;325
 ;;21,"54569-3432-00 ")
 ;;326
 ;;21,"54569-3432-01 ")
 ;;327
 ;;21,"54569-3432-03 ")
 ;;328
 ;;21,"54569-3432-04 ")
 ;;329
 ;;21,"54569-3432-05 ")
 ;;330
 ;;21,"54569-3654-00 ")
 ;;331
 ;;21,"54569-3654-03 ")
 ;;332
 ;;21,"54569-3787-00 ")
 ;;333
 ;;21,"54569-3787-01 ")
 ;;334
 ;;21,"54569-3787-02 ")
 ;;335
 ;;21,"54569-3790-00 ")
 ;;336
 ;;21,"54569-3885-00 ")
 ;;337
 ;;21,"54569-3885-02 ")
 ;;338
 ;;21,"54569-4288-00 ")
 ;;339
 ;;21,"54569-8574-00 ")
 ;;340
 ;;21,"54569-8591-00 ")
 ;;341
 ;;21,"54868-0052-02 ")
 ;;342
 ;;21,"54868-0053-07 ")
 ;;343
 ;;21,"54868-0293-00 ")
 ;;344
 ;;21,"54868-0293-01 ")
 ;;345
 ;;21,"54868-0293-03 ")
 ;;346
 ;;21,"54868-0293-04 ")
 ;;347
 ;;21,"54868-0696-01 ")
 ;;348
 ;;21,"54868-1078-01 ")
 ;;349
 ;;21,"54868-1078-03 ")
 ;;350
 ;;21,"54868-1871-00 ")
 ;;351
 ;;21,"54868-1871-01 ")
 ;;352
 ;;21,"54868-1871-02 ")
 ;;353
 ;;21,"54868-1971-00 ")
 ;;354
 ;;21,"54868-1971-01 ")
 ;;355
 ;;21,"54868-1971-03 ")
 ;;356
 ;;21,"54868-2349-02 ")
 ;;357
 ;;21,"54868-2349-03 ")
 ;;358
 ;;21,"54868-2989-00 ")
 ;;359
 ;;21,"54868-2989-02 ")
 ;;360
 ;;21,"54868-2989-03 ")
 ;;361
 ;;21,"54868-2990-00 ")
 ;;362
 ;;21,"54868-2990-02 ")
 ;;363
 ;;21,"54868-3257-01 ")
 ;;364
 ;;21,"54868-3713-00 ")
 ;;365
 ;;21,"54868-4435-00 ")
 ;;366
 ;;21,"55289-0093-30 ")
 ;;367
 ;;21,"55289-0227-30 ")
 ;;368
 ;;21,"55289-0233-10 ")
 ;;369
 ;;21,"55289-0233-90 ")
 ;;370
 ;;21,"55289-0234-01 ")
 ;;371
 ;;21,"55289-0234-30 ")
 ;;372
 ;;21,"55289-0413-01 ")
 ;;373
 ;;21,"55289-0413-30 ")
 ;;374
 ;;21,"55289-0413-90 ")
 ;;375
 ;;21,"55289-0653-30 ")
 ;;376
 ;;21,"56789-0366-10 ")
 ;;377
 ;;21,"57664-0166-08 ")
 ;;378
 ;;21,"57664-0166-18 ")
 ;;379
 ;;21,"57664-0167-08 ")
 ;;380
 ;;21,"57664-0167-18 ")
 ;;381
 ;;21,"57994-0477-18 ")
 ;;382
 ;;21,"58016-0333-00 ")
 ;;383
 ;;21,"58016-0333-15 ")
 ;;384
 ;;21,"58016-0333-30 ")
 ;;385
 ;;21,"58016-0333-60 ")
 ;;386
 ;;21,"58016-0467-30 ")
 ;;387
 ;;21,"58016-0528-00 ")
 ;;388
 ;;21,"58016-0528-15- ")
 ;;389
 ;;21,"58016-0528-30 ")
 ;;390
 ;;21,"58016-0528-60 ")
 ;;391
 ;;21,"58016-0529-00 ")
 ;;392
 ;;21,"58016-0529-15 ")
 ;;393
 ;;21,"58016-0529-20 ")
 ;;394
 ;
OTHER ; OTHER ROUTINES
 D ^BGP6BXBB
 D ^BGP6BXBC
 D ^BGP6BXBD
 D ^BGP6BXBE
 D ^BGP6BXBF
 D ^BGP6BXBG
 Q
