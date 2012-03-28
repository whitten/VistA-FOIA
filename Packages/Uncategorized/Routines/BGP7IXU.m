BGP7IXU ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON SEP 16, 2006 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;;BGP HEDIS STATIN NDC
 ;
 ; This routine loads Taxonomy BGP HEDIS STATIN NDC
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
 ;;21,"00003-5154-05 ")
 ;;184
 ;;21,"00003-5154-06 ")
 ;;185
 ;;21,"00003-5168-11 ")
 ;;305
 ;;21,"00003-5169-11 ")
 ;;306
 ;;21,"00003-5173-11 ")
 ;;307
 ;;21,"00003-5174-11 ")
 ;;308
 ;;21,"00003-5178-05 ")
 ;;192
 ;;21,"00003-5178-06 ")
 ;;193
 ;;21,"00003-5178-75 ")
 ;;194
 ;;21,"00003-5183-11 ")
 ;;309
 ;;21,"00003-5184-11 ")
 ;;310
 ;;21,"00003-5194-10 ")
 ;;200
 ;;21,"00003-5194-33 ")
 ;;201
 ;;21,"00003-5195-10 ")
 ;;206
 ;;21,"00003-5195-33 ")
 ;;207
 ;;21,"00006-0543-28 ")
 ;;256
 ;;21,"00006-0543-31 ")
 ;;257
 ;;21,"00006-0543-54 ")
 ;;258
 ;;21,"00006-0543-61 ")
 ;;259
 ;;21,"00006-0543-82 ")
 ;;260
 ;;21,"00006-0726-28 ")
 ;;208
 ;;21,"00006-0726-31 ")
 ;;209
 ;;21,"00006-0726-54 ")
 ;;210
 ;;21,"00006-0726-61 ")
 ;;211
 ;;21,"00006-0726-82 ")
 ;;212
 ;;21,"00006-0730-61 ")
 ;;101
 ;;21,"00006-0731-28 ")
 ;;121
 ;;21,"00006-0731-61 ")
 ;;122
 ;;21,"00006-0731-82 ")
 ;;123
 ;;21,"00006-0731-87 ")
 ;;124
 ;;21,"00006-0731-94 ")
 ;;125
 ;;21,"00006-0732-61 ")
 ;;154
 ;;21,"00006-0732-82 ")
 ;;155
 ;;21,"00006-0732-87 ")
 ;;156
 ;;21,"00006-0732-94 ")
 ;;157
 ;;21,"00006-0735-28 ")
 ;;213
 ;;21,"00006-0735-31 ")
 ;;214
 ;;21,"00006-0735-54 ")
 ;;215
 ;;21,"00006-0735-61 ")
 ;;216
 ;;21,"00006-0735-82 ")
 ;;217
 ;;21,"00006-0735-87 ")
 ;;218
 ;;21,"00006-0740-28 ")
 ;;228
 ;;21,"00006-0740-31 ")
 ;;229
 ;;21,"00006-0740-54 ")
 ;;230
 ;;21,"00006-0740-61 ")
 ;;231
 ;;21,"00006-0740-82 ")
 ;;232
 ;;21,"00006-0740-87 ")
 ;;233
 ;;21,"00006-0749-28 ")
 ;;242
 ;;21,"00006-0749-31 ")
 ;;243
 ;;21,"00006-0749-54 ")
 ;;244
 ;;21,"00006-0749-61 ")
 ;;245
 ;;21,"00006-0749-82 ")
 ;;246
 ;;21,"00069-2150-30 ")
 ;;315
 ;;21,"00069-2160-30 ")
 ;;317
 ;;21,"00069-2170-30 ")
 ;;316
 ;;21,"00069-2180-30 ")
 ;;318
 ;;21,"00069-2190-30 ")
 ;;311
 ;;21,"00069-2250-30 ")
 ;;313
 ;;21,"00069-2260-30 ")
 ;;312
 ;;21,"00069-2270-30 ")
 ;;314
 ;;21,"00069-2960-30 ")
 ;;319
 ;;21,"00069-2970-30 ")
 ;;320
 ;;21,"00069-2980-30 ")
 ;;321
 ;;21,"00071-0155-23 ")
 ;;275
 ;;21,"00071-0155-34 ")
 ;;276
 ;;21,"00071-0155-40 ")
 ;;277
 ;;21,"00071-0156-23 ")
 ;;283
 ;;21,"00071-0156-40 ")
 ;;284
 ;;21,"00071-0156-94 ")
 ;;285
 ;;21,"00071-0157-23 ")
 ;;289
 ;;21,"00071-0157-73 ")
 ;;290
 ;;21,"00071-0158-23 ")
 ;;294
 ;;21,"00071-0158-73 ")
 ;;295
 ;;21,"00078-0176-05 ")
 ;;263
 ;;21,"00078-0176-15 ")
 ;;264
 ;;21,"00078-0234-05 ")
 ;;269
 ;;21,"00078-0234-15 ")
 ;;270
 ;;21,"00078-0354-05 ")
 ;;273
 ;;21,"00078-0354-15 ")
 ;;274
 ;;21,"00093-0576-06 ")
 ;;126
 ;;21,"00093-0576-10 ")
 ;;127
 ;;21,"00093-0926-06 ")
 ;;102
 ;;21,"00093-0926-10 ")
 ;;103
 ;;21,"00093-0928-06 ")
 ;;158
 ;;21,"00093-0928-10 ")
 ;;159
 ;;21,"00185-0070-01 ")
 ;;104
 ;;21,"00185-0070-10 ")
 ;;105
 ;;21,"00185-0070-60 ")
 ;;106
 ;;21,"00185-0072-01 ")
 ;;128
 ;;21,"00185-0072-10 ")
 ;;129
 ;;21,"00185-0072-60 ")
 ;;130
 ;;21,"00185-0074-01 ")
 ;;160
 ;;21,"00185-0074-10 ")
 ;;161
 ;;21,"00185-0074-60 ")
 ;;162
 ;;21,"00228-2633-06 ")
 ;;107
 ;;21,"00228-2633-50 ")
 ;;108
 ;;21,"00228-2634-06 ")
 ;;131
 ;;21,"00228-2634-50 ")
 ;;132
 ;;21,"00228-2635-06 ")
 ;;163
 ;;21,"00228-2635-50 ")
 ;;164
 ;;21,"00247-1139-30 ")
 ;;332
 ;;21,"00247-1139-60 ")
 ;;333
 ;;21,"00247-1140-30 ")
 ;;334
 ;;21,"00247-1152-30 ")
 ;;335
 ;;21,"00247-1152-60 ")
 ;;336
 ;;21,"00247-1153-30 ")
 ;;337
 ;;21,"00247-1153-60 ")
 ;;338
 ;;21,"00247-1276-30 ")
 ;;339
 ;;21,"00310-0751-39 ")
 ;;299
 ;;21,"00310-0751-90 ")
 ;;300
 ;;21,"00310-0752-39 ")
 ;;301
 ;;21,"00310-0752-90 ")
 ;;302
 ;;21,"00310-0754-30 ")
 ;;303
 ;;21,"00310-0755-90 ")
 ;;304
 ;;21,"00378-6510-91 ")
 ;;109
 ;;21,"00378-6520-05 ")
 ;;133
 ;;21,"00378-6520-91 ")
 ;;134
 ;;21,"00378-6540-05 ")
 ;;165
 ;;21,"00378-6540-91 ")
 ;;166
 ;;21,"00781-1210-10 ")
 ;;340
 ;;21,"00781-1210-60 ")
 ;;135
 ;;21,"00781-1213-10 ")
 ;;341
 ;;21,"00781-1213-60 ")
 ;;167
 ;;21,"00781-1323-05 ")
 ;;342
 ;;21,"00781-1323-60 ")
 ;;110
 ;;21,"12280-0038-90 ")
 ;;186
 ;;21,"12280-0108-60 ")
 ;;136
 ;;21,"12280-0181-30 ")
 ;;326
 ;;21,"49884-0754-01 ")
 ;;111
 ;;21,"49884-0754-10 ")
 ;;112
 ;;21,"49884-0755-01 ")
 ;;137
 ;;21,"49884-0755-10 ")
 ;;138
 ;;21,"49884-0756-01 ")
 ;;168
 ;;21,"49884-0756-10 ")
 ;;169
 ;;21,"49999-0293-30 ")
 ;;343
 ;;21,"49999-0306-30 ")
 ;;344
 ;;21,"49999-0392-30 ")
 ;;345
 ;;21,"49999-0470-60 ")
 ;;346
 ;;21,"49999-0471-30 ")
 ;;347
 ;;21,"49999-0471-60 ")
 ;;1
 ;;21,"51079-0974-01 ")
 ;;2
 ;;21,"51079-0974-20 ")
 ;;113
 ;;21,"51079-0975-01 ")
 ;;3
 ;;21,"51079-0975-20 ")
 ;;139
 ;;21,"51079-0976-01 ")
 ;;4
 ;;21,"51079-0976-20 ")
 ;;170
 ;;21,"52959-0046-30 ")
 ;;291
 ;;21,"52959-0112-30 ")
 ;;247
 ;;21,"52959-0720-30 ")
 ;;5
 ;;21,"54569-0613-00 ")
 ;;140
 ;;21,"54569-0613-01 ")
 ;;141
 ;;21,"54569-0613-02 ")
 ;;142
 ;;21,"54569-0613-03 ")
 ;;143
 ;;21,"54569-0613-04 ")
 ;;144
 ;;21,"54569-3256-00 ")
 ;;171
 ;;21,"54569-3256-01 ")
 ;;172
 ;;21,"54569-3821-00 ")
 ;;265
 ;;21,"54569-3821-01 ")
 ;;266
 ;;21,"54569-3840-00 ")
 ;;187
 ;;21,"54569-4180-00 ")
 ;;219
 ;;21,"54569-4180-01 ")
 ;;220
 ;;21,"54569-4346-00 ")
 ;;188
 ;;21,"54569-4346-01 ")
 ;;189
 ;;21,"54569-4403-00 ")
 ;;234
 ;;21,"54569-4404-00 ")
 ;;248
 ;;21,"54569-4466-00 ")
 ;;278
 ;;21,"54569-4466-01 ")
 ;;279
 ;;21,"54569-4467-00 ")
 ;;286
 ;;21,"54569-4584-00 ")
 ;;6
 ;;21,"54569-4587-00 ")
 ;;7
 ;;21,"54569-4610-00 ")
 ;;202
 ;;21,"54569-4761-00 ")
 ;;271
 ;;21,"54569-4761-01 ")
 ;;8
 ;;21,"54569-5345-00 ")
 ;;9
 ;;21,"54569-5346-00 ")
 ;;10
 ;;21,"54569-5347-00 ")
 ;;11
 ;;21,"54569-5382-00 ")
 ;;12
 ;;21,"54569-5498-00 ")
 ;;13
 ;;21,"54569-5600-00 ")
 ;;14
 ;;21,"54569-5640-00 ")
 ;;15
 ;;21,"54569-5648-00 ")
 ;;16
 ;;21,"54569-5672-00 ")
 ;;17
 ;;21,"54569-8011-00 ")
 ;;145
 ;;21,"54569-8598-00 ")
 ;;190
 ;;21,"54868-0686-01 ")
 ;;146
 ;;21,"54868-0686-02 ")
 ;;147
 ;;21,"54868-0686-03 ")
 ;;148
 ;;21,"54868-0686-04 ")
 ;;149
 ;;21,"54868-1087-00 ")
 ;;173
 ;;21,"54868-1087-01 ")
 ;;174
 ;;21,"54868-1207-00 ")
 ;;18
 ;;21,"54868-1890-00 ")
 ;;19
 ;;21,"54868-1968-00 ")
 ;;114
 ;;21,"54868-2287-01 ")
 ;;20
 ;;21,"54868-2287-02 ")
 ;;21
 ;;21,"54868-2288-00 ")
 ;;195
 ;;21,"54868-2639-00 ")
 ;;22
 ;;21,"54868-2639-01 ")
 ;;221
 ;;21,"54868-3104-00 ")
 ;;235
 ;;21,"54868-3104-01 ")
 ;;23
 ;;21,"54868-3270-00 ")
 ;;203
 ;;21,"54868-3270-01 ")
 ;;204
 ;;21,"54868-3270-02 ")
 ;;24
 ;;21,"54868-3287-00 ")
 ;;25
 ;;21,"54868-3329-00 ")
 ;;267
 ;;21,"54868-3934-00 ")
 ;;280
 ;;21,"54868-3934-01 ")
 ;;26
 ;;21,"54868-3934-02 ")
 ;;27
 ;;21,"54868-3946-00 ")
 ;;287
 ;;21,"54868-3946-01 ")
 ;;28
 ;;21,"54868-3946-02 ")
 ;;29
 ;;21,"54868-3946-03 ")
 ;;30
 ;;21,"54868-4157-00 ")
 ;;249
 ;;21,"54868-4181-00 ")
 ;;261
 ;;21,"54868-4181-01 ")
 ;;31
 ;;21,"54868-4224-00 ")
 ;;32
 ;;21,"54868-4224-01 ")
 ;;33
 ;;21,"54868-4229-00 ")
 ;;292
 ;;21,"54868-4229-01 ")
 ;;34
 ;;21,"54868-4229-02 ")
 ;;35
 ;;21,"54868-4585-00 ")
 ;;36
 ;;21,"54868-4585-01 ")
 ;;37
 ;;21,"54868-4585-02 ")
 ;;38
 ;;21,"54868-4585-03 ")
 ;;39
 ;;21,"54868-4593-00 ")
 ;;40
 ;;21,"54868-4601-00 ")
 ;;41
 ;;21,"54868-4634-00 ")
 ;;42
 ;;21,"54868-4774-00 ")
 ;;43
 ;;21,"54868-4774-01 ")
 ;;44
 ;;21,"54868-4774-02 ")
 ;;45
 ;;21,"54868-4807-00 ")
 ;;46
 ;;21,"54868-4807-01 ")
 ;;47
 ;;21,"54868-4807-02 ")
 ;;48
 ;;21,"54868-4934-00 ")
 ;;49
 ;;21,"54868-4963-00 ")
 ;;50
 ;;21,"54868-4963-01 ")
 ;;51
 ;;21,"54868-4963-02 ")
 ;;52
 ;;21,"54868-4963-03 ")
 ;;53
 ;;21,"54868-4999-00 ")
 ;;54
 ;;21,"54868-4999-01 ")
 ;;55
 ;;21,"54868-5085-00 ")
 ;;56
 ;;21,"54868-5085-01 ")
 ;;57
 ;;21,"54868-5085-02 ")
 ;;58
 ;;21,"54868-5087-00 ")
 ;;59
 ;;21,"54868-5179-00 ")
 ;;60
 ;;21,"54868-5187-00 ")
 ;;61
 ;;21,"54868-5189-00 ")
 ;;62
 ;;21,"54868-5200-00 ")
 ;;63
 ;;21,"54868-5209-00 ")
 ;;64
 ;;21,"54868-5250-00 ")
 ;;65
 ;;21,"54868-5259-00 ")
 ;;66
 ;;21,"55175-3002-03 ")
 ;;67
 ;;21,"55175-5325-03 ")
 ;;68
 ;;21,"55175-5325-09 ")
 ;;69
 ;;21,"55175-5390-03 ")
 ;;70
 ;;21,"55289-0104-30 ")
 ;;191
 ;;21,"55289-0400-30 ")
 ;;150
 ;;21,"55289-0476-30 ")
 ;;272
 ;;21,"55289-0548-30 ")
 ;;175
 ;;21,"55289-0692-14 ")
 ;;71
 ;;21,"55289-0740-60 ")
 ;;268
 ;;21,"55289-0800-30 ")
 ;;72
 ;;21,"55289-0861-30 ")
 ;;73
 ;;21,"55289-0870-30 ")
 ;;74
 ;;21,"55289-0871-30 ")
 ;;75
 ;;21,"55289-0873-30 ")
 ;;76
 ;;21,"55289-0874-30 ")
 ;;77
 ;;21,"57866-7982-01 ")
 ;;236
 ;;21,"57866-7983-01 ")
 ;;250
 ;;21,"57866-7986-01 ")
 ;;222
 ;;21,"57866-8615-01 ")
 ;;281
 ;;21,"58016-0364-00 ")
 ;;223
 ;;21,"58016-0364-30 ")
 ;;224
 ;;21,"58016-0364-60 ")
 ;;225
 ;;21,"58016-0364-90 ")
 ;;226
 ;;21,"58016-0365-00 ")
 ;;251
 ;;21,"58016-0365-30 ")
 ;;252
 ;;21,"58016-0365-60 ")
 ;;253
 ;;21,"58016-0365-90 ")
 ;;254
 ;;21,"58016-0385-00 ")
 ;;237
 ;;21,"58016-0385-30 ")
 ;;238
 ;;21,"58016-0385-60 ")
 ;;239
 ;;21,"58016-0385-90 ")
 ;;240
 ;;21,"58016-0425-00 ")
 ;;196
 ;;21,"58016-0425-30 ")
 ;;197
 ;;21,"58016-0425-60 ")
 ;;198
 ;;21,"58016-0425-90 ")
 ;;199
 ;;21,"58016-0546-00 ")
 ;;78
 ;;21,"58016-0979-00 ")
 ;;115
 ;;21,"58016-0979-20 ")
 ;;116
 ;;21,"58016-0979-30 ")
 ;;117
 ;;21,"58016-0979-60 ")
 ;;118
 ;;21,"58016-0979-90 ")
 ;;119
 ;;21,"58864-0608-30 ")
 ;;79
 ;;21,"58864-0623-15 ")
 ;;80
 ;;21,"58864-0623-30 ")
 ;;81
 ;;21,"58864-0653-30 ")
 ;;82
 ;;21,"58864-0682-30 ")
 ;;83
 ;;21,"58864-0685-30 ")
 ;;84
 ;;21,"58864-0739-30 ")
 ;;85
 ;;21,"58864-0743-15 ")
 ;;86
 ;;21,"58864-0743-30 ")
 ;;87
 ;;21,"58864-0760-30 ")
 ;;88
 ;;21,"58864-0780-30 ")
 ;;89
 ;;21,"58864-0781-30 ")
 ;;90
 ;;21,"60598-0006-90 ")
 ;;296
 ;;21,"60598-0007-90 ")
 ;;297
 ;;21,"60598-0008-90 ")
 ;;298
 ;;21,"62022-0627-30 ")
 ;;177
 ;;21,"62022-0628-30 ")
 ;;178
 ;;21,"62022-0629-30 ")
 ;;180
 ;;21,"62022-0630-30 ")
 ;;182
 ;;21,"62022-0770-30 ")
 ;;179
 ;;21,"62022-0780-30 ")
 ;;181
 ;;21,"62022-0781-30 ")
 ;;183
 ;;21,"63739-0280-15 ")
 ;;120
 ;;21,"63739-0281-15 ")
 ;;151
 ;;21,"66116-0238-30 ")
 ;;91
 ;;21,"66116-0276-30 ")
 ;;92
 ;;21,"66116-0277-30 ")
 ;;93
 ;;21,"66116-0451-30 ")
 ;;94
 ;;21,"66582-0311-31 ")
 ;;322
 ;;21,"66582-0311-54 ")
 ;;323
 ;;21,"66582-0311-82 ")
 ;;95
 ;;21,"66582-0312-31 ")
 ;;324
 ;;21,"66582-0312-54 ")
 ;;325
 ;;21,"66582-0312-82 ")
 ;;96
 ;;21,"66582-0313-31 ")
 ;;327
 ;;21,"66582-0313-52 ")
 ;;328
 ;;21,"66582-0313-54 ")
 ;;329
 ;;21,"66582-0313-74 ")
 ;;97
 ;;21,"66582-0315-31 ")
 ;;330
 ;;21,"66582-0315-54 ")
 ;;331
 ;;21,"66582-0315-74 ")
 ;;98
 ;;21,"68030-8615-01 ")
 ;;99
 ;;21,"68115-0218-30 ")
 ;;100
 ;;21,"68115-0219-30 ")
 ;;152
 ;;21,"68115-0219-60 ")
 ;;153
 ;;21,"68115-0658-00 ")
 ;;176
 ;;21,"68115-0664-90 ")
 ;;205
 ;;21,"68115-0668-90 ")
 ;;293
 ;;21,"68115-0672-30 ")
 ;;241
 ;;21,"68115-0720-30 ")
 ;;227
 ;;21,"68115-0759-30 ")
 ;;262
 ;;21,"68115-0777-90 ")
 ;;255
 ;;21,"68115-0800-90 ")
 ;;288
 ;;21,"68115-0836-90 ")
 ;;282
 ;;9002226,637,.01)
 ;;BGP HEDIS STATIN NDC
 ;;9002226,637,.02)
 ;;@
 ;;9002226,637,.04)
 ;;n
 ;;9002226,637,.06)
 ;;@
 ;;9002226,637,.08)
 ;;@
 ;;9002226,637,.09)
 ;;@
 ;;9002226,637,.11)
 ;;@
 ;;9002226,637,.12)
 ;;@
 ;;9002226,637,.13)
 ;;1
 ;;9002226,637,.14)
 ;;@
 ;;9002226,637,.15)
 ;;@
 ;;9002226,637,.16)
 ;;@
 ;;9002226,637,.17)
 ;;@
 ;;9002226,637,3101)
 ;;@
 ;;9002226.02101,"637,00003-5154-05 ",.01)
 ;;00003-5154-05
 ;;9002226.02101,"637,00003-5154-05 ",.02)
 ;;00003-5154-05
 ;;9002226.02101,"637,00003-5154-06 ",.01)
 ;;00003-5154-06
 ;;9002226.02101,"637,00003-5154-06 ",.02)
 ;;00003-5154-06
 ;;9002226.02101,"637,00003-5168-11 ",.01)
 ;;00003-5168-11
 ;;9002226.02101,"637,00003-5168-11 ",.02)
 ;;00003-5168-11
 ;;9002226.02101,"637,00003-5169-11 ",.01)
 ;;00003-5169-11
 ;;9002226.02101,"637,00003-5169-11 ",.02)
 ;;00003-5169-11
 ;;9002226.02101,"637,00003-5173-11 ",.01)
 ;;00003-5173-11
 ;;9002226.02101,"637,00003-5173-11 ",.02)
 ;;00003-5173-11
 ;;9002226.02101,"637,00003-5174-11 ",.01)
 ;;00003-5174-11
 ;;9002226.02101,"637,00003-5174-11 ",.02)
 ;;00003-5174-11
 ;;9002226.02101,"637,00003-5178-05 ",.01)
 ;;00003-5178-05
 ;;9002226.02101,"637,00003-5178-05 ",.02)
 ;;00003-5178-05
 ;;9002226.02101,"637,00003-5178-06 ",.01)
 ;;00003-5178-06
 ;;9002226.02101,"637,00003-5178-06 ",.02)
 ;;00003-5178-06
 ;;9002226.02101,"637,00003-5178-75 ",.01)
 ;;00003-5178-75
 ;;9002226.02101,"637,00003-5178-75 ",.02)
 ;;00003-5178-75
 ;;9002226.02101,"637,00003-5183-11 ",.01)
 ;;00003-5183-11
 ;
OTHER ; OTHER ROUTINES
 D ^BGP7IXUB
 D ^BGP7IXUC
 D ^BGP7IXUD
 D ^BGP7IXUE
 Q
