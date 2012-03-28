BGP9SXF ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON MAR 25, 2009 ;
 ;;9.0;IHS CLINICAL REPORTING;;JUL 1, 2009
 ;;BGP HEDIS ASTHMA INHALED NDC
 ;
 ; This routine loads Taxonomy BGP HEDIS ASTHMA INHALED NDC
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
 ;;21,"00054-8167-21 ")
 ;;211
 ;;21,"00054-8167-23 ")
 ;;212
 ;;21,"00054-8613-11 ")
 ;;351
 ;;21,"00075-0060-37 ")
 ;;400
 ;;21,"00083-0167-02 ")
 ;;326
 ;;21,"00083-0167-74 ")
 ;;327
 ;;21,"00085-0208-02 ")
 ;;74
 ;;21,"00085-0209-01 ")
 ;;50
 ;;21,"00085-0614-02 ")
 ;;133
 ;;21,"00085-0614-03 ")
 ;;102
 ;;21,"00085-0736-04 ")
 ;;167
 ;;21,"00085-1112-01 ")
 ;;172
 ;;21,"00085-1112-03 ")
 ;;173
 ;;21,"00085-1132-01 ")
 ;;148
 ;;21,"00085-1336-01 ")
 ;;75
 ;;21,"00085-1341-01 ")
 ;;372
 ;;21,"00085-1341-02 ")
 ;;376
 ;;21,"00085-1341-03 ")
 ;;375
 ;;21,"00085-1341-04 ")
 ;;373
 ;;21,"00085-1401-01 ")
 ;;328
 ;;21,"00085-1402-01 ")
 ;;329
 ;;21,"00085-1461-02 ")
 ;;371
 ;;21,"00085-1806-01 ")
 ;;51
 ;;21,"00089-0815-21 ")
 ;;384
 ;;21,"00172-4390-18 ")
 ;;108
 ;;21,"00172-6405-44 ")
 ;;1
 ;;21,"00172-6405-49 ")
 ;;2
 ;;21,"00172-6406-49 ")
 ;;213
 ;;21,"00172-6406-59 ")
 ;;214
 ;;21,"00173-0312-88 ")
 ;;159
 ;;21,"00173-0321-88 ")
 ;;139
 ;;21,"00173-0321-98 ")
 ;;103
 ;;21,"00173-0389-01 ")
 ;;87
 ;;21,"00173-0389-02 ")
 ;;88
 ;;21,"00173-0389-03 ")
 ;;89
 ;;21,"00173-0463-00 ")
 ;;104
 ;;21,"00173-0464-00 ")
 ;;389
 ;;21,"00173-0465-00 ")
 ;;387
 ;;21,"00173-0467-00 ")
 ;;390
 ;;21,"00173-0469-00 ")
 ;;160
 ;;21,"00173-0491-00 ")
 ;;266
 ;;21,"00173-0494-00 ")
 ;;251
 ;;21,"00173-0495-00 ")
 ;;258
 ;;21,"00173-0497-00 ")
 ;;267
 ;;21,"00173-0498-00 ")
 ;;252
 ;;21,"00173-0499-00 ")
 ;;259
 ;;21,"00173-0504-00 ")
 ;;265
 ;;21,"00173-0509-00 ")
 ;;250
 ;;21,"00173-0511-00 ")
 ;;274
 ;;21,"00173-0520-00 ")
 ;;393
 ;;21,"00173-0521-00 ")
 ;;394
 ;;21,"00173-0600-02 ")
 ;;273
 ;;21,"00173-0682-00 ")
 ;;152
 ;;21,"00173-0682-20 ")
 ;;153
 ;;21,"00173-0682-24 ")
 ;;154
 ;;21,"00173-0695-00 ")
 ;;291
 ;;21,"00173-0695-02 ")
 ;;292
 ;;21,"00173-0695-04 ")
 ;;293
 ;;21,"00173-0696-00 ")
 ;;304
 ;;21,"00173-0696-02 ")
 ;;305
 ;;21,"00173-0696-04 ")
 ;;306
 ;;21,"00173-0697-00 ")
 ;;317
 ;;21,"00173-0697-02 ")
 ;;318
 ;;21,"00173-0697-04 ")
 ;;319
 ;;21,"00173-0715-00 ")
 ;;316
 ;;21,"00173-0716-00 ")
 ;;302
 ;;21,"00173-0717-00 ")
 ;;303
 ;;21,"00173-0718-00 ")
 ;;287
 ;;21,"00173-0718-20 ")
 ;;288
 ;;21,"00173-0719-00 ")
 ;;276
 ;;21,"00173-0719-20 ")
 ;;277
 ;;21,"00173-0720-00 ")
 ;;282
 ;;21,"00173-0720-20 ")
 ;;283
 ;;21,"00182-6014-65 ")
 ;;52
 ;;21,"00186-0370-20 ")
 ;;198
 ;;21,"00186-0370-28 ")
 ;;199
 ;;21,"00186-0372-20 ")
 ;;200
 ;;21,"00186-0372-28 ")
 ;;201
 ;;21,"00186-0915-42 ")
 ;;194
 ;;21,"00186-0916-12 ")
 ;;191
 ;;21,"00186-0917-06 ")
 ;;197
 ;;21,"00186-1988-04 ")
 ;;184
 ;;21,"00186-1989-04 ")
 ;;187
 ;;21,"00186-1990-04 ")
 ;;190
 ;;21,"00247-0084-17 ")
 ;;134
 ;;21,"00247-0084-86 ")
 ;;135
 ;;21,"00247-0171-10 ")
 ;;3
 ;;21,"00247-0171-12 ")
 ;;4
 ;;21,"00247-0171-30 ")
 ;;5
 ;;21,"00247-0171-75 ")
 ;;6
 ;;21,"00247-0190-20 ")
 ;;401
 ;;21,"00247-0348-17 ")
 ;;109
 ;;21,"00247-0588-41 ")
 ;;363
 ;;21,"00247-0634-17 ")
 ;;168
 ;;21,"00247-0657-10 ")
 ;;352
 ;;21,"00247-0657-47 ")
 ;;353
 ;;21,"00247-0657-65 ")
 ;;354
 ;;21,"00247-0658-50 ")
 ;;358
 ;;21,"00247-0659-07 ")
 ;;243
 ;;21,"00247-0667-08 ")
 ;;202
 ;;21,"00247-0674-41 ")
 ;;203
 ;;21,"00247-0702-13 ")
 ;;388
 ;;21,"00247-0703-07 ")
 ;;236
 ;;21,"00247-0870-20 ")
 ;;53
 ;;21,"00247-0871-20 ")
 ;;76
 ;;21,"00247-0873-02 ")
 ;;228
 ;;21,"00247-0873-52 ")
 ;;229
 ;;21,"00247-0873-60 ")
 ;;230
 ;;21,"00247-0912-17 ")
 ;;92
 ;;21,"00247-1094-17 ")
 ;;161
 ;;21,"00247-1094-88 ")
 ;;162
 ;;21,"00247-1174-17 ")
 ;;93
 ;;21,"00247-1364-17 ")
 ;;94
 ;;21,"00247-1575-13 ")
 ;;268
 ;;21,"00247-1576-12 ")
 ;;253
 ;;21,"00247-1576-13 ")
 ;;254
 ;;21,"00247-1577-13 ")
 ;;260
 ;;21,"00247-1696-93 ")
 ;;379
 ;;21,"00247-1973-60 ")
 ;;307
 ;;21,"00247-1983-60 ")
 ;;294
 ;;21,"00247-2072-60 ")
 ;;395
 ;;21,"00247-2215-60 ")
 ;;320
 ;;21,"00247-2224-72 ")
 ;;334
 ;;21,"00247-2225-72 ")
 ;;336
 ;;21,"00247-2226-72 ")
 ;;342
 ;;21,"00247-2227-01 ")
 ;;346
 ;;21,"00247-2297-58 ")
 ;;142
 ;;21,"00405-2130-52 ")
 ;;54
 ;;21,"00405-2131-25 ")
 ;;7
 ;;21,"00456-0670-99 ")
 ;;237
 ;;21,"00456-0672-99 ")
 ;;244
 ;;21,"00472-0750-21 ")
 ;;215
 ;;21,"00472-0750-60 ")
 ;;216
 ;;21,"00472-0752-21 ")
 ;;217
 ;;21,"00472-0752-60 ")
 ;;218
 ;;21,"00472-0831-23 ")
 ;;8
 ;;21,"00472-0831-30 ")
 ;;9
 ;;21,"00472-0831-60 ")
 ;;10
 ;;21,"00472-0832-20 ")
 ;;55
 ;;21,"00472-0832-30 ")
 ;;56
 ;;21,"00472-1264-63 ")
 ;;95
 ;;21,"00487-9501-01 ")
 ;;11
 ;;21,"00487-9501-02 ")
 ;;12
 ;;21,"00487-9501-03 ")
 ;;13
 ;;21,"00487-9501-25 ")
 ;;14
 ;;21,"00487-9501-60 ")
 ;;15
 ;;21,"00487-9901-02 ")
 ;;57
 ;;21,"00487-9901-30 ")
 ;;58
 ;
OTHER ; OTHER ROUTINES
 D ^BGP9SXFB
 D ^BGP9SXFC
 D ^BGP9SXFD
 D ^BGP9SXFE
 D ^BGP9SXFF
 D ^BGP9SXFG
 D ^BGP9SXFH
 D ^BGP9SXFI
 D ^BGP9SXFJ
 D ^BGP9SXFK
 D ^BGP9SXFL
 D ^BGP9SXFM
 Q
