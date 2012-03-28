BGP12V ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON APR 14, 2011 ;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;;JUN 27, 2011;Build 33
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
 ;;21,"00074-3014-60 ")
 ;;194
 ;;21,"00075-0060-37 ")
 ;;195
 ;;21,"00083-0167-02 ")
 ;;270
 ;;21,"00083-0167-74 ")
 ;;271
 ;;21,"00085-0208-02 ")
 ;;316
 ;;21,"00085-0209-01 ")
 ;;317
 ;;21,"00085-0614-02 ")
 ;;318
 ;;21,"00085-0614-03 ")
 ;;319
 ;;21,"00085-1132-01 ")
 ;;329
 ;;21,"00085-1336-01 ")
 ;;320
 ;;21,"00085-1341-01 ")
 ;;185
 ;;21,"00085-1341-02 ")
 ;;190
 ;;21,"00085-1341-03 ")
 ;;188
 ;;21,"00085-1341-04 ")
 ;;186
 ;;21,"00085-1401-01 ")
 ;;272
 ;;21,"00085-1402-01 ")
 ;;273
 ;;21,"00085-1402-02 ")
 ;;274
 ;;21,"00085-1461-02 ")
 ;;189
 ;;21,"00085-1461-07 ")
 ;;193
 ;;21,"00085-1806-01 ")
 ;;321
 ;;21,"00085-4610-01 ")
 ;;226
 ;;21,"00085-7206-01 ")
 ;;227
 ;;21,"00089-0815-21 ")
 ;;294
 ;;21,"00093-6815-73 ")
 ;;206
 ;;21,"00093-6816-73 ")
 ;;207
 ;;21,"00172-4390-18 ")
 ;;89
 ;;21,"00172-6405-44 ")
 ;;60
 ;;21,"00172-6405-49 ")
 ;;61
 ;;21,"00172-6406-49 ")
 ;;210
 ;;21,"00172-6406-59 ")
 ;;211
 ;;21,"00173-0464-00 ")
 ;;356
 ;;21,"00173-0491-00 ")
 ;;228
 ;;21,"00173-0494-00 ")
 ;;229
 ;;21,"00173-0495-00 ")
 ;;230
 ;;21,"00173-0498-00 ")
 ;;231
 ;;21,"00173-0499-00 ")
 ;;232
 ;;21,"00173-0504-00 ")
 ;;269
 ;;21,"00173-0520-00 ")
 ;;357
 ;;21,"00173-0521-00 ")
 ;;358
 ;;21,"00173-0600-02 ")
 ;;248
 ;;21,"00173-0601-02 ")
 ;;249
 ;;21,"00173-0602-02 ")
 ;;250
 ;;21,"00173-0682-00 ")
 ;;372
 ;;21,"00173-0682-20 ")
 ;;373
 ;;21,"00173-0682-21 ")
 ;;374
 ;;21,"00173-0682-24 ")
 ;;375
 ;;21,"00173-0682-54 ")
 ;;376
 ;;21,"00173-0682-81 ")
 ;;355
 ;;21,"00173-0695-00 ")
 ;;3
 ;;21,"00173-0695-02 ")
 ;;4
 ;;21,"00173-0695-04 ")
 ;;5
 ;;21,"00173-0696-00 ")
 ;;6
 ;;21,"00173-0696-02 ")
 ;;7
 ;;21,"00173-0696-04 ")
 ;;8
 ;;21,"00173-0697-00 ")
 ;;9
 ;;21,"00173-0697-02 ")
 ;;10
 ;;21,"00173-0697-04 ")
 ;;11
 ;;21,"00173-0715-00 ")
 ;;37
 ;;21,"00173-0715-20 ")
 ;;38
 ;;21,"00173-0715-22 ")
 ;;39
 ;;21,"00173-0716-00 ")
 ;;40
 ;;21,"00173-0716-20 ")
 ;;41
 ;;21,"00173-0716-22 ")
 ;;42
 ;;21,"00173-0717-00 ")
 ;;43
 ;;21,"00173-0717-20 ")
 ;;44
 ;;21,"00173-0717-22 ")
 ;;45
 ;;21,"00173-0718-00 ")
 ;;251
 ;;21,"00173-0718-20 ")
 ;;252
 ;;21,"00173-0719-00 ")
 ;;253
 ;;21,"00173-0719-20 ")
 ;;254
 ;;21,"00173-0720-00 ")
 ;;255
 ;;21,"00173-0720-20 ")
 ;;256
 ;;21,"00182-6014-65 ")
 ;;62
 ;;21,"00186-0370-20 ")
 ;;364
 ;;21,"00186-0370-28 ")
 ;;365
 ;;21,"00186-0372-20 ")
 ;;366
 ;;21,"00186-0372-28 ")
 ;;367
 ;;21,"00186-0426-04 ")
 ;;208
 ;;21,"00186-0915-42 ")
 ;;345
 ;;21,"00186-0916-12 ")
 ;;334
 ;;21,"00186-0917-06 ")
 ;;335
 ;;21,"00186-1988-04 ")
 ;;338
 ;;21,"00186-1989-04 ")
 ;;339
 ;;21,"00186-1990-04 ")
 ;;340
 ;;21,"00247-0084-17 ")
 ;;322
 ;;21,"00247-0084-86 ")
 ;;323
 ;;21,"00247-0171-10 ")
 ;;90
 ;;21,"00247-0171-12 ")
 ;;91
 ;;21,"00247-0171-30 ")
 ;;92
 ;;21,"00247-0171-75 ")
 ;;93
 ;;21,"00247-0190-20 ")
 ;;196
 ;;21,"00247-0348-17 ")
 ;;94
 ;;21,"00247-0588-41 ")
 ;;175
 ;;21,"00247-0657-10 ")
 ;;297
 ;;21,"00247-0657-47 ")
 ;;298
 ;;21,"00247-0657-65 ")
 ;;299
 ;;21,"00247-0658-50 ")
 ;;300
 ;;21,"00247-0659-07 ")
 ;;46
 ;;21,"00247-0667-08 ")
 ;;285
 ;;21,"00247-0674-41 ")
 ;;286
 ;;21,"00247-0703-07 ")
 ;;53
 ;;21,"00247-0870-20 ")
 ;;63
 ;;21,"00247-0871-20 ")
 ;;324
 ;;21,"00247-0873-02 ")
 ;;278
 ;;21,"00247-0873-52 ")
 ;;279
 ;;21,"00247-0873-60 ")
 ;;280
 ;;21,"00247-0912-17 ")
 ;;95
 ;;21,"00247-1174-17 ")
 ;;96
 ;;21,"00247-1364-17 ")
 ;;97
 ;;21,"00247-1575-13 ")
 ;;233
 ;;21,"00247-1576-12 ")
 ;;234
 ;;21,"00247-1576-13 ")
 ;;235
 ;;21,"00247-1577-13 ")
 ;;236
 ;;21,"00247-1696-93 ")
 ;;370
 ;;21,"00247-1973-60 ")
 ;;12
 ;;21,"00247-1983-60 ")
 ;;13
 ;;21,"00247-2072-60 ")
 ;;359
 ;;21,"00247-2215-60 ")
 ;;14
 ;;21,"00247-2224-72 ")
 ;;383
 ;;21,"00247-2225-72 ")
 ;;384
 ;;21,"00247-2226-72 ")
 ;;385
 ;;21,"00247-2227-01 ")
 ;;396
 ;;21,"00247-2297-58 ")
 ;;172
 ;;21,"00378-6990-52 ")
 ;;98
 ;;21,"00378-6990-58 ")
 ;;99
 ;;21,"00378-6990-91 ")
 ;;100
 ;;21,"00378-6990-93 ")
 ;;101
 ;;21,"00378-6991-52 ")
 ;;102
 ;;21,"00378-6992-52 ")
 ;;103
 ;;21,"00378-6993-93 ")
 ;;293
 ;;21,"00456-0670-99 ")
 ;;54
 ;;21,"00456-0672-99 ")
 ;;47
 ;;21,"00472-0750-21 ")
 ;;212
 ;;21,"00472-0750-60 ")
 ;;213
 ;;21,"00472-0752-21 ")
 ;;214
 ;;21,"00472-0752-60 ")
 ;;215
 ;;21,"00472-0831-23 ")
 ;;64
 ;;21,"00472-0831-30 ")
 ;;65
 ;;21,"00472-0831-60 ")
 ;;66
 ;;21,"00472-0832-20 ")
 ;;67
 ;;21,"00472-0832-30 ")
 ;;68
 ;;21,"00472-1264-63 ")
 ;;104
 ;;21,"00487-0301-01 ")
 ;;105
 ;;21,"00487-9501-01 ")
 ;;106
 ;;21,"00487-9501-02 ")
 ;;107
 ;;21,"00487-9501-03 ")
 ;;108
 ;;21,"00487-9501-25 ")
 ;;109
 ;;21,"00487-9501-60 ")
 ;;110
 ;
OTHER ; OTHER ROUTINES
 D ^BGP12VB
 D ^BGP12VC
 D ^BGP12VD
 D ^BGP12VE
 D ^BGP12VF
 D ^BGP12VG
 D ^BGP12VH
 D ^BGP12VI
 D ^BGP12VJ
 D ^BGP12VK
 D ^BGP12VL
 D ^BGP12VM
 Q
