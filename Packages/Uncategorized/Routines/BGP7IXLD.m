BGP7IXLD ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON SEP 11, 2006 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;21,"50053-3111-02 ")
 ;;555
 ;;21,"50053-3112-01 ")
 ;;556
 ;;21,"50053-3112-02 ")
 ;;557
 ;;21,"50053-3816-03 ")
 ;;215
 ;;21,"50053-3816-08 ")
 ;;216
 ;;21,"50053-3817-03 ")
 ;;217
 ;;21,"50053-3817-08 ")
 ;;218
 ;;21,"50111-0467-01 ")
 ;;2022
 ;;21,"50111-0467-02 ")
 ;;558
 ;;21,"50111-0467-03 ")
 ;;2023
 ;;21,"50111-0467-04 ")
 ;;559
 ;;21,"50111-0467-07 ")
 ;;2024
 ;;21,"50111-0468-01 ")
 ;;2169
 ;;21,"50111-0468-02 ")
 ;;560
 ;;21,"50111-0468-03 ")
 ;;2170
 ;;21,"50111-0468-04 ")
 ;;561
 ;;21,"50111-0468-07 ")
 ;;2171
 ;;21,"50111-0469-01 ")
 ;;2025
 ;;21,"50111-0469-02 ")
 ;;562
 ;;21,"50111-0469-03 ")
 ;;2026
 ;;21,"50111-0469-04 ")
 ;;563
 ;;21,"50111-0469-07 ")
 ;;2027
 ;;21,"50111-0470-01 ")
 ;;2070
 ;;21,"50111-0470-02 ")
 ;;2071
 ;;21,"50111-0470-03 ")
 ;;564
 ;;21,"50111-0470-04 ")
 ;;565
 ;;21,"50111-0471-01 ")
 ;;2072
 ;;21,"50111-0471-02 ")
 ;;2073
 ;;21,"50111-0471-03 ")
 ;;566
 ;;21,"50111-0471-04 ")
 ;;567
 ;;21,"50111-0472-03 ")
 ;;568
 ;;21,"50111-0472-04 ")
 ;;569
 ;;21,"50111-0473-01 ")
 ;;2203
 ;;21,"50111-0473-02 ")
 ;;2204
 ;;21,"50111-0473-03 ")
 ;;1115
 ;;21,"50111-0474-01 ")
 ;;2115
 ;;21,"50111-0474-02 ")
 ;;2116
 ;;21,"50111-0474-03 ")
 ;;1116
 ;;21,"50111-0506-01 ")
 ;;23
 ;;21,"50111-0506-02 ")
 ;;24
 ;;21,"50111-0506-03 ")
 ;;25
 ;;21,"50111-0507-01 ")
 ;;26
 ;;21,"50111-0507-02 ")
 ;;27
 ;;21,"50111-0507-03 ")
 ;;28
 ;;21,"50111-0508-01 ")
 ;;1060
 ;;21,"50111-0508-02 ")
 ;;1061
 ;;21,"50111-0508-03 ")
 ;;1062
 ;;21,"50111-0509-01 ")
 ;;1063
 ;;21,"50111-0509-02 ")
 ;;1064
 ;;21,"50111-0509-03 ")
 ;;1065
 ;;21,"50111-0553-01 ")
 ;;29
 ;;21,"50111-0553-02 ")
 ;;30
 ;;21,"50111-0553-03 ")
 ;;31
 ;;21,"50111-0855-01 ")
 ;;317
 ;;21,"50111-0855-03 ")
 ;;318
 ;;21,"50111-0855-06 ")
 ;;319
 ;;21,"50111-0856-01 ")
 ;;256
 ;;21,"50111-0856-03 ")
 ;;257
 ;;21,"50419-0105-10 ")
 ;;2057
 ;;21,"50419-0105-11 ")
 ;;2058
 ;;21,"50419-0106-10 ")
 ;;2059
 ;;21,"50419-0106-11 ")
 ;;2060
 ;;21,"50419-0107-10 ")
 ;;2106
 ;;21,"50419-0107-11 ")
 ;;2107
 ;;21,"50419-0109-10 ")
 ;;2061
 ;;21,"50419-0109-11 ")
 ;;2062
 ;;21,"50419-0115-06 ")
 ;;2036
 ;;21,"50419-0115-11 ")
 ;;2037
 ;;21,"50419-0116-06 ")
 ;;2038
 ;;21,"50419-0116-11 ")
 ;;2039
 ;;21,"50419-0119-06 ")
 ;;2040
 ;;21,"50419-0119-11 ")
 ;;2041
 ;;21,"50752-0308-05 ")
 ;;2080
 ;;21,"50752-0308-09 ")
 ;;2081
 ;;21,"50752-0309-05 ")
 ;;2082
 ;;21,"51079-0255-01 ")
 ;;271
 ;;21,"51079-0255-20 ")
 ;;2051
 ;;21,"51079-0277-01 ")
 ;;570
 ;;21,"51079-0277-19 ")
 ;;2015
 ;;21,"51079-0277-20 ")
 ;;2016
 ;;21,"51079-0278-19 ")
 ;;833
 ;;21,"51079-0278-20 ")
 ;;2165
 ;;21,"51079-0279-20 ")
 ;;2017
 ;;21,"51079-0280-20 ")
 ;;2014
 ;;21,"51079-0684-19 ")
 ;;2172
 ;;21,"51079-0684-20 ")
 ;;2173
 ;;21,"51079-0684-24 ")
 ;;168
 ;;21,"51079-0685-20 ")
 ;;2052
 ;;21,"51079-0759-19 ")
 ;;2076
 ;;21,"51079-0759-20 ")
 ;;2077
 ;;21,"51079-0801-19 ")
 ;;2093
 ;;21,"51079-0801-20 ")
 ;;2094
 ;;21,"51079-0801-23 ")
 ;;320
 ;;21,"51079-0801-24 ")
 ;;2095
 ;;21,"51079-0801-57 ")
 ;;2096
 ;;21,"51079-0802-01 ")
 ;;258
 ;;21,"51079-0802-19 ")
 ;;2097
 ;;21,"51079-0802-20 ")
 ;;2098
 ;;21,"51079-0812-20 ")
 ;;2032
 ;;21,"51079-0813-20 ")
 ;;2153
 ;;21,"51079-0814-20 ")
 ;;2154
 ;;21,"51079-0928-01 ")
 ;;1164
 ;;21,"51079-0928-20 ")
 ;;2042
 ;;21,"51079-0929-01 ")
 ;;1223
 ;;21,"51079-0929-20 ")
 ;;2155
 ;;21,"51079-0954-20 ")
 ;;2074
 ;;21,"51079-0955-20 ")
 ;;2075
 ;;21,"51079-0956-20 ")
 ;;2168
 ;;21,"51129-0232-01 ")
 ;;419
 ;;21,"51129-1109-01 ")
 ;;321
 ;;21,"51129-1126-01 ")
 ;;1143
 ;;21,"51129-1304-01 ")
 ;;741
 ;;21,"51129-1328-01 ")
 ;;1066
 ;;21,"51129-1419-01 ")
 ;;322
 ;;21,"51129-1585-01 ")
 ;;1067
 ;;21,"51129-1593-01 ")
 ;;2905
 ;;21,"51129-1652-01 ")
 ;;1044
 ;;21,"51129-1653-01 ")
 ;;1045
 ;;21,"51129-8233-01 ")
 ;;571
 ;;21,"51129-9912-01 ")
 ;;572
 ;;21,"51285-0040-01 ")
 ;;2156
 ;;21,"51285-0047-02 ")
 ;;2065
 ;;21,"51285-0048-02 ")
 ;;2066
 ;;21,"51285-0049-01 ")
 ;;2157
 ;;21,"51285-0050-02 ")
 ;;2067
 ;;21,"51285-0060-01 ")
 ;;2068
 ;;21,"51285-0061-01 ")
 ;;2069
 ;;21,"51285-0320-02 ")
 ;;936
 ;;21,"51285-0320-04 ")
 ;;937
 ;;21,"51285-0321-02 ")
 ;;573
 ;;21,"51285-0321-05 ")
 ;;574
 ;;21,"51285-0321-09 ")
 ;;575
 ;;21,"51285-0322-02 ")
 ;;834
 ;;21,"51285-0322-05 ")
 ;;835
 ;;21,"51285-0322-09 ")
 ;;836
 ;;21,"51285-0323-02 ")
 ;;878
 ;;21,"51285-0323-05 ")
 ;;879
 ;;21,"51285-0323-09 ")
 ;;880
 ;;21,"51285-0324-02 ")
 ;;975
 ;;21,"51285-0324-04 ")
 ;;976
 ;;21,"51285-0332-02 ")
 ;;1117
 ;;21,"51285-0333-02 ")
 ;;1130
 ;;21,"51285-0836-02 ")
 ;;2099
 ;;21,"51285-0836-04 ")
 ;;2100
 ;;21,"51285-0836-05 ")
 ;;2101
 ;;21,"51285-0837-02 ")
 ;;2174
 ;;21,"51285-0837-04 ")
 ;;2175
 ;;21,"51285-0837-05 ")
 ;;2176
 ;;21,"51285-0838-02 ")
 ;;2053
 ;;21,"51285-0838-04 ")
 ;;2054
 ;;21,"51285-0838-05 ")
 ;;2055
 ;;21,"51285-0906-02 ")
 ;;479
 ;;21,"51432-0073-03 ")
 ;;497
 ;;21,"51432-0075-03 ")
 ;;480
 ;;21,"51432-0395-03 ")
 ;;1118
 ;;21,"51432-0397-03 ")
 ;;1131
 ;;21,"51432-0449-03 ")
 ;;133
 ;;21,"51432-0567-03 ")
 ;;1056
 ;;21,"51432-0569-03 ")
 ;;1029
 ;;21,"51432-0571-03 ")
 ;;1046
 ;;21,"51432-0937-03 ")
 ;;908
 ;;21,"51432-0938-03 ")
 ;;952
 ;;21,"51432-0939-03 ")
 ;;742
 ;;21,"51432-0941-03 ")
 ;;808
 ;;21,"51432-0971-06 ")
 ;;576
 ;;21,"51432-0972-06 ")
 ;;837
 ;;21,"51432-0973-06 ")
 ;;881
 ;;21,"51432-0974-03 ")
 ;;938
 ;;21,"51432-0975-05 ")
 ;;977
 ;;21,"51655-0019-24 ")
 ;;169
 ;;21,"51655-0019-25 ")
 ;;170
 ;;21,"51655-0060-25 ")
 ;;1165
 ;;21,"51655-0149-24 ")
 ;;323
 ;;21,"51655-0149-25 ")
 ;;324
 ;;21,"51655-0149-53 ")
 ;;325
 ;;21,"51655-0284-24 ")
 ;;2965
 ;;21,"51655-0319-24 ")
 ;;171
 ;;21,"51655-0319-25 ")
 ;;172
 ;;21,"51655-0331-24 ")
 ;;420
 ;;21,"51655-0345-24 ")
 ;;326
 ;;21,"51655-0349-24 ")
 ;;2114
 ;;21,"51655-0349-25 ")
 ;;577
 ;;21,"51655-0349-26 ")
 ;;578
 ;;21,"51655-0349-82 ")
 ;;579
 ;;21,"51655-0350-21 ")
 ;;580
 ;;21,"51655-0350-24 ")
 ;;2207
 ;;21,"51655-0350-25 ")
 ;;581
 ;;21,"51655-0350-26 ")
 ;;582
 ;;21,"51655-0350-82 ")
 ;;583
 ;;21,"51655-0381-21 ")
 ;;584
 ;;21,"51655-0381-24 ")
 ;;585
 ;;21,"51655-0381-82 ")
 ;;586
 ;;21,"51655-0382-24 ")
 ;;587
 ;;21,"51655-0382-25 ")
 ;;588
 ;;21,"51655-0382-26 ")
 ;;589
 ;;21,"51655-0382-82 ")
 ;;590
 ;;21,"51655-0418-25 ")
 ;;1224
 ;;21,"51655-0421-24 ")
 ;;743
 ;;21,"51655-0530-24 ")
 ;;173
 ;;21,"51655-0530-53 ")
 ;;174
 ;;21,"51655-0532-24 ")
 ;;2092
 ;;21,"51655-0928-26 ")
 ;;327
 ;;21,"51655-0928-52 ")
 ;;328
 ;;21,"51655-0928-82 ")
 ;;329
 ;;21,"51947-3300-01 ")
 ;;1166
 ;;21,"51947-3301-01 ")
 ;;1167
 ;;21,"51947-3302-01 ")
 ;;1168
 ;;21,"51947-3340-01 ")
 ;;1169
 ;;21,"51947-3340-02 ")
 ;;1170
 ;;21,"52152-0179-02 ")
 ;;2180
 ;;21,"52152-0180-02 ")
 ;;2056
 ;;21,"52544-0305-01 ")
 ;;2131
 ;;21,"52544-0305-05 ")
 ;;591
 ;;21,"52544-0305-10 ")
 ;;2132
 ;;21,"52544-0305-51 ")
 ;;592
 ;;21,"52544-0306-01 ")
 ;;2183
 ;;21,"52544-0306-05 ")
 ;;593
 ;;21,"52544-0306-10 ")
 ;;2184
 ;;21,"52544-0306-51 ")
 ;;594
 ;;21,"52544-0307-01 ")
 ;;2133
 ;;21,"52544-0307-05 ")
 ;;595
 ;;21,"52544-0307-10 ")
 ;;2134
 ;;21,"52544-0307-51 ")
 ;;596
 ;;21,"52544-0308-01 ")
 ;;2136
 ;;21,"52544-0308-05 ")
 ;;2137
 ;;21,"52544-0308-10 ")
 ;;597
 ;;21,"52544-0352-01 ")
 ;;2138
 ;;21,"52544-0352-05 ")
 ;;2139
 ;;21,"52544-0352-10 ")
 ;;598
 ;;21,"52544-0353-05 ")
 ;;599
 ;;21,"52544-0437-01 ")
 ;;2202
 ;;21,"52544-0437-10 ")
 ;;2906
 ;;21,"52544-0438-01 ")
 ;;2140
 ;;21,"52544-0438-10 ")
 ;;2907
 ;;21,"52544-0462-01 ")
 ;;2123
 ;;21,"52544-0462-10 ")
 ;;2124
 ;;21,"52544-0462-30 ")
 ;;330
 ;;21,"52544-0463-01 ")
 ;;2125
 ;;21,"52544-0463-10 ")
 ;;2126
 ;;21,"52544-0463-30 ")
 ;;331
 ;;21,"52544-0605-01 ")
 ;;2119
 ;;21,"52544-0605-05 ")
 ;;2120
 ;;21,"52544-0605-10 ")
 ;;1225
 ;;21,"52544-0606-01 ")
 ;;2217
 ;;21,"52544-0606-05 ")
 ;;2218
 ;;21,"52544-0606-10 ")
 ;;1226
 ;;21,"52544-0607-01 ")
 ;;2141
 ;;21,"52544-0607-05 ")
 ;;1227
 ;;21,"52544-0607-10 ")
 ;;1228
 ;;21,"52544-0654-01 ")
 ;;2102
 ;;21,"52544-0655-01 ")
 ;;2103
 ;;21,"52544-0656-01 ")
 ;;2135
 ;;21,"52544-0665-01 ")
 ;;2104
 ;;21,"52544-0710-05 ")
 ;;498
 ;;21,"52544-0711-01 ")
 ;;2105
 ;;21,"52544-0841-01 ")
 ;;2185
 ;;21,"52544-0841-05 ")
 ;;2186
 ;;21,"52544-0842-01 ")
 ;;2187
 ;;21,"52544-0842-05 ")
 ;;2188
 ;;21,"52544-0843-30 ")
 ;;2208
 ;;21,"52555-0006-01 ")
 ;;2237
 ;;21,"52555-0006-10 ")
 ;;2238
 ;;21,"52555-0007-01 ")
 ;;2142
 ;;21,"52555-0249-01 ")
 ;;1119
 ;;21,"52555-0250-01 ")
 ;;1132
 ;;21,"52555-0343-10 ")
 ;;2163
 ;;21,"52555-0344-10 ")
 ;;2333
 ;;21,"52555-0345-10 ")
 ;;2164
 ;;21,"52555-0346-01 ")
 ;;978
 ;;21,"52555-0450-01 ")
 ;;2357
 ;;21,"52555-0450-10 ")
 ;;1082
 ;;21,"52555-0451-01 ")
 ;;2199
 ;;21,"52555-0451-10 ")
 ;;1068
 ;;21,"52555-0454-01 ")
 ;;2143
 ;;21,"52555-0454-10 ")
 ;;499
 ;;21,"52555-0455-01 ")
 ;;2144
 ;;21,"52555-0455-10 ")
 ;;481
 ;;21,"52555-0471-01 ")
 ;;500
 ;;21,"52555-0472-01 ")
 ;;482
 ;;21,"52555-0499-01 ")
 ;;332
 ;;21,"52555-0499-10 ")
 ;;2201
 ;;21,"52555-0500-01 ")
 ;;259
 ;;21,"52555-0500-10 ")
 ;;260
 ;;21,"52555-0531-01 ")
 ;;2335
 ;;21,"52555-0531-05 ")
 ;;175
 ;;21,"52555-0531-10 ")
 ;;2336
 ;;21,"52555-0534-01 ")
 ;;2145
 ;;21,"52555-0545-01 ")
 ;;2146
 ;;21,"52555-0546-01 ")
 ;;2147
 ;;21,"52555-0547-01 ")
 ;;2358
 ;;21,"52555-0548-01 ")
 ;;2200
 ;;21,"52555-0668-01 ")
 ;;501
 ;;21,"52555-0669-01 ")
 ;;483
 ;;21,"52555-0673-01 ")
 ;;2337
 ;;21,"52555-0673-10 ")
 ;;2338
 ;;21,"52555-0674-01 ")
 ;;2148
 ;;21,"52555-0689-01 ")
 ;;134
 ;;21,"52903-1421-01 ")
 ;;600
 ;;21,"52903-1421-02 ")
 ;;601
 ;;21,"52903-1422-03 ")
 ;;602
 ;;21,"52903-1422-04 ")
 ;;603
 ;;21,"52903-1424-03 ")
 ;;604
 ;;21,"52903-1424-04 ")
 ;;605
 ;;21,"52903-1426-02 ")
 ;;606
 ;;21,"52903-1428-01 ")
 ;;607
 ;;21,"52903-1428-03 ")
 ;;608
 ;;21,"52903-1455-01 ")
 ;;1134
 ;;21,"52903-1457-01 ")
 ;;1135
 ;;21,"52903-1459-01 ")
 ;;1136
 ;;21,"52903-1471-08 ")
 ;;744
 ;;21,"52903-1471-09 ")
 ;;745
 ;;21,"52903-1473-01 ")
 ;;746
 ;;21,"52903-1473-02 ")
 ;;747
 ;;21,"52903-1479-04 ")
 ;;748
 ;;21,"52903-1479-05 ")
 ;;749
 ;;21,"52903-1484-02 ")
 ;;1120
 ;;21,"52903-1488-01 ")
 ;;1121
 ;;21,"52903-2471-05 ")
 ;;750
 ;;21,"52903-2471-06 ")
 ;;751
 ;;21,"52903-2471-07 ")
 ;;752
 ;;21,"52903-3816-02 ")
 ;;219
 ;;21,"52903-3816-08 ")
 ;;220
 ;;21,"52903-3817-08 ")
 ;;221
 ;;21,"52903-4177-01 ")
 ;;2915
 ;;21,"52903-4179-01 ")
 ;;2908
 ;;21,"52903-5470-01 ")
 ;;909
 ;;21,"52903-5470-02 ")
 ;;910
 ;;21,"52903-5471-01 ")
 ;;911
 ;;21,"52903-5471-02 ")
 ;;912
 ;;21,"52903-5473-01 ")
 ;;913
 ;;21,"52903-5473-02 ")
 ;;914
 ;;21,"52903-5479-01 ")
 ;;915
 ;;21,"52903-5479-02 ")
 ;;916
 ;;21,"52903-5842-02 ")
 ;;2909
 ;;21,"52903-5844-02 ")
 ;;2910
 ;;21,"52932-0720-00 ")
 ;;213
 ;;21,"52959-0134-10 ")
 ;;2158
 ;;21,"52959-0134-30 ")
 ;;2159
 ;;21,"52959-0137-30 ")
 ;;2246
 ;;21,"52959-0212-10 ")
 ;;2247
 ;;21,"52959-0212-20 ")
 ;;2248
 ;;21,"52959-0241-30 ")
 ;;2266
 ;;21,"52959-0247-30 ")
 ;;2160
 ;;21,"52959-0253-00 ")
 ;;2278
 ;;21,"52959-0253-20 ")
 ;;176
 ;;21,"52959-0253-30 ")
 ;;2279
 ;;21,"52959-0253-40 ")
 ;;2280
 ;;21,"52959-0280-30 ")
 ;;2269
 ;;21,"52959-0337-10 ")
 ;;2161
 ;;21,"52959-0337-30 ")
 ;;2162
 ;;21,"52959-0463-01 ")
 ;;2149
 ;;21,"52959-0463-30 ")
 ;;32
 ;;21,"52985-0003-01 ")
 ;;421
 ;;21,"52985-0019-01 ")
 ;;609
 ;;21,"52985-0036-01 ")
 ;;610
 ;;21,"52985-0037-01 ")
 ;;611
 ;;21,"52985-0119-01 ")
 ;;1229
 ;;21,"52985-0134-01 ")
 ;;1171
 ;;21,"52985-0140-01 ")
 ;;753
 ;;21,"52985-0141-01 ")
 ;;754
 ;;21,"52985-0157-01 ")
 ;;755
 ;;21,"52985-0162-01 ")
 ;;1230
 ;;21,"52985-0189-01 ")
 ;;177
 ;;21,"52985-0219-01 ")
 ;;1172
 ;;21,"53002-0360-00 ")
 ;;612
 ;;21,"53002-0360-20 ")
 ;;613
 ;;21,"53002-0360-30 ")
 ;;614
 ;;21,"53002-0360-50 ")
 ;;615
 ;;21,"53002-0360-60 ")
 ;;616
 ;;21,"53002-0495-00 ")
 ;;617
 ;;21,"53002-0495-20 ")
 ;;618
 ;;21,"53002-0495-30 ")
 ;;619
 ;;21,"53002-0495-40 ")
 ;;620
 ;;21,"53002-0495-60 ")
 ;;621
 ;;21,"53002-1005-00 ")
 ;;333
 ;;21,"53002-1005-02 ")
 ;;334
 ;;21,"53002-1005-03 ")
 ;;335
 ;;21,"53002-1005-05 ")
 ;;336
 ;;21,"53002-1005-06 ")
 ;;337
 ;;21,"53002-1010-00 ")
 ;;1231
 ;;21,"53002-1010-02 ")
 ;;1232
 ;;21,"53002-1010-03 ")
 ;;1233
 ;;21,"53002-1010-05 ")
 ;;1234
 ;;21,"53002-1010-06 ")
 ;;1235
 ;;21,"53002-1018-00 ")
 ;;422
 ;;21,"53002-1018-03 ")
 ;;423
 ;;21,"53002-1018-05 ")
 ;;424
 ;;21,"53002-1018-06 ")
 ;;425
 ;;21,"53002-1018-09 ")
 ;;426
 ;;21,"53002-1026-00 ")
 ;;338
 ;;21,"53002-1026-02 ")
 ;;339
