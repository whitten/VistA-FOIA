BGPM5BEG ;IHS/MSC/MMT-CREATED BY ^ATXSTX ON SEP 12, 2011;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;21,"00378112501 ")
 ;;4104
 ;;21,"00378112510 ")
 ;;4105
 ;;21,"00378114201 ")
 ;;4106
 ;;21,"00378114210 ")
 ;;4107
 ;;21,"00378282010 ")
 ;;4372
 ;;21,"00378282027 ")
 ;;4373
 ;;21,"00378282077 ")
 ;;4374
 ;;21,"00378282110 ")
 ;;4375
 ;;21,"00378282127 ")
 ;;4376
 ;;21,"00378282177 ")
 ;;4377
 ;;21,"00378282210 ")
 ;;4378
 ;;21,"00378282227 ")
 ;;4379
 ;;21,"00378282277 ")
 ;;4380
 ;;21,"00378313101 ")
 ;;222
 ;;21,"00378313105 ")
 ;;223
 ;;21,"00378313201 ")
 ;;224
 ;;21,"00378313205 ")
 ;;225
 ;;21,"00378313301 ")
 ;;226
 ;;21,"00378313305 ")
 ;;227
 ;;21,"00378401101 ")
 ;;4114
 ;;21,"00378401105 ")
 ;;4115
 ;;21,"00378401201 ")
 ;;4116
 ;;21,"00378401205 ")
 ;;4117
 ;;21,"00378401301 ")
 ;;4118
 ;;21,"00378401305 ")
 ;;4119
 ;;21,"00378718501 ")
 ;;1829
 ;;21,"00378718505 ")
 ;;1830
 ;;21,"00378718591 ")
 ;;1831
 ;;21,"00378718601 ")
 ;;1832
 ;;21,"00378718605 ")
 ;;1833
 ;;21,"00378718691 ")
 ;;1834
 ;;21,"00378718701 ")
 ;;1835
 ;;21,"00378718705 ")
 ;;1836
 ;;21,"00378718791 ")
 ;;1837
 ;;21,"00406202800 ")
 ;;1838
 ;;21,"00406202801 ")
 ;;1839
 ;;21,"00406202805 ")
 ;;1840
 ;;21,"00406202810 ")
 ;;1841
 ;;21,"00406202900 ")
 ;;1842
 ;;21,"00406202901 ")
 ;;1843
 ;;21,"00406202905 ")
 ;;1844
 ;;21,"00406202910 ")
 ;;1845
 ;;21,"00406203000 ")
 ;;1846
 ;;21,"00406203001 ")
 ;;1847
 ;;21,"00406203005 ")
 ;;1848
 ;;21,"00406203010 ")
 ;;1849
 ;;21,"00440756630 ")
 ;;4120
 ;;21,"00440756660 ")
 ;;4121
 ;;21,"00440756690 ")
 ;;4122
 ;;21,"00440756691 ")
 ;;4123
 ;;21,"00440756692 ")
 ;;4124
 ;;21,"00440756990 ")
 ;;4125
 ;;21,"00440757020 ")
 ;;4126
 ;;21,"00440757130 ")
 ;;4127
 ;;21,"00440757160 ")
 ;;4128
 ;;21,"00440757190 ")
 ;;4129
 ;;21,"00440757191 ")
 ;;4130
 ;;21,"00440757192 ")
 ;;4131
 ;;21,"00440757194 ")
 ;;4132
 ;;21,"00440757195 ")
 ;;4133
 ;;21,"00440773900 ")
 ;;1850
 ;;21,"00440773902 ")
 ;;1851
 ;;21,"00440773910 ")
 ;;1852
 ;;21,"00440773920 ")
 ;;1853
 ;;21,"00440773928 ")
 ;;1854
 ;;21,"00440773960 ")
 ;;1855
 ;;21,"00440773992 ")
 ;;1856
 ;;21,"00440773994 ")
 ;;1857
 ;;21,"00440774892 ")
 ;;1858
 ;;21,"00480104801 ")
 ;;1859
 ;;21,"00480104805 ")
 ;;1860
 ;;21,"00480104810 ")
 ;;1861
 ;;21,"00480104901 ")
 ;;1862
 ;;21,"00480104905 ")
 ;;1863
 ;;21,"00480104910 ")
 ;;1864
 ;;21,"00480571001 ")
 ;;228
 ;;21,"00480571005 ")
 ;;229
 ;;21,"00480571083 ")
 ;;230
 ;;21,"00480571101 ")
 ;;231
 ;;21,"00480571105 ")
 ;;232
 ;;21,"00480571115 ")
 ;;233
 ;;21,"00480571201 ")
 ;;234
 ;;21,"00480571205 ")
 ;;235
 ;;21,"00480571215 ")
 ;;236
 ;;21,"00480721201 ")
 ;;1874
 ;;21,"00480721401 ")
 ;;1875
 ;;21,"00480721405 ")
 ;;1876
 ;;21,"00480721410 ")
 ;;1877
 ;;21,"00480725401 ")
 ;;4143
 ;;21,"00480725501 ")
 ;;4144
 ;;21,"00480725601 ")
 ;;4145
 ;;21,"00480725652 ")
 ;;4146
 ;;21,"00480726001 ")
 ;;237
 ;;21,"00480726101 ")
 ;;238
 ;;21,"00480726105 ")
 ;;239
 ;;21,"00480726201 ")
 ;;240
 ;;21,"00480726205 ")
 ;;241
 ;;21,"00480726701 ")
 ;;1883
 ;;21,"00480726710 ")
 ;;1884
 ;;21,"00480745501 ")
 ;;242
 ;;21,"00480745601 ")
 ;;243
 ;;21,"00480745701 ")
 ;;244
 ;;21,"00555010702 ")
 ;;1888
 ;;21,"00555038501 ")
 ;;1889
 ;;21,"00555038502 ")
 ;;1890
 ;;21,"00555038504 ")
 ;;1891
 ;;21,"00555038601 ")
 ;;1892
 ;;21,"00555038602 ")
 ;;1893
 ;;21,"00555038610 ")
 ;;1894
 ;;21,"00555038701 ")
 ;;1895
 ;;21,"00555038702 ")
 ;;1896
 ;;21,"00555038704 ")
 ;;1897
 ;;21,"00555044202 ")
 ;;4155
 ;;21,"00555044204 ")
 ;;4156
 ;;21,"00555044205 ")
 ;;4157
 ;;21,"00555044220 ")
 ;;4158
 ;;21,"00555044302 ")
 ;;4159
 ;;21,"00555044304 ")
 ;;4160
 ;;21,"00555044305 ")
 ;;4161
 ;;21,"00555044320 ")
 ;;4162
 ;;21,"00555062502 ")
 ;;245
 ;;21,"00555062602 ")
 ;;246
 ;;21,"00555062702 ")
 ;;247
 ;;21,"00591046000 ")
 ;;4166
 ;;21,"00591046001 ")
 ;;4167
 ;;21,"00591046005 ")
 ;;4168
 ;;21,"00591046010 ")
 ;;4169
 ;;21,"00591046100 ")
 ;;4170
 ;;21,"00591046101 ")
 ;;4171
 ;;21,"00591046105 ")
 ;;4172
 ;;21,"00591046110 ")
 ;;4173
 ;;21,"00591084401 ")
 ;;4174
 ;;21,"00591084410 ")
 ;;4175
 ;;21,"00591084415 ")
 ;;4176
 ;;21,"00591084501 ")
 ;;4177
 ;;21,"00591084510 ")
 ;;4178
 ;;21,"00591084515 ")
 ;;4179
 ;;21,"00591090001 ")
 ;;4180
 ;;21,"00591090010 ")
 ;;4181
 ;;21,"00591090030 ")
 ;;4182
 ;;21,"00591245501 ")
 ;;1901
 ;;21,"00591245505 ")
 ;;1902
 ;;21,"00591245510 ")
 ;;1903
 ;;21,"00591271301 ")
 ;;1904
 ;;21,"00591271305 ")
 ;;1905
 ;;21,"00591271310 ")
 ;;1906
 ;;21,"00591277501 ")
 ;;1907
 ;;21,"00591277525 ")
 ;;1908
 ;;21,"00591277560 ")
 ;;1909
 ;;21,"00591335401 ")
 ;;2025
 ;;21,"00591335410 ")
 ;;2026
 ;;21,"00591335430 ")
 ;;2027
 ;;21,"00591335501 ")
 ;;2028
 ;;21,"00591335510 ")
 ;;2029
 ;;21,"00591335530 ")
 ;;2030
 ;;21,"00591397101 ")
 ;;248
 ;;21,"00591397201 ")
 ;;249
 ;;21,"00591397301 ")
 ;;250
 ;;21,"00603283521 ")
 ;;4186
 ;;21,"00603283528 ")
 ;;4187
 ;;21,"00603283621 ")
 ;;4188
 ;;21,"00603283628 ")
 ;;4189
 ;;21,"00603283632 ")
 ;;4190
 ;;21,"00603374421 ")
 ;;4191
 ;;21,"00603374428 ")
 ;;4192
 ;;21,"00603374521 ")
 ;;4193
 ;;21,"00603374528 ")
 ;;4194
 ;;21,"00603374621 ")
 ;;4195
 ;;21,"00603374628 ")
 ;;4196
 ;;21,"00603375521 ")
 ;;4197
 ;;21,"00603375528 ")
 ;;4198
 ;;21,"00603375621 ")
 ;;4199
 ;;21,"00603375628 ")
 ;;4200
 ;;21,"00615155631 ")
 ;;4201
 ;;21,"00615155639 ")
 ;;4202
 ;;21,"00615359505 ")
 ;;4203
 ;;21,"00615359539 ")
 ;;4204
 ;;21,"00615359631 ")
 ;;4205
 ;;21,"00615359639 ")
 ;;4206
 ;;21,"00615450939 ")
 ;;4207
 ;;21,"00615458031 ")
 ;;1913
 ;;21,"00615458039 ")
 ;;1914
 ;;21,"00615550531 ")
 ;;1915
 ;;21,"00615550539 ")
 ;;1916
 ;;21,"00615558331 ")
 ;;1917
 ;;21,"00615558339 ")
 ;;1918
 ;;21,"00615558431 ")
 ;;4208
 ;;21,"00615558439 ")
 ;;4209
 ;;21,"00615558531 ")
 ;;4210
 ;;21,"00615558539 ")
 ;;4211
 ;;21,"00615559031 ")
 ;;1919
 ;;21,"00615559039 ")
 ;;1920
 ;;21,"00615657539 ")
 ;;4212
 ;;21,"00615657639 ")
 ;;4213
 ;;21,"00615659639 ")
 ;;4214
 ;;21,"00615752339 ")
 ;;4215
 ;;21,"00615758539 ")
 ;;2031
 ;;21,"00615758639 ")
 ;;2032
 ;;21,"00615762631 ")
 ;;1921
 ;;21,"00615762639 ")
 ;;1922
 ;;21,"00677154401 ")
 ;;4216
 ;;21,"00677154405 ")
 ;;4217
 ;;21,"00677154501 ")
 ;;4218
 ;;21,"00677154505 ")
 ;;4219
 ;;21,"00781113801 ")
 ;;4220
 ;;21,"00781113810 ")
 ;;4221
 ;;21,"00781114601 ")
 ;;4222
 ;;21,"00781114610 ")
 ;;4223
 ;;21,"00781119101 ")
 ;;4224
 ;;21,"00781119110 ")
 ;;4225
 ;;21,"00781119111 ")
 ;;4226
 ;;21,"00781145201 ")
 ;;4227
 ;;21,"00781145210 ")
 ;;4228
 ;;21,"00781145213 ")
 ;;4229
 ;;21,"00781145301 ")
 ;;4230
 ;;21,"00781145310 ")
 ;;4231
 ;;21,"00781145313 ")
 ;;4232
 ;;21,"00781504501 ")
 ;;4233
 ;;21,"00781504510 ")
 ;;4234
 ;;21,"00781504601 ")
 ;;4235
 ;;21,"00781504610 ")
 ;;4236
 ;;21,"00781504701 ")
 ;;4237
 ;;21,"00781504710 ")
 ;;4238
 ;;21,"00781505001 ")
 ;;1923
 ;;21,"00781505005 ")
 ;;1924
 ;;21,"00781505010 ")
 ;;1925
 ;;21,"00781505061 ")
 ;;1926
 ;;21,"00781505101 ")
 ;;1927
 ;;21,"00781505105 ")
 ;;1928
 ;;21,"00781505161 ")
 ;;1929
 ;;21,"00781505201 ")
 ;;1930
 ;;21,"00781505205 ")
 ;;1931
 ;;21,"00781505261 ")
 ;;1932
 ;;21,"00781517001 ")
 ;;251
 ;;21,"00781517005 ")
 ;;252
 ;;21,"00781517010 ")
 ;;253
 ;;21,"00781517101 ")
 ;;254
 ;;21,"00781517105 ")
 ;;255
 ;;21,"00781517110 ")
 ;;256
 ;;21,"00781517201 ")
 ;;257
 ;;21,"00781517205 ")
 ;;258
 ;;21,"00781517210 ")
 ;;259
 ;;21,"00781530401 ")
 ;;260
 ;;21,"00781530405 ")
 ;;261
 ;;21,"00781530410 ")
 ;;262
 ;;21,"00781530501 ")
 ;;263
 ;;21,"00781530505 ")
 ;;264
 ;;21,"00781530510 ")
 ;;265
 ;;21,"00781530601 ")
 ;;266
 ;;21,"00781530605 ")
 ;;267
 ;;21,"00781530610 ")
 ;;268
 ;;21,"00855202950 ")
 ;;4257
 ;;21,"00855203550 ")
 ;;4258
 ;;21,"00855203650 ")
 ;;4259
 ;;21,"00904022360 ")
 ;;4260
 ;;21,"00904022380 ")
 ;;4261
 ;;21,"00904022540 ")
 ;;4262
 ;;21,"00904022560 ")
 ;;4263
 ;;21,"00904022660 ")
 ;;4264
 ;;21,"00904022680 ")
 ;;4265
 ;;21,"00904023460 ")
 ;;4266
 ;;21,"00904023660 ")
 ;;4267
 ;;21,"00904507560 ")
 ;;4268
 ;;21,"00904507660 ")
 ;;4269
 ;;21,"00904507740 ")
 ;;4270
 ;;21,"00904507760 ")
 ;;4271
 ;;21,"00904507780 ")
 ;;4272
 ;;21,"00904560118 ")
 ;;1951
 ;;21,"00904560152 ")
 ;;1952
 ;;21,"00904560154 ")
 ;;1953
 ;;21,"00904560161 ")
 ;;1954
 ;;21,"00904560180 ")
 ;;1955
 ;;21,"00904560189 ")
 ;;1956
 ;;21,"00904560193 ")
 ;;1957
 ;;21,"00904560240 ")
 ;;1958
 ;;21,"00904560253 ")
 ;;1959
 ;;21,"00904560261 ")
 ;;1960
 ;;21,"00904560289 ")
 ;;1961
 ;;21,"00904560293 ")
 ;;1962
 ;;21,"00904560340 ")
 ;;1963
 ;;21,"00904560352 ")
 ;;1964
 ;;21,"00904560361 ")
 ;;1965
 ;;21,"00904560389 ")
 ;;1966
 ;;21,"00904560393 ")
 ;;1967
 ;;21,"00904563461 ")
 ;;1968
 ;;21,"00904563561 ")
 ;;1969
 ;;21,"00904563661 ")
 ;;1970
 ;;21,"00904579461 ")
 ;;1971
 ;;21,"00904579561 ")
 ;;1972
 ;;21,"00904584914 ")
 ;;1973
 ;;21,"00904584918 ")
 ;;1974
 ;;21,"00904584940 ")
 ;;1975
 ;;21,"00904584952 ")
 ;;1976
 ;;21,"00904584953 ")
 ;;1977
 ;;21,"00904584954 ")
 ;;1978
 ;;21,"00904584980 ")
 ;;1979
 ;;21,"00904584989 ")
 ;;1980
 ;;21,"00904584993 ")
 ;;1981
 ;;21,"00904585040 ")
 ;;1982
 ;;21,"00904585053 ")
 ;;1983
 ;;21,"00904585089 ")
 ;;1984
 ;;21,"00904585093 ")
 ;;1985
 ;;21,"00904585140 ")
 ;;1986
 ;;21,"00904585152 ")
 ;;1987
 ;;21,"00904585189 ")
 ;;1988
 ;;21,"00904585193 ")
 ;;1989
 ;;21,"00904609061 ")
 ;;1990
 ;;21,"00904609161 ")
 ;;1991
 ;;21,"00904609210 ")
 ;;1992
 ;;21,"00904609261 ")
 ;;1993
 ;;21,"00904610740 ")
 ;;1994
 ;;21,"00904610761 ")
 ;;1995
 ;;21,"00904610860 ")
 ;;1996
 ;;21,"00904610861 ")
 ;;1997
 ;;21,"00904612361 ")
 ;;4273
 ;;21,"00904612461 ")
 ;;4274
 ;;21,"00904613760 ")
 ;;4275
 ;;21,"00904613840 ")
 ;;4276
 ;;21,"00904613860 ")
 ;;4277
 ;;21,"00904613960 ")
 ;;4278
 ;;21,"00904613980 ")
 ;;4279
 ;;21,"00904792440 ")
 ;;4280
 ;;21,"00904792460 ")
 ;;4281
 ;;21,"00904792461 ")
 ;;4282
 ;;21,"00904792480 ")
 ;;4283
 ;;21,"00904792540 ")
 ;;4284
 ;;21,"00904792560 ")
 ;;4285
 ;;21,"00904792561 ")
 ;;4286
 ;;21,"00904792580 ")
 ;;4287
 ;;21,"05486853840 ")
 ;;4381
 ;;21,"05486853842 ")
 ;;4382
 ;;21,"06678021007 ")
 ;;4383
 ;;21,"06678021009 ")
 ;;4384
 ;;21,"06678021201 ")
 ;;4385
 ;;21,"10544019230 ")
 ;;2033
 ;;21,"10768715001 ")
 ;;2034
 ;;21,"10768715002 ")
 ;;2035
 ;;21,"10768747501 ")
 ;;2036
 ;;21,"10768747502 ")
 ;;2037
 ;;21,"10768770001 ")
 ;;2038
 ;;21,"10768770002 ")
 ;;2039
 ;;21,"11819006560 ")
 ;;2040
 ;;21,"11819006590 ")
 ;;2041
 ;;21,"11819006591 ")
 ;;2042
 ;;21,"11819006595 ")
 ;;2043
 ;;21,"11819006597 ")
 ;;2044
 ;;21,"11819006598 ")
 ;;2045
 ;;21,"11819008760 ")
 ;;2046
 ;;21,"11819008790 ")
 ;;2047
 ;;21,"11819008791 ")
 ;;2048
 ;;21,"11819008795 ")
 ;;2049
 ;;21,"11819008797 ")
 ;;2050
 ;;21,"11819008798 ")
 ;;2051
