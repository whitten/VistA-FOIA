BGPM5BEM ;IHS/MSC/MMT-CREATED BY ^ATXSTX ON SEP 12, 2011;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;21,"58016046609 ")
 ;;1000
 ;;21,"58016046610 ")
 ;;1001
 ;;21,"58016046612 ")
 ;;1002
 ;;21,"58016046614 ")
 ;;1003
 ;;21,"58016046615 ")
 ;;1004
 ;;21,"58016046616 ")
 ;;1005
 ;;21,"58016046618 ")
 ;;1006
 ;;21,"58016046620 ")
 ;;1007
 ;;21,"58016046621 ")
 ;;1008
 ;;21,"58016046624 ")
 ;;1009
 ;;21,"58016046625 ")
 ;;1010
 ;;21,"58016046626 ")
 ;;1011
 ;;21,"58016046627 ")
 ;;1012
 ;;21,"58016046628 ")
 ;;1013
 ;;21,"58016046630 ")
 ;;1014
 ;;21,"58016046632 ")
 ;;1015
 ;;21,"58016046635 ")
 ;;1016
 ;;21,"58016046636 ")
 ;;1017
 ;;21,"58016046640 ")
 ;;1018
 ;;21,"58016046642 ")
 ;;1019
 ;;21,"58016046644 ")
 ;;1020
 ;;21,"58016046645 ")
 ;;1021
 ;;21,"58016046648 ")
 ;;1022
 ;;21,"58016046650 ")
 ;;1023
 ;;21,"58016046656 ")
 ;;1024
 ;;21,"58016046660 ")
 ;;1025
 ;;21,"58016046667 ")
 ;;1026
 ;;21,"58016046669 ")
 ;;1027
 ;;21,"58016046670 ")
 ;;1028
 ;;21,"58016046671 ")
 ;;1029
 ;;21,"58016046672 ")
 ;;1030
 ;;21,"58016046673 ")
 ;;1031
 ;;21,"58016046675 ")
 ;;1032
 ;;21,"58016046676 ")
 ;;1033
 ;;21,"58016046677 ")
 ;;1034
 ;;21,"58016046679 ")
 ;;1035
 ;;21,"58016046680 ")
 ;;1036
 ;;21,"58016046681 ")
 ;;1037
 ;;21,"58016046682 ")
 ;;1038
 ;;21,"58016046683 ")
 ;;1039
 ;;21,"58016046684 ")
 ;;1040
 ;;21,"58016046687 ")
 ;;1041
 ;;21,"58016046689 ")
 ;;1042
 ;;21,"58016046690 ")
 ;;1043
 ;;21,"58016046691 ")
 ;;1044
 ;;21,"58016046692 ")
 ;;1045
 ;;21,"58016046693 ")
 ;;1046
 ;;21,"58016046696 ")
 ;;1047
 ;;21,"58016046697 ")
 ;;1048
 ;;21,"58016046698 ")
 ;;1049
 ;;21,"58016046699 ")
 ;;1050
 ;;21,"58016046700 ")
 ;;3170
 ;;21,"58016046730 ")
 ;;3171
 ;;21,"58016046760 ")
 ;;3172
 ;;21,"58016046790 ")
 ;;3173
 ;;21,"58016053600 ")
 ;;1051
 ;;21,"58016053630 ")
 ;;1052
 ;;21,"58016053660 ")
 ;;1053
 ;;21,"58016053690 ")
 ;;1054
 ;;21,"58016069100 ")
 ;;3174
 ;;21,"58016069101 ")
 ;;3175
 ;;21,"58016069102 ")
 ;;3176
 ;;21,"58016069103 ")
 ;;3177
 ;;21,"58016069104 ")
 ;;3178
 ;;21,"58016069105 ")
 ;;3179
 ;;21,"58016069106 ")
 ;;3180
 ;;21,"58016069107 ")
 ;;3181
 ;;21,"58016069108 ")
 ;;3182
 ;;21,"58016069109 ")
 ;;3183
 ;;21,"58016069110 ")
 ;;3184
 ;;21,"58016069112 ")
 ;;3185
 ;;21,"58016069114 ")
 ;;3186
 ;;21,"58016069115 ")
 ;;3187
 ;;21,"58016069116 ")
 ;;3188
 ;;21,"58016069118 ")
 ;;3189
 ;;21,"58016069120 ")
 ;;3190
 ;;21,"58016069121 ")
 ;;3191
 ;;21,"58016069124 ")
 ;;3192
 ;;21,"58016069125 ")
 ;;3193
 ;;21,"58016069126 ")
 ;;3194
 ;;21,"58016069127 ")
 ;;3195
 ;;21,"58016069128 ")
 ;;3196
 ;;21,"58016069130 ")
 ;;3197
 ;;21,"58016069132 ")
 ;;3198
 ;;21,"58016069135 ")
 ;;3199
 ;;21,"58016069136 ")
 ;;3200
 ;;21,"58016069140 ")
 ;;3201
 ;;21,"58016069142 ")
 ;;3202
 ;;21,"58016069144 ")
 ;;3203
 ;;21,"58016069145 ")
 ;;3204
 ;;21,"58016069148 ")
 ;;3205
 ;;21,"58016069150 ")
 ;;3206
 ;;21,"58016069156 ")
 ;;3207
 ;;21,"58016069160 ")
 ;;3208
 ;;21,"58016069167 ")
 ;;3209
 ;;21,"58016069169 ")
 ;;3210
 ;;21,"58016069170 ")
 ;;3211
 ;;21,"58016069171 ")
 ;;3212
 ;;21,"58016069172 ")
 ;;3213
 ;;21,"58016069173 ")
 ;;3214
 ;;21,"58016069175 ")
 ;;3215
 ;;21,"58016069176 ")
 ;;3216
 ;;21,"58016069177 ")
 ;;3217
 ;;21,"58016069179 ")
 ;;3218
 ;;21,"58016069180 ")
 ;;3219
 ;;21,"58016069181 ")
 ;;3220
 ;;21,"58016069182 ")
 ;;3221
 ;;21,"58016069183 ")
 ;;3222
 ;;21,"58016069184 ")
 ;;3223
 ;;21,"58016069187 ")
 ;;3224
 ;;21,"58016069189 ")
 ;;3225
 ;;21,"58016069190 ")
 ;;3226
 ;;21,"58016069191 ")
 ;;3227
 ;;21,"58016069192 ")
 ;;3228
 ;;21,"58016069193 ")
 ;;3229
 ;;21,"58016069196 ")
 ;;3230
 ;;21,"58016069197 ")
 ;;3231
 ;;21,"58016069198 ")
 ;;3232
 ;;21,"58016069199 ")
 ;;3233
 ;;21,"58016077200 ")
 ;;1055
 ;;21,"58016077201 ")
 ;;1056
 ;;21,"58016077202 ")
 ;;1057
 ;;21,"58016077203 ")
 ;;1058
 ;;21,"58016077204 ")
 ;;1059
 ;;21,"58016077205 ")
 ;;1060
 ;;21,"58016077206 ")
 ;;1061
 ;;21,"58016077207 ")
 ;;1062
 ;;21,"58016077208 ")
 ;;1063
 ;;21,"58016077209 ")
 ;;1064
 ;;21,"58016077210 ")
 ;;1065
 ;;21,"58016077212 ")
 ;;1066
 ;;21,"58016077214 ")
 ;;1067
 ;;21,"58016077215 ")
 ;;1068
 ;;21,"58016077216 ")
 ;;1069
 ;;21,"58016077218 ")
 ;;1070
 ;;21,"58016077220 ")
 ;;1071
 ;;21,"58016077221 ")
 ;;1072
 ;;21,"58016077224 ")
 ;;1073
 ;;21,"58016077225 ")
 ;;1074
 ;;21,"58016077226 ")
 ;;1075
 ;;21,"58016077227 ")
 ;;1076
 ;;21,"58016077228 ")
 ;;1077
 ;;21,"58016077230 ")
 ;;1078
 ;;21,"58016077232 ")
 ;;1079
 ;;21,"58016077235 ")
 ;;1080
 ;;21,"58016077236 ")
 ;;1081
 ;;21,"58016077240 ")
 ;;1082
 ;;21,"58016077242 ")
 ;;1083
 ;;21,"58016077244 ")
 ;;1084
 ;;21,"58016077245 ")
 ;;1085
 ;;21,"58016077248 ")
 ;;1086
 ;;21,"58016077250 ")
 ;;1087
 ;;21,"58016077256 ")
 ;;1088
 ;;21,"58016077260 ")
 ;;1089
 ;;21,"58016077267 ")
 ;;1090
 ;;21,"58016077269 ")
 ;;1091
 ;;21,"58016077270 ")
 ;;1092
 ;;21,"58016077271 ")
 ;;1093
 ;;21,"58016077272 ")
 ;;1094
 ;;21,"58016077273 ")
 ;;1095
 ;;21,"58016077275 ")
 ;;1096
 ;;21,"58016077276 ")
 ;;1097
 ;;21,"58016077277 ")
 ;;1098
 ;;21,"58016077279 ")
 ;;1099
 ;;21,"58016077280 ")
 ;;1100
 ;;21,"58016077281 ")
 ;;1101
 ;;21,"58016077282 ")
 ;;1102
 ;;21,"58016077283 ")
 ;;1103
 ;;21,"58016077284 ")
 ;;1104
 ;;21,"58016077287 ")
 ;;1105
 ;;21,"58016077289 ")
 ;;1106
 ;;21,"58016077290 ")
 ;;1107
 ;;21,"58016077291 ")
 ;;1108
 ;;21,"58016077292 ")
 ;;1109
 ;;21,"58016077293 ")
 ;;1110
 ;;21,"58016077296 ")
 ;;1111
 ;;21,"58016077297 ")
 ;;1112
 ;;21,"58016077298 ")
 ;;1113
 ;;21,"58016077299 ")
 ;;1114
 ;;21,"58016079630 ")
 ;;3234
 ;;21,"58016084400 ")
 ;;3235
 ;;21,"58016084430 ")
 ;;3236
 ;;21,"58016084460 ")
 ;;3237
 ;;21,"58016084490 ")
 ;;3238
 ;;21,"58016087600 ")
 ;;3239
 ;;21,"58016087610 ")
 ;;3240
 ;;21,"58016087612 ")
 ;;3241
 ;;21,"58016087614 ")
 ;;3242
 ;;21,"58016087615 ")
 ;;3243
 ;;21,"58016087620 ")
 ;;3244
 ;;21,"58016087621 ")
 ;;3245
 ;;21,"58016087624 ")
 ;;3246
 ;;21,"58016087628 ")
 ;;3247
 ;;21,"58016087630 ")
 ;;3248
 ;;21,"58016087640 ")
 ;;3249
 ;;21,"58016087650 ")
 ;;3250
 ;;21,"58016087660 ")
 ;;3251
 ;;21,"58016088300 ")
 ;;1115
 ;;21,"58016088330 ")
 ;;1116
 ;;21,"58016088360 ")
 ;;1117
 ;;21,"58016088390 ")
 ;;1118
 ;;21,"58864001528 ")
 ;;1119
 ;;21,"58864001530 ")
 ;;1120
 ;;21,"58864001560 ")
 ;;1121
 ;;21,"58864001590 ")
 ;;1122
 ;;21,"58864002714 ")
 ;;3252
 ;;21,"58864002730 ")
 ;;3253
 ;;21,"58864002760 ")
 ;;3254
 ;;21,"58864002790 ")
 ;;3255
 ;;21,"58864010030 ")
 ;;3256
 ;;21,"58864016130 ")
 ;;3257
 ;;21,"58864016160 ")
 ;;3258
 ;;21,"58864022430 ")
 ;;3259
 ;;21,"58864022460 ")
 ;;3260
 ;;21,"58864022493 ")
 ;;3261
 ;;21,"58864069330 ")
 ;;1123
 ;;21,"58864069360 ")
 ;;1124
 ;;21,"58864078930 ")
 ;;1125
 ;;21,"58864078960 ")
 ;;1126
 ;;21,"58864085130 ")
 ;;1127
 ;;21,"58864095230 ")
 ;;109
 ;;21,"58864095330 ")
 ;;110
 ;;21,"58864095630 ")
 ;;3264
 ;;21,"58864095730 ")
 ;;3265
 ;;21,"59115001201 ")
 ;;3266
 ;;21,"59115001210 ")
 ;;3267
 ;;21,"59115001255 ")
 ;;3268
 ;;21,"59115001299 ")
 ;;3269
 ;;21,"59115001301 ")
 ;;3270
 ;;21,"59115001310 ")
 ;;3271
 ;;21,"59115001355 ")
 ;;3272
 ;;21,"59115001399 ")
 ;;3273
 ;;21,"59115001401 ")
 ;;3274
 ;;21,"59115001455 ")
 ;;3275
 ;;21,"59115001499 ")
 ;;3276
 ;;21,"59762233003 ")
 ;;111
 ;;21,"59762233005 ")
 ;;112
 ;;21,"59762233007 ")
 ;;113
 ;;21,"59762233103 ")
 ;;114
 ;;21,"59762233106 ")
 ;;115
 ;;21,"59762233108 ")
 ;;116
 ;;21,"59762233203 ")
 ;;117
 ;;21,"59762233206 ")
 ;;118
 ;;21,"59762233208 ")
 ;;119
 ;;21,"59762372501 ")
 ;;3286
 ;;21,"59762372603 ")
 ;;3287
 ;;21,"59762372704 ")
 ;;3288
 ;;21,"59762372706 ")
 ;;3289
 ;;21,"59762372707 ")
 ;;3290
 ;;21,"59762375704 ")
 ;;3291
 ;;21,"59762375706 ")
 ;;3292
 ;;21,"59762375707 ")
 ;;3293
 ;;21,"59762378101 ")
 ;;3294
 ;;21,"59762378201 ")
 ;;3295
 ;;21,"59762378203 ")
 ;;3296
 ;;21,"59762378301 ")
 ;;3297
 ;;21,"59762378302 ")
 ;;3298
 ;;21,"59762378303 ")
 ;;3299
 ;;21,"59762432000 ")
 ;;1139
 ;;21,"59762432002 ")
 ;;1140
 ;;21,"59762432006 ")
 ;;1141
 ;;21,"59762432100 ")
 ;;1142
 ;;21,"59762432102 ")
 ;;1143
 ;;21,"59762432106 ")
 ;;1144
 ;;21,"59762432200 ")
 ;;1145
 ;;21,"59762432202 ")
 ;;1146
 ;;21,"59762432206 ")
 ;;1147
 ;;21,"59762503101 ")
 ;;3300
 ;;21,"59762503201 ")
 ;;3301
 ;;21,"59762503202 ")
 ;;3302
 ;;21,"59762503301 ")
 ;;3303
 ;;21,"59762503302 ")
 ;;3304
 ;;21,"59762702009 ")
 ;;3305
 ;;21,"59762702101 ")
 ;;3306
 ;;21,"59762702105 ")
 ;;3307
 ;;21,"59762702109 ")
 ;;3308
 ;;21,"59762702201 ")
 ;;3309
 ;;21,"59762702205 ")
 ;;3310
 ;;21,"59762702209 ")
 ;;3311
 ;;21,"59930159202 ")
 ;;3312
 ;;21,"59930162202 ")
 ;;3313
 ;;21,"59930162203 ")
 ;;3314
 ;;21,"59930163902 ")
 ;;3315
 ;;21,"60429008512 ")
 ;;3316
 ;;21,"60429008518 ")
 ;;3317
 ;;21,"60429008527 ")
 ;;3318
 ;;21,"60429008530 ")
 ;;3319
 ;;21,"60429008536 ")
 ;;3320
 ;;21,"60429008560 ")
 ;;3321
 ;;21,"60429008590 ")
 ;;3322
 ;;21,"60429072218 ")
 ;;1148
 ;;21,"60429072260 ")
 ;;1149
 ;;21,"60429072301 ")
 ;;1150
 ;;21,"60429072305 ")
 ;;1151
 ;;21,"60429072312 ")
 ;;1152
 ;;21,"60429072318 ")
 ;;1153
 ;;21,"60429072360 ")
 ;;1154
 ;;21,"60429072390 ")
 ;;1155
 ;;21,"60429072418 ")
 ;;1156
 ;;21,"60429072460 ")
 ;;1157
 ;;21,"60429072505 ")
 ;;1158
 ;;21,"60429072518 ")
 ;;1159
 ;;21,"60429072560 ")
 ;;1160
 ;;21,"60429072590 ")
 ;;1161
 ;;21,"60505014100 ")
 ;;3323
 ;;21,"60505014101 ")
 ;;3324
 ;;21,"60505014102 ")
 ;;3325
 ;;21,"60505014108 ")
 ;;3326
 ;;21,"60505014200 ")
 ;;3327
 ;;21,"60505014201 ")
 ;;3328
 ;;21,"60505014202 ")
 ;;3329
 ;;21,"60505014204 ")
 ;;3330
 ;;21,"60505019000 ")
 ;;1162
 ;;21,"60505019001 ")
 ;;1163
 ;;21,"60505019004 ")
 ;;1164
 ;;21,"60505019008 ")
 ;;1165
 ;;21,"60505019100 ")
 ;;1166
 ;;21,"60505019101 ")
 ;;1167
 ;;21,"60505019104 ")
 ;;1168
 ;;21,"60505019108 ")
 ;;1169
 ;;21,"60505019200 ")
 ;;1170
 ;;21,"60505019201 ")
 ;;1171
 ;;21,"60505019204 ")
 ;;1172
 ;;21,"60505019208 ")
 ;;1173
 ;;21,"60505026001 ")
 ;;1174
 ;;21,"60505026002 ")
 ;;1175
 ;;21,"60505026007 ")
 ;;1176
 ;;21,"60505132901 ")
 ;;1177
 ;;21,"60505132903 ")
 ;;1178
 ;;21,"60505132905 ")
 ;;1179
 ;;21,"60951071170 ")
 ;;3331
 ;;21,"60951071185 ")
 ;;3332
 ;;21,"60951071470 ")
 ;;3333
 ;;21,"60951071485 ")
 ;;3334
 ;;21,"61073003001 ")
 ;;1180
 ;;21,"61073003005 ")
 ;;1181
 ;;21,"61073003010 ")
 ;;1182
 ;;21,"61073003101 ")
 ;;1183
 ;;21,"61073003105 ")
 ;;1184
 ;;21,"61073003110 ")
 ;;1185
 ;;21,"61073003201 ")
 ;;1186
 ;;21,"61073003205 ")
 ;;1187
 ;;21,"61073003210 ")
 ;;1188
 ;;21,"61258673700 ")
 ;;1189
 ;;21,"61258673800 ")
 ;;1190
 ;;21,"61258673900 ")
 ;;1191
 ;;21,"61392006325 ")
 ;;3335
 ;;21,"61392006330 ")
 ;;3336
 ;;21,"61392006339 ")
 ;;3337
 ;;21,"61392006345 ")
 ;;3338
 ;;21,"61392006354 ")
 ;;3339
 ;;21,"61392006391 ")
 ;;3340
