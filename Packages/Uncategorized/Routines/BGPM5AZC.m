BGPM5AZC ;IHS/MSC/MMT-CREATED BY ^ATXSTX ON SEP 02, 2011;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;;BGPMU TAMOXIFEN AROMATASE NDCS
 ;
 ; This routine loads Taxonomy BGPMU TAMOXIFEN AROMATASE NDCS
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
 ;;21,"00017565760 ")
 ;;108
 ;;21,"00017565770 ")
 ;;109
 ;;21,"00054008013 ")
 ;;110
 ;;21,"00054016413 ")
 ;;111
 ;;21,"00054026913 ")
 ;;112
 ;;21,"00054483121 ")
 ;;113
 ;;21,"00054483126 ")
 ;;114
 ;;21,"00054483413 ")
 ;;115
 ;;21,"00054483422 ")
 ;;116
 ;;21,"00054883125 ")
 ;;117
 ;;21,"00054883425 ")
 ;;118
 ;;21,"00093078201 ")
 ;;119
 ;;21,"00093078205 ")
 ;;120
 ;;21,"00093078210 ")
 ;;121
 ;;21,"00093078256 ")
 ;;122
 ;;21,"00093078405 ")
 ;;123
 ;;21,"00093078406 ")
 ;;124
 ;;21,"00093078410 ")
 ;;125
 ;;21,"00093078486 ")
 ;;126
 ;;21,"00093753656 ")
 ;;127
 ;;21,"00093762056 ")
 ;;128
 ;;21,"00172565649 ")
 ;;129
 ;;21,"00172565658 ")
 ;;130
 ;;21,"00172565670 ")
 ;;131
 ;;21,"00172565680 ")
 ;;132
 ;;21,"00172565746 ")
 ;;133
 ;;21,"00172565755 ")
 ;;134
 ;;21,"00172565780 ")
 ;;135
 ;;21,"00179006870 ")
 ;;136
 ;;21,"00179195201 ")
 ;;137
 ;;21,"00179195230 ")
 ;;138
 ;;21,"00179195250 ")
 ;;139
 ;;21,"00179195260 ")
 ;;140
 ;;21,"00179195290 ")
 ;;141
 ;;21,"00247085960 ")
 ;;142
 ;;21,"00310044603 ")
 ;;143
 ;;21,"00378014405 ")
 ;;144
 ;;21,"00378014491 ")
 ;;145
 ;;21,"00378027401 ")
 ;;146
 ;;21,"00378027425 ")
 ;;147
 ;;21,"00378027493 ")
 ;;148
 ;;21,"00378207105 ")
 ;;149
 ;;21,"00378207193 ")
 ;;150
 ;;21,"00378603405 ")
 ;;151
 ;;21,"00378603477 ")
 ;;152
 ;;21,"00378603493 ")
 ;;153
 ;;21,"00480078201 ")
 ;;154
 ;;21,"00480078205 ")
 ;;155
 ;;21,"00480078210 ")
 ;;156
 ;;21,"00480078256 ")
 ;;157
 ;;21,"00480078405 ")
 ;;158
 ;;21,"00480078406 ")
 ;;159
 ;;21,"00480078410 ")
 ;;160
 ;;21,"00480078486 ")
 ;;161
 ;;21,"00555044603 ")
 ;;162
 ;;21,"00555044605 ")
 ;;163
 ;;21,"00555044609 ")
 ;;164
 ;;21,"00555044612 ")
 ;;165
 ;;21,"00555044663 ")
 ;;166
 ;;21,"00555090401 ")
 ;;167
 ;;21,"00555090405 ")
 ;;168
 ;;21,"00555090414 ")
 ;;169
 ;;21,"00555090464 ")
 ;;170
 ;;21,"00591223218 ")
 ;;171
 ;;21,"00591223260 ")
 ;;172
 ;;21,"00591223319 ")
 ;;173
 ;;21,"00591223330 ")
 ;;174
 ;;21,"00591247218 ")
 ;;175
 ;;21,"00591247260 ")
 ;;176
 ;;21,"00591247319 ")
 ;;177
 ;;21,"00591247330 ")
 ;;178
 ;;21,"00603418016 ")
 ;;179
 ;;21,"00781535631 ")
 ;;180
 ;;21,"00904619546 ")
 ;;181
 ;;21,"00904622961 ")
 ;;182
 ;;21,"16571042103 ")
 ;;1
 ;;21,"16729003410 ")
 ;;2
 ;;21,"16729003510 ")
 ;;3
 ;;21,"16729003515 ")
 ;;4
 ;;21,"16729003516 ")
 ;;5
 ;;21,"21695099030 ")
 ;;6
 ;;21,"24724003001 ")
 ;;7
 ;;21,"24724003003 ")
 ;;8
 ;;21,"26053000601 ")
 ;;9
 ;;21,"26053004401 ")
 ;;10
 ;;21,"42043018003 ")
 ;;11
 ;;21,"42291010530 ")
 ;;12
 ;;21,"42291037390 ")
 ;;13
 ;;21,"48581622101 ")
 ;;14
 ;;21,"48581622102 ")
 ;;15
 ;;21,"48581622103 ")
 ;;16
 ;;21,"48581622104 ")
 ;;17
 ;;21,"48581622201 ")
 ;;18
 ;;21,"48581622202 ")
 ;;19
 ;;21,"48581622203 ")
 ;;20
 ;;21,"51079032301 ")
 ;;21
 ;;21,"51079032306 ")
 ;;22
 ;;21,"51129262201 ")
 ;;23
 ;;21,"51129421801 ")
 ;;24
 ;;21,"51129421802 ")
 ;;25
 ;;21,"51129466201 ")
 ;;26
 ;;21,"51129466202 ")
 ;;27
 ;;21,"51991062010 ")
 ;;28
 ;;21,"51991062033 ")
 ;;29
 ;;21,"51991075910 ")
 ;;30
 ;;21,"51991075933 ")
 ;;31
 ;;21,"53002103203 ")
 ;;32
 ;;21,"53002103206 ")
 ;;33
 ;;21,"54569376500 ")
 ;;34
 ;;21,"54569376501 ")
 ;;35
 ;;21,"54569619800 ")
 ;;36
 ;;21,"54569860200 ")
 ;;37
 ;;21,"54868300401 ")
 ;;38
 ;;21,"54868300402 ")
 ;;39
 ;;21,"54868300403 ")
 ;;40
 ;;21,"54868300404 ")
 ;;41
 ;;21,"54868300405 ")
 ;;42
 ;;21,"54868428700 ")
 ;;43
 ;;21,"54868428701 ")
 ;;44
 ;;21,"54868428702 ")
 ;;45
 ;;21,"54868428703 ")
 ;;46
 ;;21,"54868428704 ")
 ;;47
 ;;21,"54868613000 ")
 ;;48
 ;;21,"54868613001 ")
 ;;49
 ;;21,"54868625200 ")
 ;;50
 ;;21,"55045270300 ")
 ;;51
 ;;21,"55045270309 ")
 ;;52
 ;;21,"55111064630 ")
 ;;53
 ;;21,"55111064678 ")
 ;;54
 ;;21,"55111064679 ")
 ;;55
 ;;21,"55111064730 ")
 ;;56
 ;;21,"55160014901 ")
 ;;57
 ;;21,"55160014905 ")
 ;;58
 ;;21,"55160015001 ")
 ;;59
 ;;21,"55160015005 ")
 ;;60
 ;;21,"55887087201 ")
 ;;61
 ;;21,"55887087290 ")
 ;;62
 ;;21,"59564014401 ")
 ;;63
 ;;21,"59564014407 ")
 ;;64
 ;;21,"59762285801 ")
 ;;65
 ;;21,"60258086603 ")
 ;;66
 ;;21,"62037096418 ")
 ;;67
 ;;21,"62037096425 ")
 ;;68
 ;;21,"62037096460 ")
 ;;69
 ;;21,"62037096512 ")
 ;;70
 ;;21,"62037096530 ")
 ;;71
 ;;21,"62037096590 ")
 ;;72
 ;;21,"62175071032 ")
 ;;73
 ;;21,"62175088832 ")
 ;;74
 ;;21,"62540565600 ")
 ;;75
 ;;21,"62540565602 ")
 ;;76
 ;;21,"62540565603 ")
 ;;77
 ;;21,"62540565609 ")
 ;;78
 ;;21,"62540565701 ")
 ;;79
 ;;21,"62540565702 ")
 ;;80
 ;;21,"62756025013 ")
 ;;81
 ;;21,"62756025018 ")
 ;;82
 ;;21,"62756025083 ")
 ;;83
 ;;21,"62756025088 ")
 ;;84
 ;;21,"62756051108 ")
 ;;85
 ;;21,"62756051118 ")
 ;;86
 ;;21,"62756051183 ")
 ;;87
 ;;21,"62756051188 ")
 ;;88
 ;;21,"63304060028 ")
 ;;89
 ;;21,"63304060060 ")
 ;;90
 ;;21,"63304060130 ")
 ;;91
 ;;21,"63304060190 ")
 ;;92
 ;;21,"63323012930 ")
 ;;93
 ;;21,"63323077230 ")
 ;;94
 ;;21,"63629441301 ")
 ;;95
 ;;21,"63672001500 ")
 ;;96
 ;;21,"63739026910 ")
 ;;97
 ;;21,"65841074306 ")
 ;;98
 ;;21,"65841074310 ")
 ;;99
 ;;21,"66267087360 ")
 ;;100
 ;;21,"66435041530 ")
 ;;101
 ;;21,"67877017110 ")
 ;;102
 ;;21,"67877017130 ")
 ;;103
 ;;21,"68084044811 ")
 ;;104
 ;;21,"68084044821 ")
 ;;105
 ;;21,"68382020906 ")
 ;;106
 ;;21,"68382020910 ")
 ;;107
 ;;9002226,1050,.01)
 ;;BGPMU TAMOXIFEN AROMATASE NDCS
 ;;9002226,1050,.02)
 ;;Breast CA Treatment NDCs
 ;;9002226,1050,.04)
 ;;n
 ;;9002226,1050,.06)
 ;;@
 ;;9002226,1050,.08)
 ;;@
 ;;9002226,1050,.09)
 ;;3110901
 ;;9002226,1050,.11)
 ;;@
 ;;9002226,1050,.12)
 ;;@
 ;;9002226,1050,.13)
 ;;@
 ;;9002226,1050,.14)
 ;;@
 ;;9002226,1050,.15)
 ;;@
 ;;9002226,1050,.16)
 ;;@
 ;;9002226,1050,.17)
 ;;@
 ;;9002226,1050,3101)
 ;;@
 ;;9002226.02101,"1050,00017565760 ",.01)
 ;;00017565760
 ;;9002226.02101,"1050,00017565760 ",.02)
 ;;00017565760
 ;;9002226.02101,"1050,00017565770 ",.01)
 ;;00017565770
 ;;9002226.02101,"1050,00017565770 ",.02)
 ;;00017565770
 ;;9002226.02101,"1050,00054008013 ",.01)
 ;;00054008013
 ;;9002226.02101,"1050,00054008013 ",.02)
 ;;00054008013
 ;;9002226.02101,"1050,00054016413 ",.01)
 ;;00054016413
 ;;9002226.02101,"1050,00054016413 ",.02)
 ;;00054016413
 ;;9002226.02101,"1050,00054026913 ",.01)
 ;;00054026913
 ;;9002226.02101,"1050,00054026913 ",.02)
 ;;00054026913
 ;;9002226.02101,"1050,00054483121 ",.01)
 ;;00054483121
 ;;9002226.02101,"1050,00054483121 ",.02)
 ;;00054483121
 ;;9002226.02101,"1050,00054483126 ",.01)
 ;;00054483126
 ;;9002226.02101,"1050,00054483126 ",.02)
 ;;00054483126
 ;;9002226.02101,"1050,00054483413 ",.01)
 ;;00054483413
 ;;9002226.02101,"1050,00054483413 ",.02)
 ;;00054483413
 ;;9002226.02101,"1050,00054483422 ",.01)
 ;;00054483422
 ;;9002226.02101,"1050,00054483422 ",.02)
 ;;00054483422
 ;;9002226.02101,"1050,00054883125 ",.01)
 ;;00054883125
 ;;9002226.02101,"1050,00054883125 ",.02)
 ;;00054883125
 ;;9002226.02101,"1050,00054883425 ",.01)
 ;;00054883425
 ;;9002226.02101,"1050,00054883425 ",.02)
 ;;00054883425
 ;;9002226.02101,"1050,00093078201 ",.01)
 ;;00093078201
 ;;9002226.02101,"1050,00093078201 ",.02)
 ;;00093078201
 ;;9002226.02101,"1050,00093078205 ",.01)
 ;;00093078205
 ;;9002226.02101,"1050,00093078205 ",.02)
 ;;00093078205
 ;;9002226.02101,"1050,00093078210 ",.01)
 ;;00093078210
 ;;9002226.02101,"1050,00093078210 ",.02)
 ;;00093078210
 ;;9002226.02101,"1050,00093078256 ",.01)
 ;;00093078256
 ;;9002226.02101,"1050,00093078256 ",.02)
 ;;00093078256
 ;;9002226.02101,"1050,00093078405 ",.01)
 ;;00093078405
 ;;9002226.02101,"1050,00093078405 ",.02)
 ;;00093078405
 ;;9002226.02101,"1050,00093078406 ",.01)
 ;;00093078406
 ;;9002226.02101,"1050,00093078406 ",.02)
 ;;00093078406
 ;;9002226.02101,"1050,00093078410 ",.01)
 ;;00093078410
 ;;9002226.02101,"1050,00093078410 ",.02)
 ;;00093078410
 ;;9002226.02101,"1050,00093078486 ",.01)
 ;;00093078486
 ;;9002226.02101,"1050,00093078486 ",.02)
 ;;00093078486
 ;;9002226.02101,"1050,00093753656 ",.01)
 ;;00093753656
 ;;9002226.02101,"1050,00093753656 ",.02)
 ;;00093753656
 ;;9002226.02101,"1050,00093762056 ",.01)
 ;;00093762056
 ;;9002226.02101,"1050,00093762056 ",.02)
 ;;00093762056
 ;;9002226.02101,"1050,00172565649 ",.01)
 ;;00172565649
 ;;9002226.02101,"1050,00172565649 ",.02)
 ;;00172565649
 ;;9002226.02101,"1050,00172565658 ",.01)
 ;;00172565658
 ;;9002226.02101,"1050,00172565658 ",.02)
 ;;00172565658
 ;;9002226.02101,"1050,00172565670 ",.01)
 ;;00172565670
 ;;9002226.02101,"1050,00172565670 ",.02)
 ;;00172565670
 ;;9002226.02101,"1050,00172565680 ",.01)
 ;;00172565680
 ;;9002226.02101,"1050,00172565680 ",.02)
 ;;00172565680
 ;;9002226.02101,"1050,00172565746 ",.01)
 ;;00172565746
 ;;9002226.02101,"1050,00172565746 ",.02)
 ;;00172565746
 ;;9002226.02101,"1050,00172565755 ",.01)
 ;;00172565755
 ;;9002226.02101,"1050,00172565755 ",.02)
 ;;00172565755
 ;;9002226.02101,"1050,00172565780 ",.01)
 ;;00172565780
 ;;9002226.02101,"1050,00172565780 ",.02)
 ;;00172565780
 ;;9002226.02101,"1050,00179006870 ",.01)
 ;;00179006870
 ;;9002226.02101,"1050,00179006870 ",.02)
 ;;00179006870
 ;;9002226.02101,"1050,00179195201 ",.01)
 ;;00179195201
 ;;9002226.02101,"1050,00179195201 ",.02)
 ;;00179195201
 ;;9002226.02101,"1050,00179195230 ",.01)
 ;;00179195230
 ;;9002226.02101,"1050,00179195230 ",.02)
 ;;00179195230
 ;;9002226.02101,"1050,00179195250 ",.01)
 ;;00179195250
 ;;9002226.02101,"1050,00179195250 ",.02)
 ;;00179195250
 ;;9002226.02101,"1050,00179195260 ",.01)
 ;;00179195260
 ;;9002226.02101,"1050,00179195260 ",.02)
 ;;00179195260
 ;;9002226.02101,"1050,00179195290 ",.01)
 ;;00179195290
 ;;9002226.02101,"1050,00179195290 ",.02)
 ;;00179195290
 ;;9002226.02101,"1050,00247085960 ",.01)
 ;;00247085960
 ;;9002226.02101,"1050,00247085960 ",.02)
 ;;00247085960
 ;;9002226.02101,"1050,00310044603 ",.01)
 ;;00310044603
 ;;9002226.02101,"1050,00310044603 ",.02)
 ;;00310044603
 ;;9002226.02101,"1050,00378014405 ",.01)
 ;;00378014405
 ;;9002226.02101,"1050,00378014405 ",.02)
 ;;00378014405
 ;;9002226.02101,"1050,00378014491 ",.01)
 ;;00378014491
 ;;9002226.02101,"1050,00378014491 ",.02)
 ;;00378014491
 ;;9002226.02101,"1050,00378027401 ",.01)
 ;;00378027401
 ;;9002226.02101,"1050,00378027401 ",.02)
 ;;00378027401
 ;;9002226.02101,"1050,00378027425 ",.01)
 ;;00378027425
 ;;9002226.02101,"1050,00378027425 ",.02)
 ;;00378027425
 ;;9002226.02101,"1050,00378027493 ",.01)
 ;;00378027493
 ;;9002226.02101,"1050,00378027493 ",.02)
 ;;00378027493
 ;;9002226.02101,"1050,00378207105 ",.01)
 ;;00378207105
 ;;9002226.02101,"1050,00378207105 ",.02)
 ;;00378207105
 ;;9002226.02101,"1050,00378207193 ",.01)
 ;;00378207193
 ;;9002226.02101,"1050,00378207193 ",.02)
 ;;00378207193
 ;;9002226.02101,"1050,00378603405 ",.01)
 ;;00378603405
 ;;9002226.02101,"1050,00378603405 ",.02)
 ;;00378603405
 ;;9002226.02101,"1050,00378603477 ",.01)
 ;;00378603477
 ;;9002226.02101,"1050,00378603477 ",.02)
 ;;00378603477
 ;
OTHER ; OTHER ROUTINES
 D ^BGPM5AZD
 D ^BGPM5AZE
 Q
