BGPM3AAM ;IHS/MSC/SAT-CREATED BY ^ATXSTX ON APR 21, 2011;
 ;;11.0;IHS CLINICAL REPORTING;**4**;JAN 06, 2011;Build 84
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"774,55289077314 ",.02)
 ;;55289077314
 ;;9002226.02101,"774,55289077330 ",.01)
 ;;55289077330
 ;;9002226.02101,"774,55289077330 ",.02)
 ;;55289077330
 ;;9002226.02101,"774,55289077360 ",.01)
 ;;55289077360
 ;;9002226.02101,"774,55289077360 ",.02)
 ;;55289077360
 ;;9002226.02101,"774,55289077390 ",.01)
 ;;55289077390
 ;;9002226.02101,"774,55289077390 ",.02)
 ;;55289077390
 ;;9002226.02101,"774,55567010000 ",.01)
 ;;55567010000
 ;;9002226.02101,"774,55567010000 ",.02)
 ;;55567010000
 ;;9002226.02101,"774,55567010001 ",.01)
 ;;55567010001
 ;;9002226.02101,"774,55567010001 ",.02)
 ;;55567010001
 ;;9002226.02101,"774,55567010010 ",.01)
 ;;55567010010
 ;;9002226.02101,"774,55567010010 ",.02)
 ;;55567010010
 ;;9002226.02101,"774,55567010100 ",.01)
 ;;55567010100
 ;;9002226.02101,"774,55567010100 ",.02)
 ;;55567010100
 ;;9002226.02101,"774,55567010101 ",.01)
 ;;55567010101
 ;;9002226.02101,"774,55567010101 ",.02)
 ;;55567010101
 ;;9002226.02101,"774,55567010110 ",.01)
 ;;55567010110
 ;;9002226.02101,"774,55567010110 ",.02)
 ;;55567010110
 ;;9002226.02101,"774,55567010118 ",.01)
 ;;55567010118
 ;;9002226.02101,"774,55567010118 ",.02)
 ;;55567010118
 ;;9002226.02101,"774,55567010135 ",.01)
 ;;55567010135
 ;;9002226.02101,"774,55567010135 ",.02)
 ;;55567010135
 ;;9002226.02101,"774,55567010200 ",.01)
 ;;55567010200
 ;;9002226.02101,"774,55567010200 ",.02)
 ;;55567010200
 ;;9002226.02101,"774,55567010201 ",.01)
 ;;55567010201
 ;;9002226.02101,"774,55567010201 ",.02)
 ;;55567010201
 ;;9002226.02101,"774,55567010210 ",.01)
 ;;55567010210
 ;;9002226.02101,"774,55567010210 ",.02)
 ;;55567010210
 ;;9002226.02101,"774,55567010218 ",.01)
 ;;55567010218
 ;;9002226.02101,"774,55567010218 ",.02)
 ;;55567010218
 ;;9002226.02101,"774,55567010235 ",.01)
 ;;55567010235
 ;;9002226.02101,"774,55567010235 ",.02)
 ;;55567010235
 ;;9002226.02101,"774,55567010600 ",.01)
 ;;55567010600
 ;;9002226.02101,"774,55567010600 ",.02)
 ;;55567010600
 ;;9002226.02101,"774,55567010601 ",.01)
 ;;55567010601
 ;;9002226.02101,"774,55567010601 ",.02)
 ;;55567010601
 ;;9002226.02101,"774,55567010610 ",.01)
 ;;55567010610
 ;;9002226.02101,"774,55567010610 ",.02)
 ;;55567010610
 ;;9002226.02101,"774,55567010618 ",.01)
 ;;55567010618
 ;;9002226.02101,"774,55567010618 ",.02)
 ;;55567010618
 ;;9002226.02101,"774,55567010635 ",.01)
 ;;55567010635
 ;;9002226.02101,"774,55567010635 ",.02)
 ;;55567010635
 ;;9002226.02101,"774,55567010700 ",.01)
 ;;55567010700
 ;;9002226.02101,"774,55567010700 ",.02)
 ;;55567010700
 ;;9002226.02101,"774,55567010701 ",.01)
 ;;55567010701
 ;;9002226.02101,"774,55567010701 ",.02)
 ;;55567010701
 ;;9002226.02101,"774,55567010710 ",.01)
 ;;55567010710
 ;;9002226.02101,"774,55567010710 ",.02)
 ;;55567010710
 ;;9002226.02101,"774,55567010718 ",.01)
 ;;55567010718
 ;;9002226.02101,"774,55567010718 ",.02)
 ;;55567010718
 ;;9002226.02101,"774,55567010735 ",.01)
 ;;55567010735
 ;;9002226.02101,"774,55567010735 ",.02)
 ;;55567010735
 ;;9002226.02101,"774,55567010800 ",.01)
 ;;55567010800
 ;;9002226.02101,"774,55567010800 ",.02)
 ;;55567010800
 ;;9002226.02101,"774,55567010801 ",.01)
 ;;55567010801
 ;;9002226.02101,"774,55567010801 ",.02)
 ;;55567010801
 ;;9002226.02101,"774,55567010810 ",.01)
 ;;55567010810
 ;;9002226.02101,"774,55567010810 ",.02)
 ;;55567010810
 ;;9002226.02101,"774,55567010818 ",.01)
 ;;55567010818
 ;;9002226.02101,"774,55567010818 ",.02)
 ;;55567010818
 ;;9002226.02101,"774,55567010835 ",.01)
 ;;55567010835
 ;;9002226.02101,"774,55567010835 ",.02)
 ;;55567010835
 ;;9002226.02101,"774,55567026600 ",.01)
 ;;55567026600
 ;;9002226.02101,"774,55567026600 ",.02)
 ;;55567026600
 ;;9002226.02101,"774,55567026601 ",.01)
 ;;55567026601
 ;;9002226.02101,"774,55567026601 ",.02)
 ;;55567026601
 ;;9002226.02101,"774,55567026610 ",.01)
 ;;55567026610
 ;;9002226.02101,"774,55567026610 ",.02)
 ;;55567026610
 ;;9002226.02101,"774,55567026700 ",.01)
 ;;55567026700
 ;;9002226.02101,"774,55567026700 ",.02)
 ;;55567026700
 ;;9002226.02101,"774,55567026701 ",.01)
 ;;55567026701
 ;;9002226.02101,"774,55567026701 ",.02)
 ;;55567026701
 ;;9002226.02101,"774,55567026710 ",.01)
 ;;55567026710
 ;;9002226.02101,"774,55567026710 ",.02)
 ;;55567026710
 ;;9002226.02101,"774,55567026800 ",.01)
 ;;55567026800
 ;;9002226.02101,"774,55567026800 ",.02)
 ;;55567026800
 ;;9002226.02101,"774,55567026801 ",.01)
 ;;55567026801
 ;;9002226.02101,"774,55567026801 ",.02)
 ;;55567026801
 ;;9002226.02101,"774,55567026810 ",.01)
 ;;55567026810
 ;;9002226.02101,"774,55567026810 ",.02)
 ;;55567026810
 ;;9002226.02101,"774,55587057810 ",.01)
 ;;55587057810
 ;;9002226.02101,"774,55587057810 ",.02)
 ;;55587057810
 ;;9002226.02101,"774,55587057830 ",.01)
 ;;55587057830
 ;;9002226.02101,"774,55587057830 ",.02)
 ;;55587057830
 ;;9002226.02101,"774,55587057845 ",.01)
 ;;55587057845
 ;;9002226.02101,"774,55587057845 ",.02)
 ;;55587057845
 ;;9002226.02101,"774,55587057860 ",.01)
 ;;55587057860
 ;;9002226.02101,"774,55587057860 ",.02)
 ;;55587057860
 ;;9002226.02101,"774,55587057886 ",.01)
 ;;55587057886
 ;;9002226.02101,"774,55587057886 ",.02)
 ;;55587057886
 ;;9002226.02101,"774,55587057890 ",.01)
 ;;55587057890
 ;;9002226.02101,"774,55587057890 ",.02)
 ;;55587057890
 ;;9002226.02101,"774,55887004401 ",.01)
 ;;55887004401
 ;;9002226.02101,"774,55887004401 ",.02)
 ;;55887004401
 ;;9002226.02101,"774,55887004430 ",.01)
 ;;55887004430
 ;;9002226.02101,"774,55887004430 ",.02)
 ;;55887004430
 ;;9002226.02101,"774,55887004460 ",.01)
 ;;55887004460
 ;;9002226.02101,"774,55887004460 ",.02)
 ;;55887004460
 ;;9002226.02101,"774,55887004490 ",.01)
 ;;55887004490
 ;;9002226.02101,"774,55887004490 ",.02)
 ;;55887004490
 ;;9002226.02101,"774,55887026401 ",.01)
 ;;55887026401
 ;;9002226.02101,"774,55887026401 ",.02)
 ;;55887026401
 ;;9002226.02101,"774,55887026430 ",.01)
 ;;55887026430
 ;;9002226.02101,"774,55887026430 ",.02)
 ;;55887026430
 ;;9002226.02101,"774,55887026490 ",.01)
 ;;55887026490
 ;;9002226.02101,"774,55887026490 ",.02)
 ;;55887026490
 ;;9002226.02101,"774,55887046430 ",.01)
 ;;55887046430
 ;;9002226.02101,"774,55887046430 ",.02)
 ;;55887046430
 ;;9002226.02101,"774,55887046490 ",.01)
 ;;55887046490
 ;;9002226.02101,"774,55887046490 ",.02)
 ;;55887046490
 ;;9002226.02101,"774,55887057710 ",.01)
 ;;55887057710
 ;;9002226.02101,"774,55887057710 ",.02)
 ;;55887057710
 ;;9002226.02101,"774,55887057730 ",.01)
 ;;55887057730
 ;;9002226.02101,"774,55887057730 ",.02)
 ;;55887057730
 ;;9002226.02101,"774,55887057760 ",.01)
 ;;55887057760
 ;;9002226.02101,"774,55887057760 ",.02)
 ;;55887057760
 ;;9002226.02101,"774,55887057790 ",.01)
 ;;55887057790
 ;;9002226.02101,"774,55887057790 ",.02)
 ;;55887057790
 ;;9002226.02101,"774,55887057810 ",.01)
 ;;55887057810
 ;;9002226.02101,"774,55887057810 ",.02)
 ;;55887057810
 ;;9002226.02101,"774,55887057830 ",.01)
 ;;55887057830
 ;;9002226.02101,"774,55887057830 ",.02)
 ;;55887057830
 ;;9002226.02101,"774,55887057860 ",.01)
 ;;55887057860
 ;;9002226.02101,"774,55887057860 ",.02)
 ;;55887057860
 ;;9002226.02101,"774,55887057886 ",.01)
 ;;55887057886
 ;;9002226.02101,"774,55887057886 ",.02)
 ;;55887057886
 ;;9002226.02101,"774,55887057890 ",.01)
 ;;55887057890
 ;;9002226.02101,"774,55887057890 ",.02)
 ;;55887057890
 ;;9002226.02101,"774,55887092630 ",.01)
 ;;55887092630
 ;;9002226.02101,"774,55887092630 ",.02)
 ;;55887092630
 ;;9002226.02101,"774,55887092690 ",.01)
 ;;55887092690
 ;;9002226.02101,"774,55887092690 ",.02)
 ;;55887092690
 ;;9002226.02101,"774,57866994601 ",.01)
 ;;57866994601
 ;;9002226.02101,"774,57866994601 ",.02)
 ;;57866994601
 ;;9002226.02101,"774,57866994602 ",.01)
 ;;57866994602
 ;;9002226.02101,"774,57866994602 ",.02)
 ;;57866994602
 ;;9002226.02101,"774,58016008300 ",.01)
 ;;58016008300
 ;;9002226.02101,"774,58016008300 ",.02)
 ;;58016008300
 ;;9002226.02101,"774,58016008301 ",.01)
 ;;58016008301
 ;;9002226.02101,"774,58016008301 ",.02)
 ;;58016008301
 ;;9002226.02101,"774,58016008302 ",.01)
 ;;58016008302
 ;;9002226.02101,"774,58016008302 ",.02)
 ;;58016008302
 ;;9002226.02101,"774,58016008303 ",.01)
 ;;58016008303
 ;;9002226.02101,"774,58016008303 ",.02)
 ;;58016008303
 ;;9002226.02101,"774,58016008304 ",.01)
 ;;58016008304
 ;;9002226.02101,"774,58016008304 ",.02)
 ;;58016008304
 ;;9002226.02101,"774,58016008305 ",.01)
 ;;58016008305
 ;;9002226.02101,"774,58016008305 ",.02)
 ;;58016008305
 ;;9002226.02101,"774,58016008306 ",.01)
 ;;58016008306
 ;;9002226.02101,"774,58016008306 ",.02)
 ;;58016008306
 ;;9002226.02101,"774,58016008307 ",.01)
 ;;58016008307
 ;;9002226.02101,"774,58016008307 ",.02)
 ;;58016008307
 ;;9002226.02101,"774,58016008308 ",.01)
 ;;58016008308
 ;;9002226.02101,"774,58016008308 ",.02)
 ;;58016008308
 ;;9002226.02101,"774,58016008309 ",.01)
 ;;58016008309
 ;;9002226.02101,"774,58016008309 ",.02)
 ;;58016008309
 ;;9002226.02101,"774,58016008310 ",.01)
 ;;58016008310
 ;;9002226.02101,"774,58016008310 ",.02)
 ;;58016008310
 ;;9002226.02101,"774,58016008312 ",.01)
 ;;58016008312
 ;;9002226.02101,"774,58016008312 ",.02)
 ;;58016008312
 ;;9002226.02101,"774,58016008314 ",.01)
 ;;58016008314
 ;;9002226.02101,"774,58016008314 ",.02)
 ;;58016008314
 ;;9002226.02101,"774,58016008315 ",.01)
 ;;58016008315
 ;;9002226.02101,"774,58016008315 ",.02)
 ;;58016008315
 ;;9002226.02101,"774,58016008316 ",.01)
 ;;58016008316
 ;;9002226.02101,"774,58016008316 ",.02)
 ;;58016008316
 ;;9002226.02101,"774,58016008318 ",.01)
 ;;58016008318
 ;;9002226.02101,"774,58016008318 ",.02)
 ;;58016008318
 ;;9002226.02101,"774,58016008320 ",.01)
 ;;58016008320
 ;;9002226.02101,"774,58016008320 ",.02)
 ;;58016008320
 ;;9002226.02101,"774,58016008321 ",.01)
 ;;58016008321
 ;;9002226.02101,"774,58016008321 ",.02)
 ;;58016008321
 ;;9002226.02101,"774,58016008324 ",.01)
 ;;58016008324
 ;;9002226.02101,"774,58016008324 ",.02)
 ;;58016008324
 ;;9002226.02101,"774,58016008325 ",.01)
 ;;58016008325
 ;;9002226.02101,"774,58016008325 ",.02)
 ;;58016008325
 ;;9002226.02101,"774,58016008326 ",.01)
 ;;58016008326
 ;;9002226.02101,"774,58016008326 ",.02)
 ;;58016008326
 ;;9002226.02101,"774,58016008327 ",.01)
 ;;58016008327
 ;;9002226.02101,"774,58016008327 ",.02)
 ;;58016008327
 ;;9002226.02101,"774,58016008328 ",.01)
 ;;58016008328
 ;;9002226.02101,"774,58016008328 ",.02)
 ;;58016008328
 ;;9002226.02101,"774,58016008330 ",.01)
 ;;58016008330
 ;;9002226.02101,"774,58016008330 ",.02)
 ;;58016008330
 ;;9002226.02101,"774,58016008332 ",.01)
 ;;58016008332
 ;;9002226.02101,"774,58016008332 ",.02)
 ;;58016008332
 ;;9002226.02101,"774,58016008335 ",.01)
 ;;58016008335
 ;;9002226.02101,"774,58016008335 ",.02)
 ;;58016008335
 ;;9002226.02101,"774,58016008336 ",.01)
 ;;58016008336
 ;;9002226.02101,"774,58016008336 ",.02)
 ;;58016008336
 ;;9002226.02101,"774,58016008340 ",.01)
 ;;58016008340
 ;;9002226.02101,"774,58016008340 ",.02)
 ;;58016008340
 ;;9002226.02101,"774,58016008342 ",.01)
 ;;58016008342
 ;;9002226.02101,"774,58016008342 ",.02)
 ;;58016008342
 ;;9002226.02101,"774,58016008344 ",.01)
 ;;58016008344
 ;;9002226.02101,"774,58016008344 ",.02)
 ;;58016008344
 ;;9002226.02101,"774,58016008345 ",.01)
 ;;58016008345
 ;;9002226.02101,"774,58016008345 ",.02)
 ;;58016008345
 ;;9002226.02101,"774,58016008348 ",.01)
 ;;58016008348
 ;;9002226.02101,"774,58016008348 ",.02)
 ;;58016008348
 ;;9002226.02101,"774,58016008350 ",.01)
 ;;58016008350
 ;;9002226.02101,"774,58016008350 ",.02)
 ;;58016008350
 ;;9002226.02101,"774,58016008356 ",.01)
 ;;58016008356
