BGPM3ABP ;IHS/MSC/SAT-CREATED BY ^ATXSTX ON APR 21, 2011;
 ;;11.0;IHS CLINICAL REPORTING;**4**;JAN 06, 2011;Build 84
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"775,55045290208 ",.01)
 ;;55045290208
 ;;9002226.02101,"775,55045290208 ",.02)
 ;;55045290208
 ;;9002226.02101,"775,55045290300 ",.01)
 ;;55045290300
 ;;9002226.02101,"775,55045290300 ",.02)
 ;;55045290300
 ;;9002226.02101,"775,55045329201 ",.01)
 ;;55045329201
 ;;9002226.02101,"775,55045329201 ",.02)
 ;;55045329201
 ;;9002226.02101,"775,55111019601 ",.01)
 ;;55111019601
 ;;9002226.02101,"775,55111019601 ",.02)
 ;;55111019601
 ;;9002226.02101,"775,55111019630 ",.01)
 ;;55111019630
 ;;9002226.02101,"775,55111019630 ",.02)
 ;;55111019630
 ;;9002226.02101,"775,55111019690 ",.01)
 ;;55111019690
 ;;9002226.02101,"775,55111019690 ",.02)
 ;;55111019690
 ;;9002226.02101,"775,55154014105 ",.01)
 ;;55154014105
 ;;9002226.02101,"775,55154014105 ",.02)
 ;;55154014105
 ;;9002226.02101,"775,55154087609 ",.01)
 ;;55154087609
 ;;9002226.02101,"775,55154087609 ",.02)
 ;;55154087609
 ;;9002226.02101,"775,55154087709 ",.01)
 ;;55154087709
 ;;9002226.02101,"775,55154087709 ",.02)
 ;;55154087709
 ;;9002226.02101,"775,55154087809 ",.01)
 ;;55154087809
 ;;9002226.02101,"775,55154087809 ",.02)
 ;;55154087809
 ;;9002226.02101,"775,55154087909 ",.01)
 ;;55154087909
 ;;9002226.02101,"775,55154087909 ",.02)
 ;;55154087909
 ;;9002226.02101,"775,55154088009 ",.01)
 ;;55154088009
 ;;9002226.02101,"775,55154088009 ",.02)
 ;;55154088009
 ;;9002226.02101,"775,55154088309 ",.01)
 ;;55154088309
 ;;9002226.02101,"775,55154088309 ",.02)
 ;;55154088309
 ;;9002226.02101,"775,55154088409 ",.01)
 ;;55154088409
 ;;9002226.02101,"775,55154088409 ",.02)
 ;;55154088409
 ;;9002226.02101,"775,55154236205 ",.01)
 ;;55154236205
 ;;9002226.02101,"775,55154236205 ",.02)
 ;;55154236205
 ;;9002226.02101,"775,55154239705 ",.01)
 ;;55154239705
 ;;9002226.02101,"775,55154239705 ",.02)
 ;;55154239705
 ;;9002226.02101,"775,55154420507 ",.01)
 ;;55154420507
 ;;9002226.02101,"775,55154420507 ",.02)
 ;;55154420507
 ;;9002226.02101,"775,55154510105 ",.01)
 ;;55154510105
 ;;9002226.02101,"775,55154510105 ",.02)
 ;;55154510105
 ;;9002226.02101,"775,55154510107 ",.01)
 ;;55154510107
 ;;9002226.02101,"775,55154510107 ",.02)
 ;;55154510107
 ;;9002226.02101,"775,55154510305 ",.01)
 ;;55154510305
 ;;9002226.02101,"775,55154510305 ",.02)
 ;;55154510305
 ;;9002226.02101,"775,55154510307 ",.01)
 ;;55154510307
 ;;9002226.02101,"775,55154510307 ",.02)
 ;;55154510307
 ;;9002226.02101,"775,55154510905 ",.01)
 ;;55154510905
 ;;9002226.02101,"775,55154510905 ",.02)
 ;;55154510905
 ;;9002226.02101,"775,55154512205 ",.01)
 ;;55154512205
 ;;9002226.02101,"775,55154512205 ",.02)
 ;;55154512205
 ;;9002226.02101,"775,55154512207 ",.01)
 ;;55154512207
 ;;9002226.02101,"775,55154512207 ",.02)
 ;;55154512207
 ;;9002226.02101,"775,55154512605 ",.01)
 ;;55154512605
 ;;9002226.02101,"775,55154512605 ",.02)
 ;;55154512605
 ;;9002226.02101,"775,55154870107 ",.01)
 ;;55154870107
 ;;9002226.02101,"775,55154870107 ",.02)
 ;;55154870107
 ;;9002226.02101,"775,55154935105 ",.01)
 ;;55154935105
 ;;9002226.02101,"775,55154935105 ",.02)
 ;;55154935105
 ;;9002226.02101,"775,55154935305 ",.01)
 ;;55154935305
 ;;9002226.02101,"775,55154935305 ",.02)
 ;;55154935305
 ;;9002226.02101,"775,55154935405 ",.01)
 ;;55154935405
 ;;9002226.02101,"775,55154935405 ",.02)
 ;;55154935405
 ;;9002226.02101,"775,55154935505 ",.01)
 ;;55154935505
 ;;9002226.02101,"775,55154935505 ",.02)
 ;;55154935505
 ;;9002226.02101,"775,55154935905 ",.01)
 ;;55154935905
 ;;9002226.02101,"775,55154935905 ",.02)
 ;;55154935905
 ;;9002226.02101,"775,55154936005 ",.01)
 ;;55154936005
 ;;9002226.02101,"775,55154936005 ",.02)
 ;;55154936005
 ;;9002226.02101,"775,55154937305 ",.01)
 ;;55154937305
 ;;9002226.02101,"775,55154937305 ",.02)
 ;;55154937305
 ;;9002226.02101,"775,55154937705 ",.01)
 ;;55154937705
 ;;9002226.02101,"775,55154937705 ",.02)
 ;;55154937705
 ;;9002226.02101,"775,55175169300 ",.01)
 ;;55175169300
 ;;9002226.02101,"775,55175169300 ",.02)
 ;;55175169300
 ;;9002226.02101,"775,55289034030 ",.01)
 ;;55289034030
 ;;9002226.02101,"775,55289034030 ",.02)
 ;;55289034030
 ;;9002226.02101,"775,55289077314 ",.01)
 ;;55289077314
 ;;9002226.02101,"775,55289077314 ",.02)
 ;;55289077314
 ;;9002226.02101,"775,55289077330 ",.01)
 ;;55289077330
 ;;9002226.02101,"775,55289077330 ",.02)
 ;;55289077330
 ;;9002226.02101,"775,55289077360 ",.01)
 ;;55289077360
 ;;9002226.02101,"775,55289077360 ",.02)
 ;;55289077360
 ;;9002226.02101,"775,55289077390 ",.01)
 ;;55289077390
 ;;9002226.02101,"775,55289077390 ",.02)
 ;;55289077390
 ;;9002226.02101,"775,55567005400 ",.01)
 ;;55567005400
 ;;9002226.02101,"775,55567005400 ",.02)
 ;;55567005400
 ;;9002226.02101,"775,55567005407 ",.01)
 ;;55567005407
 ;;9002226.02101,"775,55567005407 ",.02)
 ;;55567005407
 ;;9002226.02101,"775,55567005413 ",.01)
 ;;55567005413
 ;;9002226.02101,"775,55567005413 ",.02)
 ;;55567005413
 ;;9002226.02101,"775,55567005418 ",.01)
 ;;55567005418
 ;;9002226.02101,"775,55567005418 ",.02)
 ;;55567005418
 ;;9002226.02101,"775,55567005425 ",.01)
 ;;55567005425
 ;;9002226.02101,"775,55567005425 ",.02)
 ;;55567005425
 ;;9002226.02101,"775,55567010000 ",.01)
 ;;55567010000
 ;;9002226.02101,"775,55567010000 ",.02)
 ;;55567010000
 ;;9002226.02101,"775,55567010001 ",.01)
 ;;55567010001
 ;;9002226.02101,"775,55567010001 ",.02)
 ;;55567010001
 ;;9002226.02101,"775,55567010010 ",.01)
 ;;55567010010
 ;;9002226.02101,"775,55567010010 ",.02)
 ;;55567010010
 ;;9002226.02101,"775,55567010100 ",.01)
 ;;55567010100
 ;;9002226.02101,"775,55567010100 ",.02)
 ;;55567010100
 ;;9002226.02101,"775,55567010101 ",.01)
 ;;55567010101
 ;;9002226.02101,"775,55567010101 ",.02)
 ;;55567010101
 ;;9002226.02101,"775,55567010110 ",.01)
 ;;55567010110
 ;;9002226.02101,"775,55567010110 ",.02)
 ;;55567010110
 ;;9002226.02101,"775,55567010118 ",.01)
 ;;55567010118
 ;;9002226.02101,"775,55567010118 ",.02)
 ;;55567010118
 ;;9002226.02101,"775,55567010135 ",.01)
 ;;55567010135
 ;;9002226.02101,"775,55567010135 ",.02)
 ;;55567010135
 ;;9002226.02101,"775,55567010200 ",.01)
 ;;55567010200
 ;;9002226.02101,"775,55567010200 ",.02)
 ;;55567010200
 ;;9002226.02101,"775,55567010201 ",.01)
 ;;55567010201
 ;;9002226.02101,"775,55567010201 ",.02)
 ;;55567010201
 ;;9002226.02101,"775,55567010210 ",.01)
 ;;55567010210
 ;;9002226.02101,"775,55567010210 ",.02)
 ;;55567010210
 ;;9002226.02101,"775,55567010218 ",.01)
 ;;55567010218
 ;;9002226.02101,"775,55567010218 ",.02)
 ;;55567010218
 ;;9002226.02101,"775,55567010235 ",.01)
 ;;55567010235
 ;;9002226.02101,"775,55567010235 ",.02)
 ;;55567010235
 ;;9002226.02101,"775,55567010600 ",.01)
 ;;55567010600
 ;;9002226.02101,"775,55567010600 ",.02)
 ;;55567010600
 ;;9002226.02101,"775,55567010601 ",.01)
 ;;55567010601
 ;;9002226.02101,"775,55567010601 ",.02)
 ;;55567010601
 ;;9002226.02101,"775,55567010610 ",.01)
 ;;55567010610
 ;;9002226.02101,"775,55567010610 ",.02)
 ;;55567010610
 ;;9002226.02101,"775,55567010618 ",.01)
 ;;55567010618
 ;;9002226.02101,"775,55567010618 ",.02)
 ;;55567010618
 ;;9002226.02101,"775,55567010635 ",.01)
 ;;55567010635
 ;;9002226.02101,"775,55567010635 ",.02)
 ;;55567010635
 ;;9002226.02101,"775,55567010700 ",.01)
 ;;55567010700
 ;;9002226.02101,"775,55567010700 ",.02)
 ;;55567010700
 ;;9002226.02101,"775,55567010701 ",.01)
 ;;55567010701
 ;;9002226.02101,"775,55567010701 ",.02)
 ;;55567010701
 ;;9002226.02101,"775,55567010710 ",.01)
 ;;55567010710
 ;;9002226.02101,"775,55567010710 ",.02)
 ;;55567010710
 ;;9002226.02101,"775,55567010718 ",.01)
 ;;55567010718
 ;;9002226.02101,"775,55567010718 ",.02)
 ;;55567010718
 ;;9002226.02101,"775,55567010735 ",.01)
 ;;55567010735
 ;;9002226.02101,"775,55567010735 ",.02)
 ;;55567010735
 ;;9002226.02101,"775,55567010800 ",.01)
 ;;55567010800
 ;;9002226.02101,"775,55567010800 ",.02)
 ;;55567010800
 ;;9002226.02101,"775,55567010801 ",.01)
 ;;55567010801
 ;;9002226.02101,"775,55567010801 ",.02)
 ;;55567010801
 ;;9002226.02101,"775,55567010810 ",.01)
 ;;55567010810
 ;;9002226.02101,"775,55567010810 ",.02)
 ;;55567010810
 ;;9002226.02101,"775,55567010818 ",.01)
 ;;55567010818
 ;;9002226.02101,"775,55567010818 ",.02)
 ;;55567010818
 ;;9002226.02101,"775,55567010835 ",.01)
 ;;55567010835
 ;;9002226.02101,"775,55567010835 ",.02)
 ;;55567010835
 ;;9002226.02101,"775,55567026600 ",.01)
 ;;55567026600
 ;;9002226.02101,"775,55567026600 ",.02)
 ;;55567026600
 ;;9002226.02101,"775,55567026601 ",.01)
 ;;55567026601
 ;;9002226.02101,"775,55567026601 ",.02)
 ;;55567026601
 ;;9002226.02101,"775,55567026610 ",.01)
 ;;55567026610
 ;;9002226.02101,"775,55567026610 ",.02)
 ;;55567026610
 ;;9002226.02101,"775,55567026700 ",.01)
 ;;55567026700
 ;;9002226.02101,"775,55567026700 ",.02)
 ;;55567026700
 ;;9002226.02101,"775,55567026701 ",.01)
 ;;55567026701
 ;;9002226.02101,"775,55567026701 ",.02)
 ;;55567026701
 ;;9002226.02101,"775,55567026710 ",.01)
 ;;55567026710
 ;;9002226.02101,"775,55567026710 ",.02)
 ;;55567026710
 ;;9002226.02101,"775,55567026800 ",.01)
 ;;55567026800
 ;;9002226.02101,"775,55567026800 ",.02)
 ;;55567026800
 ;;9002226.02101,"775,55567026801 ",.01)
 ;;55567026801
 ;;9002226.02101,"775,55567026801 ",.02)
 ;;55567026801
 ;;9002226.02101,"775,55567026810 ",.01)
 ;;55567026810
 ;;9002226.02101,"775,55567026810 ",.02)
 ;;55567026810
 ;;9002226.02101,"775,55587057810 ",.01)
 ;;55587057810
 ;;9002226.02101,"775,55587057810 ",.02)
 ;;55587057810
 ;;9002226.02101,"775,55587057830 ",.01)
 ;;55587057830
 ;;9002226.02101,"775,55587057830 ",.02)
 ;;55587057830
 ;;9002226.02101,"775,55587057845 ",.01)
 ;;55587057845
 ;;9002226.02101,"775,55587057845 ",.02)
 ;;55587057845
 ;;9002226.02101,"775,55587057860 ",.01)
 ;;55587057860
 ;;9002226.02101,"775,55587057860 ",.02)
 ;;55587057860
 ;;9002226.02101,"775,55587057886 ",.01)
 ;;55587057886
 ;;9002226.02101,"775,55587057886 ",.02)
 ;;55587057886
 ;;9002226.02101,"775,55587057890 ",.01)
 ;;55587057890
 ;;9002226.02101,"775,55587057890 ",.02)
 ;;55587057890
 ;;9002226.02101,"775,55829015310 ",.01)
 ;;55829015310
 ;;9002226.02101,"775,55829015310 ",.02)
 ;;55829015310
 ;;9002226.02101,"775,55887004401 ",.01)
 ;;55887004401
 ;;9002226.02101,"775,55887004401 ",.02)
 ;;55887004401
 ;;9002226.02101,"775,55887004430 ",.01)
 ;;55887004430
 ;;9002226.02101,"775,55887004430 ",.02)
 ;;55887004430
 ;;9002226.02101,"775,55887004460 ",.01)
 ;;55887004460
 ;;9002226.02101,"775,55887004460 ",.02)
 ;;55887004460
 ;;9002226.02101,"775,55887004490 ",.01)
 ;;55887004490
 ;;9002226.02101,"775,55887004490 ",.02)
 ;;55887004490
 ;;9002226.02101,"775,55887026401 ",.01)
 ;;55887026401
 ;;9002226.02101,"775,55887026401 ",.02)
 ;;55887026401
 ;;9002226.02101,"775,55887026430 ",.01)
 ;;55887026430
 ;;9002226.02101,"775,55887026430 ",.02)
 ;;55887026430
 ;;9002226.02101,"775,55887026490 ",.01)
 ;;55887026490
 ;;9002226.02101,"775,55887026490 ",.02)
 ;;55887026490
 ;;9002226.02101,"775,55887038630 ",.01)
 ;;55887038630
 ;;9002226.02101,"775,55887038630 ",.02)
 ;;55887038630
 ;;9002226.02101,"775,55887046430 ",.01)
 ;;55887046430
 ;;9002226.02101,"775,55887046430 ",.02)
 ;;55887046430
 ;;9002226.02101,"775,55887046490 ",.01)
 ;;55887046490
 ;;9002226.02101,"775,55887046490 ",.02)
 ;;55887046490
 ;;9002226.02101,"775,55887057710 ",.01)
 ;;55887057710
 ;;9002226.02101,"775,55887057710 ",.02)
 ;;55887057710
 ;;9002226.02101,"775,55887057730 ",.01)
 ;;55887057730
 ;;9002226.02101,"775,55887057730 ",.02)
 ;;55887057730
 ;;9002226.02101,"775,55887057760 ",.01)
 ;;55887057760
 ;;9002226.02101,"775,55887057760 ",.02)
 ;;55887057760
