BGPM5AGD ;IHS/MSC/MMT-CREATED BY ^ATXSTX ON JUL 15, 2011;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"882,00143117130 ",.02)
 ;;00143117130
 ;;9002226.02101,"882,00143117190 ",.01)
 ;;00143117190
 ;;9002226.02101,"882,00143117190 ",.02)
 ;;00143117190
 ;;9002226.02101,"882,00143117201 ",.01)
 ;;00143117201
 ;;9002226.02101,"882,00143117201 ",.02)
 ;;00143117201
 ;;9002226.02101,"882,00143117210 ",.01)
 ;;00143117210
 ;;9002226.02101,"882,00143117210 ",.02)
 ;;00143117210
 ;;9002226.02101,"882,00143117225 ",.01)
 ;;00143117225
 ;;9002226.02101,"882,00143117225 ",.02)
 ;;00143117225
 ;;9002226.02101,"882,00143117230 ",.01)
 ;;00143117230
 ;;9002226.02101,"882,00143117230 ",.02)
 ;;00143117230
 ;;9002226.02101,"882,00143117290 ",.01)
 ;;00143117290
 ;;9002226.02101,"882,00143117290 ",.02)
 ;;00143117290
 ;;9002226.02101,"882,00143117301 ",.01)
 ;;00143117301
 ;;9002226.02101,"882,00143117301 ",.02)
 ;;00143117301
 ;;9002226.02101,"882,00143117309 ",.01)
 ;;00143117309
 ;;9002226.02101,"882,00143117309 ",.02)
 ;;00143117309
 ;;9002226.02101,"882,00143117310 ",.01)
 ;;00143117310
 ;;9002226.02101,"882,00143117310 ",.02)
 ;;00143117310
 ;;9002226.02101,"882,00143117325 ",.01)
 ;;00143117325
 ;;9002226.02101,"882,00143117325 ",.02)
 ;;00143117325
 ;;9002226.02101,"882,00143117330 ",.01)
 ;;00143117330
 ;;9002226.02101,"882,00143117330 ",.02)
 ;;00143117330
 ;;9002226.02101,"882,00143117401 ",.01)
 ;;00143117401
 ;;9002226.02101,"882,00143117401 ",.02)
 ;;00143117401
 ;;9002226.02101,"882,00143117409 ",.01)
 ;;00143117409
 ;;9002226.02101,"882,00143117409 ",.02)
 ;;00143117409
 ;;9002226.02101,"882,00143117410 ",.01)
 ;;00143117410
 ;;9002226.02101,"882,00143117410 ",.02)
 ;;00143117410
 ;;9002226.02101,"882,00143117425 ",.01)
 ;;00143117425
 ;;9002226.02101,"882,00143117425 ",.02)
 ;;00143117425
 ;;9002226.02101,"882,00143117430 ",.01)
 ;;00143117430
 ;;9002226.02101,"882,00143117430 ",.02)
 ;;00143117430
 ;;9002226.02101,"882,00143126201 ",.01)
 ;;00143126201
 ;;9002226.02101,"882,00143126201 ",.02)
 ;;00143126201
 ;;9002226.02101,"882,00143126205 ",.01)
 ;;00143126205
 ;;9002226.02101,"882,00143126205 ",.02)
 ;;00143126205
 ;;9002226.02101,"882,00143126210 ",.01)
 ;;00143126210
 ;;9002226.02101,"882,00143126210 ",.02)
 ;;00143126210
 ;;9002226.02101,"882,00143126230 ",.01)
 ;;00143126230
 ;;9002226.02101,"882,00143126230 ",.02)
 ;;00143126230
 ;;9002226.02101,"882,00143126301 ",.01)
 ;;00143126301
 ;;9002226.02101,"882,00143126301 ",.02)
 ;;00143126301
 ;;9002226.02101,"882,00143126305 ",.01)
 ;;00143126305
 ;;9002226.02101,"882,00143126305 ",.02)
 ;;00143126305
 ;;9002226.02101,"882,00143126310 ",.01)
 ;;00143126310
 ;;9002226.02101,"882,00143126310 ",.02)
 ;;00143126310
 ;;9002226.02101,"882,00143126330 ",.01)
 ;;00143126330
 ;;9002226.02101,"882,00143126330 ",.02)
 ;;00143126330
 ;;9002226.02101,"882,00143126401 ",.01)
 ;;00143126401
 ;;9002226.02101,"882,00143126401 ",.02)
 ;;00143126401
 ;;9002226.02101,"882,00143126405 ",.01)
 ;;00143126405
 ;;9002226.02101,"882,00143126405 ",.02)
 ;;00143126405
 ;;9002226.02101,"882,00143126410 ",.01)
 ;;00143126410
 ;;9002226.02101,"882,00143126410 ",.02)
 ;;00143126410
 ;;9002226.02101,"882,00143126430 ",.01)
 ;;00143126430
 ;;9002226.02101,"882,00143126430 ",.02)
 ;;00143126430
 ;;9002226.02101,"882,00143126501 ",.01)
 ;;00143126501
 ;;9002226.02101,"882,00143126501 ",.02)
 ;;00143126501
 ;;9002226.02101,"882,00143126509 ",.01)
 ;;00143126509
 ;;9002226.02101,"882,00143126509 ",.02)
 ;;00143126509
 ;;9002226.02101,"882,00143126510 ",.01)
 ;;00143126510
 ;;9002226.02101,"882,00143126510 ",.02)
 ;;00143126510
 ;;9002226.02101,"882,00143126530 ",.01)
 ;;00143126530
 ;;9002226.02101,"882,00143126530 ",.02)
 ;;00143126530
 ;;9002226.02101,"882,00143126601 ",.01)
 ;;00143126601
 ;;9002226.02101,"882,00143126601 ",.02)
 ;;00143126601
 ;;9002226.02101,"882,00143126609 ",.01)
 ;;00143126609
 ;;9002226.02101,"882,00143126609 ",.02)
 ;;00143126609
 ;;9002226.02101,"882,00143126610 ",.01)
 ;;00143126610
 ;;9002226.02101,"882,00143126610 ",.02)
 ;;00143126610
 ;;9002226.02101,"882,00143126630 ",.01)
 ;;00143126630
 ;;9002226.02101,"882,00143126630 ",.02)
 ;;00143126630
 ;;9002226.02101,"882,00143126645 ",.01)
 ;;00143126645
 ;;9002226.02101,"882,00143126645 ",.02)
 ;;00143126645
 ;;9002226.02101,"882,00143126701 ",.01)
 ;;00143126701
 ;;9002226.02101,"882,00143126701 ",.02)
 ;;00143126701
 ;;9002226.02101,"882,00143126709 ",.01)
 ;;00143126709
 ;;9002226.02101,"882,00143126709 ",.02)
 ;;00143126709
 ;;9002226.02101,"882,00143126710 ",.01)
 ;;00143126710
 ;;9002226.02101,"882,00143126710 ",.02)
 ;;00143126710
 ;;9002226.02101,"882,00143126718 ",.01)
 ;;00143126718
 ;;9002226.02101,"882,00143126718 ",.02)
 ;;00143126718
 ;;9002226.02101,"882,00143126730 ",.01)
 ;;00143126730
 ;;9002226.02101,"882,00143126730 ",.02)
 ;;00143126730
 ;;9002226.02101,"882,00143126745 ",.01)
 ;;00143126745
 ;;9002226.02101,"882,00143126745 ",.02)
 ;;00143126745
 ;;9002226.02101,"882,00143126751 ",.01)
 ;;00143126751
 ;;9002226.02101,"882,00143126751 ",.02)
 ;;00143126751
 ;;9002226.02101,"882,00143126801 ",.01)
 ;;00143126801
 ;;9002226.02101,"882,00143126801 ",.02)
 ;;00143126801
 ;;9002226.02101,"882,00143126809 ",.01)
 ;;00143126809
 ;;9002226.02101,"882,00143126809 ",.02)
 ;;00143126809
 ;;9002226.02101,"882,00143126810 ",.01)
 ;;00143126810
 ;;9002226.02101,"882,00143126810 ",.02)
 ;;00143126810
 ;;9002226.02101,"882,00143126818 ",.01)
 ;;00143126818
 ;;9002226.02101,"882,00143126818 ",.02)
 ;;00143126818
 ;;9002226.02101,"882,00143126830 ",.01)
 ;;00143126830
 ;;9002226.02101,"882,00143126830 ",.02)
 ;;00143126830
 ;;9002226.02101,"882,00143126845 ",.01)
 ;;00143126845
 ;;9002226.02101,"882,00143126845 ",.02)
 ;;00143126845
 ;;9002226.02101,"882,00143126851 ",.01)
 ;;00143126851
 ;;9002226.02101,"882,00143126851 ",.02)
 ;;00143126851
 ;;9002226.02101,"882,00143127001 ",.01)
 ;;00143127001
 ;;9002226.02101,"882,00143127001 ",.02)
 ;;00143127001
 ;;9002226.02101,"882,00143127009 ",.01)
 ;;00143127009
 ;;9002226.02101,"882,00143127009 ",.02)
 ;;00143127009
 ;;9002226.02101,"882,00143127010 ",.01)
 ;;00143127010
 ;;9002226.02101,"882,00143127010 ",.02)
 ;;00143127010
 ;;9002226.02101,"882,00143127018 ",.01)
 ;;00143127018
 ;;9002226.02101,"882,00143127018 ",.02)
 ;;00143127018
 ;;9002226.02101,"882,00143127030 ",.01)
 ;;00143127030
 ;;9002226.02101,"882,00143127030 ",.02)
 ;;00143127030
 ;;9002226.02101,"882,00143127045 ",.01)
 ;;00143127045
 ;;9002226.02101,"882,00143127045 ",.02)
 ;;00143127045
 ;;9002226.02101,"882,00143127051 ",.01)
 ;;00143127051
 ;;9002226.02101,"882,00143127051 ",.02)
 ;;00143127051
 ;;9002226.02101,"882,00143128001 ",.01)
 ;;00143128001
 ;;9002226.02101,"882,00143128001 ",.02)
 ;;00143128001
 ;;9002226.02101,"882,00143128010 ",.01)
 ;;00143128010
 ;;9002226.02101,"882,00143128010 ",.02)
 ;;00143128010
 ;;9002226.02101,"882,00143128030 ",.01)
 ;;00143128030
 ;;9002226.02101,"882,00143128030 ",.02)
 ;;00143128030
 ;;9002226.02101,"882,00143912501 ",.01)
 ;;00143912501
 ;;9002226.02101,"882,00143912501 ",.02)
 ;;00143912501
 ;;9002226.02101,"882,00143912601 ",.01)
 ;;00143912601
 ;;9002226.02101,"882,00143912601 ",.02)
 ;;00143912601
 ;;9002226.02101,"882,00172375700 ",.01)
 ;;00172375700
 ;;9002226.02101,"882,00172375700 ",.02)
 ;;00172375700
 ;;9002226.02101,"882,00172375710 ",.01)
 ;;00172375710
 ;;9002226.02101,"882,00172375710 ",.02)
 ;;00172375710
 ;;9002226.02101,"882,00172375760 ",.01)
 ;;00172375760
 ;;9002226.02101,"882,00172375760 ",.02)
 ;;00172375760
 ;;9002226.02101,"882,00172375770 ",.01)
 ;;00172375770
 ;;9002226.02101,"882,00172375770 ",.02)
 ;;00172375770
 ;;9002226.02101,"882,00172375800 ",.01)
 ;;00172375800
 ;;9002226.02101,"882,00172375800 ",.02)
 ;;00172375800
 ;;9002226.02101,"882,00172375810 ",.01)
 ;;00172375810
 ;;9002226.02101,"882,00172375810 ",.02)
 ;;00172375810
 ;;9002226.02101,"882,00172375860 ",.01)
 ;;00172375860
 ;;9002226.02101,"882,00172375860 ",.02)
 ;;00172375860
 ;;9002226.02101,"882,00172375870 ",.01)
 ;;00172375870
 ;;9002226.02101,"882,00172375870 ",.02)
 ;;00172375870
 ;;9002226.02101,"882,00172375880 ",.01)
 ;;00172375880
 ;;9002226.02101,"882,00172375880 ",.02)
 ;;00172375880
 ;;9002226.02101,"882,00172375900 ",.01)
 ;;00172375900
 ;;9002226.02101,"882,00172375900 ",.02)
 ;;00172375900
 ;;9002226.02101,"882,00172375910 ",.01)
 ;;00172375910
 ;;9002226.02101,"882,00172375910 ",.02)
 ;;00172375910
 ;;9002226.02101,"882,00172375960 ",.01)
 ;;00172375960
 ;;9002226.02101,"882,00172375960 ",.02)
 ;;00172375960
 ;;9002226.02101,"882,00172375970 ",.01)
 ;;00172375970
 ;;9002226.02101,"882,00172375970 ",.02)
 ;;00172375970
 ;;9002226.02101,"882,00172375980 ",.01)
 ;;00172375980
 ;;9002226.02101,"882,00172375980 ",.02)
 ;;00172375980
 ;;9002226.02101,"882,00172376000 ",.01)
 ;;00172376000
 ;;9002226.02101,"882,00172376000 ",.02)
 ;;00172376000
 ;;9002226.02101,"882,00172376010 ",.01)
 ;;00172376010
 ;;9002226.02101,"882,00172376010 ",.02)
 ;;00172376010
 ;;9002226.02101,"882,00172376060 ",.01)
 ;;00172376060
 ;;9002226.02101,"882,00172376060 ",.02)
 ;;00172376060
 ;;9002226.02101,"882,00172376070 ",.01)
 ;;00172376070
 ;;9002226.02101,"882,00172376070 ",.02)
 ;;00172376070
 ;;9002226.02101,"882,00172376080 ",.01)
 ;;00172376080
 ;;9002226.02101,"882,00172376080 ",.02)
 ;;00172376080
 ;;9002226.02101,"882,00172376100 ",.01)
 ;;00172376100
 ;;9002226.02101,"882,00172376100 ",.02)
 ;;00172376100
 ;;9002226.02101,"882,00172376110 ",.01)
 ;;00172376110
 ;;9002226.02101,"882,00172376110 ",.02)
 ;;00172376110
 ;;9002226.02101,"882,00172376160 ",.01)
 ;;00172376160
 ;;9002226.02101,"882,00172376160 ",.02)
 ;;00172376160
 ;;9002226.02101,"882,00172376170 ",.01)
 ;;00172376170
 ;;9002226.02101,"882,00172376170 ",.02)
 ;;00172376170
 ;;9002226.02101,"882,00172376180 ",.01)
 ;;00172376180
 ;;9002226.02101,"882,00172376180 ",.02)
 ;;00172376180
 ;;9002226.02101,"882,00172376200 ",.01)
 ;;00172376200
 ;;9002226.02101,"882,00172376200 ",.02)
 ;;00172376200
 ;;9002226.02101,"882,00172376210 ",.01)
 ;;00172376210
 ;;9002226.02101,"882,00172376210 ",.02)
 ;;00172376210
 ;;9002226.02101,"882,00172376260 ",.01)
 ;;00172376260
 ;;9002226.02101,"882,00172376260 ",.02)
 ;;00172376260
 ;;9002226.02101,"882,00172376270 ",.01)
 ;;00172376270
 ;;9002226.02101,"882,00172376270 ",.02)
 ;;00172376270
 ;;9002226.02101,"882,00172419510 ",.01)
 ;;00172419510
 ;;9002226.02101,"882,00172419510 ",.02)
 ;;00172419510
 ;;9002226.02101,"882,00172419610 ",.01)
 ;;00172419610
 ;;9002226.02101,"882,00172419610 ",.02)
 ;;00172419610
 ;;9002226.02101,"882,00172419710 ",.01)
 ;;00172419710
 ;;9002226.02101,"882,00172419710 ",.02)
 ;;00172419710
 ;;9002226.02101,"882,00172419810 ",.01)
 ;;00172419810
 ;;9002226.02101,"882,00172419810 ",.02)
 ;;00172419810
 ;;9002226.02101,"882,00172503200 ",.01)
 ;;00172503200
 ;;9002226.02101,"882,00172503200 ",.02)
 ;;00172503200
 ;;9002226.02101,"882,00172503210 ",.01)
 ;;00172503210
 ;;9002226.02101,"882,00172503210 ",.02)
 ;;00172503210
 ;;9002226.02101,"882,00172503260 ",.01)
 ;;00172503260
 ;;9002226.02101,"882,00172503260 ",.02)
 ;;00172503260
 ;;9002226.02101,"882,00172503270 ",.01)
 ;;00172503270
 ;;9002226.02101,"882,00172503270 ",.02)
 ;;00172503270
 ;;9002226.02101,"882,00172503300 ",.01)
 ;;00172503300
 ;;9002226.02101,"882,00172503300 ",.02)
 ;;00172503300
 ;;9002226.02101,"882,00172503310 ",.01)
 ;;00172503310
 ;;9002226.02101,"882,00172503310 ",.02)
 ;;00172503310
 ;;9002226.02101,"882,00172503360 ",.01)
 ;;00172503360
