BGPM5ALT ;IHS/MSC/MMT-CREATED BY ^ATXSTX ON JUL 15, 2011;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"900,51138045710 ",.01)
 ;;51138045710
 ;;9002226.02101,"900,51138045710 ",.02)
 ;;51138045710
 ;;9002226.02101,"900,51138045730 ",.01)
 ;;51138045730
 ;;9002226.02101,"900,51138045730 ",.02)
 ;;51138045730
 ;;9002226.02101,"900,51138045810 ",.01)
 ;;51138045810
 ;;9002226.02101,"900,51138045810 ",.02)
 ;;51138045810
 ;;9002226.02101,"900,51138045830 ",.01)
 ;;51138045830
 ;;9002226.02101,"900,51138045830 ",.02)
 ;;51138045830
 ;;9002226.02101,"900,51645079201 ",.01)
 ;;51645079201
 ;;9002226.02101,"900,51645079201 ",.02)
 ;;51645079201
 ;;9002226.02101,"900,51645079210 ",.01)
 ;;51645079210
 ;;9002226.02101,"900,51645079210 ",.02)
 ;;51645079210
 ;;9002226.02101,"900,51655022624 ",.01)
 ;;51655022624
 ;;9002226.02101,"900,51655022624 ",.02)
 ;;51655022624
 ;;9002226.02101,"900,51655028124 ",.01)
 ;;51655028124
 ;;9002226.02101,"900,51655028124 ",.02)
 ;;51655028124
 ;;9002226.02101,"900,51655028150 ",.01)
 ;;51655028150
 ;;9002226.02101,"900,51655028150 ",.02)
 ;;51655028150
 ;;9002226.02101,"900,51655028152 ",.01)
 ;;51655028152
 ;;9002226.02101,"900,51655028152 ",.02)
 ;;51655028152
 ;;9002226.02101,"900,51655031424 ",.01)
 ;;51655031424
 ;;9002226.02101,"900,51655031424 ",.02)
 ;;51655031424
 ;;9002226.02101,"900,51655031450 ",.01)
 ;;51655031450
 ;;9002226.02101,"900,51655031450 ",.02)
 ;;51655031450
 ;;9002226.02101,"900,51655031452 ",.01)
 ;;51655031452
 ;;9002226.02101,"900,51655031452 ",.02)
 ;;51655031452
 ;;9002226.02101,"900,51655031453 ",.01)
 ;;51655031453
 ;;9002226.02101,"900,51655031453 ",.02)
 ;;51655031453
 ;;9002226.02101,"900,51655037024 ",.01)
 ;;51655037024
 ;;9002226.02101,"900,51655037024 ",.02)
 ;;51655037024
 ;;9002226.02101,"900,51655037052 ",.01)
 ;;51655037052
 ;;9002226.02101,"900,51655037052 ",.02)
 ;;51655037052
 ;;9002226.02101,"900,51655057924 ",.01)
 ;;51655057924
 ;;9002226.02101,"900,51655057924 ",.02)
 ;;51655057924
 ;;9002226.02101,"900,51655057925 ",.01)
 ;;51655057925
 ;;9002226.02101,"900,51655057925 ",.02)
 ;;51655057925
 ;;9002226.02101,"900,51655057990 ",.01)
 ;;51655057990
 ;;9002226.02101,"900,51655057990 ",.02)
 ;;51655057990
 ;;9002226.02101,"900,52297001778 ",.01)
 ;;52297001778
 ;;9002226.02101,"900,52297001778 ",.02)
 ;;52297001778
 ;;9002226.02101,"900,52297052278 ",.01)
 ;;52297052278
 ;;9002226.02101,"900,52297052278 ",.02)
 ;;52297052278
 ;;9002226.02101,"900,52544045405 ",.01)
 ;;52544045405
 ;;9002226.02101,"900,52544045405 ",.02)
 ;;52544045405
 ;;9002226.02101,"900,52544045460 ",.01)
 ;;52544045460
 ;;9002226.02101,"900,52544045460 ",.02)
 ;;52544045460
 ;;9002226.02101,"900,52735007601 ",.01)
 ;;52735007601
 ;;9002226.02101,"900,52735007601 ",.02)
 ;;52735007601
 ;;9002226.02101,"900,52959004630 ",.01)
 ;;52959004630
 ;;9002226.02101,"900,52959004630 ",.02)
 ;;52959004630
 ;;9002226.02101,"900,52959011230 ",.01)
 ;;52959011230
 ;;9002226.02101,"900,52959011230 ",.02)
 ;;52959011230
 ;;9002226.02101,"900,52959072030 ",.01)
 ;;52959072030
 ;;9002226.02101,"900,52959072030 ",.02)
 ;;52959072030
 ;;9002226.02101,"900,52959075990 ",.01)
 ;;52959075990
 ;;9002226.02101,"900,52959075990 ",.02)
 ;;52959075990
 ;;9002226.02101,"900,52959076090 ",.01)
 ;;52959076090
 ;;9002226.02101,"900,52959076090 ",.02)
 ;;52959076090
 ;;9002226.02101,"900,52959093730 ",.01)
 ;;52959093730
 ;;9002226.02101,"900,52959093730 ",.02)
 ;;52959093730
 ;;9002226.02101,"900,52959097430 ",.01)
 ;;52959097430
 ;;9002226.02101,"900,52959097430 ",.02)
 ;;52959097430
 ;;9002226.02101,"900,53002057000 ",.01)
 ;;53002057000
 ;;9002226.02101,"900,53002057000 ",.02)
 ;;53002057000
 ;;9002226.02101,"900,53002057030 ",.01)
 ;;53002057030
 ;;9002226.02101,"900,53002057030 ",.02)
 ;;53002057030
 ;;9002226.02101,"900,53002057060 ",.01)
 ;;53002057060
 ;;9002226.02101,"900,53002057060 ",.02)
 ;;53002057060
 ;;9002226.02101,"900,53002057090 ",.01)
 ;;53002057090
 ;;9002226.02101,"900,53002057090 ",.02)
 ;;53002057090
 ;;9002226.02101,"900,53489060701 ",.01)
 ;;53489060701
 ;;9002226.02101,"900,53489060701 ",.02)
 ;;53489060701
 ;;9002226.02101,"900,53489060703 ",.01)
 ;;53489060703
 ;;9002226.02101,"900,53489060703 ",.02)
 ;;53489060703
 ;;9002226.02101,"900,53489060705 ",.01)
 ;;53489060705
 ;;9002226.02101,"900,53489060705 ",.02)
 ;;53489060705
 ;;9002226.02101,"900,53489060706 ",.01)
 ;;53489060706
 ;;9002226.02101,"900,53489060706 ",.02)
 ;;53489060706
 ;;9002226.02101,"900,53489060707 ",.01)
 ;;53489060707
 ;;9002226.02101,"900,53489060707 ",.02)
 ;;53489060707
 ;;9002226.02101,"900,53489060710 ",.01)
 ;;53489060710
 ;;9002226.02101,"900,53489060710 ",.02)
 ;;53489060710
 ;;9002226.02101,"900,53489060801 ",.01)
 ;;53489060801
 ;;9002226.02101,"900,53489060801 ",.02)
 ;;53489060801
 ;;9002226.02101,"900,53489060803 ",.01)
 ;;53489060803
 ;;9002226.02101,"900,53489060803 ",.02)
 ;;53489060803
 ;;9002226.02101,"900,53489060805 ",.01)
 ;;53489060805
 ;;9002226.02101,"900,53489060805 ",.02)
 ;;53489060805
 ;;9002226.02101,"900,53489060806 ",.01)
 ;;53489060806
 ;;9002226.02101,"900,53489060806 ",.02)
 ;;53489060806
 ;;9002226.02101,"900,53489060807 ",.01)
 ;;53489060807
 ;;9002226.02101,"900,53489060807 ",.02)
 ;;53489060807
 ;;9002226.02101,"900,53489060810 ",.01)
 ;;53489060810
 ;;9002226.02101,"900,53489060810 ",.02)
 ;;53489060810
 ;;9002226.02101,"900,53489060901 ",.01)
 ;;53489060901
 ;;9002226.02101,"900,53489060901 ",.02)
 ;;53489060901
 ;;9002226.02101,"900,53489060903 ",.01)
 ;;53489060903
 ;;9002226.02101,"900,53489060903 ",.02)
 ;;53489060903
 ;;9002226.02101,"900,53489060905 ",.01)
 ;;53489060905
 ;;9002226.02101,"900,53489060905 ",.02)
 ;;53489060905
 ;;9002226.02101,"900,53489060906 ",.01)
 ;;53489060906
 ;;9002226.02101,"900,53489060906 ",.02)
 ;;53489060906
 ;;9002226.02101,"900,53489060907 ",.01)
 ;;53489060907
 ;;9002226.02101,"900,53489060907 ",.02)
 ;;53489060907
 ;;9002226.02101,"900,53489060910 ",.01)
 ;;53489060910
 ;;9002226.02101,"900,53489060910 ",.02)
 ;;53489060910
 ;;9002226.02101,"900,53808025101 ",.01)
 ;;53808025101
 ;;9002226.02101,"900,53808025101 ",.02)
 ;;53808025101
 ;;9002226.02101,"900,53808036801 ",.01)
 ;;53808036801
 ;;9002226.02101,"900,53808036801 ",.02)
 ;;53808036801
 ;;9002226.02101,"900,53808067301 ",.01)
 ;;53808067301
 ;;9002226.02101,"900,53808067301 ",.02)
 ;;53808067301
 ;;9002226.02101,"900,53869015503 ",.01)
 ;;53869015503
 ;;9002226.02101,"900,53869015503 ",.02)
 ;;53869015503
 ;;9002226.02101,"900,53869015504 ",.01)
 ;;53869015504
 ;;9002226.02101,"900,53869015504 ",.02)
 ;;53869015504
 ;;9002226.02101,"900,53869015505 ",.01)
 ;;53869015505
 ;;9002226.02101,"900,53869015505 ",.02)
 ;;53869015505
 ;;9002226.02101,"900,53869015506 ",.01)
 ;;53869015506
 ;;9002226.02101,"900,53869015506 ",.02)
 ;;53869015506
 ;;9002226.02101,"900,53869015509 ",.01)
 ;;53869015509
 ;;9002226.02101,"900,53869015509 ",.02)
 ;;53869015509
 ;;9002226.02101,"900,53869015603 ",.01)
 ;;53869015603
 ;;9002226.02101,"900,53869015603 ",.02)
 ;;53869015603
 ;;9002226.02101,"900,53869015604 ",.01)
 ;;53869015604
 ;;9002226.02101,"900,53869015604 ",.02)
 ;;53869015604
 ;;9002226.02101,"900,53869015605 ",.01)
 ;;53869015605
 ;;9002226.02101,"900,53869015605 ",.02)
 ;;53869015605
 ;;9002226.02101,"900,53869015606 ",.01)
 ;;53869015606
 ;;9002226.02101,"900,53869015606 ",.02)
 ;;53869015606
 ;;9002226.02101,"900,53869015609 ",.01)
 ;;53869015609
 ;;9002226.02101,"900,53869015609 ",.02)
 ;;53869015609
 ;;9002226.02101,"900,53869015703 ",.01)
 ;;53869015703
 ;;9002226.02101,"900,53869015703 ",.02)
 ;;53869015703
 ;;9002226.02101,"900,53869015707 ",.01)
 ;;53869015707
 ;;9002226.02101,"900,53869015707 ",.02)
 ;;53869015707
 ;;9002226.02101,"900,53869015709 ",.01)
 ;;53869015709
 ;;9002226.02101,"900,53869015709 ",.02)
 ;;53869015709
 ;;9002226.02101,"900,53869145700 ",.01)
 ;;53869145700
 ;;9002226.02101,"900,53869145700 ",.02)
 ;;53869145700
 ;;9002226.02101,"900,53869145800 ",.01)
 ;;53869145800
 ;;9002226.02101,"900,53869145800 ",.02)
 ;;53869145800
 ;;9002226.02101,"900,53869215003 ",.01)
 ;;53869215003
 ;;9002226.02101,"900,53869215003 ",.02)
 ;;53869215003
 ;;9002226.02101,"900,53869215004 ",.01)
 ;;53869215004
 ;;9002226.02101,"900,53869215004 ",.02)
 ;;53869215004
 ;;9002226.02101,"900,53869215007 ",.01)
 ;;53869215007
 ;;9002226.02101,"900,53869215007 ",.02)
 ;;53869215007
 ;;9002226.02101,"900,53869215008 ",.01)
 ;;53869215008
 ;;9002226.02101,"900,53869215008 ",.02)
 ;;53869215008
 ;;9002226.02101,"900,53869215009 ",.01)
 ;;53869215009
 ;;9002226.02101,"900,53869215009 ",.02)
 ;;53869215009
 ;;9002226.02101,"900,53869215100 ",.01)
 ;;53869215100
 ;;9002226.02101,"900,53869215100 ",.02)
 ;;53869215100
 ;;9002226.02101,"900,53869216003 ",.01)
 ;;53869216003
 ;;9002226.02101,"900,53869216003 ",.02)
 ;;53869216003
 ;;9002226.02101,"900,53869216008 ",.01)
 ;;53869216008
 ;;9002226.02101,"900,53869216008 ",.02)
 ;;53869216008
 ;;9002226.02101,"900,53869216009 ",.01)
 ;;53869216009
 ;;9002226.02101,"900,53869216009 ",.02)
 ;;53869216009
 ;;9002226.02101,"900,53869217003 ",.01)
 ;;53869217003
 ;;9002226.02101,"900,53869217003 ",.02)
 ;;53869217003
 ;;9002226.02101,"900,53869217004 ",.01)
 ;;53869217004
 ;;9002226.02101,"900,53869217004 ",.02)
 ;;53869217004
 ;;9002226.02101,"900,53869217007 ",.01)
 ;;53869217007
 ;;9002226.02101,"900,53869217007 ",.02)
 ;;53869217007
 ;;9002226.02101,"900,53869217008 ",.01)
 ;;53869217008
 ;;9002226.02101,"900,53869217008 ",.02)
 ;;53869217008
 ;;9002226.02101,"900,53869217009 ",.01)
 ;;53869217009
 ;;9002226.02101,"900,53869217009 ",.02)
 ;;53869217009
 ;;9002226.02101,"900,53869217100 ",.01)
 ;;53869217100
 ;;9002226.02101,"900,53869217100 ",.02)
 ;;53869217100
 ;;9002226.02101,"900,53869218003 ",.01)
 ;;53869218003
 ;;9002226.02101,"900,53869218003 ",.02)
 ;;53869218003
 ;;9002226.02101,"900,53869218004 ",.01)
 ;;53869218004
 ;;9002226.02101,"900,53869218004 ",.02)
 ;;53869218004
 ;;9002226.02101,"900,53869218007 ",.01)
 ;;53869218007
 ;;9002226.02101,"900,53869218007 ",.02)
 ;;53869218007
 ;;9002226.02101,"900,53869218008 ",.01)
 ;;53869218008
 ;;9002226.02101,"900,53869218008 ",.02)
 ;;53869218008
 ;;9002226.02101,"900,53869218009 ",.01)
 ;;53869218009
 ;;9002226.02101,"900,53869218009 ",.02)
 ;;53869218009
 ;;9002226.02101,"900,53869219003 ",.01)
 ;;53869219003
 ;;9002226.02101,"900,53869219003 ",.02)
 ;;53869219003
 ;;9002226.02101,"900,53869219004 ",.01)
 ;;53869219004
 ;;9002226.02101,"900,53869219004 ",.02)
 ;;53869219004
 ;;9002226.02101,"900,53869219007 ",.01)
 ;;53869219007
 ;;9002226.02101,"900,53869219007 ",.02)
 ;;53869219007
 ;;9002226.02101,"900,53869219008 ",.01)
 ;;53869219008
 ;;9002226.02101,"900,53869219008 ",.02)
 ;;53869219008
 ;;9002226.02101,"900,53869219009 ",.01)
 ;;53869219009
 ;;9002226.02101,"900,53869219009 ",.02)
 ;;53869219009
 ;;9002226.02101,"900,53869219100 ",.01)
 ;;53869219100
 ;;9002226.02101,"900,53869219100 ",.02)
 ;;53869219100
 ;;9002226.02101,"900,53869225001 ",.01)
 ;;53869225001
 ;;9002226.02101,"900,53869225001 ",.02)
 ;;53869225001
 ;;9002226.02101,"900,53869225003 ",.01)
 ;;53869225003
 ;;9002226.02101,"900,53869225003 ",.02)
 ;;53869225003
 ;;9002226.02101,"900,53869226003 ",.01)
 ;;53869226003
 ;;9002226.02101,"900,53869226003 ",.02)
 ;;53869226003
 ;;9002226.02101,"900,53869227003 ",.01)
 ;;53869227003
 ;;9002226.02101,"900,53869227003 ",.02)
 ;;53869227003
 ;;9002226.02101,"900,53869296003 ",.01)
 ;;53869296003
 ;;9002226.02101,"900,53869296003 ",.02)
 ;;53869296003
