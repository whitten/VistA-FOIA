BGPM3AFD ;IHS/MSC/SAT-CREATED BY ^ATXSTX ON APR 21, 2011;
 ;;11.0;IHS CLINICAL REPORTING;**4**;JAN 06, 2011;Build 84
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"791,00725004701 ",.02)
 ;;00725004701
 ;;9002226.02101,"791,00725004710 ",.01)
 ;;00725004710
 ;;9002226.02101,"791,00725004710 ",.02)
 ;;00725004710
 ;;9002226.02101,"791,00725005001 ",.01)
 ;;00725005001
 ;;9002226.02101,"791,00725005001 ",.02)
 ;;00725005001
 ;;9002226.02101,"791,00725005010 ",.01)
 ;;00725005010
 ;;9002226.02101,"791,00725005010 ",.02)
 ;;00725005010
 ;;9002226.02101,"791,00781035207 ",.01)
 ;;00781035207
 ;;9002226.02101,"791,00781035207 ",.02)
 ;;00781035207
 ;;9002226.02101,"791,00781035208 ",.01)
 ;;00781035208
 ;;9002226.02101,"791,00781035208 ",.02)
 ;;00781035208
 ;;9002226.02101,"791,00781036306 ",.01)
 ;;00781036306
 ;;9002226.02101,"791,00781036306 ",.02)
 ;;00781036306
 ;;9002226.02101,"791,00781036307 ",.01)
 ;;00781036307
 ;;9002226.02101,"791,00781036307 ",.02)
 ;;00781036307
 ;;9002226.02101,"791,00781036308 ",.01)
 ;;00781036308
 ;;9002226.02101,"791,00781036308 ",.02)
 ;;00781036308
 ;;9002226.02101,"791,00781036406 ",.01)
 ;;00781036406
 ;;9002226.02101,"791,00781036406 ",.02)
 ;;00781036406
 ;;9002226.02101,"791,00781036407 ",.01)
 ;;00781036407
 ;;9002226.02101,"791,00781036407 ",.02)
 ;;00781036407
 ;;9002226.02101,"791,00781036408 ",.01)
 ;;00781036408
 ;;9002226.02101,"791,00781036408 ",.02)
 ;;00781036408
 ;;9002226.02101,"791,00781036607 ",.01)
 ;;00781036607
 ;;9002226.02101,"791,00781036607 ",.02)
 ;;00781036607
 ;;9002226.02101,"791,00781036608 ",.01)
 ;;00781036608
 ;;9002226.02101,"791,00781036608 ",.02)
 ;;00781036608
 ;;9002226.02101,"791,00781036907 ",.01)
 ;;00781036907
 ;;9002226.02101,"791,00781036907 ",.02)
 ;;00781036907
 ;;9002226.02101,"791,00781036908 ",.01)
 ;;00781036908
 ;;9002226.02101,"791,00781036908 ",.02)
 ;;00781036908
 ;;9002226.02101,"791,00781037704 ",.01)
 ;;00781037704
 ;;9002226.02101,"791,00781037704 ",.02)
 ;;00781037704
 ;;9002226.02101,"791,00781037706 ",.01)
 ;;00781037706
 ;;9002226.02101,"791,00781037706 ",.02)
 ;;00781037706
 ;;9002226.02101,"791,00781037707 ",.01)
 ;;00781037707
 ;;9002226.02101,"791,00781037707 ",.02)
 ;;00781037707
 ;;9002226.02101,"791,00781037708 ",.01)
 ;;00781037708
 ;;9002226.02101,"791,00781037708 ",.02)
 ;;00781037708
 ;;9002226.02101,"791,00781038107 ",.01)
 ;;00781038107
 ;;9002226.02101,"791,00781038107 ",.02)
 ;;00781038107
 ;;9002226.02101,"791,00781038108 ",.01)
 ;;00781038108
 ;;9002226.02101,"791,00781038108 ",.02)
 ;;00781038108
 ;;9002226.02101,"791,00781038707 ",.01)
 ;;00781038707
 ;;9002226.02101,"791,00781038707 ",.02)
 ;;00781038707
 ;;9002226.02101,"791,00781038708 ",.01)
 ;;00781038708
 ;;9002226.02101,"791,00781038708 ",.02)
 ;;00781038708
 ;;9002226.02101,"791,00814852114 ",.01)
 ;;00814852114
 ;;9002226.02101,"791,00814852114 ",.02)
 ;;00814852114
 ;;9002226.02101,"791,00814852214 ",.01)
 ;;00814852214
 ;;9002226.02101,"791,00814852214 ",.02)
 ;;00814852214
 ;;9002226.02101,"791,00814852230 ",.01)
 ;;00814852230
 ;;9002226.02101,"791,00814852230 ",.02)
 ;;00814852230
 ;;9002226.02101,"791,00832062500 ",.01)
 ;;00832062500
 ;;9002226.02101,"791,00832062500 ",.02)
 ;;00832062500
 ;;9002226.02101,"791,00832062600 ",.01)
 ;;00832062600
 ;;9002226.02101,"791,00832062600 ",.02)
 ;;00832062600
 ;;9002226.02101,"791,00832062700 ",.01)
 ;;00832062700
 ;;9002226.02101,"791,00832062700 ",.02)
 ;;00832062700
 ;;9002226.02101,"791,00832062710 ",.01)
 ;;00832062710
 ;;9002226.02101,"791,00832062710 ",.02)
 ;;00832062710
 ;;9002226.02101,"791,00832062725 ",.01)
 ;;00832062725
 ;;9002226.02101,"791,00832062725 ",.02)
 ;;00832062725
 ;;9002226.02101,"791,00839662606 ",.01)
 ;;00839662606
 ;;9002226.02101,"791,00839662606 ",.02)
 ;;00839662606
 ;;9002226.02101,"791,00839662706 ",.01)
 ;;00839662706
 ;;9002226.02101,"791,00839662706 ",.02)
 ;;00839662706
 ;;9002226.02101,"791,00839662806 ",.01)
 ;;00839662806
 ;;9002226.02101,"791,00839662806 ",.02)
 ;;00839662806
 ;;9002226.02101,"791,00839663006 ",.01)
 ;;00839663006
 ;;9002226.02101,"791,00839663006 ",.02)
 ;;00839663006
 ;;9002226.02101,"791,00894556001 ",.01)
 ;;00894556001
 ;;9002226.02101,"791,00894556001 ",.02)
 ;;00894556001
 ;;9002226.02101,"791,00894556002 ",.01)
 ;;00894556002
 ;;9002226.02101,"791,00894556002 ",.02)
 ;;00894556002
 ;;9002226.02101,"791,00894556501 ",.01)
 ;;00894556501
 ;;9002226.02101,"791,00894556501 ",.02)
 ;;00894556501
 ;;9002226.02101,"791,00894556502 ",.01)
 ;;00894556502
 ;;9002226.02101,"791,00894556502 ",.02)
 ;;00894556502
 ;;9002226.02101,"791,00894557001 ",.01)
 ;;00894557001
 ;;9002226.02101,"791,00894557001 ",.02)
 ;;00894557001
 ;;9002226.02101,"791,00894557002 ",.01)
 ;;00894557002
 ;;9002226.02101,"791,00894557002 ",.02)
 ;;00894557002
 ;;9002226.02101,"791,00894558001 ",.01)
 ;;00894558001
 ;;9002226.02101,"791,00894558001 ",.02)
 ;;00894558001
 ;;9002226.02101,"791,00894558002 ",.01)
 ;;00894558002
 ;;9002226.02101,"791,00894558002 ",.02)
 ;;00894558002
 ;;9002226.02101,"791,00904256060 ",.01)
 ;;00904256060
 ;;9002226.02101,"791,00904256060 ",.02)
 ;;00904256060
 ;;9002226.02101,"791,00904256160 ",.01)
 ;;00904256160
 ;;9002226.02101,"791,00904256160 ",.02)
 ;;00904256160
 ;;9002226.02101,"791,00904256260 ",.01)
 ;;00904256260
 ;;9002226.02101,"791,00904256260 ",.02)
 ;;00904256260
 ;;9002226.02101,"791,00904256270 ",.01)
 ;;00904256270
 ;;9002226.02101,"791,00904256270 ",.02)
 ;;00904256270
 ;;9002226.02101,"791,00904256360 ",.01)
 ;;00904256360
 ;;9002226.02101,"791,00904256360 ",.02)
 ;;00904256360
 ;;9002226.02101,"791,10009008401 ",.01)
 ;;10009008401
 ;;9002226.02101,"791,10009008401 ",.02)
 ;;10009008401
 ;;9002226.02101,"791,10544011830 ",.01)
 ;;10544011830
 ;;9002226.02101,"791,10544011830 ",.02)
 ;;10544011830
 ;;9002226.02101,"791,10647017202 ",.01)
 ;;10647017202
 ;;9002226.02101,"791,10647017202 ",.02)
 ;;10647017202
 ;;9002226.02101,"791,11722092199 ",.01)
 ;;11722092199
 ;;9002226.02101,"791,11722092199 ",.02)
 ;;11722092199
 ;;9002226.02101,"791,11722092299 ",.01)
 ;;11722092299
 ;;9002226.02101,"791,11722092299 ",.02)
 ;;11722092299
 ;;9002226.02101,"791,11722092399 ",.01)
 ;;11722092399
 ;;9002226.02101,"791,11722092399 ",.02)
 ;;11722092399
 ;;9002226.02101,"791,11722092499 ",.01)
 ;;11722092499
 ;;9002226.02101,"791,11722092499 ",.02)
 ;;11722092499
 ;;9002226.02101,"791,11722092599 ",.01)
 ;;11722092599
 ;;9002226.02101,"791,11722092599 ",.02)
 ;;11722092599
 ;;9002226.02101,"791,11722092699 ",.01)
 ;;11722092699
 ;;9002226.02101,"791,11722092699 ",.02)
 ;;11722092699
 ;;9002226.02101,"791,11722092799 ",.01)
 ;;11722092799
 ;;9002226.02101,"791,11722092799 ",.02)
 ;;11722092799
 ;;9002226.02101,"791,11722092999 ",.01)
 ;;11722092999
 ;;9002226.02101,"791,11722092999 ",.02)
 ;;11722092999
 ;;9002226.02101,"791,13411033903 ",.01)
 ;;13411033903
 ;;9002226.02101,"791,13411033903 ",.02)
 ;;13411033903
 ;;9002226.02101,"791,15330010001 ",.01)
 ;;15330010001
 ;;9002226.02101,"791,15330010001 ",.02)
 ;;15330010001
 ;;9002226.02101,"791,15330010010 ",.01)
 ;;15330010010
 ;;9002226.02101,"791,15330010010 ",.02)
 ;;15330010010
 ;;9002226.02101,"791,15330010101 ",.01)
 ;;15330010101
 ;;9002226.02101,"791,15330010101 ",.02)
 ;;15330010101
 ;;9002226.02101,"791,15330010110 ",.01)
 ;;15330010110
 ;;9002226.02101,"791,15330010110 ",.02)
 ;;15330010110
 ;;9002226.02101,"791,15330010201 ",.01)
 ;;15330010201
 ;;9002226.02101,"791,15330010201 ",.02)
 ;;15330010201
 ;;9002226.02101,"791,15330010210 ",.01)
 ;;15330010210
 ;;9002226.02101,"791,15330010210 ",.02)
 ;;15330010210
 ;;9002226.02101,"791,15330010601 ",.01)
 ;;15330010601
 ;;9002226.02101,"791,15330010601 ",.02)
 ;;15330010601
 ;;9002226.02101,"791,15330010801 ",.01)
 ;;15330010801
 ;;9002226.02101,"791,15330010801 ",.02)
 ;;15330010801
 ;;9002226.02101,"791,15330026601 ",.01)
 ;;15330026601
 ;;9002226.02101,"791,15330026601 ",.02)
 ;;15330026601
 ;;9002226.02101,"791,15330026701 ",.01)
 ;;15330026701
 ;;9002226.02101,"791,15330026701 ",.02)
 ;;15330026701
 ;;9002226.02101,"791,15330026801 ",.01)
 ;;15330026801
 ;;9002226.02101,"791,15330026801 ",.02)
 ;;15330026801
 ;;9002226.02101,"791,15330026810 ",.01)
 ;;15330026810
 ;;9002226.02101,"791,15330026810 ",.02)
 ;;15330026810
 ;;9002226.02101,"791,17856402902 ",.01)
 ;;17856402902
 ;;9002226.02101,"791,17856402902 ",.02)
 ;;17856402902
 ;;9002226.02101,"791,19458044301 ",.01)
 ;;19458044301
 ;;9002226.02101,"791,19458044301 ",.02)
 ;;19458044301
 ;;9002226.02101,"791,19458047101 ",.01)
 ;;19458047101
 ;;9002226.02101,"791,19458047101 ",.02)
 ;;19458047101
 ;;9002226.02101,"791,19458059801 ",.01)
 ;;19458059801
 ;;9002226.02101,"791,19458059801 ",.02)
 ;;19458059801
 ;;9002226.02101,"791,21695067330 ",.01)
 ;;21695067330
 ;;9002226.02101,"791,21695067330 ",.02)
 ;;21695067330
 ;;9002226.02101,"791,21695067730 ",.01)
 ;;21695067730
 ;;9002226.02101,"791,21695067730 ",.02)
 ;;21695067730
 ;;9002226.02101,"791,23490083305 ",.01)
 ;;23490083305
 ;;9002226.02101,"791,23490083305 ",.02)
 ;;23490083305
 ;;9002226.02101,"791,23490141903 ",.01)
 ;;23490141903
 ;;9002226.02101,"791,23490141903 ",.02)
 ;;23490141903
 ;;9002226.02101,"791,23490141909 ",.01)
 ;;23490141909
 ;;9002226.02101,"791,23490141909 ",.02)
 ;;23490141909
 ;;9002226.02101,"791,23490142003 ",.01)
 ;;23490142003
 ;;9002226.02101,"791,23490142003 ",.02)
 ;;23490142003
 ;;9002226.02101,"791,23490142009 ",.01)
 ;;23490142009
 ;;9002226.02101,"791,23490142009 ",.02)
 ;;23490142009
 ;;9002226.02101,"791,23490142203 ",.01)
 ;;23490142203
 ;;9002226.02101,"791,23490142203 ",.02)
 ;;23490142203
 ;;9002226.02101,"791,23490142206 ",.01)
 ;;23490142206
 ;;9002226.02101,"791,23490142206 ",.02)
 ;;23490142206
 ;;9002226.02101,"791,23490142209 ",.01)
 ;;23490142209
 ;;9002226.02101,"791,23490142209 ",.02)
 ;;23490142209
 ;;9002226.02101,"791,23490647801 ",.01)
 ;;23490647801
 ;;9002226.02101,"791,23490647801 ",.02)
 ;;23490647801
 ;;9002226.02101,"791,23490647802 ",.01)
 ;;23490647802
 ;;9002226.02101,"791,23490647802 ",.02)
 ;;23490647802
 ;;9002226.02101,"791,23490647803 ",.01)
 ;;23490647803
 ;;9002226.02101,"791,23490647803 ",.02)
 ;;23490647803
 ;;9002226.02101,"791,23490648001 ",.01)
 ;;23490648001
 ;;9002226.02101,"791,23490648001 ",.02)
 ;;23490648001
 ;;9002226.02101,"791,23490648002 ",.01)
 ;;23490648002
 ;;9002226.02101,"791,23490648002 ",.02)
 ;;23490648002
 ;;9002226.02101,"791,23490648003 ",.01)
 ;;23490648003
 ;;9002226.02101,"791,23490648003 ",.02)
 ;;23490648003
 ;;9002226.02101,"791,23490648101 ",.01)
 ;;23490648101
 ;;9002226.02101,"791,23490648101 ",.02)
 ;;23490648101
 ;;9002226.02101,"791,23490648102 ",.01)
 ;;23490648102
 ;;9002226.02101,"791,23490648102 ",.02)
 ;;23490648102
 ;;9002226.02101,"791,23490648103 ",.01)
 ;;23490648103
 ;;9002226.02101,"791,23490648103 ",.02)
 ;;23490648103
 ;;9002226.02101,"791,23490648201 ",.01)
 ;;23490648201
 ;;9002226.02101,"791,23490648201 ",.02)
 ;;23490648201
 ;;9002226.02101,"791,23490648202 ",.01)
 ;;23490648202
 ;;9002226.02101,"791,23490648202 ",.02)
 ;;23490648202
 ;;9002226.02101,"791,23490648203 ",.01)
 ;;23490648203
 ;;9002226.02101,"791,23490648203 ",.02)
 ;;23490648203
 ;;9002226.02101,"791,23490648301 ",.01)
 ;;23490648301
 ;;9002226.02101,"791,23490648301 ",.02)
 ;;23490648301
 ;;9002226.02101,"791,23490648302 ",.01)
 ;;23490648302
 ;;9002226.02101,"791,23490648302 ",.02)
 ;;23490648302
 ;;9002226.02101,"791,23490648303 ",.01)
 ;;23490648303
 ;;9002226.02101,"791,23490648303 ",.02)
 ;;23490648303
 ;;9002226.02101,"791,24236022802 ",.01)
 ;;24236022802
