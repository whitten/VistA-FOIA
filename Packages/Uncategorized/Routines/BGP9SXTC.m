BGP9SXTC ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON MAR 25, 2009 ;
 ;;9.0;IHS CLINICAL REPORTING;;JUL 1, 2009
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;21,"58016-0142-20 ")
 ;;304
 ;;21,"58016-0142-24 ")
 ;;305
 ;;21,"58016-0142-28 ")
 ;;306
 ;;21,"58016-0142-30 ")
 ;;307
 ;;21,"58016-0142-40 ")
 ;;308
 ;;21,"58016-0886-00 ")
 ;;309
 ;;21,"58016-0886-20 ")
 ;;310
 ;;21,"58016-0886-30 ")
 ;;311
 ;;21,"58016-0886-60 ")
 ;;312
 ;;21,"58016-0886-90 ")
 ;;313
 ;;21,"58016-0937-00 ")
 ;;314
 ;;21,"58016-0967-00 ")
 ;;315
 ;;21,"58016-0967-30 ")
 ;;316
 ;;21,"58016-0967-60 ")
 ;;317
 ;;21,"58016-0967-90 ")
 ;;318
 ;;21,"58864-0323-14 ")
 ;;319
 ;;21,"59630-0450-16 ")
 ;;320
 ;;21,"61392-0168-30 ")
 ;;321
 ;;21,"61392-0168-31 ")
 ;;322
 ;;21,"61392-0168-32 ")
 ;;323
 ;;21,"61392-0168-39 ")
 ;;324
 ;;21,"61392-0168-45 ")
 ;;325
 ;;21,"61392-0168-51 ")
 ;;326
 ;;21,"61392-0168-54 ")
 ;;327
 ;;21,"61392-0168-60 ")
 ;;328
 ;;21,"61392-0168-90 ")
 ;;329
 ;;21,"61392-0168-91 ")
 ;;330
 ;;21,"61392-0169-30 ")
 ;;331
 ;;21,"61392-0169-31 ")
 ;;332
 ;;21,"61392-0169-32 ")
 ;;333
 ;;21,"61392-0169-39 ")
 ;;334
 ;;21,"61392-0169-45 ")
 ;;335
 ;;21,"61392-0169-51 ")
 ;;336
 ;;21,"61392-0169-54 ")
 ;;337
 ;;21,"61392-0169-60 ")
 ;;338
 ;;21,"61392-0169-90 ")
 ;;339
 ;;21,"61392-0169-91 ")
 ;;340
 ;;21,"62584-0808-01 ")
 ;;341
 ;;21,"63304-0518-01 ")
 ;;342
 ;;21,"64727-3299-01 ")
 ;;343
 ;;21,"64727-3300-01 ")
 ;;344
 ;;21,"64727-3308-01 ")
 ;;345
 ;;21,"64727-3312-01 ")
 ;;346
 ;;21,"64727-7070-01 ")
 ;;347
 ;;21,"64727-7070-02 ")
 ;;348
 ;;21,"64727-7073-02 ")
 ;;349
 ;;21,"64727-7075-08 ")
 ;;350
 ;;21,"64727-7080-01 ")
 ;;351
 ;;21,"64727-7080-02 ")
 ;;352
 ;;21,"64727-7087-01 ")
 ;;353
 ;;21,"64727-7087-02 ")
 ;;354
 ;;21,"68115-0220-14 ")
 ;;355
 ;;21,"68115-0258-28 ")
 ;;356
 ;;21,"68115-0666-00 ")
 ;;357
 ;;9002226,720,.01)
 ;;BGP HEDIS OTHER NDC AVOID ELD
 ;;9002226,720,.02)
 ;;@
 ;;9002226,720,.04)
 ;;n
 ;;9002226,720,.06)
 ;;@
 ;;9002226,720,.08)
 ;;@
 ;;9002226,720,.09)
 ;;@
 ;;9002226,720,.11)
 ;;@
 ;;9002226,720,.12)
 ;;@
 ;;9002226,720,.13)
 ;;1
 ;;9002226,720,.14)
 ;;@
 ;;9002226,720,.15)
 ;;@
 ;;9002226,720,.16)
 ;;@
 ;;9002226,720,.17)
 ;;@
 ;;9002226,720,3101)
 ;;@
 ;;9002226.02101,"720,00076-0301-03 ",.01)
 ;;00076-0301-03
 ;;9002226.02101,"720,00076-0301-03 ",.02)
 ;;00076-0301-03
 ;;9002226.02101,"720,00076-0301-04 ",.01)
 ;;00076-0301-04
 ;;9002226.02101,"720,00076-0301-04 ",.02)
 ;;00076-0301-04
 ;;9002226.02101,"720,00115-3982-01 ",.01)
 ;;00115-3982-01
 ;;9002226.02101,"720,00115-3982-01 ",.02)
 ;;00115-3982-01
 ;;9002226.02101,"720,00115-3982-02 ",.01)
 ;;00115-3982-02
 ;;9002226.02101,"720,00115-3982-02 ",.02)
 ;;00115-3982-02
 ;;9002226.02101,"720,00115-3982-03 ",.01)
 ;;00115-3982-03
 ;;9002226.02101,"720,00115-3982-03 ",.02)
 ;;00115-3982-03
 ;;9002226.02101,"720,00115-3984-01 ",.01)
 ;;00115-3984-01
 ;;9002226.02101,"720,00115-3984-01 ",.02)
 ;;00115-3984-01
 ;;9002226.02101,"720,00115-3984-02 ",.01)
 ;;00115-3984-02
 ;;9002226.02101,"720,00115-3984-02 ",.02)
 ;;00115-3984-02
 ;;9002226.02101,"720,00115-3984-03 ",.01)
 ;;00115-3984-03
 ;;9002226.02101,"720,00115-3984-03 ",.02)
 ;;00115-3984-03
 ;;9002226.02101,"720,00115-3986-01 ",.01)
 ;;00115-3986-01
 ;;9002226.02101,"720,00115-3986-01 ",.02)
 ;;00115-3986-01
 ;;9002226.02101,"720,00115-3986-02 ",.01)
 ;;00115-3986-02
 ;;9002226.02101,"720,00115-3986-02 ",.02)
 ;;00115-3986-02
 ;;9002226.02101,"720,00115-3986-03 ",.01)
 ;;00115-3986-03
 ;;9002226.02101,"720,00115-3986-03 ",.02)
 ;;00115-3986-03
 ;;9002226.02101,"720,00115-4824-01 ",.01)
 ;;00115-4824-01
 ;;9002226.02101,"720,00115-4824-01 ",.02)
 ;;00115-4824-01
 ;;9002226.02101,"720,00115-4824-03 ",.01)
 ;;00115-4824-03
 ;;9002226.02101,"720,00115-4824-03 ",.02)
 ;;00115-4824-03
 ;;9002226.02101,"720,00115-4826-01 ",.01)
 ;;00115-4826-01
 ;;9002226.02101,"720,00115-4826-01 ",.02)
 ;;00115-4826-01
 ;;9002226.02101,"720,00115-4826-03 ",.01)
 ;;00115-4826-03
 ;;9002226.02101,"720,00115-4826-03 ",.02)
 ;;00115-4826-03
 ;;9002226.02101,"720,00115-7037-01 ",.01)
 ;;00115-7037-01
 ;;9002226.02101,"720,00115-7037-01 ",.02)
 ;;00115-7037-01
 ;;9002226.02101,"720,00115-7038-01 ",.01)
 ;;00115-7038-01
 ;;9002226.02101,"720,00115-7038-01 ",.02)
 ;;00115-7038-01
 ;;9002226.02101,"720,00149-0007-05 ",.01)
 ;;00149-0007-05
 ;;9002226.02101,"720,00149-0007-05 ",.02)
 ;;00149-0007-05
 ;;9002226.02101,"720,00149-0008-05 ",.01)
 ;;00149-0008-05
 ;;9002226.02101,"720,00149-0008-05 ",.02)
 ;;00149-0008-05
 ;;9002226.02101,"720,00149-0008-66 ",.01)
 ;;00149-0008-66
 ;;9002226.02101,"720,00149-0008-66 ",.02)
 ;;00149-0008-66
 ;;9002226.02101,"720,00149-0008-67 ",.01)
 ;;00149-0008-67
 ;;9002226.02101,"720,00149-0008-67 ",.02)
 ;;00149-0008-67
 ;;9002226.02101,"720,00149-0008-77 ",.01)
 ;;00149-0008-77
 ;;9002226.02101,"720,00149-0008-77 ",.02)
 ;;00149-0008-77
 ;;9002226.02101,"720,00149-0009-05 ",.01)
 ;;00149-0009-05
 ;;9002226.02101,"720,00149-0009-05 ",.02)
 ;;00149-0009-05
 ;;9002226.02101,"720,00149-0009-66 ",.01)
 ;;00149-0009-66
 ;;9002226.02101,"720,00149-0009-66 ",.02)
 ;;00149-0009-66
 ;;9002226.02101,"720,00149-0009-67 ",.01)
 ;;00149-0009-67
