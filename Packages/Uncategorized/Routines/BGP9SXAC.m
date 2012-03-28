BGP9SXAC ; IHS/CMI/LAB - AB-CREATED BY ^ATXSTX ON MAR 25, 2009 ;
 ;;9.0;IHS CLINICAL REPORTING;;JUL 1, 2009
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;21,"52959-0426-30 ")
 ;;304
 ;;21,"52959-0440-07 ")
 ;;305
 ;;21,"52959-0440-15 ")
 ;;306
 ;;21,"52959-0440-30 ")
 ;;307
 ;;21,"52959-0628-28 ")
 ;;308
 ;;21,"52959-0628-30 ")
 ;;309
 ;;21,"52959-0812-20 ")
 ;;310
 ;;21,"52959-0812-60 ")
 ;;311
 ;;21,"53014-0530-07 ")
 ;;312
 ;;21,"53014-0530-12 ")
 ;;313
 ;;21,"53014-0531-07 ")
 ;;314
 ;;21,"53014-0531-12 ")
 ;;315
 ;;21,"53014-0532-07 ")
 ;;316
 ;;21,"53014-0532-12 ")
 ;;317
 ;;21,"53014-0574-07 ")
 ;;318
 ;;21,"53014-0575-07 ")
 ;;319
 ;;21,"53014-0575-30 ")
 ;;320
 ;;21,"53014-0575-72 ")
 ;;321
 ;;21,"53014-0576-07 ")
 ;;322
 ;;21,"53014-0579-07 ")
 ;;323
 ;;21,"53014-0580-07 ")
 ;;324
 ;;21,"53014-0581-07 ")
 ;;325
 ;;21,"53014-0582-07 ")
 ;;326
 ;;21,"53014-0583-07 ")
 ;;327
 ;;21,"53014-0584-07 ")
 ;;328
 ;;21,"53014-0593-07 ")
 ;;329
 ;;21,"53014-0594-07 ")
 ;;330
 ;;21,"53014-0903-71 ")
 ;;331
 ;;21,"53014-0903-84 ")
 ;;332
 ;;21,"53014-0904-71 ")
 ;;333
 ;;21,"53014-0904-84 ")
 ;;334
 ;;21,"53265-0253-10 ")
 ;;335
 ;;21,"53265-0253-11 ")
 ;;336
 ;;21,"53265-0254-10 ")
 ;;337
 ;;21,"53265-0254-11 ")
 ;;338
 ;;21,"53265-0255-10 ")
 ;;339
 ;;21,"53265-0257-10 ")
 ;;340
 ;;21,"53265-0257-11 ")
 ;;341
 ;;21,"53265-0257-40 ")
 ;;342
 ;;21,"53265-0258-10 ")
 ;;343
 ;;21,"53265-0258-11 ")
 ;;344
 ;;21,"53265-0259-10 ")
 ;;345
 ;;21,"53265-0262-10 ")
 ;;346
 ;;21,"53265-0346-10 ")
 ;;347
 ;;21,"53265-0346-11 ")
 ;;348
 ;;21,"53265-0396-10 ")
 ;;349
 ;;21,"53489-0406-01 ")
 ;;350
 ;;21,"53489-0406-10 ")
 ;;351
 ;;21,"53489-0433-01 ")
 ;;352
 ;;21,"53489-0433-10 ")
 ;;353
 ;;21,"53489-0564-01 ")
 ;;354
 ;;21,"53489-0565-01 ")
 ;;355
 ;;21,"53489-0566-01 ")
 ;;356
 ;;21,"53489-0567-01 ")
 ;;357
 ;;21,"53489-0568-01 ")
 ;;358
 ;;21,"53489-0569-01 ")
 ;;359
 ;;21,"53489-0570-01 ")
 ;;360
 ;;21,"54092-0371-01 ")
 ;;361
 ;;21,"54092-0372-01 ")
 ;;362
 ;;21,"54092-0373-01 ")
 ;;363
 ;;21,"54092-0374-01 ")
 ;;364
 ;;21,"54092-0375-01 ")
 ;;365
 ;;21,"54092-0376-01 ")
 ;;366
 ;;21,"54092-0377-01 ")
 ;;367
 ;;21,"54092-0381-01 ")
 ;;368
 ;;21,"54092-0383-01 ")
 ;;369
 ;;21,"54092-0385-01 ")
 ;;370
 ;;21,"54092-0387-01 ")
 ;;371
 ;;21,"54092-0389-01 ")
 ;;372
 ;;21,"54092-0391-01 ")
 ;;373
 ;;21,"54092-0448-01 ")
 ;;374
 ;;21,"54092-0452-01 ")
 ;;375
 ;;21,"54092-0552-10 ")
 ;;376
 ;;21,"54092-0552-30 ")
 ;;377
 ;;21,"54092-0553-10 ")
 ;;378
 ;;21,"54092-0553-30 ")
 ;;379
 ;;21,"54092-0554-10 ")
 ;;380
 ;;21,"54092-0554-30 ")
 ;;381
 ;;21,"54092-0555-10 ")
 ;;382
 ;;21,"54092-0555-30 ")
 ;;383
 ;;21,"54569-1718-00 ")
 ;;384
 ;;21,"54569-1718-01 ")
 ;;385
 ;;21,"54569-1718-03 ")
 ;;386
 ;;21,"54569-1718-04 ")
 ;;387
 ;;21,"54569-1718-05 ")
 ;;388
 ;;21,"54569-2198-00 ")
 ;;389
 ;;21,"54569-2198-01 ")
 ;;390
 ;;21,"54569-2198-02 ")
 ;;391
 ;;21,"54569-2669-00 ")
 ;;392
 ;;21,"54569-2669-01 ")
 ;;393
 ;;21,"54569-2669-02 ")
 ;;394
 ;;21,"54569-2669-03 ")
 ;;395
 ;;21,"54868-0479-01 ")
 ;;396
 ;;21,"54868-3674-00 ")
 ;;397
 ;;21,"54868-3976-00 ")
 ;;398
 ;;21,"55289-0379-30 ")
 ;;399
 ;;21,"55499-1082-01 ")
 ;;400
 ;;21,"57844-0009-01 ")
 ;;401
 ;;21,"57844-0009-10 ")
 ;;402
 ;;21,"57844-0019-01 ")
 ;;403
 ;;21,"57866-9029-01 ")
 ;;404
 ;;21,"57866-9030-01 ")
 ;;405
 ;;21,"58016-0043-00 ")
 ;;406
 ;;21,"58016-0043-30 ")
 ;;407
 ;;21,"58016-0043-60 ")
 ;;408
 ;;21,"58016-0043-90 ")
 ;;409
 ;;21,"58016-0310-00 ")
 ;;410
 ;;21,"58016-0310-30 ")
 ;;411
 ;;21,"58016-0310-60 ")
 ;;412
 ;;21,"58016-0310-90 ")
 ;;413
 ;;21,"58016-0836-07 ")
 ;;414
 ;;21,"58016-0836-14 ")
 ;;415
 ;;21,"58016-0836-15 ")
 ;;416
 ;;21,"58016-0836-30 ")
 ;;417
 ;;21,"58016-0854-42 ")
 ;;418
 ;;21,"58016-0856-00 ")
 ;;419
 ;;21,"58016-0856-15 ")
 ;;420
 ;;21,"58016-0856-21 ")
 ;;421
 ;;21,"58016-0856-42 ")
 ;;422
 ;;21,"58016-0861-14 ")
 ;;423
 ;;21,"58016-0861-30 ")
 ;;424
 ;;21,"58016-0861-60 ")
 ;;425
 ;;21,"58016-0868-30 ")
 ;;426
 ;;21,"58016-0868-60 ")
 ;;427
 ;;21,"58016-0868-90 ")
 ;;428
 ;;21,"58177-0311-04 ")
 ;;429
 ;;21,"58177-0312-04 ")
 ;;430
 ;;21,"58521-0031-01 ")
 ;;431
 ;;21,"58521-0032-01 ")
 ;;432
 ;;21,"58521-0033-01 ")
 ;;433
 ;;21,"58521-0034-01 ")
 ;;434
 ;;21,"58521-0075-01 ")
 ;;435
 ;;21,"58521-0125-01 ")
 ;;436
 ;;21,"58521-0150-01 ")
 ;;437
 ;;21,"58521-0333-05 ")
 ;;438
 ;;21,"58521-0451-01 ")
 ;;439
 ;;21,"58521-0452-01 ")
 ;;440
 ;;21,"58605-0503-01 ")
 ;;441
 ;;21,"58605-0504-01 ")
 ;;442
 ;;21,"58605-0508-01 ")
 ;;443
 ;;21,"59772-8840-01 ")
 ;;444
 ;;21,"59772-8840-03 ")
 ;;445
 ;;21,"59772-8841-01 ")
 ;;446
 ;;21,"59772-8841-03 ")
 ;;447
 ;;21,"59772-8842-01 ")
 ;;448
 ;;21,"59772-8842-03 ")
 ;;449
 ;;21,"59772-8843-01 ")
 ;;450
 ;;21,"60793-0009-01 ")
 ;;451
 ;;21,"62269-0391-24 ")
 ;;452
 ;;21,"62269-0392-24 ")
 ;;453
 ;;21,"62269-0393-24 ")
 ;;454
 ;;21,"63304-0908-01 ")
 ;;455
 ;;21,"63304-0909-01 ")
 ;;456
