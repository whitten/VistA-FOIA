BGPM5ABM ;IHS/MSC/MMT-CREATED BY ^ATXSTX ON JUL 15, 2011;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;21,"51138008530 ")
 ;;379
 ;;21,"51138047030 ")
 ;;380
 ;;21,"51138047130 ")
 ;;381
 ;;21,"51138047230 ")
 ;;382
 ;;21,"51138047330 ")
 ;;383
 ;;21,"51138047430 ")
 ;;384
 ;;21,"51138047530 ")
 ;;385
 ;;21,"51655004504 ")
 ;;386
 ;;21,"51655004524 ")
 ;;387
 ;;21,"51655004528 ")
 ;;388
 ;;21,"51655004552 ")
 ;;389
 ;;21,"51655004553 ")
 ;;390
 ;;21,"51655029124 ")
 ;;391
 ;;21,"51655029125 ")
 ;;392
 ;;21,"51655029152 ")
 ;;393
 ;;21,"51655029153 ")
 ;;394
 ;;21,"51655040024 ")
 ;;395
 ;;21,"51655040025 ")
 ;;396
 ;;21,"51655040052 ")
 ;;397
 ;;21,"51655040053 ")
 ;;398
 ;;21,"52959020700 ")
 ;;399
 ;;21,"52959020728 ")
 ;;400
 ;;21,"52959020730 ")
 ;;401
 ;;21,"52959020760 ")
 ;;402
 ;;21,"52959086002 ")
 ;;403
 ;;21,"52959086030 ")
 ;;404
 ;;21,"52959086060 ")
 ;;405
 ;;21,"52959086090 ")
 ;;406
 ;;21,"52959089601 ")
 ;;407
 ;;21,"52959089660 ")
 ;;408
 ;;21,"53489046701 ")
 ;;409
 ;;21,"53489046703 ")
 ;;410
 ;;21,"53489046705 ")
 ;;411
 ;;21,"53489046710 ")
 ;;412
 ;;21,"53489046788 ")
 ;;413
 ;;21,"53489046801 ")
 ;;414
 ;;21,"53489046803 ")
 ;;415
 ;;21,"53489046805 ")
 ;;416
 ;;21,"53489046810 ")
 ;;417
 ;;21,"53489046888 ")
 ;;418
 ;;21,"53489046901 ")
 ;;419
 ;;21,"53489046903 ")
 ;;420
 ;;21,"53489046905 ")
 ;;421
 ;;21,"53489046910 ")
 ;;422
 ;;21,"53489046988 ")
 ;;423
 ;;21,"53489058501 ")
 ;;424
 ;;21,"53489058503 ")
 ;;425
 ;;21,"53489058505 ")
 ;;426
 ;;21,"53489058506 ")
 ;;427
 ;;21,"53489058507 ")
 ;;428
 ;;21,"53489058510 ")
 ;;429
 ;;21,"53746017801 ")
 ;;430
 ;;21,"53746017805 ")
 ;;431
 ;;21,"53746017810 ")
 ;;432
 ;;21,"53746017890 ")
 ;;433
 ;;21,"53746017901 ")
 ;;434
 ;;21,"53746017905 ")
 ;;435
 ;;21,"53746017910 ")
 ;;436
 ;;21,"53808018301 ")
 ;;437
 ;;21,"53808018401 ")
 ;;438
 ;;21,"53808018501 ")
 ;;439
 ;;21,"53808038101 ")
 ;;440
 ;;21,"53808045901 ")
 ;;441
 ;;21,"53808061301 ")
 ;;442
 ;;21,"53808071201 ")
 ;;443
 ;;21,"53808071301 ")
 ;;444
 ;;21,"53808071701 ")
 ;;445
 ;;21,"53808071801 ")
 ;;446
 ;;21,"53808072001 ")
 ;;447
 ;;21,"54569535300 ")
 ;;448
 ;;21,"54569535302 ")
 ;;449
 ;;21,"54569535303 ")
 ;;450
 ;;21,"54569536000 ")
 ;;451
 ;;21,"54569536003 ")
 ;;452
 ;;21,"54569537300 ")
 ;;453
 ;;21,"54569537302 ")
 ;;454
 ;;21,"54569554600 ")
 ;;455
 ;;21,"54569554601 ")
 ;;456
 ;;21,"54569554602 ")
 ;;457
 ;;21,"54569561800 ")
 ;;458
 ;;21,"54569561801 ")
 ;;459
 ;;21,"54569561900 ")
 ;;460
 ;;21,"54569561901 ")
 ;;461
 ;;21,"54569599100 ")
 ;;462
 ;;21,"54569599200 ")
 ;;463
 ;;21,"54569599300 ")
 ;;464
 ;;21,"54569599301 ")
 ;;465
 ;;21,"54868079500 ")
 ;;466
 ;;21,"54868456100 ")
 ;;467
 ;;21,"54868456101 ")
 ;;468
 ;;21,"54868456102 ")
 ;;469
 ;;21,"54868456103 ")
 ;;470
 ;;21,"54868456104 ")
 ;;471
 ;;21,"54868456400 ")
 ;;472
 ;;21,"54868456401 ")
 ;;473
 ;;21,"54868456402 ")
 ;;474
 ;;21,"54868456403 ")
 ;;475
 ;;21,"54868456404 ")
 ;;476
 ;;21,"54868456405 ")
 ;;477
 ;;21,"54868456600 ")
 ;;478
 ;;21,"54868456601 ")
 ;;479
 ;;21,"54868456602 ")
 ;;480
 ;;21,"54868456603 ")
 ;;481
 ;;21,"54868456604 ")
 ;;482
 ;;21,"54868514800 ")
 ;;483
 ;;21,"54868514801 ")
 ;;484
 ;;21,"54868514802 ")
 ;;485
 ;;21,"54868514803 ")
 ;;486
 ;;21,"54868514804 ")
 ;;487
 ;;21,"54868518500 ")
 ;;488
 ;;21,"54868518501 ")
 ;;489
 ;;21,"54868518502 ")
 ;;490
 ;;21,"54868518503 ")
 ;;491
 ;;21,"54868518800 ")
 ;;492
 ;;21,"54868518801 ")
 ;;493
 ;;21,"54868518802 ")
 ;;494
 ;;21,"54868521700 ")
 ;;495
 ;;21,"54868521701 ")
 ;;496
 ;;21,"54868521702 ")
 ;;497
 ;;21,"54868521703 ")
 ;;498
 ;;21,"54868521704 ")
 ;;499
 ;;21,"54868521705 ")
 ;;500
 ;;21,"54868524300 ")
 ;;501
 ;;21,"54868524301 ")
 ;;502
 ;;21,"54868524302 ")
 ;;503
 ;;21,"54868524303 ")
 ;;504
 ;;21,"54868524304 ")
 ;;505
 ;;21,"54868546700 ")
 ;;506
 ;;21,"54868546701 ")
 ;;507
 ;;21,"54868546702 ")
 ;;508
 ;;21,"54868550500 ")
 ;;509
 ;;21,"54868550501 ")
 ;;510
 ;;21,"54868550502 ")
 ;;511
 ;;21,"54907076701 ")
 ;;512
 ;;21,"54907086001 ")
 ;;513
 ;;21,"54907086003 ")
 ;;514
 ;;21,"54907086004 ")
 ;;515
 ;;21,"54907086005 ")
 ;;516
 ;;21,"55045290400 ")
 ;;517
 ;;21,"55045290402 ")
 ;;518
 ;;21,"55045290409 ")
 ;;519
 ;;21,"55045290500 ")
 ;;520
 ;;21,"55045290503 ")
 ;;521
 ;;21,"55045290506 ")
 ;;522
 ;;21,"55045290508 ")
 ;;523
 ;;21,"55045290600 ")
 ;;524
 ;;21,"55045290601 ")
 ;;525
 ;;21,"55045290602 ")
 ;;526
 ;;21,"55045290606 ")
 ;;527
 ;;21,"55045290609 ")
 ;;528
 ;;21,"55045328701 ")
 ;;529
 ;;21,"55111042901 ")
 ;;530
 ;;21,"55111042905 ")
 ;;531
 ;;21,"55111042910 ")
 ;;532
 ;;21,"55111042930 ")
 ;;533
 ;;21,"55111042960 ")
 ;;534
 ;;21,"55111042978 ")
 ;;535
 ;;21,"55111043001 ")
 ;;536
 ;;21,"55111043005 ")
 ;;537
 ;;21,"55111043030 ")
 ;;538
 ;;21,"55111043060 ")
 ;;539
 ;;21,"55111043078 ")
 ;;540
 ;;21,"55111043101 ")
 ;;541
 ;;21,"55111043105 ")
 ;;542
 ;;21,"55111043130 ")
 ;;543
 ;;21,"55111043160 ")
 ;;544
 ;;21,"55111043178 ")
 ;;545
 ;;21,"55111069501 ")
 ;;546
 ;;21,"55111069505 ")
 ;;547
 ;;21,"55111069510 ")
 ;;548
 ;;21,"55111069601 ")
 ;;549
 ;;21,"55111069605 ")
 ;;550
 ;;21,"55111069610 ")
 ;;551
 ;;21,"55111069701 ")
 ;;552
 ;;21,"55111069705 ")
 ;;553
 ;;21,"55111069710 ")
 ;;554
 ;;21,"55154167509 ")
 ;;555
 ;;21,"55154167609 ")
 ;;556
 ;;21,"55154205800 ")
 ;;557
 ;;21,"55154343709 ")
 ;;558
 ;;21,"55154375001 ")
 ;;559
 ;;21,"55154456103 ")
 ;;560
 ;;21,"55154542209 ")
 ;;561
 ;;21,"55154548700 ")
 ;;562
 ;;21,"55154548707 ")
 ;;563
 ;;21,"55154585003 ")
 ;;564
 ;;21,"55154665703 ")
 ;;565
 ;;21,"55160014301 ")
 ;;566
 ;;21,"55160014305 ")
 ;;567
 ;;21,"55160014401 ")
 ;;568
 ;;21,"55160014501 ")
 ;;569
 ;;21,"55289028130 ")
 ;;570
 ;;21,"55289028160 ")
 ;;571
 ;;21,"55289028186 ")
 ;;572
 ;;21,"55289028190 ")
 ;;573
 ;;21,"55289038430 ")
 ;;574
 ;;21,"55289038460 ")
 ;;575
 ;;21,"55289038486 ")
 ;;576
 ;;21,"55289038490 ")
 ;;577
 ;;21,"55289038493 ")
 ;;578
 ;;21,"55289038494 ")
 ;;579
 ;;21,"55289061514 ")
 ;;580
 ;;21,"55289061530 ")
 ;;581
 ;;21,"55289061560 ")
 ;;582
 ;;21,"55289061586 ")
 ;;583
 ;;21,"55289061590 ")
 ;;584
 ;;21,"55289061593 ")
 ;;585
 ;;21,"55289061594 ")
 ;;586
 ;;21,"55289061598 ")
 ;;587
 ;;21,"55289091930 ")
 ;;588
 ;;21,"55289091960 ")
 ;;589
 ;;21,"55289091990 ")
 ;;590
 ;;21,"55289091993 ")
 ;;591
 ;;21,"55289091994 ")
 ;;592
 ;;21,"55289091998 ")
 ;;593
 ;;21,"55289093430 ")
 ;;594
 ;;21,"55289093460 ")
 ;;595
 ;;21,"55289093493 ")
 ;;596
 ;;21,"55289093494 ")
 ;;597
 ;;21,"55289093498 ")
 ;;598
 ;;21,"55370075607 ")
 ;;599
 ;;21,"55370075609 ")
 ;;600
 ;;21,"55370075707 ")
 ;;601
 ;;21,"55370075709 ")
 ;;602
 ;;21,"55370075907 ")
 ;;603
 ;;21,"55370075909 ")
 ;;604
 ;;21,"55567011300 ")
 ;;605
 ;;21,"55567011318 ")
 ;;606
 ;;21,"55567011325 ")
 ;;607
 ;;21,"55567014400 ")
 ;;608
 ;;21,"55567014418 ")
 ;;609
 ;;21,"55567014425 ")
 ;;610
 ;;21,"55567014500 ")
 ;;611
 ;;21,"55567014518 ")
 ;;612
 ;;21,"55567014525 ")
 ;;613
 ;;21,"55567014600 ")
 ;;614
 ;;21,"55567014618 ")
 ;;615
 ;;21,"55567014625 ")
 ;;616
 ;;21,"55887017330 ")
 ;;617
 ;;21,"55887036830 ")
 ;;618
 ;;21,"55887036860 ")
 ;;619
 ;;21,"55887036890 ")
 ;;620
 ;;21,"55887036896 ")
 ;;621
 ;;21,"55887041482 ")
 ;;622
 ;;21,"55887041492 ")
 ;;623
 ;;21,"55887057101 ")
 ;;624
 ;;21,"55887057130 ")
 ;;625
 ;;21,"55887057160 ")
 ;;626
 ;;21,"55887057182 ")
 ;;627
 ;;21,"55887057186 ")
 ;;628
 ;;21,"55887057190 ")
 ;;629
 ;;21,"55887057192 ")
 ;;630
 ;;21,"55887061430 ")
 ;;631
 ;;21,"55887061490 ")
 ;;632
 ;;21,"55887062701 ")
 ;;633
 ;;21,"55887062730 ")
 ;;634
 ;;21,"55887062760 ")
 ;;635
 ;;21,"55887062782 ")
 ;;636
 ;;21,"55887062790 ")
 ;;637
 ;;21,"55887062792 ")
 ;;638
 ;;21,"55887084530 ")
 ;;639
 ;;21,"55887084560 ")
 ;;640
 ;;21,"55887094030 ")
 ;;641
 ;;21,"55887094060 ")
 ;;642
 ;;21,"55887094090 ")
 ;;643
 ;;21,"57315004701 ")
 ;;644
 ;;21,"57315004703 ")
 ;;645
 ;;21,"57315004704 ")
 ;;646
 ;;21,"57315004801 ")
 ;;647
 ;;21,"57315004803 ")
 ;;648
 ;;21,"57315004804 ")
 ;;649
 ;;21,"57315004805 ")
 ;;650
 ;;21,"57315005001 ")
 ;;651
 ;;21,"57315005002 ")
 ;;652
 ;;21,"57315005003 ")
 ;;653
 ;;21,"57664039713 ")
 ;;654
 ;;21,"57664039718 ")
 ;;655
 ;;21,"57664039751 ")
 ;;656
 ;;21,"57664039753 ")
 ;;657
 ;;21,"57664039758 ")
 ;;658
 ;;21,"57664039759 ")
 ;;659
 ;;21,"57664039788 ")
 ;;660
 ;;21,"57664039799 ")
 ;;661
 ;;21,"57664043513 ")
 ;;662
 ;;21,"57664043518 ")
 ;;663
 ;;21,"57664043551 ")
 ;;664
 ;;21,"57664043553 ")
 ;;665
 ;;21,"57664043558 ")
 ;;666
 ;;21,"57664043559 ")
 ;;667
 ;;21,"57664043588 ")
 ;;668
 ;;21,"57664043599 ")
 ;;669
 ;;21,"57664047413 ")
 ;;670
 ;;21,"57664047418 ")
 ;;671
 ;;21,"57664047451 ")
 ;;672
 ;;21,"57664047453 ")
 ;;673
 ;;21,"57664047458 ")
 ;;674
 ;;21,"57664047459 ")
 ;;675
 ;;21,"57664047488 ")
 ;;676
 ;;21,"57664047499 ")
 ;;677
 ;;21,"57664072413 ")
 ;;678
 ;;21,"57664072418 ")
 ;;679
 ;;21,"57664072488 ")
 ;;680
 ;;21,"57664072513 ")
 ;;681
 ;;21,"57664072518 ")
 ;;682
 ;;21,"57664072588 ")
 ;;683
 ;;21,"57664072713 ")
 ;;684
 ;;21,"57664072718 ")
 ;;685
 ;;21,"57664072788 ")
 ;;686
 ;;21,"57664099813 ")
 ;;687
 ;;21,"57664099818 ")
 ;;688
 ;;21,"57664099888 ")
 ;;689
 ;;21,"57664099899 ")
 ;;690
 ;;21,"57664099913 ")
 ;;691
 ;;21,"57664099918 ")
 ;;692
 ;;21,"57664099988 ")
 ;;693
 ;;21,"57664099999 ")
 ;;694
 ;;21,"57866905401 ")
 ;;695
 ;;21,"57866905402 ")
 ;;696
 ;;21,"57866905403 ")
 ;;697
 ;;21,"57866905404 ")
 ;;698
 ;;21,"57866905501 ")
 ;;699
 ;;21,"57866905502 ")
 ;;700
 ;;21,"57866905503 ")
 ;;701
 ;;21,"57866905504 ")
 ;;702
 ;;21,"57866905505 ")
 ;;703
 ;;21,"57866905601 ")
 ;;704
 ;;21,"57866905602 ")
 ;;705
 ;;21,"57866905603 ")
 ;;706
 ;;21,"57866905604 ")
 ;;707
 ;;21,"57866905606 ")
 ;;708
 ;;21,"58016005800 ")
 ;;709
 ;;21,"58016005801 ")
 ;;710
 ;;21,"58016005802 ")
 ;;711
 ;;21,"58016005803 ")
 ;;712
 ;;21,"58016005804 ")
 ;;713
 ;;21,"58016005805 ")
 ;;714
 ;;21,"58016005830 ")
 ;;715
 ;;21,"58016005860 ")
 ;;716
 ;;21,"58016005890 ")
 ;;717
 ;;21,"58016041100 ")
 ;;718
 ;;21,"58016041102 ")
 ;;719
 ;;21,"58016041130 ")
 ;;720
 ;;21,"58016041160 ")
 ;;721
 ;;21,"58016041190 ")
 ;;722
 ;;21,"58016046600 ")
 ;;723
 ;;21,"58016046601 ")
 ;;724
 ;;21,"58016046602 ")
 ;;725
 ;;21,"58016046603 ")
 ;;726
 ;;21,"58016046604 ")
 ;;727
 ;;21,"58016046605 ")
 ;;728
 ;;21,"58016046606 ")
 ;;729
 ;;21,"58016046607 ")
 ;;730
 ;;21,"58016046608 ")
 ;;731
 ;;21,"58016046609 ")
 ;;732
 ;;21,"58016046610 ")
 ;;733
 ;;21,"58016046612 ")
 ;;734
 ;;21,"58016046614 ")
 ;;735
 ;;21,"58016046615 ")
 ;;736
 ;;21,"58016046616 ")
 ;;737
 ;;21,"58016046618 ")
 ;;738
 ;;21,"58016046620 ")
 ;;739
 ;;21,"58016046621 ")
 ;;740
