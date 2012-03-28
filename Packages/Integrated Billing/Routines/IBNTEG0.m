IBNTEG0	;ISC/XTSUMBLD KERNEL - Package checksum checker ;MAR 21, 1994@00:43:44
	;;Version 2.0 ; INTEGRATED BILLING ;; 21-MAR-94
	;;7.2;MAR 21, 1994@00:43:44
	S XT4="I 1",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
CONT	F XT1=1:1 S XT2=$T(ROU+XT1) Q:XT2=""  S X=$P(XT2," ",1),XT3=$P(XT2,";",3) X XT4 I $T W !,X X ^%ZOSF("TEST") S:'$T XT3=0 X:XT3 ^%ZOSF("RSUM") W ?10,$S('XT3:"Routine not in UCI",XT3'=Y:"Calculated "_$C(7)_Y_", off by "_(Y-XT3),1:"ok")
	G CONT^IBNTEG01
	K %1,%2,%3,X,Y,XT1,XT2,XT3,XT4 Q
ONE	S XT4="I $D(^UTILITY($J,X))",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
	W !,"Check a subset of routines:" K ^UTILITY($J) X ^%ZOSF("RSEL")
	W ! G CONT
ROU	;;
IBCNQ1	;;8213160
IBCNS	;;5047234
IBCNS1	;;5577010
IBCNS2	;;3745315
IBCNSA	;;6396949
IBCNSA0	;;3165953
IBCNSA1	;;11651972
IBCNSA2	;;2611849
IBCNSBL	;;2445320
IBCNSBL1	;;8197763
IBCNSC	;;7465999
IBCNSC0	;;6126055
IBCNSC01	;;7789069
IBCNSC1	;;3850831
IBCNSC2	;;8154718
IBCNSC3	;;6631804
IBCNSD	;;6629671
IBCNSD1	;;1158146
IBCNSEH	;;4917879
IBCNSEVT	;;393303
IBCNSM	;;5264394
IBCNSM1	;;5083086
IBCNSM2	;;4308601
IBCNSM3	;;11770601
IBCNSM31	;;5120018
IBCNSM4	;;1683566
IBCNSM5	;;6602815
IBCNSM6	;;5555330
IBCNSM7	;;8914463
IBCNSM8	;;8047222
IBCNSM9	;;11256347
IBCNSOK	;;10324180
IBCNSOK1	;;6457752
IBCNSP	;;8601571
IBCNSP0	;;5809789
IBCNSP01	;;8590441
IBCNSP02	;;8471424
IBCNSP1	;;4868050
IBCNSP2	;;8141199
IBCNSP3	;;6580126
IBCNSU	;;3908800
IBCNSU1	;;2310797
IBCNSV	;;640320
IBCOC	;;950251
IBCOC1	;;892863
IBCONS1	;;7413645
IBCONS2	;;11154246
IBCONS3	;;3101025
IBCONSC	;;9041021
IBCOPV	;;9900610
IBCOPV1	;;9480865
IBCOPV2	;;7533725
IBCORC	;;9398632
IBCORC1	;;5338099
IBCORC2	;;3690596
IBCRTN	;;4948111
IBCSC1	;;5176501
IBCSC2	;;5200799
IBCSC3	;;12494861
IBCSC4	;;9729300
IBCSC4A	;;14651904
IBCSC4B	;;16543108
IBCSC4C	;;11565822
IBCSC4D	;;9469474
IBCSC4E	;;9915642
IBCSC5	;;8009258
IBCSC5A	;;12648510
IBCSC5B	;;9285889
IBCSC5C	;;8047246
IBCSC6	;;6853801
IBCSC61	;;2995269
IBCSC7	;;4127768
IBCSC8	;;2681062
IBCSC82	;;4196265
IBCSC8H	;;1582522
IBCSCE	;;6334542
IBCSCE1	;;6140000
IBCSCH	;;15072243
IBCSCH1	;;9538370
IBCSCP	;;5142249
IBCSCU	;;6000762
IBCU	;;8741634
IBCU1	;;5276295
IBCU2	;;6391688
IBCU3	;;6929850
IBCU4	;;7789095
IBCU41	;;5944776
IBCU5	;;5504494
IBCU6	;;8585767
IBCU61	;;3598334
IBCU62	;;6633813
IBCU63	;;6510058
IBCU64	;;6703156
IBCU7	;;16823266
IBCU71	;;3477243
IBCU8	;;4608945
IBCU81	;;3744610
IBCU82	;;11651075
IBCVA	;;3230375
IBCVA0	;;10422773
IBCVA1	;;9853216
IBDE	;;3003311
IBDE1	;;6677694
IBDE1A	;;3361379
IBDE1B	;;1339223
IBDE2	;;383336
IBDE3	;;9522902
IBDEHELP	;;7829287
IBDEI001	;;5914267
IBDEI002	;;3199545
IBDEI003	;;3723952
IBDEI004	;;6281872
IBDEI005	;;6938907
IBDEI006	;;2677613
IBDEI007	;;13070825
IBDEI008	;;14787357
IBDEI009	;;16942755
IBDEI00A	;;1964916
IBDEI00B	;;6821303
IBDEI00C	;;7822738
IBDEI00D	;;8086479
IBDEI00E	;;7766991
IBDEI00F	;;2531852
IBDEI00G	;;3990534
IBDEI00H	;;4023981
IBDEI00I	;;974293
IBDEI00J	;;6810223
IBDEI00K	;;3736068
IBDEI00L	;;3749473
IBDEI00M	;;3784750
IBDEI00N	;;3796078
IBDEI00O	;;2452621
IBDEI00P	;;6331684
IBDEI00Q	;;2106265
IBDEI00R	;;7573266
IBDEI00S	;;7561219
IBDEI00T	;;7123935
IBDEI00U	;;7766530
IBDEI00V	;;878660
IBDEI00W	;;4726931
IBDEI00X	;;4236815
IBDEI00Y	;;4411642
IBDEI00Z	;;3862497
IBDEI010	;;4088410
IBDEI011	;;3897227
IBDEI012	;;3867997
IBDEI013	;;3919512
IBDEI014	;;3960565
IBDEI015	;;3834719
IBDEI016	;;4326482
IBDEI017	;;4364433
IBDEI018	;;3977322
IBDEI019	;;4203924
IBDEI01A	;;3887525
IBDEI01B	;;3920282
IBDEI01C	;;2644559
IBDEI01D	;;8724015
IBDEI01E	;;9940913
IBDEI01F	;;7432564
IBDEI01G	;;6432912
