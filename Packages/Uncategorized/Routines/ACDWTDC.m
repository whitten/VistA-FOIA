ACDWTDC ;IHS/ADC/EDE/KML - SET LOC VARS FROM ACDTDC FILE;
 ;;4.1;CHEMICAL DEPENDENCY MIS;;MAY 11, 1998
 ;*********************************************************************
 ;//^ACDWDRV2, ^ACDWDRV3, ^ACDWCD2
 ;Needs ACDDA as internal DA to file entry
 ;********************************************************************
 K ACDN0,ACDPROBP,ACDPROBS,ACDDUA,ACDDUD,ACDDTA,ACDPTA,ACDDH,ACDAAR,ACDSUS,ACDPS,ACDES,ACDSS,ACDCS,ACDBS,ACDPLAR,ACDP,ACDPLAA,ACDIF,ACDID,ACDSTAT,ACDHRS
 S ACDN0=^ACDTDC(ACDDA,0)
 S ACDPROBP=$P(ACDN0,U,27) S:'ACDPROBP ACDPROBP="NONE" S ACDPROBP=$S($D(^ACDPROB(ACDPROBP,0)):$P(^(0),U),1:"NONE")
 S ACDPROBS=$P(ACDN0,U,28) S:'ACDPROBS ACDPROBS="NONE" S ACDPROBS=$S($D(^ACDPROB(ACDPROBS,0)):$P(^(0),U),1:"NONE")
 S ACDDUA=$P(ACDN0,U)
 S ACDDUD=$P(ACDN0,U,2)
 I $D(^ACDTDC(ACDDA,2)) F ACDAE=0:0 S ACDAE=$O(^ACDTDC(ACDDA,2,ACDAE)) Q:'ACDAE  I $D(^(ACDAE,0)) S ACDDTP=$P(^(0),U) S:'$D(^ACDDRUG(ACDDTP,0)) ACDDTP="XX" I ACDDTP'="XX" S ACDDTA($P(^ACDDRUG(ACDDTP,0),U,2)_"="_$P(^(0),U))=""
XXX ;
 I $D(^ACDTDC(ACDDA,3)) F ACDAE=0:0 S ACDAE=$O(^ACDTDC(ACDDA,3,ACDAE)) Q:'ACDAE  I $D(^(ACDAE,0)) S ACDPTP=$P(^(0),U) S:'$D(^ACDPROB(ACDPTP,0)) ACDPTP="XX" I ACDPTP'="XX" S ACDPTA($P(^ACDPROB(ACDPTP,0),U,2)_"="_$P(^(0),U))=""
 S ACDDH=$P(ACDN0,U,4)
 S ACDAAR=$P(ACDN0,U,5)
 S ACDSUS=$P(ACDN0,U,7)
 S ACDPS=$P(ACDN0,U,8)
 S ACDES=$P(ACDN0,U,9)
 S ACDSS=$P(ACDN0,U,10)
 S ACDCS=$P(ACDN0,U,11)
 S ACDBS=$P(ACDN0,U,12)
 S ACDBV=$P(ACDN0,U,3)
 S Y=0 F X="ACDSUS","ACDPS","ACDES","ACDSS","ACDCS","ACDBS","ACDBV" S:@X>0 Y=Y+1
 S ACDSAVG=0
 I Y S ACDSAVG=$J(((ACDSUS+ACDPS+ACDES+ACDSS+ACDCS+ACDBS+ACDBV)/Y),3,1)
 S ACDPLAR=$P(ACDN0,U,13) S:'ACDPLAR ACDPLAR="NONE" S ACDPLAR=$S($D(^ACDCOMP(ACDPLAR,0)):$P(^(0),U),1:"NONE")
 S ACDPLAR1=$P(ACDN0,U,14)
 S ACDP(3)=$P(ACDN0,U,14),ACDP(1)=9002171,ACDP(2)=13 S ACDPLARL=$$SETS^ACDFUNC(.ACDP)
 S ACDPLAA=$P(ACDN0,U,15) S:'ACDPLAA ACDPLAA="NONE" S ACDPLAA=$S($D(^ACDCOMP(ACDPLAA,0)):$P(^(0),U),1:"NONE")
 S ACDPLAA1=$P(ACDN0,U,16)
 S ACDP(3)=$P(ACDN0,U,16),ACDP(1)=9002171,ACDP(2)=15 S ACDPLAAL=$$SETS^ACDFUNC(.ACDP)
 S ACDDIF=$P(ACDN0,U,17) S:'ACDDIF ACDDIF="NONE" S ACDDIF=$S($D(^ACDPLEX(ACDDIF,0)):$P(^(0),U),1:"NONE")
 S ACDID=$P(ACDN0,U,18)
 S ACDP(3)=$P(ACDN0,U,19),ACDP(1)=9002171,ACDP(2)=18 S ACDGA=$$SETS^ACDFUNC(.ACDP)
 S ACDP(3)=$P(ACDN0,U,20),ACDP(1)=9002171,ACDP(2)=19 S ACDTDCR=$$SETS^ACDFUNC(.ACDP)
 S ACDP(3)=$P(ACDN0,U,21),ACDP(1)=9002171,ACDP(2)=20 S ACDDAP=$$SETS^ACDFUNC(.ACDP)
 S ACDP(3)=$P(ACDN0,U,26),ACDP(1)=9002171,ACDP(2)=26 S ACDSTAT=$$SETS^ACDFUNC(.ACDP)
 S ACDHRS=$P(ACDN0,U,29)
 S ACDP(3)=$P(ACDN0,U,30),ACDP(1)=9002171,ACDP(2)=30 S ACDTOB=$$SETS^ACDFUNC(.ACDP)
 Q
