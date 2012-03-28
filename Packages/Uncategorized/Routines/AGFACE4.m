AGFACE4 ; IHS/ASDS/EFG - FACE SHEET (3RD PARTY (MEDICARE) ELIGIBILITY) ; 
 ;;7.1;PATIENT REGISTRATION;**2,4**;JAN 31, 2007
 ;MOVE LINES SO CAN READ THEM
 W !,$G(AG("-")),!?23,"*** THIRD PARTY ELIGIBILITY ***",!
 G END:'$D(^AUPNMCR(DFN,0))
 W "MEDICARE:",!,"TYPE",?20,"NUMBER",?46,"ELIG. DATE",?60,"ELIG. END DATE"
 ;F AG=1:1 S AG("DRENT")=AG,DIC=9000003.11,DA=DFN,DR=.02 D ^AGDICLK Q:$D(AG("LKERR"))  D L3A
 ;IM25256 OLD CODE ASSUMES CONTIGUOUS RECORDS EXIST
 N IENS,COVTYP
 S AG=0
 F  S AG=$O(^AUPNMCR(DFN,11,AG)) Q:'AG  D
 .S IENS=AG_","_DFN_","
 .S COVTYP=$$GET1^DIQ(9000003.11,IENS,.03,"I")
 .I COVTYP="D" W !,COVTYP,?20,$$GET1^DIQ(9000003.11,IENS,.06,"E")
 .E  W !,COVTYP,?20,$$GET1^DIQ(9000003,DFN_",",.03,"E")
 .;
 .I COVTYP'="D" W $$GET1^DIQ(9000003,DFN_",",.04,"E")  ;SUFFIX
 .W ?46,$$GET1^DIQ(9000003.11,IENS,.01,"E")
 .W ?60,$$GET1^DIQ(9000003.11,IENS,.02,"E")
 ;END IM25256
 S DIC=9000003,DR=2101,DA=DFN D ^AGDICLK
 I $D(AG("LKPRINT")),AG("LKPRINT")]"" W !?3,"NAME: ",AG("LKPRINT")
 S DR=2102 D ^AGDICLK I $D(AG("LKPRINT")),AG("LKPRINT")]""
 ;W !?3,"DATE OF BIRTH: ",AG("LKPRINT")
 W !?3,"DATE OF BIRTH: ",$G(AG("LKPRINT"))  ;AG*71.*2 IM21507
END Q
 ;CODE BELOW MADE OBSOLETE BY IM25256 FIX ABOVE - REMOVE AT NEXT VERSION
L3A S AG("DRENT")=AG,DR=.03 D ^AGDICLK Q:$D(AG("LKERR"))
 ;W !,AG("LKPRINT"),?20,$P(^AUPNMCR(DFN,0),U,3) ;AG*7.1*2 IM20222 IHS/SD/TPF 3/27/2006
 I $P($G(^AUPNMCR(DFN,11,AG,0)),U,3)="D" W !,AG("LKPRINT"),?20,$P($G(^AUPNMCR(DFN,11,AG,0)),U,6)
 E  W !,AG("LKPRINT"),?20,$P($G(^AUPNMCR(DFN,0)),U,3)
 ;END IM20222
 ;S DIC=9000003,DR=.04,DA=DFN D ^AGDICLK Q:$D(AG("LKERR"))  W AG("LKPRINT")
 I $P($G(^AUPNMCR(DFN,11,AG,0)),U,3)'="D" D
 .S DIC=9000003,DR=.04,DA=DFN D ^AGDICLK Q:$D(AG("LKERR"))  W AG("LKPRINT")  ;AG*7.1*2 IM23259
 S AG("DRENT")=AG,DR=.01,DIC=9000003.11 D ^AGDICLK I '$D(AG("LKERR")) W ?46,AG("LKPRINT")
 S AG("DRENT")=AG,DR=.02,DIC=9000003.11 D ^AGDICLK I '$D(AG("LKERR")) W ?60,AG("LKPRINT")
 Q
