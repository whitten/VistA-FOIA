IBJYL4 ; List Template Exporter ; 04-JAN-1996
 ;;Version 2.0 ; INTEGRATED BILLING ;**39**; 21-MAR-94
 W !,"'IBJT NS VIEW INS CO' List Template..."
 S DA=$O(^SD(409.61,"B","IBJT NS VIEW INS CO",0)),DIK="^SD(409.61," D ^DIK:DA
 K DO,DD S DIC(0)="L",DIC="^SD(409.61,",X="IBJT NS VIEW INS CO" D FILE^DICN S VALM=+Y
 I VALM>0 D
 .S ^SD(409.61,VALM,0)="IBJT NS VIEW INS CO^1^^90^5^17^1^1^Insurance Company^IBJT NS VIEW INS CO MENU^Insurance Company^1^^1"
 .S ^SD(409.61,VALM,1)="^VALM HIDDEN ACTIONS"
 .S ^SD(409.61,VALM,"ARRAY")=" ^TMP(""IBCNSC"",$J)"
 .S ^SD(409.61,VALM,"FNL")="D EXITI^IBJTNA"
 .S ^SD(409.61,VALM,"HDR")="D HDRI^IBJTNA"
 .S ^SD(409.61,VALM,"HLP")="D HELPI^IBJTNA"
 .S ^SD(409.61,VALM,"INIT")="D INITI^IBJTNA"
 .S DA=VALM,DIK="^SD(409.61," D IX1^DIK K DA,DIK
 .W "Filed."
 ;
 W !,"'IBJT PT ELIGIBILITY' List Template..."
 S DA=$O(^SD(409.61,"B","IBJT PT ELIGIBILITY",0)),DIK="^SD(409.61," D ^DIK:DA
 K DO,DD S DIC(0)="L",DIC="^SD(409.61,",X="IBJT PT ELIGIBILITY" D FILE^DICN S VALM=+Y
 I VALM>0 D
 .S ^SD(409.61,VALM,0)="IBJT PT ELIGIBILITY^1^^80^4^17^1^1^Eligibility^IBJT PT ELIGIBILITY MENU^Patient Eligibility^1^^1"
 .S ^SD(409.61,VALM,1)="^VALM HIDDEN ACTIONS"
 .S ^SD(409.61,VALM,"ARRAY")=" ^TMP(""IBJTEA"",$J)"
 .S ^SD(409.61,VALM,"FNL")="D EXIT^IBJTEA"
 .S ^SD(409.61,VALM,"HDR")="D HDR^IBJTEA"
 .S ^SD(409.61,VALM,"HLP")="D HELP^IBJTEA"
 .S ^SD(409.61,VALM,"INIT")="D INIT^IBJTEA"
 .S DA=VALM,DIK="^SD(409.61," D IX1^DIK K DA,DIK
 .W "Filed."
 ;
 K DIC,DIK,VALM,X,DA Q