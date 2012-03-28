BCERENCK ;IHS/SD/TPF - BCER ENVIRONMENT CHECKER FOR EHR
 ;;1.0;IHS;;JUL 5, 2011;Build 18
 ;
 I '$G(DUZ) W !,"DUZ UNDEFINED OR 0." D SORRY(2) Q
 I '$L($G(DUZ(0))) W !,"DUZ(0) UNDEFINED OR NULL." D SORRY(2) Q
 S X=$P(^VA(200,DUZ,0),U)
 W !!,$$CJ^XLFSTR("Hello, "_$P(X,",",2)_" "_$P(X,","),IOM)
 W !!,$$CJ^XLFSTR("Checking Environment for EHR Software",IOM),!
 W !,$$CJ^XLFSTR("At Facility "_$P($G(^DIC(4,DUZ(2),0)),U),IOM),! 
 N BCERQUIT
 S BCERQUIT=0
 ;
 S ENSEMBLE=$system.Version.GetVersion()
 I ENSEMBLE'[("2009.1.6"),(ENSEMBLE'[("2010.2.3")) W !,$$CJ^XLFSTR("Need Ensemble version 2009.1.6 or version 2010.2.3  ....neither was found",IOM) S BCERQUIT=2
 E  W !,$$CJ^XLFSTR("Need Ensemble version 2009.1.6 or version 2010.2.3 ....."_ENSEMBLE_" was found",IOM)
 ;
 ;I '$$PATCH("AG*7.1*8") W !,$$CJ^XLFSTR("Need at least AG patch 8....patch 8 NOT INSTALLED",IOM) S BCERQUIT=2
 ;E  W !,$$CJ^XLFSTR("Need at least AG patch 8....patch 8 Present",IOM)
 ;
 I '$$PATCH("AG*7.1*9") W !,$$CJ^XLFSTR("Need at least AG V7.1 patch 9....patch 9 NOT INSTALLED",IOM) S BCERQUIT=2
 E  W !,$$CJ^XLFSTR("Need at least AG V7.1 patch 9....patch 9 Present",IOM)
 ;
 ;I '$$VCHK("AG","7.2",2) D
 ;.W !,$$CJ^XLFSTR("AGMPI V7.2 NOT INSTALLED",IOM) S BCERQUIT=2
 ;
 I '$$PATCH("APCL*3.0*27") W !,$$CJ^XLFSTR("Need at least APCL V3.0 patch 27....patch 27 NOT INSTALLED",IOM) S BCERQUIT=2
 E  W !,$$CJ^XLFSTR("Need at least APCL V3.0 patch 27....patch 27 Present",IOM)
 ;
 I '$$PATCH("APSP*7.0*1010") W !,$$CJ^XLFSTR("Need at least APSP V7.0 patch 1010....patch 1010 NOT INSTALLED",IOM) S BCERQUIT=2
 E  W !,$$CJ^XLFSTR("Need at least APSP V7.0 patch 1010....patch 1010 Present",IOM)
 ;
 ;I '$$PATCH("BGP*11.0*2") W !,$$CJ^XLFSTR("Need at least BGP patch 2....patch 2 NOT INSTALLED",IOM) S BCERQUIT=2
 ;E  W !,$$CJ^XLFSTR("Need at least BGP patch 2....patch 2 Present",IOM)
 ;
 I '$$PATCH("BGP*11.0*3") W !,$$CJ^XLFSTR("Need at least BGP V11.0 patch 3....patch 3 NOT INSTALLED",IOM) S BCERQUIT=2
 E  W !,$$CJ^XLFSTR("Need at least BGP V11.0 patch 3....patch 3 Present",IOM)
 ;
 ;I '$$VCHK("BGP*11.1") W !,$$CJ^XLFSTR("Need at least BGP V11.1....V11.1 NOT INSTALLED",IOM) S BCERQUIT=2
 ;E  W !,$$CJ^XLFSTR("Need at least BGP V11.1 Present",IOM)
 ; 
 ;I '$$VCHK("BJMD","1.0",2) D
 .W !,$$CJ^XLFSTR("BJMD V1.0 NOT INSTALLED",IOM) S BCERQUIT=2
 ;
 ;I '$$VCHK("BJMD","1.1",2) D
 ;.W !,$$CJ^XLFSTR("BJMD V1.1 NOT INSTALLED",IOM) S BCERQUIT=2
 I '$$PATCH("BJMD*1.0*1") W !,$$CJ^XLFSTR("Need at least BJMD V1.0 patch 1....patch 1 NOT INSTALLED",IOM) S BCERQUIT=2
 E  W !,$$CJ^XLFSTR("Need at least BJMD V1.0 patch 1....patch 1 Present",IOM)
 ;
 ;I '$$PATCH("BJPC*2.0*5") W !,$$CJ^XLFSTR("Need at least BJPC patch 5....patch 5 NOT INSTALLED",IOM) S BCERQUIT=2
 ;E  W !,$$CJ^XLFSTR("Need at least BJPC patch 5....patch 5 Present",IOM)
 ;
 I '$$PATCH("BJPC*2.0*6") W !,$$CJ^XLFSTR("Need at least BJPC V2.0 patch 6....patch 6 NOT INSTALLED",IOM) S BCERQUIT=2
 E  W !,$$CJ^XLFSTR("Need at least BJPC V2.0 patch 6....patch 6 Present",IOM)
 ;
 I '$$PATCH("BMC*4.0*7") W !,$$CJ^XLFSTR("Need at least BMC V4.0 patch 7....patch 7 NOT INSTALLED",IOM) S BCERQUIT=2
 E  W !,$$CJ^XLFSTR("Need at least BMC V4.0 patch 7....patch 7 Present",IOM)
 ;
 I '$$VCHK("BQI","2.1",2) D
 .W !,$$CJ^XLFSTR("BQI V2.1 NOT INSTALLED",IOM) S BCERQUIT=2
 ;
 I '$$PATCH("BRN*2.0*3") W !,$$CJ^XLFSTR("Need at least BRN V2.0 patch 3....patch 3 NOT INSTALLED",IOM) S BCERQUIT=2
 E  W !,$$CJ^XLFSTR("Need at least BRN V2.0 patch 3....patch 3 Present",IOM)
 ;
 I '$$PATCH("BYIM*2.0*1") W !,$$CJ^XLFSTR("Need at least BYIM V2.0 patch 1....patch 1 NOT INSTALLED",IOM) S BCERQUIT=2
 E  W !,$$CJ^XLFSTR("Need at least BYIM V2.0 patch 1....patch 1 Present",IOM)
 ;
 ;I '$$PATCH("BEHU*1.1*8") W !,$$CJ^XLFSTR("Need at least BEHU V1.1 patch 8....patch 8 NOT INSTALLED",IOM) S BCERQUIT=2
 ;E  W !,$$CJ^XLFSTR("Need at least BEHU V1.1 patch 8....patch 8 Present",IOM)
 I '$$PATCH("BGO*1.1*8") W !,$$CJ^XLFSTR("Need at least BGO V1.1 patch 8....patch 8 NOT INSTALLED",IOM) S BCERQUIT=2
 E  W !,$$CJ^XLFSTR("Need at least BGO V1.1 patch 8....patch 8 Present",IOM)
 ;
 I '$$PATCH("LR*5.2*1028") W !,$$CJ^XLFSTR("Need at least LR V5.2 patch 1027....patch 1027 NOT INSTALLED",IOM) S BCERQUIT=2
 E  W !,$$CJ^XLFSTR("Need at least LR V5.2 patch 1027....patch 1027 Present",IOM)
 ;
 ;I '$$VCHK("BPHR","1.0",2) D
 ;.W !,$$CJ^XLFSTR("BPHR V1.0 NOT INSTALLED",IOM) S BCERQUIT=2
 ;
 I '$$PATCH("PXRM*1.5*1007") W !,$$CJ^XLFSTR("Need at least PXRM V1.5 patch 1007....patch 1007 NOT INSTALLED",IOM) S BCERQUIT=2
 E  W !,$$CJ^XLFSTR("Need at least PXRM V1.5 patch 1007....patch 1007 Present",IOM)
 ;
 ;I '$$PATCH("TIU*1.0*1008") W !,$$CJ^XLFSTR("Need at least TIU patch 1008....patch 1008 NOT INSTALLED",IOM) S BCERQUIT=2
 ;E  W !,$$CJ^XLFSTR("Need at least TIU patch 1008....patch 1008 Present",IOM)
 ;
 ;I '$$PATCH("XU*8.0*1016") W !,$$CJ^XLFSTR("Need at least Kernel patch 1016....patch 1016 NOT INSTALLED",IOM) S BCERQUIT=2
 ;E  W !,$$CJ^XLFSTR("Need at least Kernel patch 1016....patch 1016 Present",IOM)
 I BCERQUIT D SORRY(2) Q
 ;
 Q
SORRY(X) ;
 KILL DIFQ
 S XPDQUIT=X
 W:'$D(ZTQUEUED) *7,!,$$CJ^XLFSTR("Sorry....",IOM),$$DIR^XBDIR("E","Press RETURN")
 Q
VCHK(BCERPRE,BCERVER,BCERQUIT) ;Check versions needed.
 NEW BCERV
 S BCERV=$$VERSION^XPDUTL(BCERPRE)
 W !,$$CJ^XLFSTR("Need at least "_BCERPRE_" v "_BCERVER_"....."_BCERPRE_" v "_BCERV_" Present",IOM)
 I BCERV<BCERVER W *7,!,$$CJ^XLFSTR("^^^^**NEED TO UPGRADE**^^^^",IOM) Q 0
 Q 1
 ;
INSTALLD(BCERSTAL) ;EP
 NEW DIC,X,Y
 S X=$P(BCERSTAL,"*",1)
 S DIC="^DIC(9.4,",DIC(0)="FM",D="C"
 D IX^DIC
 I Y<1 Q 0
 S DIC=DIC_+Y_",22,",X=$P(BCERSTAL,"*",2)
 D ^DIC
 I Y<1 Q 0
 S DIC=DIC_+Y_",""PAH"",",X=$P(AGINSTAL,"*",3)
 D ^DIC
 Q $S(Y<1:0,1:1)
 ;
PATCH(X) ;return 1 if patch X was installed, X=aaaa*nn.nn*nnnn
 Q:X'?1.4UN1"*"1.2N1"."1.2N.1(1"V",1"T").2N1"*"1.4N 0
 N %,I,J
 S I=$O(^DIC(9.4,"C",$P(X,"*"),0)) Q:'I 0
 S J=$O(^DIC(9.4,I,22,"B",$P(X,"*",2),0)),X=$P(X,"*",3) Q:'J 0
 ;check if patch is just a number
 Q:$O(^DIC(9.4,I,22,J,"PAH","B",X,0)) 1
 S %=$O(^DIC(9.4,I,22,J,"PAH","B",X_" SEQ"))
 Q (X=+%)
