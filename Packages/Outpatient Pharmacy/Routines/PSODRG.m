PSODRG ;IHS/DSD/JCM-ORDER ENTRY DRUG SELECTION ;05-Jan-2007 09:40;SM
 ;;7.0;OUTPATIENT PHARMACY;**20,23,36,53,54,46,112,139,1005**;DEC 1997
 ;External reference ^PSDRUG supported by DBIA 221
 ;External reference ^PS(50.7 supported by DBIA 2223
 ;External reference to PSSDIN supported by DBIA 3166
 ; Modified IHS/CIA/PLS - 12/30/03 - Line SET+15 and POST+4
 ;          IHS/MSC/PLS - 01/05/07 - Line SET+9
 ;----------------------------------------------------------
START ;
 S (PSONEW("DFLG"),PSONEW("FIELD"),PSODRG("QFLG"))=0
 D SELECT G:$G(PSORXED("DFLG")) END ; Select Drug
 I $G(PSORX("EDIT"))]"",'PSONEW("FIELD") D TRADE
 G:PSONEW("DFLG")!(PSODRG("QFLG"))!($G(PSORXED("DFLG"))) END
 D SET ; Set various drug information
 D NFI ; Display dispense drug/orderable item text
 D:'$G(PSOEDIT) POST I $G(PSORX("DFLG")) S PSONEW("DFLG")=1 K:'$G(PSORX("EDIT")) PSORX("DFLG") ; Do any post selection action
END ;D EOJ
 Q
 ;------------------------------------------------------------
 ;
SELECT ;
 K:'$G(PSORXED) CLOZPAT
 K DIC,X,Y,PSODRUG("TRADE NAME") S:$G(POERR)&($P($G(OR0),"^",9)) Y=$P(^PSDRUG($P(OR0,"^",9),0),"^")
 I $G(PSODRUG("IEN"))]"" S Y=PSODRUG("NAME"),PSONEW("OLD VAL")=PSODRUG("IEN")
 W !,"DRUG: "_$S($G(Y)]"":Y_"// ",1:"") R X:$S($D(DTIME):DTIME,1:300) I '$T S DTOUT=1
 I X="",$G(Y)]"" S:Y X=Y S:'X X=$G(PSODRUG("IEN")) S:X X="`"_X
 G:X="" SELECT
 I X?1."?" W !!,"Answer with DRUG NUMBER, or GENERIC NAME, or VA PRODUCT NAME, or",!,"NATIONAL DRUG CLASS, or SYNONYM" G SELECT
 I $G(PSORXED),X["^" S PSORXED("DFLG")=1 G SELECTX
 I X="^"!(X["^^")!($D(DTOUT)) S PSONEW("DFLG")=1 G SELECTX
 I '$G(POERR),X[U,$L(X)>1 S PSODIR("FLD")=PSONEW("FLD") D JUMP^PSODIR1 S:$G(PSODIR("FIELD")) PSONEW("FIELD")=PSODIR("FIELD") K PSODIR S PSODRG("QFLG")=1 G SELECTX
 S DIC=50,DIC(0)="EMQZVT",DIC("T")="",D="B^C^VAPN^VAC"
 S DIC("S")="I $S('$D(^PSDRUG(+Y,""I"")):1,'^(""I""):1,DT'>^(""I""):1,1:0),$S($P($G(^PSDRUG(+Y,2)),""^"",3)'[""O"":0,1:1),$D(^PSDRUG(""ASP"",+$G(^(2)),+Y))"
 D MIX^DIC1 K DIC,D
 I $D(DTOUT) S PSONEW("DFLG")=1 G SELECTX
 I $D(DUOUT) K DUOUT G SELECT
 I Y<0 G SELECT
 S:$G(PSONEW("OLD VAL"))=+Y&('$G(PSOEDIT)) PSODRG("QFLG")=1
 K PSOY S PSOY=Y,PSOY(0)=Y(0)
 I $P(PSOY(0),"^")="OTHER DRUG"!($P(PSOY(0),"^")="OUTSIDE DRUG") D TRADE
SELECTX K X,Y,DTOUT,DUOUT,PSONEW("OLD VAL")
 Q
 ;
TRADE ;
 K DIR,DIC,DA,X,Y
 S DIR(0)="52,6.5" S:$G(PSOTRN)]"" DIR("B")=$G(PSOTRN) D ^DIR K DIR,DIC
 I X="@" S Y=X K DIRUT
 I $D(DIRUT) S:$D(DUOUT)!$D(DTOUT)&('$D(PSORX("EDIT"))) PSONEW("DFLG")=1 G TRADEX
 S PSODRUG("TRADE NAME")=Y
TRADEX I $G(PSORXED("DFLG")),$D(DIRUT) S PSORXED("DFLG")=1
 K DIRUT,DTOUT,DUOUT,X,Y,DA,DR,DIE
 Q
SET ;
 N STAT S PSODRUG("IEN")=+PSOY,PSODRUG("VA CLASS")=$P(PSOY(0),"^",2)
 S PSODRUG("NAME")=$P(PSOY(0),"^")
 S:+$G(^PSDRUG(+PSOY,2)) PSODRUG("OI")=+$G(^(2)),PSODRUG("OIN")=$P(^PS(50.7,+$G(^(2)),0),"^")
 S PSODRUG("NDF")=$S($G(^PSDRUG(+PSOY,"ND"))]"":+^("ND")_"A"_$P(^("ND"),"^",3),1:0)
 S PSODRUG("MAXDOSE")=$P(PSOY(0),"^",4),PSODRUG("DEA")=$P(PSOY(0),"^",3)
 S PSODRUG("CLN")=$S($D(^PSDRUG(+PSOY,"ND")):+$P(^("ND"),"^",6),1:0)
 S PSODRUG("SIG")=$P(PSOY(0),"^",5)
 S PSODRUG("NDC")=$P($G(^PSDRUG(+PSOY,2)),"^",4)
 S:$D(PSONEW("NDC")) PSONEW("NDC")=PSODRUG("NDC")
 S PSODRUG("STKLVL")=$G(^PSDRUG(+PSOY,660.1))
 G:$G(^PSDRUG(+PSOY,660))']"" SETX
 S PSOX1=$G(^PSDRUG(+PSOY,660))
 S PSODRUG("COST")=$P($G(PSOX1),"^",6)
 S PSODRUG("UNIT")=$P($G(PSOX1),"^",8)
 S PSODRUG("EXPIRATION DATE")=$P($G(PSOX1),"^",9)
 ; IHS/CIA/PLS - 12/29/03 - Added IHS specific fields
 S PSODRUG("AWP")=+$P($G(^PSDRUG(+PSOY,999999931)),U,2)  ; AWP Price
 S PSODRUG("MANUFACTURER")=$$GET1^DIQ(50,+PSOY,9999999.24,"E")
 S PSODRUG("LOT #")=$$GET1^DIQ(50,+PSOY,9999999.25)
 S PSODRUG("EXPIRATION DATE")=$$GET1^DIQ(50,+PSOY,9999999.26,"I")
 S:($P(%APSITE,"^",11)]"")&((PSODRUG("EXPIRATION DATE")="")!(PSODRUG("EXPIRATION DATE")'>DT)) PSODRUG("EXPIRATION DATE")=$$FMADD^XLFDT(DT,$P(%APSITE,"^",11))
SETX K PSOX1,PSOY
 Q
NFI ;display restriction/guidelines
 D EN^PSSDIN(PSODRUG("OI"),PSODRUG("IEN")) S NFI=$$PROMPT^PSSDIN
 I NFI]"","ODY"[NFI D TD^PSONFI
 K NFI Q
POST ;order checks
 K PSORX("INTERVENE") N STAT,SIG,PTR,NDF,VAP S PSORX("DFLG")=0
 D ^PSOBUILD
 D @$S($G(COPY):"^PSOCPDUP",1:"^PSODRDUP") ; Set PSORX("DFLG")=1 if process to stop
 Q:$G(PSORX("DFLG"))
 W:$G(PSOFIN)']"" !,"Now doing order checks.  Please wait...",!
 D ^PSODGDGI
 I $G(PSORX("INTERVENE"))]"" D FULL^VALM1,^PSORXI S VALMBCK="R"
 G:PSORX("DFLG") POSTX
 D:$P($G(^PSDRUG(PSODRUG("IEN"),"CLOZ1")),"^")]"" CLOZ G:PSORX("DFLG") POSTX
 K PSORX("INTERVENE")
 S X="APSQLAB" X ^%ZOSF("TEST") I $T D PRINT^APSQLAB   ; IHS/CIA/PLS - 01/18/04 - Output lab information
 I $D(PSODRUG("NDF")) S NDF=$P(PSODRUG("NDF"),"A"),VAP=$P(PSODRUG("NDF"),"A",2),PTR=NDF_"."_VAP
 I $G(NDF) D CHK^PSODGAL(PSODFN,"DR",PTR) K NDF,VAP,PTR
 I $P($G(PSODRUG("NDF")),"A")=0 D CHK1^PSODGAL(PSODFN)
 I $D(PSODRUG("VA CLASS")) D CLASS^PSODGAL
POSTX ;
 K PSORX("INTERVENE"),DA
 Q
 ;
EOJ ;
 K PSODRG
 Q
 ;
CLOZ ;
 S ANQRTN=$P(^PSDRUG(PSODRUG("IEN"),"CLOZ1"),"^"),ANQX=0
 S P(5)=PSODRUG("IEN"),DFN=PSODFN,X=ANQRTN
 X ^%ZOSF("TEST") I  D @("^"_ANQRTN) S:$G(ANQX) PSORX("DFLG")=1
 K P(5),ANQRTN,ANQX,X
 Q
EN(DRG) ;returns lab test identified for clozapine order checking
 K LAB I $P($G(^PSDRUG(DRG,"CLOZ1")),"^")'="PSOCLO1" S LAB("NOT")=0 Q
 I $P($G(^PSDRUG(DRG,"CLOZ1")),"^")="PSOCLO1" D
 .S (CNT,I)=0 F  S I=$O(^PSDRUG(DRG,"CLOZ2",I)) Q:'I  S CNT=$G(CNT)+1
 .I CNT'=2 S LAB("BAD TEST")=0 K CNT Q
 .K CNT F I=0:0 S I=$O(^PSDRUG(DRG,"CLOZ2",I)) Q:'I  D
 ..S LABT=$S($P(^PSDRUG(DRG,"CLOZ2",I,0),"^",4)=1:"WBC",1:"ANC"),LAB(LABT)=$P(^PSDRUG(DRG,"CLOZ2",I,0),"^")_"^"_$P(^(0),"^",3)_"^"_$P(^(0),"^",4)
 K LABT,I
 Q
