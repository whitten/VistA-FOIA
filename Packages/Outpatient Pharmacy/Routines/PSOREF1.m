PSOREF1 ;IHS/DSD/JCM-ASKS ALL QUESTIONS FOR REFILL RX ORDER ENTRY;21-Apr-2005 14:19;SM
 ;;7.0;OUTPATIENT PHARMACY;**26,1001,1003**;DEC 1997
 ;External reference ^PSDRUG( supported by DBIA 221
 ; Modified - IHS/CIA/PLS - 01/06/04 - Line 2 and added several fields
 ;                          10/27/04 - Move previous mod to separate EP.
START ;
 S (PSOREF("DFLG"),PSOREF("FIELD"),PSOREF1)=0
 S X="T-6M",%DT="X" D ^%DT
 S (PSOID,PSOREF("ISSUE DATE"))=$S($P(^PSRX(PSOREF("IRXN"),0),"^",13)<Y:Y,1:$P(^PSRX(PSOREF("IRXN"),0),"^",13))
 S:$G(PSORX("BAR CODE"))&($G(PSOBBC1("FROM"))="NEW") PSOREF("ISSUE DATE")=DT
 K X,X1,X2
 S PSOREF("CS")=0,PSODRUG("DEA")=$P(^PSDRUG($P(^PSRX(PSOREF("IRXN"),0),"^",6),0),"^",3)
 F DEA=1:1 Q:$E(PSODRUG("DEA"),DEA)=""  I $E(+PSODRUG("DEA"),DEA)>1,$E(+PSODRUG("DEA"),DEA)<6 S $P(PSOREF("CS"),"^")=1 S:$E(+PSODRUG("DEA"),DEA)=2 $P(PSOREF("CS"),"^",2)=1
 ;
 D IHSSET  ; IHS/CIA/PLS - 10/27/04 - Setup additional fields for IHS
 ;
1 S PSONEW("DAYS SUPPLY")=$P(^PSRX(PSOREF("IRXN"),0),"^",8),PSONEW("# OF REFILLS")=$P(^(0),"^",9)
 S PSOREF("FLD")=1 D FILLDT^PSODIR2(.PSOREF) ; Get Fill date
 G:PSOREF("DFLG") END G:PSOREF("FIELD") @PSOREF("FIELD")
 ;
 ; IHS/CIA/PLS - 01/06/04 Changed default from MAIL to WINDOW
2 ;S PSOREF("FLD")=2,PSORX("MAIL/WINDOW")="MAIL" D MW^PSODIR2(.PSOREF)
 S PSOREF("FLD")=2,PSORX("MAIL/WINDOW")="WINDOW" D MW^PSODIR2(.PSOREF)
 ;
 G:PSOREF("DFLG") END G:PSOREF("FIELD") @PSOREF("FIELD")
 ;
3 I $G(DUZ("AG"))="I" S PSOREF("FLD")=3 D CLERK^PSODIR2(.PSOREF) ; Get Clerk Code
 G:PSOREF("DFLG") END G:PSOREF("FIELD") @PSOREF("FIELD")
 ;
4 I $G(DUZ("AG"))="I" S PSOREF("FLD")=4 D EXP^PSODIR2(.PSOREF) ; Get Expiration Date - Indian Health Service ONLY
 G:PSOREF("DFLG") END G:PSOREF("FIELD") @PSOREF("FIELD")
 ;
5 I $G(PSOBILST) D  G:PSOREF("DFLG") END G:PSOREF("FIELD") @PSOREF("FIELD")
 .S PSOREF("FLD")=5 D BST^APSPDIR(.PSOREF)  ;GET BILLING STATUS CODE
 ;
6 I $G(PSOBILST) D  G:PSOREF("DFLG") END G:PSOREF("FIELD") @PSOREF("FIELD")
 .S PSOREF("FLD")=6 D INSURER^APSPDIR(.PSOREF)  ;GET INSURER INFO
 ;
7 I $G(PSONDC)=1 D  G:PSOREF("DFLG") END G:PSOREF("FIELD") @PSOREF("FIELD")
 .S PSOREF("FLD")=7 D NDC^APSPDIR(.PSOREF)
 .Q:PSOREF("DFLG")
 .K PSOREF("AWP")  ;ALWAYS KILL AWP IF CHECKING NDC TO RESET
 .K PSOREF("COST")  ;ALWAYS KILL COST IF CHECKING NDC TO RESET
 ;
8 I $G(PSOAWP)=1 D  G:PSOREF("DFLG") END G:PSOREF("FIELD") @PSOREF("FIELD")
 .S PSOREF("FLD")=8 D AWP^APSPDIR(.PSOREF)
 ;
9 I $G(PSOCOST)=1 D  G:PSOREF("DFLG") END G:PSOREF("FIELD") @PSOREF("FIELD")
 .S PSOREF("FLD")=9 D COST^APSPDIR(.PSOREF)  ;GET UNIT COST OF DRUG
 ;
10 I $G(APSPMAN)>0,$G(APSPMAN)<3 D  G:PSOREF("DFLG") END G:PSOREF("FIELD") @PSOREF("FIELD")
 .S PSOREF("FLD")=10 D MANUF^APSPDIR(.PSOREF)  ;GET MANUFACTURER DATA
 ;
11 I $G(APSPMAN)>0,$G(APSPMAN)<3 D  G:PSOREF("DFLG") END G:PSOREF("FIELD") @PSOREF("FIELD")
 .S PSOREF("FLD")=60 D LOT^APSPDIR(.PSOREF)  ;GET LOT #
 ;IHS/CIA/PLS - End IHS Fields
END ;
 K PSOREF1
 ; IHS/CIA/PLS - 01/27/04 - Setup AWP and COST
 S:('$D(PSOREF("AWP")))&('$G(PSOREF("DFLG"))) PSOREF("AWP")=$$AWP^APSQDAWP($S($D(PSOREF("NDC")):PSOREF("NDC"),1:PSODRUG("NDC")),PSODRUG("IEN"),.TALK)
 S:('$D(PSOREF("COST")))&('$G(PSOREF("DFLG"))) PSOREF("COST")=$$COST^APSQDAWP($S($D(PSOREF("NDC")):PSOREF("NDC"),1:PSODRUG("NDC")),PSODRUG("IEN"),.TALK)
 Q
JUMP ;
 S PSOREF("FIELD")=$S(+Y=22:1,+Y=11:2,+Y=16:3,+Y=29:4,1:PSOREF("FLD"))
 ; IHS/CIA/PLS - 01/27/04 - Added $S for additional fields
 S PSONEW("FIELD")=$S(+Y=9999999.07:5,+Y=9999999.12:6,+Y=27:7,+Y=9999999.06:8,+Y=17:9,+Y=28:10,+Y=24:11,1:PSONEW("FIELD"))
 I PSOREF("FIELD")>PSOREF("FLD") W !,$C(7),"Cannot jump ahead ..",! S PSOREF("FIELD")=PSOREF("FLD")
 Q
 ;
EN(PSOREF) ;
 D START
 Q
PROFILE ;
 S (PSORX("REFILL"),PSORX("RENEW"))=0,PSOX=""
 D ^PSOBUILD
 I '$G(PSOSD) W !,"This patient has no prescriptions" S:'$D(DFN) DFN=PSODFN D GMRA^PSODEM G PROFILEX
 S (PSODRG,PSOX)="" F  S PSODRG=$O(PSOSD(PSODRG)) Q:PSODRG=""  F  S PSOX=$O(PSOSD(PSODRG,PSOX)) Q:PSOX=""  S:$P(PSOSD(PSODRG,PSOX),"^",3)="" PSORX("RENEW")=1 S:$P(PSOSD(PSODRG,PSOX),"^",4)="" PSORX("REFILL")=1
 K PSOX
PROFILEX Q
 ;
 ; IHS/CIA/PLS - 10/27/04 - Setup for additional fields
IHSSET ; EP
 S PSODRUG("IEN")=$P(^PSRX(PSOREF("IRXN"),0),U,6)
 S (PSODRUG("NDC"),PSOREF("NDC"))=$P(^PSDRUG(PSODRUG("IEN"),2),U,4)
 S PSOREF("AWP")=$P($G(^PSDRUG(PSODRUG("IEN"),999999931)),U,2)
 S:'$D(PSOREF("BST")) PSOREF("BST")=$G(PSOBILST)  ; CIA/IHS/PLS - Added with patch 1003 - 04/21/05
 S:$G(^PSDRUG(PSODRUG("IEN"),660))]"" PSOREF("COST")=$P(^PSDRUG(PSODRUG("IEN"),660),"^",6)
 I $D(^PSDRUG(PSODRUG("IEN"),999999924)) D
 .N NODE
 .S NODE=^PSDRUG(PSODRUG("IEN"),999999924)
 .I $G(APSPMAN)=1 D
 ..S PSOREF("MANUFACTURER")=$P(NODE,U,1) S:+PSOREF("MANUFACTURER") PSOREF("MANUFACTURER")=$P(^PS(55.95,PSOREF("MANUFACTURER"),0),U)
 ..S PSOREF("LOT #")=$P(NODE,U,2)
 .I (($G(APSPMAN)=1)!($G(APSPMAN)=2)) S PSOREF("EXPIRATION DATE")=$P(NODE,U,3)
 ; IHS/CIA/PLS - End setup
 Q
