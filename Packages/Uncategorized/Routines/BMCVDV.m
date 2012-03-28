BMCVDV ; IHS/OIT/FCJ - 1/2 VIEW/EDIT PROVIDER-VENDOR FILE ;
 ;;4.0;REFERRED CARE INFO SYSTEM;**5,7,8**;JAN 09, 2006
 ;BMC*4.0*5 5.13.2009 IHS.OIT.FCJ ORIGIAL ROUTINE FR ACHSVDV
 ;
A1 ;EP
 D HDR
 S DIC(0)="AEQMZ"_$S($D(^XUSEC("BMCZVEN",DUZ)):"L",1:""),DIC="^AUTTVNDR(",DIC("A")="Enter Provider/Vendor:  "
 S:DIC(0)["L" DLAYGO=9999999.11
 D ^DIC
 K DIC,DLAYGO
 G END:Y=-1
 S BMCPROV=+Y
 G ^BMCVDVD
 ;
A1A ;EP
 S:'$D(^AUTTVNDR(BMCPROV,11))#2 ^AUTTVNDR(BMCPROV,11)=""
 S:'$D(^AUTTVNDR(BMCPROV,13))#2 ^AUTTVNDR(BMCPROV,13)=""
A2 ;EP
 Q:'$D(BMCPROV)
 S BMC0=$G(^AUTTVNDR(BMCPROV,0)),BMC11=$S($D(^AUTTVNDR(BMCPROV,11)):^(11),1:""),BMC13=$S($D(^AUTTVNDR(BMCPROV,13)):^(13),1:""),BMC17=$S($D(^AUTTVNDR(BMCPROV,17)):^(17),1:"")
 I $D(^AUTTVNDR(BMCPROV,11)),$P($G(^AUTTVNDR(BMCPROV,11)),U)="" G MESSAGE^BMCVDVD
 D HDR
 S BMCVT=$P(BMC11,U,3),BMCDISP=""
 I BMCVT]"",BMCVT?1N.N S BMCVT=$P(^AUTTVTYP(+BMCVT,0),U,2)
 S BMCEDOS=DT
 K BMCRT,BMCCTNA
 D A1^BMCVURT
 S BMCACO="",P=BMCPROV
 D ^BMCVUCN
 K BMCRT,BMCCTNA
 D A1^BMCVURT
 S BMCMS=$P(BMC13,U,3)
 I BMCMS?1N.N S BMCMS=$P(^DIC(5,BMCMS,0),U)
 S BMCBS=$P(BMC13,U,8)
 I BMCBS?1N.N S BMCBS=$P(^DIC(5,BMCBS,0),U)
 W !," 1) ",$P(^AUTTVNDR(BMCPROV,0),U),?46," 2) EIN No: ",$P(BMC11,U),$S($P(BMC11,U,2)]"":"-"_$P(BMC11,U,2),1:"")
 W !," 3) Status: ",$S(+$P(BMC0,U,5):"IN",1:""),"ACTIVE"
 W ?46," 4) Contracts: ",$S(BMCACO="N":"NONE ACTIVE",+C>0:"ACTIVE="_+C,1:"NONE")
 I '$D(BMCRT("RQ")) S BMCRT("RQ")=-1
 W !," 5) UPIN: ",$P(BMC17,U),?46," 6) Rate Quotation: ",$S('$D(BMCRT("RQ")):"NONE",'$D(BMCRT("RQ","ACTIVE")):"NONE ACTIVE",BMCRT("RQ","ACTIVE")>0:"ACTIVE="_+$P(BMCRT("RQ","ACTIVE"),U,1),1:"")
 W !," 7) Type of Business: "
 S X=$P(BMC11,U,26)
 I X W $P($G(^AUTTTOB(X,0)),U)
 W ?46," 8) Agreement: ",$S('$D(BMCRT("PA")):"NONE",'$D(BMCRT("PA","ACTIVE")):"NONE ACTIVE",BMCRT("PA","ACTIVE")>0:"ACTIVE="_+$P(BMCRT("PA","ACTIVE"),U,1),1:"")
 I $D(^AUTTVNDR(BMCPROV,23)) S BMCMP=$P($G(^(23)),U) S BMCMP=$$EXTSET^XBFUNC(9999999.11,2301,BMCMP)
 I '$D(^AUTTVNDR(BMCPROV,23)) S BMCMP=""
 W !," 9) Medicare Provider: ",$S(BMCMP="":"No entry",BMCMP'="":BMCMP,1:"")
 W ?46,"10) BPA: ",$S('$D(BMCRT("BPA")):"NONE",'$D(BMCRT("BPA","ACTIVE")):"NONE ACTIVE",BMCRT("BPA","ACTIVE")>0:"ACTIVE="_+$P(BMCRT("BPA","ACTIVE"),U,1),1:"")
 W !,"11) E-Mail: ",$$VAL^XBDIQ1(9999999.11,BMCPROV,2103)   ;BMC*4.0*7 IHS.OIT.FCJ
 W ?46,"12) DUNS: ",$$VAL^XBDIQ1(9999999.11,BMCPROV,.07)
 W !!,"**** MAILING/BILLING ADDRESS ****",?44,"**** PROVIDER LOCATION ADDRESS ****"
 W !,"13) Street: ",$P(BMC13,U),?46,"14) Street: ",$E($P(BMC13,U,6),1,17)
 ;BMC*4.0*7 IHS.OIT.FCJ CHANGES FOR FAX
 W !?6,"City: ",$P(BMC13,U,2),?52,"City: ",$P(BMC13,U,7),!?5,"State: ",BMCMS,?26,"Zip: ",$P(BMC13,U,4),?51,"State: ",BMCBS
 W !?5,"Phone: ",$P(BMC11,U,9),?26,"Fax: ",$P(BMC11,U,14),?48,"Zip Code: ",$P(BMC13,U,9),!?7,"Attn: ",$P(BMC13,U,5)
 W !,"15) Vendor Type: ",BMCVT
 W ?46,"16) Fed/Non-Fed: ",$S($P(BMC11,U,10)=1:"NON-FED",$P(BMC11,U,10)=2:"FED",$P(BMC11,U,10)="":"")
 W !,"17) Specialty: ",$S($P(BMC11,U,4):$P(^DIC(7,$P(BMC11,U,4),0),U),1:"")
 W ?46,"18) Geographic Loc: ",$S($P(BMC11,U,25)="":"",$P(BMC11,U,25):$P(^AUTTGL($P(BMC11,U,25),0),U))
 W !,$$REPEAT^XLFSTR("*",79)
 G:'$D(^XUSEC("BMCZVEN",DUZ)) A4^BMCVDV1
 ;BMC*4.0*8 CHANGED 14 TO 15 IN NXT LINE
 I BMCVT="" W *7,!!,"MUST HAVE VENDOR TYPE." D 15^BMCVDVA G A2:'$D(Y),A1
A3 ;
 K BMCCTFL,BMCRQFL,BMCPAFL,BMCBPFL
 S Y=$$DIR^XBDIR("Y","Want to Edit","NO","","","",2)
 G END:$D(DTOUT),A1:$D(DUOUT),A4^BMCVDV1:('Y),EDIT^BMCVDVA:Y
 G A3
 ;
END ;EP
 K X,P,S,C,L,D,D0,DA,DI,DIC,DR
 K BMCMP,BMC0,BMC11,BMC13,BMC17,BMCACO,BMCBS,BMCCTNA,BMCDISP,BMCEDOS,BMCEIN,BMCEINS,BMCI
 K BMCMS,BMCPROV,BMCRT,BMCSUFF,BMCVT,BMCYAYA
 ;D EN^XBVK("BMC")
 Q
 ;
HDR ;
 S X="PROVIDER/VENDOR UPDATE",Y="BMC"
 W @IOF,!,$$C^XBFUNC(X),!!
 W $$REPEAT^XLFSTR("*",79)
 Q
 ;
