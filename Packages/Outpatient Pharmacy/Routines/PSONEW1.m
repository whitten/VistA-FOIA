PSONEW1 ;IHS/DSD/JCM - new Rx order entry ;23-Jan-2009 10:19;PLS
 ;;7.0;OUTPATIENT PHARMACY;**46,104,117,1006,1008**;DEC 1997
 ;External reference ^PS(55 supported by DBIA 2228
 ;
 ; Modified - IHS/CIA/PLS - 01/02/04 -  New jump labels, END+2 and JUMP+2
 ;                          04/15/04 -  Re-numbered some of the line labels to prevent jump aheads.
 ;            IHS/MSC/PLS - 08/20/07 -  Line label 35 changed to call TRPDCLS^APSPDIR entry point.
 ;                          03/13/08 -  Added label 218 for SUBSTITUTION
 ;                          01/23/09 -  Added label 220 for CASH DUE
START ;
 S (PSONEW("DFLG"),PSONEW("FIELD"),PSONEW1)=0
 ;
1 S PSONEW("FLD")=1 S PSONEW("FIELD")=0
 I $P($G(PSOPAR),"^",7)'=1 D MANUAL^PSONRXN ; Get Manual Rx number
 G:PSONEW("QFLG")!PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
2 S PSONEW("FLD")=2 D PTSTAT^PSODIR1(.PSONEW) ; Get Patient Status
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
3 S PSONEW("FLD")=3 D ^PSODRG ; Get drug and related information
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
31 S PSONEW("FLD")=31 D DOSE^PSODIR(.PSONEW) ; Get Dosing
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
32 I '$G(PSONEW("ENT")) W !,"Incomplete Dosaging Data!",! K DIRUT G 31
 S PSONEW("FLD")=32 D INS^PSODIR(.PSONEW) ; Get Patient Instructions
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 I $P($G(^PS(55,PSODFN,"LAN")),"^") D SINS^PSODIR(.PSONEW)
 ;
35 ; IHS/CIA/PLS - 01/02/04 - Get Triplicate number
 I $$TRPDCLS^APSPDIR($G(PSOTRIP),$G(PSODRUG("DEA"))) D  G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 .S PSONEW("FLD")=35 D TRIP^APSPDIR(.PSONEW)
 ;
40 ; IHS/CIA/PLS - 01/02/04 - Get NDC Number
 I $G(PSONDC)=1 D  G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 .S PSONEW("FLD")=40 D NDC^APSPDIR(.PSONEW)
 .K PSONEW("AWP"),PSONEW("COST")
 ;
50 ; IHS/CIA/PLS - 01/02/04 - Get AWP Price
 I $G(PSOAWP)=1 D  G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 .S PSONEW("FLD")=50 D AWP^APSPDIR(.PSONEW)
 ;
51 ; IHS/CIA/PLS - 01/02/04 - Get Cost of Drug
 I $G(PSOCOST)=1 D  G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 .S PSONEW("FLD")=51 D COST^APSPDIR(.PSONEW)
 ;
 ; IHS/CIA/PLS - 01/02/04 - Branching logic
 G:(($G(APSPMAN)=3)!($G(APSPMAN)="")) 4 G:$G(APSPMAN)=2 57
53 ; IHS/CIA/PLS - 01/02/04 - Get Manufacturer data
 S PSONEW("FLD")=53 D MANUF^APSPDIR(.PSONEW)
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
55 ; IHS/CIA/PLS - 01/02/04 - Get Lot #
 S PSONEW("FLD")=55 D LOT^APSPDIR(.PSONEW)
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
57 ; IHS/CIA/PLS - 01/02/04 - Get Expiration Date
 S PSONEW("FLD")=57 D EXPDATE^APSPDIR(.PSONEW)
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
4 D EN^PSOFSIG(.PSONEW) I $O(SIG(0)) S SIGOK=1
 ;S PSONEW("FLD")=4 D SIG^PSODIR1(.PSONEW) ; Get Rx directions
 ;G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
67 S PSONEW("FLD")=67 D DAYS^PSODIR1(.PSONEW) ; Get days supply
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
75 S PSONEW("FLD")=75 D QTY^PSODIR1(.PSONEW) ; Get quantity
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
76 I $P($G(PSOPAR),"^",15) S PSONEW("FLD")=76 D COPIES^PSODIR1(.PSONEW) ; Get label copies
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
78 S PSONEW("FLD")=78 D REFILL^PSODIR1(.PSONEW) ; Get # of refills
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
79 S PSONEW("FLD")=79 D PROV^PSODIR(.PSONEW) ; Get Provider
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 G:$G(DUZ("AG"))'="I" 211
 ;
 ; IHS/CIA/PLS - 01/02/04 - Next two lines commented out.
100 ;Q:$G(DUZ("AG"))'="I"  S PSONEW("FLD")=10 D EXP^PSODIR2(.PSONEW) ; Get Expiration Date - Indian Health Service ONLY
 ;G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
101 ; IHS/CIA/PLS - 01/02/04 - Branching logic
 S PSONEW("BST")=PSOBILST G:$G(PSOBILRX)'=1 120
115 ; IHS/CIA/PLS - 01/02/04 - Billing Status
 S PSONEW("FLD")=115 D BST^APSPDIR(.PSONEW)
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
117 ; IHS/CIA/PLS - 01/02/04 - Get Insurer info
 S PSONEW("FLD")=117 D INSURER^APSPDIR(.PSONEW)
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
120 ; IHS/CIA/PLS - 01/02/04 - Get Chronic Med data
 I $G(APSPCMP) S PSONEW("FLD")=120 D CM^APSPDIR(.PSONEW)
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
211 S PSONEW("FLD")=211 D CLINIC^PSODIR2(.PSONEW) ; Get Clinic
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
212 S PSONEW("FLD")=212 D MW^PSODIR2(.PSONEW) ; Get Mail/Window Info
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
213 S PSONEW("FLD")=213 D RMK^PSODIR2(.PSONEW) ; Get Remarks
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
214 S PSONEW("FLD")=214 D ISSDT^PSODIR2(.PSONEW) ; Get Issue Date
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
215 S PSONEW("FLD")=215 D FILLDT^PSODIR2(.PSONEW) ; Get Fill date
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
216 S PSONEW("FLD")=216 D CLERK^PSODIR2(.PSONEW) ; Get Clerk Code
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
218 ; IHS/MSC/PLS - 03/13/08 - Get Substitution
 S PSONEW("FLD")=218 D SUBS^APSPDIR(.PSONEW)
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
220 ; IHS/MSC/PLS - 01/23/09 - Get Cash Due
 I $$GET1^DIQ(9009033,PSOSITE,319,"I") S PSONEW("FLD")=220 D CASHDUE^APSPDIR(.PSONEW)
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
END ;
 ; IHS/CIA/PLS - 01/06/04 - Set AWP and COST array variables
 S:('$D(PSONEW("AWP")))&('$G(PSONEW("DFLG")))&('$G(PSONEW("QFLG"))) PSONEW("AWP")=$$AWP^APSQDAWP($S($D(PSONEW("NDC")):PSONEW("NDC"),1:PSODRUG("NDC")),PSODRUG("IEN"),.TALK)
 S:('$D(PSONEW("COST")))&('$G(PSONEW("DFLG")))&('$G(PSONEW("QFLG"))) PSONEW("COST")=$$COST^APSQDAWP($S($D(PSONEW("NDC")):PSONEW("NDC"),1:PSODRUG("NDC")),PSODRUG("IEN"),.TALK)
 K PSONEW1
 Q
 ;
JUMP ; IHS/CIA/PLS - 04/15/04 - Modified line labels
 ;S PSONEW("FIELD")=$S(+Y=.01:1,+Y=3:2,+Y=6:3,+Y=10:4,+Y=7:5,+Y=10.6:6,+Y=8:7,+Y=9:8,+Y=4:9,+Y=29:10,+Y=5:11,+Y=11:12,+Y=12:13,+Y=1:14,+Y=22:15,+Y=16:16,+Y=113:31,+Y=114:32,1:PSONEW("FLD"))
 S PSONEW("FIELD")=$S(+Y=.01:1,+Y=3:2,+Y=6:3,+Y=10:4,+Y=7:75,+Y=10.6:76,+Y=8:67,+Y=9:78,+Y=4:79,+Y=29:100,+Y=5:211,+Y=11:212,+Y=12:213,+Y=1:214,+Y=22:215,+Y=16:216,+Y=113:31,+Y=114:32,1:PSONEW("FLD"))
 ; IHS/CIA/PLS - 01/02/04 - Added $S for additional fields
 ;S PSONEW("FIELD")=$S(+Y=9999999.06:50,+Y=9999999.02:120,+Y=9999999.07:115,+Y=28:53,+Y=24:55,+Y=26:57,+Y=9999999.12:117,+Y=17:51,+Y=9999999.14:35,1:PSONEW("FIELD"))
 ; IHS/MSC/PLS - 01/23/09 - Added field 220 to $S
 S PSONEW("FIELD")=$S(+Y=9999999.06:50,+Y=9999999.02:120,+Y=9999999.07:115,+Y=28:53,+Y=24:55,+Y=26:57,+Y=9999999.12:117,+Y=17:51,+Y=9999999.14:35,+Y=9999999.26:220,1:PSONEW("FIELD"))
 I PSONEW("FIELD")>PSONEW("FLD") W !,$C(7),"Cannot jump ahead ..",! S PSONEW("FIELD")=PSONEW("FLD")
 Q
