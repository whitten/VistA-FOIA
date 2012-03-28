ABMEBA0 ; IHS/ASDST/DMJ - HCFA-1500 EMC RECORD BA0 (Provider) Envoy version ;     
 ;;2.6;IHS 3P BILLING SYSTEM;;NOV 12, 2009
 ;
 ; IHS/ASDS/DMJ - 03/01/01 - V2.4 P5 - NOIS HQW-0301-100010
 ;    This is a new routine for a new Envoy electronic format
 ;
 ; IHS/FCS/DRS - Patch 9 Part 4b - Provider Specialty, at tag 220
 ;
START ;START HERE
 K ABMREC(10),ABMR(10)
 S ABME("RTYPE")=10
 D LOOP
 S ABMRT(95,"RTOT")=+$G(ABMRT(95,"RTOT"))+1
 K ABME,ABM
 Q
 ;
LOOP ;LOOP HERE
 F I=10:10:280 D
 .D @I
 .I $D(^ABMEXLM("AA",+$G(ABMP("INS")),+$G(ABMP("EXP")),10,I)) D @(^(I))
 .I '$G(ABMP("NOFMT")) S ABMREC(10)=$G(ABMREC(10))_ABMR(10,I)
 Q
 ;
10 ;Record type
 S ABMR(10,10)="BA0"
 Q
20 ;4-18 EMC Provider ID
 S ABMR(10,20)=$P($G(^ABMNINS(ABMP("LDFN"),ABMP("INS"),1,ABMP("VTYP"),0)),U,8)
 S:ABMR(10,20)="" ABMR(10,20)=$P($G(^ABMNINS(DUZ(2),ABMP("INS"),1,ABMP("VTYP"),0)),"^",8)
 S:ABMR(10,20)="" ABMR(10,20)=$P($G(^AUTNINS(ABMP("INS"),15,ABMP("LDFN"),0)),"^",2)
 S ABMP("EMCPRID")=ABMR(10,20)
 S ABMR(10,20)=$$FMT^ABMERUTL(ABMR(10,20),15)
 Q
30 ;19-21 Type of Batch
 S ABMR(10,30)=100
 S:ABMP("VTYP")=998 ABMR(10,30)=200
 S:ABMP("VTYP")=997 ABMR(10,30)=300
 S ABMP("TOB")=ABMR(10,30)
 Q
 ;
40 ;22-25 Batch Number
 S ABMR(10,40)=$G(ABMEF("BATCH#"))
 S ABMR(10,40)=$$FMT^ABMERUTL(ABMR(10,40),"4NR")
 Q
50 ;26-31 Batch ID
 S ABMR(10,50)=$G(ABMR(1,50))
 S ABMR(10,50)=$$FMT^ABMERUTL(ABMR(10,50),6)
 Q
60 ;32-40 Federal Tax ID or EIN
 D DIQ1
 S ABMR(10,60)=ABM(9999999.06,ABMP("LDFN"),.21,"E")
 S ABMR(10,60)=$$FMT^ABMERUTL(ABMR(10,60),"9S")
 S ABMRT(95,60)=ABMR(10,60)
 Q
70 ;41-46 Filler
 S ABMR(10,70)=""
 S ABMR(10,70)=$$FMT^ABMERUTL(ABMR(10,70),6)
 Q
80 ;47-47 Provider Tax ID Type
 S ABMR(10,80)="E"
 S ABMR(10,80)=$$FMT^ABMERUTL(ABMR(10,80),1)
 Q
90 ;48-62 Medicare Provider Number
 S ABMR(10,90)=""
 I ABMP("ITYPE")="R" D
 .S ABMR(10,90)=$P($G(^ABMNINS(ABMP("LDFN"),ABMP("INS"),1,ABMP("VTYP"),0)),U,8)
 .S:ABMR(10,90)="" ABMR(10,90)=$P($G(^ABMNINS(DUZ(2),ABMP("INS"),1,ABMP("VTYP"),0)),"^",8)
 .S:ABMR(10,90)="" ABMR(10,60)=$P($G(^AUTNINS(ABMP("INS"),15,ABMP("LDFN"),0)),"^",2)
 .I ABMR(10,90)="" D
 ..D DIQ1
 ..S ABMR(10,90)=ABM(9999999.06,ABMP("LDFN"),.22,"E")
 ..Q
 .S ABMR(10,90)=$TR(ABMR(10,90),"-")
 S ABMR(10,90)=$$FMT^ABMERUTL(ABMR(10,90),15)
 Q
100 ;63-68 Provider UPIN-USIN ID
 S ABMR(10,100)=$$UPIN^ABMEEPRV(ABMAPRV)
 S ABMR(10,100)=$$FMT^ABMERUTL(ABMR(10,100),6)
 Q
110 ;69-74 Filler
 S ABMR(10,110)=""
 S ABMR(10,110)=$$FMT^ABMERUTL(ABMR(10,110),6)
 Q
120 ;75-89 Medicaid Provider Number (SOURCE: FILE=9999999.181501, FIELD=.02)
 S ABMR(10,120)=""
 I ABMP("ITYPE")="D"!(ABMP("ITYPE")="K") S ABMR(10,120)=$G(ABMR(10,20))
 S ABMR(10,120)=$$FMT^ABMERUTL(ABMR(10,120),15)
 Q
 ;
130 ;90-104 Champus Insurer Provider Number
 ; (SOURCE: FILE=9999999.181501, FIELD=.02)
 S ABMR(10,130)=""
 S ABMR(10,130)=$$FMT^ABMERUTL(ABMR(10,130),15)
 Q
140 ;105-119 Provider BC/BS Number
 S ABMR(10,140)=""
 I $G(ABMP("BCBS")) D
 .D DIQ1
 .S ABMR(10,140)=$P($G(^ABMNINS(ABMP("LDFN"),ABMP("INS"),1,ABMP("VTYP"),0)),U,8)
 .S:ABMR(10,140)="" ABMR(10,140)=$P($G(^ABMNINS(DUZ(2),ABMP("INS"),1,ABMP("VTYP"),0)),"^",8)
 .S:ABMR(10,140)="" ABMR(10,140)=$P($G(^AUTNINS(ABMP("INS"),15,ABMP("LDFN"),0)),"^",2)
 .S ABMR(10,140)=ABMR(10,140)_" "_$E(ABM(9999999.06,ABMP("LDFN"),.01,"E"),1,2)
 S ABMR(10,140)=$$FMT^ABMERUTL(ABMR(10,140),15)
 Q
150 ;120-134 Provider Commercial Number
 S ABMR(10,150)=""
 S ABMR(10,150)=$$FMT^ABMERUTL(ABMR(10,150),15)
 Q
160 ;135-149 Other Insurer Provider Number 1
 S ABMR(10,160)=""
 S ABMR(10,160)=$$FMT^ABMERUTL(ABMR(10,160),15)
 Q
170 ;Other Insurer Provider Number 2
 S ABMR(10,170)=""
 S ABMR(10,170)=$$FMT^ABMERUTL(ABMR(10,170),15)
 Q
180 ;165-197 Organization Name
 D DIQ2
 S ABMR(10,180)=ABM(9002274.5,1,.26,"E")
 S:ABMR(10,180)="" ABMR(10,180)=$P(^AUTTLOC(DUZ(2),0),"^",2)
 S ABMR(10,180)=$$FMT^ABMERUTL(ABMR(10,180),33)
 Q
190 ;198-217 Provider Last Name
 S ABMR(10,190)=$$LNM^ABMEEPRV(ABMAPRV)
 S ABMR(10,190)=$$FMT^ABMERUTL(ABMR(10,190),20)
 Q
200 ;218-229 Provider First Name
 S ABMR(10,200)=$$FNM^ABMEEPRV(ABMAPRV)
 S ABMR(10,200)=$$FMT^ABMERUTL(ABMR(10,200),12)
 Q
210 ;230-230 Provider MI
 S ABMR(10,210)=$$MI^ABMEEPRV(ABMAPRV)
 S ABMR(10,210)=$$FMT^ABMERUTL(ABMR(10,210),1)
 Q
220 ;231-233 Provider Specialty
 ; ABM*2.4*9 IHS/FCS/DRS 09/21/01 ; Part 4b - call $$ENVSPEC instead of $$SPEC, 
 S ABMR(10,220)=$$ENVSPEC^ABMEEPRV(ABMAPRV)
 S ABMR(10,220)=$$FMT^ABMERUTL(ABMR(10,220),3)
 Q
230 ;234-248 Specialty License Number
 S ABMR(10,230)=""
 S ABMR(10,230)=$$FMT^ABMERUTL(ABMR(10,230),15)
 Q
240 ;249-263 State License Number
 S ABMR(10,240)=$$SLN^ABMEEPRV(ABMAPRV)
 S ABMR(10,240)=$$FMT^ABMERUTL(ABMR(10,240),15)
 Q
250 ;264-278 Dentist License Number
 S ABMR(10,250)=""
 S ABMR(10,250)=$$FMT^ABMERUTL(ABMR(10,250),15)
 Q
260 ;279-293 Anesthesia License Number
 S ABMR(10,260)=""
 S ABMR(10,260)=$$FMT^ABMERUTL(ABMR(10,260),15)
 Q
270 ;294-306 Filler (National Use)
 S ABMR(10,270)=""
 S ABMR(10,270)=$$FMT^ABMERUTL(ABMR(10,270),13)
 Q
280 ;307-320 Filler (Local Use)
 S ABMR(10,280)=""
 S ABMR(10,280)=$$FMT^ABMERUTL(ABMR(10,280),14)
 Q
DIQ1 ;PULL LOCATION DATA VIA DIQ1
 Q:$D(ABM(9999999.06,ABMP("LDFN")))
 N I
 S DIQ="ABM("
 S DIQ(0)="IE"
 S DIC="^AUTTLOC("
 S DA=ABMP("LDFN")
 S DR=".01;.21;.22"
 D EN^DIQ1
 S ABMP("PAYDFN")=$P($G(^ABMDPARM(DUZ(2),1,2)),"^",3)
 S:'$D(^AUTTLOC(+ABMP("PAYDFN"),0)) ABMP("PAYDFN")=ABMP("LDFN")
 S DA=ABMP("PAYDFN")
 S DR=".13;.14;.15;.16;.17;.21"
 D EN^DIQ1
 K DIQ
 Q
 ;
DIQ2 ;GET SITE PARAMETER INFO    
 Q:$D(ABM(9002274.5,DUZ(2)))
 N I
 S DIQ="ABM("
 S DIQ(0)="E"
 S DIC="^ABMDPARM(DUZ(2),"
 S DA=1
 S DR=.26
 D EN^DIQ1 K DIQ
 Q
 ;
EX(ABMX,ABMY) ;EXTRINSIC FUNCTION HERE
 ;
 ;  INPUT:  ABMX = data element
 ;             Y = bill internal entry number
 ;
 ; OUTPUT:     Y = bill internal entry number
 ;
 S ABMP("BDFN")=ABMY
 D SET^ABMERUTL
 I '$G(ABMP("NOFMT")) S ABMP("FMT")=0
 D @ABMX
 S Y=ABMR(20,ABMX)
 K ABMR(20,ABMX),ABME,ABM,ABMX,ABMY
 I $D(ABMP("FMT")) S ABMP("FMT")=1
 Q Y
