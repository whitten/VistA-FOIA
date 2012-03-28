ABME601 ; IHS/ASDST/DMJ - UB92 V5 EMC RECORD 01 (Processor Label Data) ;    
 ;;2.6;IHS 3P BILLING SYSTEM;;NOV 12, 2009
 ;Original;DMJ;07/08/96 4:53 PM Created routine
 ;
 ; IHS/ASDS/DMJ - 01/23/01 - V2.4 Patch 3 - NOIS HQW-0101-100032
 ;   Created to correct rejections for Medicare electronic trans.
 ;
 ; IHS/ASDS/SDH - 09/27/01 - V2.4 Patch 9 - NOIS XAA-0901-20095
 ;     After moving Kidscare to Page 5 from Page 7 found that there are
 ;     checks that are done for Medicaid that should also be done for
 ;     Kidscare.
 ;
 ; *********************************************************************
 ;
START ;START HERE
 K ABMREC(1),ABMR(1)
 S ABME("RTYPE")=1
 D BCBS^ABMERUTL
 D LOOP
 K ABME,ABM
 Q
LOOP ;LOOP HERE
 F I=10:10:270 D
 .D @I
 .I $D(^ABMEXLM("AA",+$G(ABMP("INS")),+$G(ABMP("EXP")),1,I)) D @(^(I))
 .I '$G(ABMP("NOFMT")) S ABMREC(1)=$G(ABMREC(1))_ABMR(1,I)
 Q
10 ;Record type, 1-2
 S ABMR(1,10)="01"
 Q
20 ;Submitter EIN, 3-12 (SOURCE: FILE=9999999.06, FIELD=.21)
 D DIQ2 S ABMR(1,20)=ABM(9999999.06,DUZ(2),.21,"E")
 S ABMR(1,20)=+$TR(ABMR(1,20),"-")
 S ABMR(1,20)=$$FMT^ABMERUTL(ABMR(1,20),"10NR")
 S ABMRT(99,20)=ABMR(1,20)
 Q
30 ;Multiple Provider Billing, 13 
 S ABMR(1,30)=1
 Q
40 ;Filler (National Use), 14-30
 S ABMR(1,40)=""
 S ABMR(1,40)=$$FMT^ABMERUTL(ABMR(1,40),17)
 Q
50 ;Receiver Type Code, 31 (SOURCE: FILE=9999999.18, FIELD=.21)
 S ABMR(1,50)=$S(ABMP("ITYPE")="W":"B",ABMP("ITYPE")="R":"C",ABMP("ITYPE")="D"!(ABMP("ITYPE")="K"):"D",ABMP("ITYPE")="P":"F",ABMP("ITYPE")="C":"H",$G(ABMP("BCBS")):"G",1:"I")
 S ABMR(1,50)=$$FMT^ABMERUTL(ABMR(1,50),1)
 Q
60 ;Receiver Identification, 32-36 (SOURCE: FILE=9999999.18, FIELD=.08)
 S ABMR(1,60)=$$RCID^ABMERUTL(ABMP("INS"))
 Q
70 ;Receiver Sub-Identification, 37-40
 S ABMR(1,70)=""
 S ABMR(1,70)=$$FMT^ABMERUTL(ABMR(1,70),4)
 Q
80 ;Filler (National Use) 41-46
 S ABMR(1,80)=""
 S ABMR(1,80)=$$FMT^ABMERUTL(ABMR(1,80),6)
 Q
90 ;Submitter Name, 47-67 (SOURCE: FILE=9999999.06, FIELD=.01)
 D DIQ2 S ABMR(1,90)=ABM(9999999.06,DUZ(2),.01,"E")
 S ABMR(1,90)=$$FMT^ABMERUTL(ABMR(1,90),21)
 Q
100 ;Submitter Address, 68-85 (SOURCE: FILE=9999999.06, FIELD=.14)
 D DIQ2 S ABMR(1,100)=ABM(9999999.06,DUZ(2),.14,"E")
 S ABMR(1,100)=$$FMT^ABMERUTL(ABMR(1,100),18)
 Q
110 ;Submitter City, 86-100 (SOURCE: FILE=9999999.06 FIELD=.15)
 D DIQ2 S ABMR(1,110)=ABM(9999999.06,DUZ(2),.15,"E")
 S ABMR(1,110)=$$FMT^ABMERUTL(ABMR(1,110),15)
 Q
120 ;Submitter State, 101-102 (SOURCE: FILE=9999999.06, FIELD=.16)
 D DIQ2 S ABMR(1,120)=ABM(9999999.06,DUZ(2),.16,"I")
 S ABMR(1,120)=$P($G(^DIC(5,+ABMR(1,120),0)),"^",2)
 S ABMR(1,120)=$$FMT^ABMERUTL(ABMR(1,120),2)
 Q
130 ;Submitter Zip, 103-111 (SOURCE: FILE=9999999.06, FIELD=.17)
 D DIQ2 S ABMR(1,130)=ABM(9999999.06,DUZ(2),.17,"E")
 S ABMR(1,130)=$$FMT^ABMERUTL(ABMR(1,130),9)
 Q
140 ;Submitter FAX Number, 112-121 (SOURCE: FILE=, FIELD=)
 S ABMR(1,140)=""
 S ABMR(1,140)=$$FMT^ABMERUTL(ABMR(1,140),"10R")
 Q
150 ;Country Code, 122-125 (SOURCE: FILE= FIELD=)
 S ABMR(1,150)=""
 S ABMR(1,150)=$$FMT^ABMERUTL(ABMR(1,150),4)
 Q
160 ;Submitter Telephone Number, 126-135 (SOURCE: FILE=, FIELD=)
 D DIQ2 S ABMR(1,160)=ABM(9999999.06,DUZ(2),.13,"E")
 S ABMR(1,160)=$TR(ABMR(1,160),"() -")
 S ABMR(1,160)=$$FMT^ABMERUTL(ABMR(1,160),"10R")
 Q
170 ;File Sequence & Serial Number,136-142 (SOURCE: FILE= FIELD=)
 S ABMR(1,170)=$P($G(^ABMDTXST(DUZ(2),+$G(ABMP("XMIT")),1)),"^",6)
 I ABMR(1,170)="" D
 .S ABMR(1,170)="0000"_$G(ABMP("XMIT"))
 .S ABMR(1,170)=$E(ABMR(1,170),$L(ABMR(1,170))-3,$L(ABMR(1,170)))
 .S ABMR(1,170)=$E(DUZ(2),$L(DUZ(2))-1,$L(DUZ(2)))_ABMR(1,170)
 .S ABMR(1,170)=ABMR(1,170)+1000000
 S ABMR(1,170)=$$FMT^ABMERUTL(ABMR(1,170),"7NR")
 Q
180 ;Test/Production Indicator, 143-146 (SOURCE: FILE=9002274.09, FIELD=.04)
 S ABMR(1,180)=$P($G(^ABMNINS(DUZ(2),ABMP("INS"),0)),"^",4)
 S ABMR(1,180)=""
 S ABMR(1,180)=$$FMT^ABMERUTL(ABMR(1,180),4)
 Q
190 ;Date of Receipt, 147-154
 S ABMR(1,190)=""
 S ABMR(1,190)=$$FMT^ABMERUTL(ABMR(1,190),8)
 Q
200 ;Processing Date, 155-162
 S ABMR(1,200)=$$Y2KD2^ABMDUTL(DT)
 S ABMR(1,200)=$$FMT^ABMERUTL(ABMR(1,200),8)
 Q
210 ;Filler (163-165)
 S ABMR(1,210)=""
 S ABMR(1,210)=$$FMT^ABMERUTL(ABMR(1,210),3)
 Q
220 ;Password (166-171)
 S ABMR(1,220)=$P($G(^ABMNINS(DUZ(2),ABMP("INS"),0)),"^",3)
 S ABMR(1,220)=$$FMT^ABMERUTL(ABMR(1,220),6)
 Q
230 ;Submitter ID (172-177)
 S ABMR(1,230)=$P($G(^ABMNINS(DUZ(2),ABMP("INS"),0)),"^",2)
 S ABMR(1,230)=$$FMT^ABMERUTL(ABMR(1,230),6)
 Q
240 ;Filler (178)
 S ABMR(1,240)=""
 S ABMR(1,240)=$$FMT^ABMERUTL(ABMR(1,240),1)
 Q
250 ;Arizona Medicaid Transmission Number (179-184)
 ;Use the last 6 digits of Batch ID
 S ABMR(1,250)=""
 I $$RCID^ABMERUTL(ABMP("INS"))=99999 S ABMR(1,250)=$E(ABMR(1,170),2,7)
 S ABMR(1,250)=$$FMT^ABMERUTL(ABMR(1,250),"6R")
 Q
260 ;Filler (185-189)
 S ABMR(1,260)=""
 S ABMR(1,260)=$$FMT^ABMERUTL(ABMR(1,260),5)
 Q
270 ;Version Code (190-192)
 S ABMR(1,270)="060"
 Q
DIQ1 ;PULL BILL DATA VIA DIQ1
 Q:$D(ABM(9002274.4,ABMP("BDFN"),ABME("FLD")))
 N I S DIQ="ABM(",DIQ(0)="EI",DIC="^ABMDBILL(DUZ(2),",DA=ABMP("BDFN")
 S DR=".01;.21;.51;.52;.53;.61;.62;.63;.64;.71;.72;.99"
 D EN^DIQ1 K DIQ
 Q
DIQ2 ;GET LOCATION INFORMATION
 Q:$D(ABM(9999999.06,DUZ(2)))
 N I S DIQ="ABM",DIQ(0)="IE",DIC="^AUTTLOC(",DA=DUZ(2)
 S DR=".01;.13;.14;.15;.16;.17;.21"
 D EN^DIQ1 K DIQ
 Q
EX(ABMX,ABMY) ;EXTRINSIC FUNCTION HERE (X=data element, Y=bill internal entry number)
 S ABMP("BDFN")=ABMY D SET^ABMERUTL
 I '$G(ABMP("NOFMT")) S ABMP("FMT")=0
 D @ABMX
 S Y=ABMR(20,ABMX)
 K ABMR(20,ABMX),ABME,ABM,ABMX,ABMY
 I $D(ABMP("FMT")) S ABMP("FMT")=1
 Q Y
