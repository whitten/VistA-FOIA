ABME540 ; IHS/ASDST/DMJ - UB92 V5 EMC RECORD 40 (Claim Data) ; 
 ;;2.6;IHS 3P BILLING SYSTEM;;NOV 12, 2009
 ;Original;DMJ;08/18/95 10:39 AM
 ;
START ;START HERE
 K ABMR(40),ABMREC(40)
 S ABME("RTYPE")=40
 D SET^ABMERUTL
 S ABME("S#")=1
 D LOOP
 D S90^ABMERUTL
 K ABM
 Q
 ;
LOOP ;LOOP HERE
 D ^ABME540A
 F I=180:10:280 D
 .D @I
 .I $D(^ABMEXLM("AA",+$G(ABMP("INS")),+$G(ABMP("EXP")),40,I)) D @(^(I))
 .I '$G(ABMP("NOFMT")) S ABMREC(40,ABME("S#"))=$G(ABMREC(40,ABME("S#")))_ABMR(40,I)
 Q
 ;
180 ;Occurrence Code 6, 132-133 (SOURCE: FILE=9002274.4051, FIELD=.01)
 ; form locator #33b
 D GET51
 S ABMR(40,180)=$P($G(ABM(51,6)),U)
 S ABMR(40,180)=$$FMT^ABMERUTL(ABMR(40,180),"2")
 Q
 ;
190 ;Occurrence Date 6 (SOURCE: FILE=9002274.4051, FIELD=.02)     
 ; form locator #33b
 D GET51
 S Y=$P($G(ABM(51,6)),"^",2)
 S ABMR(40,190)=$$Y2KD2^ABMDUTL(Y)
 S ABMR(40,190)=$$FMT^ABMERUTL(ABMR(40,190),8)
 Q
 ;
200 ;Occurrence Code 7, 142-143 (SOURCE: FILE=9002274.4051, FIELD=.01)
 ; form locator #34b
 D GET51
 S ABMR(40,200)=$P($G(ABM(51,7)),U)
 S ABMR(40,200)=$$FMT^ABMERUTL(ABMR(40,200),"2")
 Q
 ;
210 ;Occurrence Date 7, 144-151 (SOURCE: FILE=9002274.4051, FIELD=.02)
 ; form locator #34b
 D GET51
 S Y=$P($G(ABM(51,7)),"^",2)
 S ABMR(40,210)=$$Y2KD2^ABMDUTL(Y)
 S ABMR(40,210)=$$FMT^ABMERUTL(ABMR(40,210),8)
 Q
 ;
220 ;Occurrence Span Code 1, 152-153 (SOURCE: FILE=9002274.4057, FIELD=.01)
 ; form locator #36a
 D GET57
 S ABMR(40,220)=$P(ABM(57,1),U)
 S ABMR(40,220)=$$FMT^ABMERUTL(ABMR(40,220),2)
 Q
 ;
230 ;Occurrence Span From Date 1, 154-161 (SOURCE: FILE=9002274.4057, FIELD=.02)
 ; form locator #36a
 D GET57
 S Y=$P(ABM(57,1),"^",2)
 S ABMR(40,230)=$$Y2KD2^ABMDUTL(Y)
 S ABMR(40,230)=$$FMT^ABMERUTL(ABMR(40,230),8)
 Q
 ;
240 ;Occurrence Span Through Date 1, 162-169 (SOURCE: FILE=9002274.4057, FIELD=.03)
 ; form locator #36a
 D GET57
 S Y=$P(ABM(57,1),"^",3)
 S ABMR(40,240)=$$Y2KD2^ABMDUTL(Y)
 S ABMR(40,240)=$$FMT^ABMERUTL(ABMR(40,240),8)
 Q
 ;
250 ;Occurrence Span Code 2, 170-171 (SOURCE: FILE=9002274.4057, FIELD=.01)
 ; form locator #36b
 D GET57
 S ABMR(40,250)=$P(ABM(57,2),U)
 S ABMR(40,250)=$$FMT^ABMERUTL(ABMR(40,250),2)
 Q
 ;
260 ;Occurrence Span From Date 2, 172-179 (SOURCE: FILE=9002274.4057, FIELD=.02)
 ; form locator #36b
 D GET57
 S Y=$P(ABM(57,2),"^",2)
 S ABMR(40,260)=$$Y2KD2^ABMDUTL(Y)
 S ABMR(40,260)=$$FMT^ABMERUTL(ABMR(40,260),8)
 Q
 ;
270 ;Occurrence Span Through Date 2, 180-187 (SOURCE: FILE=9002274.4057, FIELD=.03)
 ; form locator #36b
 D GET57
 S Y=$P(ABM(57,2),"^",3)
 S ABMR(40,270)=$$Y2KD2^ABMDUTL(Y)
 S ABMR(40,270)=$$FMT^ABMERUTL(ABMR(40,270),8)
 Q
 ;
280 ;Filler (National Use) 188-192
 S ABMR(40,280)=""
 S ABMR(40,280)=$$FMT^ABMERUTL(ABMR(40,280),5)
 Q
 ;
DIQ1 ;PULL POLICY HOLDER DATA VIA DIQ1
 Q:$D(ABM(9002274.4,ABMP("BDFN"),ABME("FLD")))
 N I
 S DIQ="ABM("
 S DIQ(0)="EI"
 S DIC="^ABMDBILL(DUZ(2),"
 S DA=ABMP("BDFN")
 S DR=".02;.54;.55;.56;.58;.76;.77;.78;.79"
 D EN^DIQ1
 K DIQ
 Q
 ;
EX(ABMX,ABMY) ;EXTRINSIC FUNCTION HERE
 ;
 ;  INPUT: ABMX = data element
 ;            Y = bill internal entry number
 ;
 ; OUTPUT:    Y = bill internal entry number
 ;
 S ABMP("BDFN")=ABMY
 D SET^ABMERUTL
 I '$G(ABMP("NOFMT")) S ABMP("FMT")=0
 D @ABMX
 S Y=ABMR(40,ABMX)
 I $D(ABMP("FMT")) S ABMP("FMT")=1
 K ABMR(40,ABMX),ABME,ABMX,ABMY,ABMZ,ABM
 Q Y
 ;
GET51 ;GET OCCURANCE CODES
 Q:$D(ABM(51))
 N I
 S I=0,CNT=0
 F  S I=$O(^ABMDBILL(DUZ(2),ABMP("BDFN"),51,I)) Q:'I  D
 .S CNT=CNT+1
 .S ABM(51,CNT)=^ABMDBILL(DUZ(2),ABMP("BDFN"),51,I,0)
 .S $P(ABM(51,CNT),U)=$P($G(^ABMDCODE(+ABM(51,CNT),0)),U)
 .S:$L(ABM(51,CNT))=1 ABM(51,CNT)="0"_ABM(51,CNT)
 F I=1:1:10 I '$D(ABM(51,I)) S ABM(51,I)=""
 Q
 ;
GET57 ;GET OCCURRENCE SPAN CODES
 Q:$D(ABM(57))
 N I
 S I=0,CNT=0
 F  S I=$O(^ABMDBILL(DUZ(2),ABMP("BDFN"),57,I)) Q:'I  D
 .S CNT=CNT+1
 .S ABM(57,CNT)=^ABMDBILL(DUZ(2),ABMP("BDFN"),57,I,0)
 .S $P(ABM(57,CNT),U)=$P($G(^ABMDCODE(+ABM(57,CNT),0)),U)
 F I=1,2 I '$D(ABM(57,I)) S ABM(57,I)=""
 Q
