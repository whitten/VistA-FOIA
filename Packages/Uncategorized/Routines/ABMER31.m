ABMER31 ; IHS/ASDST/DMJ - UB92 EMC RECORD 31 (Third Party Payor Address) ;  
 ;;2.6;IHS 3P BILLING SYSTEM;;NOV 12, 2009
 ;Original;DMJ;08/18/95 10:38 AM
 ;
 ; ABM*2.4*9 IHS/FCS/DRS 09/21/01
 ; IHS/FCS/DRS 09/17/01 Part 11 - fix to Insured's State
 ;
START ;START HERE
 K ABMR(31),ABMREC(31,ABME("S#"))
 S ABME("RTYPE")=31       ; Record type
 D SET^ABMERUTL,LOOP
 D S90^ABMERUTL           ; Increment record type counter
 Q
 ;
LOOP ;LOOP HERE
 F I=10:10:150 D
 .D @I
 .I $D(^ABMEXLM("AA",+$G(ABMP("INS")),+$G(ABMP("EXP")),31,I)) D @(^(I))
 .I '$G(ABMP("NOFMT")) S ABMREC(31,ABME("S#"))=$G(ABMREC(31,ABME("S#")))_ABMR(31,I)
 Q
 ;
10 ;EP - Record type
 S ABMR(31,10)=31
 Q
 ;
20 ;EP - Payor Priority (SOURCE: FILE=9002274.4013, FIELD=.02)
 S ABMR(31,20)="0"_ABME("S#")
 S ABMR(31,20)=$$FMT^ABMERUTL(ABMR(31,20),2)
 Q
 ;
30 ;EP - Patient Control Number, (SOURCE: FILE=9000001.41,FIELD=.02)
 S ABMR(31,30)=$$EX^ABMER20(30,ABMP("BDFN"))
 S ABMR(31,30)=$$FMT^ABMERUTL(ABMR(31,30),20)
 Q
 ;
40 ;EP - Insured's Address Line 1 (SOURCE: FILE=2 or 9000003.1 FIELD=)
 S ABMR(31,40)=""
 I $G(ABME("PPP")) D               ; from patient file
 .S ABME("FLD")=.111
 .D DIQ4^ABMER31A
 .S ABMR(31,40)=$G(ABM(2,ABME("PPP"),.111,"E"))
 I ABMR(31,40)="",$G(ABME("PH")) D  ; else policy holder file
 .S ABME("FLD")=.09
 .D DIQ1^ABMER31A
 .S ABMR(31,40)=$G(ABM(9000003.1,+ABME("PH"),.09,"E"))
 S ABMR(31,40)=$$FMT^ABMERUTL(ABMR(31,40),18)
 Q
 ;
50 ;EP - Insured's Address Line 2 (SOURCE: FILE= FIELD=)
 S ABMR(31,50)=""
 I $G(ABME("PPP")) D
 .S ABME("FLD")=.112
 .D DIQ4^ABMER31A
 .S ABMR(31,50)=$G(ABM(2,ABME("PPP"),.112,"E"))
 S ABMR(31,50)=$$FMT^ABMERUTL(ABMR(31,50),18)
 Q
 ;
60 ;EP - Insured's City (SOURCE: FILE=, FIELD=)
 S ABMR(31,60)=""
 I $G(ABME("PPP")) D
 .S ABME("FLD")=.114
 .D DIQ4^ABMER31A
 .S ABMR(31,60)=$G(ABM(2,ABME("PPP"),.114,"E"))
 I ABMR(31,60)="",$G(ABME("PH")) D
 .S ABME("FLD")=.11
 .D DIQ1^ABMER31A
 .S ABMR(31,60)=$G(ABM(9000003.1,+ABME("PH"),.11,"E"))
 S ABMR(31,60)=$$FMT^ABMERUTL(ABMR(31,60),15)
 Q
 ;
70 ;EP - Insured's State (SOURCE: FILE=, FIELD=)
 S ABMR(31,70)=""
 S ABME("ISTATE")=""
 I $G(ABME("PPP")) D
 .S ABME("FLD")=.115
 .D DIQ4^ABMER31A
 .S ABME("ISTATE")=$G(ABM(2,ABME("PPP"),.115,"I"))
 I 'ABME("ISTATE"),$G(ABME("PH")) D
 .S ABME("FLD")=.12
 .D DIQ1^ABMER31A
 .S ABME("ISTATE")=$G(ABM(9000003.1,ABME("PH"),.12,"I"))
 S ABMR(31,70)=$P($G(^DIC(5,+ABME("ISTATE"),0)),"^",2)
 S ABMR(31,70)=$$FMT^ABMERUTL(ABMR(31,70),2)
 Q
 ;
80 ;EP - Insured's Zip (SOURCE: FILE=9999999.18, FIELD=.01)
 S ABMR(31,80)=""
 I $G(ABME("PPP")) D
 .S ABME("FLD")=.116
 .D DIQ4^ABMER31A
 .S ABMR(31,80)=$G(ABM(2,ABME("PPP"),.116,"E"))
 I ABMR(31,80)="",$G(ABME("PH")) D
 .S ABME("FLD")=.13
 .D DIQ1^ABMER31A
 .S ABMR(31,80)=$G(ABM(9000003.1,ABME("PH"),.13,"E"))
 S $E(ABMR(31,80),6,9)="0000"
 S ABMR(31,80)=$$FMT^ABMERUTL(ABMR(31,80),9)
 Q
 ;
90 ;EP - Employer Name (SOURCE: FILE=9999999.75, FIELD=.01)
 S ABMR(31,90)=""
 I $G(ABME("PPP")) D
 .S ABME("FLD")=.01
 .D DIQ3^ABMER31A
 .S ABMR(31,90)=$G(ABM(9999999.75,+DA,.01,"E"))
 I ABMR(31,90)="",$G(ABME("PH")) D
 .S ABME("FLD")=.16
 .D DIQ1^ABMER31A
 .S ABMR(31,90)=$G(ABM(9000003.1,ABME("PH"),.16,"E"))
 S ABMR(31,90)=$$FMT^ABMERUTL(ABMR(31,90),24)
 Q
 ;
100 ;EP - Employer Address (SOURCE: FILE=9999999.75, FIELD=.02)
 S ABMR(31,100)=""
 I $G(ABME("PPP")) D
 .S ABME("FLD")=.02
 .D DIQ3^ABMER31A
 .S ABMR(31,100)=$G(ABM(9999999.75,+DA,.02,"E"))
 I ABMR(31,100)="",$G(ABME("PH")) D
 .S ABME("FLD")=.021
 .D DIQ2^ABMER31A
 .S ABMR(31,100)=$G(ABM(9999999.75,+ABME("EMP"),.02,"E"))
 S ABMR(31,100)=$$FMT^ABMERUTL(ABMR(31,100),18)
 Q
 ;
110 ;EP - Employer City (SOURCE: FILE=9999999.75, FIELD=.03)
 ; form locator #66
 S ABMR(31,110)=""
 I $G(ABME("PPP")) D
 .S ABME("FLD")=.03
 .D DIQ3^ABMER31A
 .S ABMR(31,110)=$G(ABM(9999999.75,+DA,.03,"E"))
 I ABMR(31,110)="",$G(ABME("PH")) D
 .S ABME("FLD")=.03
 .D DIQ2^ABMER31A
 .S ABMR(31,110)=$G(ABM(9999999.75,+ABME("EMP"),.03,"E"))
 S ABMR(31,110)=$$FMT^ABMERUTL(ABMR(31,110),15)
 Q
 ;
120 ;EP - Employer State (SOURCE: FILE=9999999.75, FIELD=.04)     
 ; form locator #66
 I $G(ABME("PPP")) D
 .S ABME("FLD")=.04
 .D DIQ3^ABMER31A
 .S ABME("ESTATE")=$G(ABM(9999999.75,+DA,.04,"I"))
 I '$G(ABME("ESTATE")),$G(ABME("PH")) D
 .S ABME("FLD")=.04
 .D DIQ2^ABMER31A
 .S ABME("ESTATE")=$G(ABM(9999999.75,+ABME("EMP"),.04,"I"))
 S ABMR(31,120)=$S($G(ABME("ESTATE")):$P($G(^DIC(5,ABME("ESTATE"),0)),"^",2),1:"")
 S ABMR(31,120)=$$FMT^ABMERUTL(ABMR(31,120),2)
 Q
 ;
130 ;EP - Employer Zip (SOURCE: FILE=9999999.75, FIELD=.05)
 S ABMR(31,130)=""
 I $G(ABME("PPP")) D
 .S ABME("FLD")=.05
 .D DIQ3^ABMER31A
 .S ABMR(31,130)=$G(ABM(9999999.75,+DA,.05,"E"))
 I ABMR(31,130)="",$G(ABME("PH")) D
 .S ABME("FLD")=.05
 .D DIQ2^ABMER31A
 .S ABMR(31,130)=$G(ABM(9999999.75,+ABME("EMP"),.05,"E"))
 S ABMR(31,130)=$$FMT^ABMERUTL(ABMR(31,130),9)
 Q
 ;
140 ;EP - Form Locator 37 (ICN/DCN) (SOURCE: FILE=, FIELD=)
 S ABMR(31,140)=""
 S ABMR(31,140)=$$FMT^ABMERUTL(ABMR(31,140),23)
 Q
 ;
150 ;EP - Filler (National Use)
 S ABMR(31,150)=""
 S ABMR(31,150)=$$FMT^ABMERUTL(ABMR(31,150),15)
 Q
 ;
EX(ABMX,ABMY,ABMZ) ;EXTRINSIC FUNCTION HERE
 ;
 ;  INPUT:  ABMX = data element
 ;          ABMY = bill internal entry number
 ;          ABMZ = Insurer priority (1 thru 3)
 ;
 ; OUTPUT:     Y = bill internal entry number
 ;
 S ABMP("BDFN")=ABMY
 D SET^ABMERUTL
 S ABME("INS")=ABMZ
 I '$D(^ABMDBILL(DUZ(2),ABMP("BDFN"),13,"B",ABME("INS"))) S Y="" Q Y
 D ISET^ABMERINS
 I '$G(ABMP("NOFMT")) S ABMP("FMT")=0
 D @ABMX
 S Y=ABMR(31,ABMX)
 I $D(ABMP("FMT")) S ABMP("FMT")=1
 K ABMR(31,ABMX),ABME,ABMX,ABMY,ABMZ,ABM
 Q Y
