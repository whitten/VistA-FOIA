BGPMUAD3 ; IHS/MSC/MGH - Print MU measures NQF0055 and NQF0056 ;05-Jul-2011 18:16;MGH
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;Delimeted output
EYE(CNT) ;EP
 D P1
 K ^TMP("BGPMU0055")
 Q
P1 ;Write individual measure
 N X,Y,Z,LIST1,LIST2,LIST3
 N DEN1,NUM1,DEN2,NUM2,PC1,PC2,EXC1,EXC2,STRING1,STRING2,STRING3
 N PRD1,PRD2,PRD3,PRD4,PRD5,PRD6,PRD7,PRD8,PRN1,PRN2,PRN3,PRN4,PRN5,PRN6,PRN7,PRN8
 S STRING1=$$NUM55("C")
 S STRING2=$$NUM55("P")
 S STRING3=$$NUM55("B")
 D SUMMARY1^BGPMUAP3(STRING1,STRING2,STRING3)
 ;First population
 S PRD1=$P(STRING1,U,5)-$P(STRING2,U,5) S PRD1=$FN(PRD1,",+",1)
 S PRD2=$P(STRING1,U,6)-$P(STRING2,U,6) S PRD2=$FN(PRD2,",+",1)
 S PRD3=$P(STRING1,U,7)-$P(STRING2,U,7) S PRD3=$FN(PRD3,",+",1)
 S PRD7=$P(STRING1,U,9)-$P(STRING2,U,9) S PRD7=$FN(PRD7,",+",1)
 S PRN1=$P(STRING1,U,5)-$P(STRING3,U,5) S PRN1=$FN(PRN1,",+",1)
 S PRN2=$P(STRING1,U,6)-$P(STRING3,U,6) S PRN2=$FN(PRN2,",+",1)
 S PRN3=$P(STRING1,U,7)-$P(STRING3,U,7) S PRN3=$FN(PRN3,",+",1)
 S PRN7=$P(STRING1,U,9)-$P(STRING3,U,9) S PRN7=$FN(PRN7,",+",1)
 S X=U_"REPORT PERIOD"_U_"%"_U_"PREV YR PERIOD"_U_"%"_U_"CHG FROM PREV YR"_U_"BASE YEAR"_U_"%"_U_"CHG FROM BASE"
 D S^BGPMUDEL(X,2,1)
 S X="Pts 18-75 w/diabetes"_U_$P(STRING1,U,1)_U_U_$P(STRING2,U,1)_U_U_U_$P(STRING3,U,1) D S^BGPMUDEL(X,1,1)
 S X="# Excluded (Exc)"_U_$P(STRING1,U,4)_U_U_$P(STRING2,U,4)_U_U_U_$P(STRING3,U,4)
 D S^BGPMUDEL(X,1,1)
 S X="Pts 18-75 w/diabetes - Exc"_U_$P(STRING1,U,3)_U_U_$P(STRING2,U,3)_U_U_U_$P(STRING3,U,3)
 D S^BGPMUDEL(X,1,1)
 S X="# w/eye exam"_U_$P(STRING1,U,2)_U_$J($P(STRING1,U,5),5,1)_U_$P(STRING2,U,2)_U_$J($P(STRING2,U,5),5,1)_U_$FN(PRD1,",+",1)_U_$P(STRING3,U,2)_U_$J($P(STRING3,U,5),5,1)_U_$FN(PRN1,",+",1)
 D S^BGPMUDEL(X,2,1)
 S X="# w/o eye exam"_U_$P(STRING1,U,8)_U_$J($P(STRING1,U,9),5,1)_U_$P(STRING2,U,8)_U_$J($P(STRING2,U,9),5,1)_U_$FN(PRD7,",+",1)_U_$P(STRING3,U,8)_U_$J($P(STRING3,U,9),5,1)_U_$FN(PRN7,",+",1)
 D S^BGPMUDEL(X,1,1)
 I $D(BGPLIST(BGPIC)) D P2
 Q
NUM55(TF) ;Get the numbers for this measure
 N ARRAY,DEN,NUM,EXC,NOT,PC1,PC11,PC2,PC14,PC21,PC13,NNUM
 S DEN=+$G(^TMP("BGPMU0055",$J,TF,"DEN"))
 S NUM=+$G(^TMP("BGPMU0055",$J,TF,"NUM"))
 S NOT=+$G(^TMP("BGPMU0055",$J,TF,"NOT"))
 S EXC=+$G(^TMP("BGPMU0055",$J,TF,"EXC"))
 S NNUM=DEN-EXC
 I DEN=0 S (PC1,PC11,PC13,PC14)=0
 I DEN>0&(NNUM=0) D
 .S (PC1,PC11,PC14)=0
 .S PC13=$$ROUND^BGPMUA01((EXC/DEN),3)*100
 I DEN>0&(NNUM>0) D
 .S PC1=$$ROUND^BGPMUA01((NUM/NNUM),3)*100
 .S PC11=$$ROUND^BGPMUA01((NNUM/DEN),3)*100
 .S PC13=$$ROUND^BGPMUA01((EXC/DEN),3)*100
 .S PC14=$$ROUND^BGPMUA01((NOT/NNUM),3)*100
 S ARRAY=DEN_U_NUM_U_NNUM_U_EXC_U_PC1_U_PC11_U_PC13_U_NOT_U_PC14
 Q ARRAY
P2 ;Do the Details
 N PT,NODE,NAME,VST,BMI,FOL,X,PTCT
 S PTCT=0
 S X="**** CONFIDENTIAL PATIENT INFORMATION COVERED BY PRIVACY ACT ****" D S^BGPMUDEL(X,2,1)
 S X="Diabetic patients 18-75 years of age with at least 1 or 2 encounters with the EP" D S^BGPMUDEL(X,1,1)
 S X="during the reporting period, who had a retinal or dilated eye exam during the" D S^BGPMUDEL(X,1,1)
 S X="reporting period or a negative retinal exam (no evidence of retinopathy) during" D S^BGPMUDEL(X,1,1)
 S X="the year prior to the reporting period, if any" D S^BGPMUDEL(X,1,1)
 S X="" D S^BGPMUDEL(X,1,1)
 S X="Patients who did not meet the numerator criteria are listed first (NM:)," D S^BGPMUDEL(X,1,1)
 S X="followed by patients who do meet the numerator criteria (M:).  Excluded patients" D S^BGPMUDEL(X,1,1)
 S X="are listed last." D S^BGPMUDEL(X,1,1)
 S X="" D S^BGPMUDEL(X,1,1)
 S X="The following are the abbreviations used in the denominator and numerator" D S^BGPMUDEL(X,1,1)
 S X="columns: " D S^BGPMUDEL(X,1,1)
 S X="ICD-9 Code=Diabetes Diagnosis" D S^BGPMUDEL(X,1,1)
 S X="MED=Medication Indicative of Diabetes" D S^BGPMUDEL(X,1,1)
 S X="EN=Encounter" D S^BGPMUDEL(X,1,1)
 S X="PATIENT NAME"_U_"HRN"_U_"COMMUNITY"_U_"SEX"_U_"AGE"_U_"DENOMINATOR"_U_"NUMERATOR" D S^BGPMUDEL(X,2,1)
 I BGPLIST="D"!(BGPLIST="A") D
 .K BGPARR
 .D PTLSORT^BGPMUUTL(.BGPARR,"^TMP(""BGPMU0055"","_$J_",""PAT"",""C"",""NOT"")")
 .S PT=0 F  S PT=$O(BGPARR(PT)) Q:PT=""  D
 ..S PTCT=PTCT+1
 ..S NODE=$G(BGPARR(PT))
 ..D DATA2(NODE)
 I BGPLIST="N"!(BGPLIST="A") D
 .K BGPARR
 .D PTLSORT^BGPMUUTL(.BGPARR,"^TMP(""BGPMU0055"","_$J_",""PAT"",""C"",""NUM"")")
 .S PT=0 F  S PT=$O(BGPARR(PT)) Q:PT=""  D
 ..S PTCT=PTCT+1
 ..S NODE=$G(BGPARR(PT))
 ..D DATA(NODE)
 I BGPLIST="A" D
 .K BGPARR
 .D PTLSORT^BGPMUUTL(.BGPARR,"^TMP(""BGPMU0055"","_$J_",""PAT"",""C"",""EXC"")")
 .S PT=0 F  S PT=$O(BGPARR(PT)) Q:PT=""  D
 ..S PTCT=PTCT+1
 ..S NODE=$G(BGPARR(PT))
 ..D DATA(NODE)
 S X="Total # of patients on list: "_PTCT D S^BGPMUDEL(X,2,1)
 Q
DATA(NODE) ;GET DATA
 N NAME,HRN,DEN,NUM,AGE,DFN,SEX,COMM,NUM1,NUM2
 S DFN=$P(NODE,U,1)
 S NAME=$E($$GET1^DIQ(2,$P(NODE,U,1),.01),1,30)
 S HRN=$$HRN^AUPNPAT(DFN,DUZ(2))
 S AGE=$$AGE^AUPNPAT(DFN,BGPED)
 S COMM=$$GET1^DIQ(9000001,DFN,1118)
 S SEX=$P(^DPT(DFN,0),U,2)
 S DEN=$P(NODE,U,2)
 S NUM=$P(NODE,U,3)
 S NUM1=$P(NUM,";",1),NUM2=$$DATE^BGPMUAP3($P($P(NUM,";",2),".",1))
 S X=NAME_U_HRN_U_COMM_U_SEX_U_AGE_U_DEN_U_NUM1_" "_NUM2 D S^BGPMUDEL(X,1,1)
 Q
DATA2(NODE) ;GET DATA
 N NAME,HRN,DEN,AGE,DFN,SEX,COMM,NUM,NUM1,NUM2
 S DFN=$P(NODE,U,1)
 S NAME=$E($$GET1^DIQ(2,$P(NODE,U,1),.01),1,30)
 S HRN=$$HRN^AUPNPAT(DFN,DUZ(2))
 S AGE=$$AGE^AUPNPAT(DFN,BGPED)
 S COMM=$$GET1^DIQ(9000001,DFN,1118)
 S SEX=$P(^DPT(DFN,0),U,2)
 S DEN=$P(NODE,U,2),NUM=$P(NODE,U,3)
 S NUM1=$P(NUM,";",1),NUM2=$$DATE^BGPMUAP3($P($P(NUM,";",2),".",1))
 S X=NAME_U_HRN_U_COMM_U_SEX_U_AGE_U_DEN_U_NUM1_" "_NUM2 D S^BGPMUDEL(X,1,1)
 Q
 ;Get printout for BP measure
FOOT ;EP
 D FT1
 K ^TMP("BGPMU0056")
 Q
FT1 ;Write individual measure
 N X,Y,Z,DEN,NUM,PC,STRING1,STRING2,PRD,PRN,PRD1,PRN1,PRD2,PRD3,PRN3,PRN3
 S STRING1=$$NUM0056("C")
 S STRING2=$$NUM0056("P")
 S STRING3=$$NUM0056("B")
 D SUMMARY2^BGPMUAP3(STRING1,STRING2,STRING3)
 S PRD=$P(STRING1,U,5)-$P(STRING2,U,5) S PRD=$FN(PRD,",+",1)
 S PRD1=$P(STRING1,U,6)-$P(STRING2,U,6) S PRD1=$FN(PRD1,",+",1)
 S PRD2=$P(STRING1,U,7)-$P(STRING2,U,7) S PRD2=$FN(PRD2,",+",1)
 S PRD3=$P(STRING1,U,9)-$P(STRING2,U,9) S PRD3=$FN(PRD3,",+",1)
 S PRN=$P(STRING1,U,5)-$P(STRING3,U,5) S PRN=$FN(PRN,",+",1)
 S PRN1=$P(STRING1,U,6)-$P(STRING3,U,6) S PRN1=$FN(PRN1,",+",1)
 S PRN2=$P(STRING1,U,7)-$P(STRING3,U,7) S PRN2=$FN(PRN2,",+",1)
 S PRN3=$P(STRING1,U,9)-$P(STRING3,U,9) S PRN3=$FN(PRN3,",+",1)
 S X=U_"REPORT PERIOD"_U_"%"_U_"PREV YR PERIOD"_U_"%"_U_"CHG FROM PREV YR"_U_"BASE YEAR"_U_"%"_U_"CHG FROM BASE"
 D S^BGPMUDEL(X,2,1)
 S X="Pts 18-75 w/diabetes"_U_$P(STRING1,U,1)_U_U_$P(STRING2,U,1)_U_U_U_$P(STRING3,U,1)
 D S^BGPMUDEL(X,1,1)
 S X="# Excluded (Exc)"_U_$P(STRING1,U,4)_U_U_$P(STRING2,U,4)_U_U_U_$P(STRING3,U,4)
 D S^BGPMUDEL(X,1,1)
 S X="Pts 18-75 w/diabetes - Exc"_U_$P(STRING1,U,3)_U_U_$P(STRING2,U,3)_U_U_U_$P(STRING3,U,3)
 D S^BGPMUDEL(X,1,1)
 S X="# w/foot exam"_U_$P(STRING1,U,2)_U_$J($P(STRING1,U,5),5,1)_U_$P(STRING2,U,2)_U_$J($P(STRING2,U,5),5,1)_U_$FN(PRD,",+",1)_U_$P(STRING3,U,2)_U_$J($P(STRING3,U,5),5,1)_U_$FN(PRN,",+",1)
 D S^BGPMUDEL(X,2,1)
 S X="# w/o foot exam"_U_$P(STRING1,U,8)_U_$J($P(STRING1,U,9),5,1)_U_$P(STRING2,U,8)_U_$J($P(STRING2,U,9),5,1)_U_$FN(PRD3,",+",1)_U_$P(STRING3,U,8)_U_$J($P(STRING3,U,9),5,1)_U_$FN(PRN3,",+",1)
 D S^BGPMUDEL(X,1,1)
 I $D(BGPLIST(BGPIC)) D FT2
 Q
NUM0056(TF) ;Get the numbers for this measure
 N ARRAY,DEN,NUM,PC1,PC2,NOT,EXC
 S DEN=+$G(^TMP("BGPMU0056",$J,TF,"DEN"))
 S NUM=+$G(^TMP("BGPMU0056",$J,TF,"NUM"))
 S NOT=+$G(^TMP("BGPMU0056",$J,TF,"NOT"))
 S EXC=+$G(^TMP("BGPMU0056",$J,TF,"EXC"))
 S NNUM=DEN-EXC
 I DEN=0 S (PC1,PC11,PC13,PC14)=0
 I DEN>0&(NNUM=0) D
 .S (PC1,PC11,PC14)=0
 .S PC13=$$ROUND^BGPMUA01((EXC/DEN),3)*100
 I DEN>0&(NNUM>0) D
 .S PC1=$$ROUND^BGPMUA01((NUM/NNUM),3)*100
 .S PC11=$$ROUND^BGPMUA01((NNUM/DEN),3)*100
 .S PC13=$$ROUND^BGPMUA01((EXC/DEN),3)*100
 .S PC14=$$ROUND^BGPMUA01((NOT/NNUM),3)*100
 S ARRAY=DEN_U_NUM_U_NNUM_U_EXC_U_PC1_U_PC11_U_PC13_U_NOT_U_PC14
 Q ARRAY
FT2 ;Do the Details
 N PT,NODE,NAME,BP,PTCT
 S X="**** CONFIDENTIAL PATIENT INFORMATION COVERED BY PRIVACY ACT ****" D S^BGPMUDEL(X,2,1)
 S X="Diabetic patients 18-75 years of age with at least 1 or 2 encounters with the EP" D S^BGPMUDEL(X,1,1)
 S X="during the reporting period, who had a foot exam (visual inspection, sensory" D S^BGPMUDEL(X,1,1)
 S X="exam with monofilament, or pulse exam), if any." D S^BGPMUDEL(X,1,1)
 S X="" D S^BGPMUDEL(X,1,1)
 S X="Patients who do not meet the numerator criteria are listed first (NM:), followed" D S^BGPMUDEL(X,1,1)
 S X="by patients who do meet the numerator criteria (M:).  Excluded patients are" D S^BGPMUDEL(X,1,1)
 S X="listed last" D S^BGPMUDEL(X,1,1)
 S X="" D S^BGPMUDEL(X,1,1)
 S X="The following are the abbreviations used in the denominator and numerator" D S^BGPMUDEL(X,1,1)
 S X="columns: " D S^BGPMUDEL(X,1,1)
 S X="ICD=Diabetes Diagnosis" D S^BGPMUDEL(X,1,1)
 S X="MDC=Medication Indicative of Diabetes" D S^BGPMUDEL(X,1,1)
 S X="EN=Encounter" D S^BGPMUDEL(X,1,1)
 S X="" D S^BGPMUDEL(X,1,1)
 S PTCT=0
 S X="NAME"_U_"HRN"_U_"COMMUNITY"_U_"SEX"_U_"AGE"_U_"DENOMINATOR"_U_"NUMERATOR" D S^BGPMUDEL(X,1,1)
 I BGPLIST="D"!(BGPLIST="A") D
 .K BGPARR
 .D PTLSORT^BGPMUUTL(.BGPARR,"^TMP(""BGPMU0056"","_$J_",""PAT"",""C"",""NOT"")")
 .S PT=0 F  S PT=$O(BGPARR(PT)) Q:PT=""  D
 ..S PTCT=PTCT+1
 ..S NODE=$G(BGPARR(PT))
 ..D DATA2(NODE)
 I BGPLIST="N"!(BGPLIST="A") D
 .K BGPARR
 .D PTLSORT^BGPMUUTL(.BGPARR,"^TMP(""BGPMU0056"","_$J_",""PAT"",""C"",""NUM"")")
 .S PT=0 F  S PT=$O(BGPARR(PT)) Q:PT=""  D
 ..S PTCT=PTCT+1
 ..S NODE=$G(BGPARR(PT))
 ..D DATA2(NODE)
 I BGPLIST="A" D
 .K BGPARR
 .D PTLSORT^BGPMUUTL(.BGPARR,"^TMP(""BGPMU0056"","_$J_",""PAT"",""C"",""EXC"")")
 .S PT=0 F  S PT=$O(BGPARR(PT)) Q:PT=""  D
 ..S PTCT=PTCT+1
 ..S NODE=$G(BGPARR(PT))
 ..D DATA2(NODE)
 S X="Total # of patients on list: "_PTCT D S^BGPMUDEL(X,2,1)
 Q
XML0055 ;XML output for diabetes eye exam
 ; BGPXML(i)=Population Number^Numerator Number^Denominator Count^exclusion number
 N STRING,TOTN,TOTD,TOTE
 S STRING=$$NUM55("C")
 S TOTD=$P(STRING,U,1)
 S TOTN=$P(STRING,U,2)
 S TOTE=$P(STRING,U,4)
 S BGPXML(1)="117"_U_U_+TOTD_U_TOTN_U_TOTE
 K ^TMP("BGPMU0055",$J)
 Q
XML0056 ;XML output for diabetic foot exam
 ; BGPXML(i)=Population Number^Numerator Number^Denominator Count^exclusion number
 N STRING
 S STRING=$$NUM0056("C")
 S BGPXML(1)="163"_U_U_+$P(STRING,U,1)_U_+$P(STRING,U,2)_U_+$P(STRING,U,4)
 K ^TMP("BGPMU0056",$J)
 Q
