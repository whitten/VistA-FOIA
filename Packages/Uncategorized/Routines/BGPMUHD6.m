BGPMUHD6 ; IHS/MSC/SAT - Delimited MU measure VTE3 and VTE4;07-Apr-2011 13:15;DU
 ;;11.0;IHS CLINICAL REPORTING;**4**;JAN 06, 2011;Build 84
 ;Delimited output
 ; STK2^BGPMUHD1   = delimited routine for 0435 STK-2 Antithrombolytic Therapy at discharge
 ; STK3^BGPMUHD1   = delimited routine for 0436 STK-3 Anticoagulation Therapy at discharge
 ; STK4^BGPMUHD2   = delimited routine for 0437 STK-4 thrombolytic therapy within 3 hours
 ; STK5^BGPMUHD2   = delimited routine for 0438 STK-5 Antithrombolytic Therapy by end of day 2
 ; STK6^BGPMUHD3   = delimited routine for 0439 STK-6 Statin Medicine at Discharge
 ; STK8^BGPMUHD3   = delimited routine for 0440 STK-8 Educational Materials at discharge
 ; STK10^BGPMUHD4   = delimited routine for 0441 STK-10 Rehabilitation Service at discharge
 ;
 ; VTE1^BGPMUHD5 = delimited routine for 0371 VTE-1 Prophylaxis within 24 hours
 ; VTE2^BGPMUHD5 = delimited routine for 0372 VTE-2 Prophylaxis for ICU pts
 ; VTE3^BGPMUHD6 = delimited routine for 0373 VTE-3 Anticoagulation overlap therapy
 ; VTE4^BGPMUHD6 = delimited routine for 0374 VTE-4 Platelet monitoring for UFH
 ; VTE5^BGPMUHD7 = delimited routine for 0375 VTE-5 VTE discharge instructions
 ; VTE6^BGPMUHD7 = delimited routine for 0376 VTE-6 Potentially preventable VTE
 ;Delimited output for VTE Measure 0373
VTE3 ;EP
 D P1B
 K ^TMP("BGPMU0373")
 Q
P1B ;Write individual measure
 N BGPPTYPE,X,Y,Z,DEN,NUM,PC,STRING1,STRING2,PRD,PRN,PRD1,PRD6,PRN1
 S BGPPROV=""
 S BGPPTYPE="D"
 S STRING1=$$373^BGPMUHP7("C")
 S STRING2=$$373^BGPMUHP7("P")
 S STRING3=$$373^BGPMUHP7("B")
 D SUM373
 S PRD1=$P(STRING1,U,1)-$P(STRING2,U,1)
 S PRD2=$P(STRING1,U,5)-$P(STRING2,U,5)
 S PRD3=$P(STRING1,U,5)-$P(STRING3,U,5)
 S PRD4=$P(STRING1,U,6)-$P(STRING2,U,6)
 S PRD5=$P(STRING1,U,6)-$P(STRING3,U,6)
 I $P(STRING3,U,1)'=0 S PRD6=$$ROUND^BGPMUA01(($P(STRING1,U,1)/$P(STRING3,U,1)),3)*100
 E  S PRD6=0
 S X=U_"REPORT PERIOD"_U_"%"_U_"PREV YR PERIOD"_U_"%"_U_"CHG FROM PREV YR"_U_"BASE YR"_U_"%"_U_"CHG FROM BASE %"
 D S^BGPMUDEL(X,1,1)
 S X="# Discharges for Pts w/confirmed VTE"_U_$P(STRING1,U,1)_U_U_$P(STRING2,U,1)_U_U_U_$P(STRING3,U,1)_U_U
 D S^BGPMUDEL(X,1,1)
 S X="# Excluded (Exc)"_U_$P(STRING1,U,4)_U_U_$P(STRING2,U,4)_U_U_U_$P(STRING3,U,4)
 D S^BGPMUDEL(X,1,1)
 S X="# Discharges for Pts w/confirmed VTE Less Exc"_U_($P(STRING1,U,1)-$P(STRING1,U,4))_U_U_($P(STRING2,U,1)-$P(STRING2,U,4))_U_U_U_($P(STRING3,U,1)-$P(STRING3,U,4))
 D S^BGPMUDEL(X,1,1)
 S X="# w/overlap therapy at discharge"_U_$P(STRING1,U,2)_U_$J($P(STRING1,U,5),5,1)_U_$P(STRING2,U,2)_U_$J($P(STRING2,U,5),5,1)_U_$FN(PRD2,",+",1)_U_$P(STRING3,U,2)_U_$J($P(STRING3,U,5),5,1)_U_$FN(PRD3,",+",1)
 D S^BGPMUDEL(X,2,1)
 S X="# w/o overlap therapy at discharge"_U_$P(STRING1,U,3)_U_$J($P(STRING1,U,6),5,1)_U_$P(STRING2,U,3)_U_$J($P(STRING2,U,6),5,1)_U_$FN(PRD4,",+",1)_U_$P(STRING3,U,3)_U_$J($P(STRING3,U,6),5,1)_U_$FN(PRD5,",+",1)
 D S^BGPMUDEL(X,1,1)
 I $D(BGPLIST(BGPIC)) D TC
 Q
TC ;Do the Details
 N PT,NODE,NAME,BP
 S X="**** CONFIDENTIAL PATIENT INFORMATION COVERED BY PRIVACY ACT ****" D S^BGPMUDEL(X,2,1)
 S X="This measure assesses the number of patients diagnosed with confirmed VTE who" D S^BGPMUDEL(X,2,1)
 S X="received an overlap of parenteral (intravenous [IV] or subcutaneous [subcu])" D S^BGPMUDEL(X,1,1)
 S X="anticoagulation and warfarin therapy.  For patients who received less than 5 days" D S^BGPMUDEL(X,1,1)
 S X="of overlap therapy, they must be discharged on both medications.  Overlap therapy" D S^BGPMUDEL(X,1,1)
 S X="must be administered for at least 5 days with an international normalized" D S^BGPMUDEL(X,1,1)
 S X="ratio (INR) >= 2 prior to discontinuation of the parenteral anticoagulation therapy" D S^BGPMUDEL(X,1,1)
 S X="or the patient must be discharged on both medications, if any." D S^BGPMUDEL(X,1,1)
 S X="Patients who do not meet the numerator criteria are listed first (NM:), followed by patients who do meet the numerator criteria (M:).  Excluded patients are listed last."
 D S^BGPMUDEL(X,2,1)
 S X="The following are the abbreviations used in the denominator and numerator columns:"
 D S^BGPMUDEL(X,2,1)
 S X="ADM=Admit Date for Inpatient w/confirmed VTE who received warfarin" D S^BGPMUDEL(X,1,1)
 S X="OVLP DAYS=Number of inpt overlap days" D S^BGPMUDEL(X,1,1)
 S X="INR=Last INR before discharge" D S^BGPMUDEL(X,1,1)
 S X="RX=Discharged on overlap therapy" D S^BGPMUDEL(X,1,1)
 ;
 S X="PATIENT NAME"_U_"HRN"_U_"COMMUNITY"_U_"SEX"_U_"AGE"_U_"DENOMINATOR"_U_"NUMERATOR" D S^BGPMUDEL(X,2,1)
 S PTCT=0
 I (BGPLIST="A")!(BGPLIST="D")&($D(^TMP("BGPMU0373",$J,"PAT","C","NOT"))) D
 .K BGPARR
 .D PTLSORT^BGPMUUTL(.BGPARR,"^TMP(""BGPMU0373"","_$J_",""PAT"",""C"",""NOT"")")
 .S PT=0 F  S PT=$O(BGPARR(PT)) Q:PT=""  D
 ..S PTCT=PTCT+1
 ..S NODE=$G(BGPARR(PT))
 ..D D373(NODE)
 I (BGPLIST="A")!(BGPLIST="N")&($D(^TMP("BGPMU0373",$J,"PAT","C","NUM"))) D
 .K BGPARR
 .D PTLSORT^BGPMUUTL(.BGPARR,"^TMP(""BGPMU0373"","_$J_",""PAT"",""C"",""NUM"")")
 .S PT=0 F  S PT=$O(BGPARR(PT)) Q:PT=""  D
 ..S PTCT=PTCT+1
 ..S NODE=$G(BGPARR(PT))
 ..D D373(NODE)
 ;Excluded patients
 I BGPLIST="A"&($D(^TMP("BGPMU0373",$J,"PAT","C","EXC"))) D
 .D PTLSORT^BGPMUUTL(.BGPARR,"^TMP(""BGPMU0373"","_$J_",""PAT"",""C"",""EXC"")")
 .S PT=0 F  S PT=$O(BGPARR(PT)) Q:PT=""  D
 ..S PTCT=PTCT+1
 ..S NODE=$G(BGPARR(PT))
 ..D DEXC373(NODE)
 S X="Total # of patients on list: "_PTCT D S^BGPMUDEL(X,2,1)
 Q
 ;
 ;Delimited output for VTE Measure 0374
VTE4 ;EP
 D P1
 K ^TMP("BGPMU0374")
 Q
P1 ;Write individual measure
 N BGPPTYPE,X,Y,Z,DEN,NUM,PC,STRING1,STRING2,PRD,PRN,PRD1,PRD6,PRN1
 S BGPPROV=""
 S BGPPTYPE="D"
 S STRING1=$$374^BGPMUHP7("C")
 S STRING2=$$374^BGPMUHP7("P")
 S STRING3=$$374^BGPMUHP7("B")
 D SUM374
 S PRD1=$P(STRING1,U,1)-$P(STRING2,U,1)
 S PRD2=$P(STRING1,U,5)-$P(STRING2,U,5)
 S PRD3=$P(STRING1,U,5)-$P(STRING3,U,5)
 S PRD4=$P(STRING1,U,6)-$P(STRING2,U,6)
 S PRD5=$P(STRING1,U,6)-$P(STRING3,U,6)
 I $P(STRING3,U,1)'=0 S PRD6=$$ROUND^BGPMUA01(($P(STRING1,U,1)/$P(STRING3,U,1)),3)*100
 E  S PRD6=0
 S X=U_"REPORT PERIOD"_U_"%"_U_"PREV YR PERIOD"_U_"%"_U_"CHG FROM PREV YR"_U_"BASE YR"_U_"%"_U_"CHG FROM BASE %"
 D S^BGPMUDEL(X,1,1)
 S X="# Discharges for Pts w/confirmed VTE"_U_$P(STRING1,U,1)_U_U_$P(STRING2,U,1)_U_U_U_$P(STRING3,U,1)_U_U
 D S^BGPMUDEL(X,1,1)
 S X="# Excluded (Exc)"_U_$P(STRING1,U,4)_U_U_$P(STRING2,U,4)_U_U_U_$P(STRING3,U,4)
 D S^BGPMUDEL(X,1,1)
 S X="# Discharges for Pts w/confirmed VTE Less Exc"_U_($P(STRING1,U,1)-$P(STRING1,U,4))_U_U_($P(STRING2,U,1)-$P(STRING2,U,4))_U_U_U_($P(STRING3,U,1)-$P(STRING3,U,4))
 D S^BGPMUDEL(X,1,1)
 S X="# w/IV UFH therapy & platelet count"_U_$P(STRING1,U,2)_U_$J($P(STRING1,U,5),5,1)_U_$P(STRING2,U,2)_U_$J($P(STRING2,U,5),5,1)_U_$FN(PRD2,",+",1)_U_$P(STRING3,U,2)_U_$J($P(STRING3,U,5),5,1)_U_$FN(PRD3,",+",1)
 D S^BGPMUDEL(X,2,1)
 S X="# w/o IV UFH therapy & platelet count"_U_$P(STRING1,U,3)_U_$J($P(STRING1,U,6),5,1)_U_$P(STRING2,U,3)_U_$J($P(STRING2,U,6),5,1)_U_$FN(PRD4,",+",1)_U_$P(STRING3,U,3)_U_$J($P(STRING3,U,6),5,1)_U_$FN(PRD5,",+",1)
 D S^BGPMUDEL(X,1,1)
 I $D(BGPLIST(BGPIC)) D TC1
 Q
TC1 ;Do the Details
 N PT,NODE,NAME,BP
 S X="**** CONFIDENTIAL PATIENT INFORMATION COVERED BY PRIVACY ACT ****" D S^BGPMUDEL(X,2,1)
 S X="This measure assesses the number of patients diagnosed with confirmed VTE who" D S^BGPMUDEL(X,2,1)
 S X="received intravenous (IV) UFH therapy dosages AND had their platelet counts" D S^BGPMUDEL(X,1,1)
 S X="monitored using defined parameters such as a nomogram or protocol, if any." D S^BGPMUDEL(X,1,1)
 S X="Patients who do not meet the numerator criteria are listed first (NM:), followed by patients who do meet the numerator criteria (M:).  Excluded patients are listed last." D S^BGPMUDEL(X,2,1)
 S X="The following are the abbreviations used in the denominator and numerator columns:" D S^BGPMUDEL(X,2,1)
 S X="ADM=Admit date for Inpatient w/confirmed VTE receiving IV UFH therapy" D S^BGPMUDEL(X,1,1)
 S X="MED=Start date of IV UFH therapy " D S^BGPMUDEL(X,1,1)
 S X="LAB#=Date of Platelet Count Laboratory Test" D S^BGPMUDEL(X,1,1)
 S X="PATIENT NAME"_U_"HRN"_U_"COMMUNITY"_U_"SEX"_U_"AGE"_U_"DENOMINATOR"_U_"NUMERATOR" D S^BGPMUDEL(X,2,1)
 S PTCT=0
 I (BGPLIST="A")!(BGPLIST="D")&($D(^TMP("BGPMU0374",$J,"PAT","C","NOT"))) D
 .K BGPARR
 .D PTLSORT^BGPMUUTL(.BGPARR,"^TMP(""BGPMU0374"","_$J_",""PAT"",""C"",""NOT"")")
 .S PT=0 F  S PT=$O(BGPARR(PT)) Q:PT=""  D
 ..S PTCT=PTCT+1
 ..S NODE=$G(BGPARR(PT))
 ..D D374(NODE)
 I (BGPLIST="A")!(BGPLIST="N")&($D(^TMP("BGPMU0374",$J,"PAT","C","NUM"))) D
 .K BGPARR
 .D PTLSORT^BGPMUUTL(.BGPARR,"^TMP(""BGPMU0374"","_$J_",""PAT"",""C"",""NUM"")")
 .S PT=0 F  S PT=$O(BGPARR(PT)) Q:PT=""  D
 ..S PTCT=PTCT+1
 ..S NODE=$G(BGPARR(PT))
 ..D D374(NODE)
 ;Excluded patients
 I BGPLIST="A"&($D(^TMP("BGPMU0374",$J,"PAT","C","EXC"))) D
 .D PTLSORT^BGPMUUTL(.BGPARR,"^TMP(""BGPMU0374"","_$J_",""PAT"",""C"",""EXC"")")
 .S PT=0 F  S PT=$O(BGPARR(PT)) Q:PT=""  D
 ..S PTCT=PTCT+1
 ..S NODE=$G(BGPARR(PT))
 ..D DEXC374(NODE)
 S X="Total # of patients on list: "_PTCT D S^BGPMUDEL(X,2,1)
 Q
 ;
D373(NODE) ;
 N BGPI,DATA,NAME,HRN,DEN,NUM,AGE,DFN,CNT,INR,INRDT,DD
 S DFN=$P(NODE,U,1)
 S NAME=$E($$GET1^DIQ(2,$P(NODE,U,1),.01),1,23)
 S HRN=$$HRN^AUPNPAT(DFN,DUZ(2))
 S AGE=$$AGE^AUPNPAT(DFN,BGPED)
 S SEX=$$SEX^AUPNPAT(DFN)
 S COMM=$E($$GET1^DIQ(9000001,DFN,1118),1,10)
 S DEN=$P($P($P(NODE,U,2),";",1),":",1)
 S CNT=$P(NODE,U,3),INR=$P(NODE,U,4),INRDT=$P(NODE,U,5)
 S DD=$P(NODE,U,6)
 S NUMDATA=$P(NODE,U,4)  ;$P(UFHIP,U,2)_";"_PLATE1_";"_PLATE2_";"_PLATE3
 S X=NAME_U_HRN_U_COMM_U_SEX_U_AGE_U_"ADM "_$$FMTE^XLFDT(DEN,2)_$S($L($P(NODE,"U",2),";")>1:";",1:"")_U_$S(CNT'="":"M:OVLP DAYS: "_CNT,1:"NM:")
 I INR'="" S X=X_";INR: "_INR_" "_INRDT
 I DD'="" S X=X_";RX:"_$$FMTE^XLFDT($P(DD,".",1),2)
 D S^BGPMUDEL(X,1,1)
 Q
D374(NODE) ;GET DATA
 N BGPI,DATA,NAME,HRN,DEN,AGE,DFN,NUMDATA
 S DFN=$P(NODE,U,1)
 S NAME=$E($$GET1^DIQ(2,$P(NODE,U,1),.01),1,23)
 S HRN=$$HRN^AUPNPAT(DFN,DUZ(2))
 S AGE=$$AGE^AUPNPAT(DFN,BGPED)
 S SEX=$$SEX^AUPNPAT(DFN)
 S COMM=$E($$GET1^DIQ(9000001,DFN,1118),1,10)
 S DEN=$P($P($P(NODE,U,2),";",1),":",1)
 S NUMDATA=$P(NODE,U,5)  ;$P(UFHIP,U,2)_";"_PLATE1_";"_PLATE2_";"_PLATE3
 S X=NAME_U_HRN_U_COMM_U_SEX_U_AGE_U_"ADM "_$$FMTE^XLFDT(DEN,2)_$S($L($P(NODE,"U",2),";")>1:";",1:"")_U_$S(NUMDATA'="":"M:MED "_$$FMTE^XLFDT($P(NUMDATA,";",1),2),1:"NM:")
 I NUMDATA'="" F I=1:1:3 D
 .S X=X_";LAB"_I_" "_$$FMTE^XLFDT($P(NUMDATA,";",(I+1)),2)
 D S^BGPMUDEL(X,1,1)
 Q
DEXC374(NODE) ;GET DATA
 D DEXC373(NODE)
 Q
DEXC373(NODE) ;GET DATA
 N BGPI,DATA,NAME,HRN,DEN,NUM,AGE,DFN
 S DFN=$P(NODE,U,1)
 S NAME=$E($$GET1^DIQ(2,$P(NODE,U,1),.01),1,23)
 S HRN=$$HRN^AUPNPAT(DFN,DUZ(2))
 S AGE=$$AGE^AUPNPAT(DFN,BGPED)
 S SEX=$$SEX^AUPNPAT(DFN)
 S COMM=$E($$GET1^DIQ(9000001,DFN,1118),1,10)
 S DEN=$P($P($P(NODE,U,2),";",1),":",1)
 S NUM=$P(NODE,U,3)
 S X=NAME_U_HRN_U_COMM_U_SEX_U_AGE_U_"Excluded"
 D S^BGPMUDEL(X,1,1)
 Q
 ;
SUM373 ;Populate "BGPMU SUMMARY" for VTE-3 Measure 0373
 ;Initialize iCare/summary global
 N BGPDNCNT,BGPSSTR,J,N
 S J=$J
 S BGPDNCNT=0
 K ^TMP("BGPMU SUMMARY",J,BGPIC)
 S ^TMP("BGPMU SUMMARY",J,BGPIC)="0373"_U_"VTE-3"
 S N=1 ;only 1 numerator
 ;Setup summary page/iCare ^TMP global
 S CDEN1CT=+$G(^TMP("BGPMU0373",J,"C","DEN"))                 ;current
 S CNUM1CT=+$G(^TMP("BGPMU0373",J,"C","NUM"))
 S CEXC1CT=+$G(^TMP("BGPMU0373",J,"C","EXC"))
 S CMP=$S((CDEN1CT-CEXC1CT)>0:$$ROUND^BGPMUA01(CNUM1CT/(CDEN1CT-CEXC1CT),3)*100,1:0)
 S PDEN1CT=+$G(^TMP("BGPMU0373",J,"P","DEN"))                 ;previous
 S PNUM1CT=+$G(^TMP("BGPMU0373",J,"P","NUM"))
 S PEXC1CT=+$G(^TMP("BGPMU0373",J,"P","EXC"))
 S PMP=$S((PDEN1CT-PEXC1CT)>0:$$ROUND^BGPMUA01(PNUM1CT/(PDEN1CT-PEXC1CT),3)*100,1:0)
 S BDEN1CT=+$G(^TMP("BGPMU0373",J,"B","DEN"))                 ;baseline
 S BNUM1CT=+$G(^TMP("BGPMU0373",J,"B","NUM"))
 S BEXC1CT=+$G(^TMP("BGPMU0373",J,"B","EXC"))
 S BMP=$S((BDEN1CT-BEXC1CT)>0:$$ROUND^BGPMUA01(BNUM1CT/(BDEN1CT-BEXC1CT),3)*100,1:0)
 S BGPDNCNT=BGPDNCNT+1
 S BGPSSTR="MU.EP.0373."_N_U_"# w/overlap at discharge or INR>2"_U_CEXC1CT_U_CDEN1CT_U_CNUM1CT_U_CMP_U_U_U_U
 ; 11 12 13 14
 S BGPSSTR=BGPSSTR_U_PEXC1CT_U_PDEN1CT_U_PNUM1CT_U_PMP
 ; 15 16 17 18
 S BGPSSTR=BGPSSTR_U_BEXC1CT_U_BDEN1CT_U_BNUM1CT_U_BMP
 S ^TMP("BGPMU SUMMARY",J,BGPIC,BGPDNCNT)=BGPSSTR
 Q
SUM374 ;Populate "BGPMU SUMMARY" for VTE-4 Measure 0374
 ;Initialize iCare/summary global
 N BGPDNCNT,BGPSSTR,J,N
 S J=$J
 S BGPDNCNT=0
 K ^TMP("BGPMU SUMMARY",J,BGPIC)
 S ^TMP("BGPMU SUMMARY",J,BGPIC)="0374"_U_"VTE-4"
 S N=1 ;only 1 numerator
 ;Setup summary page/iCare ^TMP global
 S CDEN1CT=+$G(^TMP("BGPMU0374",J,"C","DEN"))                 ;current
 S CNUM1CT=+$G(^TMP("BGPMU0374",J,"C","NUM"))
 S CEXC1CT=+$G(^TMP("BGPMU0374",J,"C","EXC"))
 S CMP=$S((CDEN1CT-CEXC1CT)>0:$$ROUND^BGPMUA01(CNUM1CT/(CDEN1CT-CEXC1CT),3)*100,1:0)
 S PDEN1CT=+$G(^TMP("BGPMU0374",J,"P","DEN"))                 ;previous
 S PNUM1CT=+$G(^TMP("BGPMU0374",J,"P","NUM"))
 S PEXC1CT=+$G(^TMP("BGPMU0374",J,"P","EXC"))
 S PMP=$S((PDEN1CT-PEXC1CT)>0:$$ROUND^BGPMUA01(PNUM1CT/(PDEN1CT-PEXC1CT),3)*100,1:0)
 S BDEN1CT=+$G(^TMP("BGPMU0374",J,"B","DEN"))                 ;baseline
 S BNUM1CT=+$G(^TMP("BGPMU0374",J,"B","NUM"))
 S BEXC1CT=+$G(^TMP("BGPMU0374",J,"B","EXC"))
 S BMP=$S((BDEN1CT-BEXC1CT)>0:$$ROUND^BGPMUA01(BNUM1CT/(BDEN1CT-BEXC1CT),3)*100,1:0)
 S BGPDNCNT=BGPDNCNT+1
 S BGPSSTR="MU.EP.0374."_N_U_"# w/IV UFH therapy/platelet count"_U_CEXC1CT_U_CDEN1CT_U_CNUM1CT_U_CMP_U_U_U_U
 ; 11 12 13 14
 S BGPSSTR=BGPSSTR_U_PEXC1CT_U_PDEN1CT_U_PNUM1CT_U_PMP
 ; 15 16 17 18
 S BGPSSTR=BGPSSTR_U_BEXC1CT_U_BDEN1CT_U_BNUM1CT_U_BMP
 S ^TMP("BGPMU SUMMARY",J,BGPIC,BGPDNCNT)=BGPSSTR
 Q
 ;
XML0373 ;XML output for VTE Measure 0373
 ; BGPXML(i)=Population Number^Numerator Number^Denominator Count
 N STRING
 S STRING=$$373^BGPMUHP7("C")
 S BGPXML(1)="NQF_0373"_U_U_+$P(STRING,U,1)_U_+$P(STRING,U,2)
 K ^TMP("BGPMU0373",$J)
 Q
 ;
XML0374 ;XML output for VTE Measure 0374
 ; BGPXML(i)=Population Number^Numerator Number^Denominator Count
 N STRING
 S STRING=$$374^BGPMUHP7("C")
 S BGPXML(1)="NQF_0374"_U_U_+$P(STRING,U,1)_U_+$P(STRING,U,2)
 K ^TMP("BGPMU0374",$J)
 Q
