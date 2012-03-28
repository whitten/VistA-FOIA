BGPMUAP3 ; IHS/MSC/MGH - MU EP  measures NQF0055 and NQF0056 ;05-Jul-2011 11:28;DU
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;This rouine does the printed output of these 2 EP measures
 ;Get printout for diabetic eye exam
EYE ;EP
 D P1
 K ^TMP("BGPMU0055")
 Q
P1 ;Write individual measure
 N X,Y,Z,LIST1,LIST2,LIST3
 N DEN1,NUM1,DEN2,NUM2,PC1,PC2,EXC1,EXC2,STRING1,STRING2,STRING3,SUMCT
 N PRD1,PRD2,PRD3,PRD4,PRD5,PRD6,PRN1,PRN2,PRN3,PRN4,PRN5,PRN6,PRD7,PRD8,PRN7,PRN8
 S SUMCT=0
 S STRING1=$$NUM55("C")
 S STRING2=$$NUM55("P")
 S STRING3=$$NUM55("B")
 D SUMMARY1(STRING1,STRING2,STRING3)
 ;First population
 S PRD1=$P(STRING1,U,5)-$P(STRING2,U,5)
 S PRD2=$P(STRING1,U,6)-$P(STRING2,U,6)
 S PRD3=$P(STRING1,U,7)-$P(STRING2,U,7)
 S PRD7=$P(STRING1,U,9)-$P(STRING2,U,9)
 S PRN1=$P(STRING1,U,5)-$P(STRING3,U,5)
 S PRN2=$P(STRING1,U,6)-$P(STRING3,U,6)
 S PRN3=$P(STRING1,U,7)-$P(STRING3,U,7)
 S PRN7=$P(STRING1,U,9)-$P(STRING3,U,9)
 D HEADER^BGPMUPH Q:BGPQUIT
 D HDRBLK^BGPMUPH
 I $Y>(BGPIOSL-3) D HEADER^BGPMUPH,HDRBLK^BGPMUPH Q:BGPQUIT
 W !,"Pts 18-75 w/diabetes",?33,$P(STRING1,U,1),?44,$P(STRING2,U,1),?64,$P(STRING3,U,1)
 I $Y>(BGPIOSL-3) D HEADER^BGPMUPH,HDRBLK^BGPMUPH Q:BGPQUIT
 W !,"# Excluded (Exc)",?33,$P(STRING1,U,4),?44,$P(STRING2,U,4),?64,$P(STRING3,U,4)
 I $Y>(BGPIOSL-3) D HEADER^BGPMUPH,HDRBLK^BGPMUPH Q:BGPQUIT
 W !,"Pts 18-75 w/diabetes less Exc",?33,$P(STRING1,U,3),?44,$P(STRING2,U,3),?64,$P(STRING3,U,3)
 I $Y>(BGPIOSL-3) D HEADER^BGPMUPH,HDRBLK^BGPMUPH Q:BGPQUIT
 W !!,"# w/eye exam",?33,$P(STRING1,U,2),?38,$J($P(STRING1,U,5),5,1),?44,$P(STRING2,U,2),?49,$J($P(STRING2,U,5),5,1),?56,$J($FN(PRD1,",+",1),6),?64,$P(STRING3,U,2),?68,$J($P(STRING3,U,5),5,1),?74,$J($FN(PRN1,",+",1),6)
 I $Y>(BGPIOSL-3) D HEADER^BGPMUPH,HDRBLK^BGPMUPH Q:BGPQUIT
 W !,"# w/o eye exam",?33,$P(STRING1,U,8),?38,$J($P(STRING1,U,9),5,1),?44,$P(STRING2,U,8),?49,$J($P(STRING2,U,9),5,1),?56,$J($FN(PRD7,",+",1),6),?64,$P(STRING3,U,8),?68,$J($P(STRING3,U,9),5,1),?74,$J($FN(PRN7,",+",1),6)
 I $Y>(BGPIOSL-3) D HEADER^BGPMUPH,HDRBLK^BGPMUPH Q:BGPQUIT
 I $D(BGPLIST(BGPIC)) D P2
 ;
 Q
NUM55(TF) ;Get the numbers for this measure
 N ARRAY,DEN,NUM,EXC,NOT,PC1,PC11,PC2,PC13,NNUM,PC14
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
 N PT,NODE,NAME,VST,BMI,FOL,X,PTCT,BGPARR,LINE
 S PTCT=0
 D HEADERL^BGPMUPH
 S X="Diabetic patients 18-75 years of age with at least 1 or 2 encounters with the EP" D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="during the reporting period, who had a retinal or dilated eye exam during the" D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="reporting period or a negative retinal exam(no evidence of retinopathy) during" D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="the year prior to the reporting period, if any." D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X=""  D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="Patients who did not meet the numerator criteria are listed first (NM:)," D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="followed by patients who do meet the numerator criteria (M:).  Excluded patients" D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="are listed last."  D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X=""  D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="The following are the abbreviations used in the denominator and numerator" D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="columns:" D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="ICD=Diabetes Diagnosis" D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="MED=Medication Indicative of Diabetes" D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="EN=Encounter" D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="" D W^BGPMUPP(X,0,1,BGPPTYPE)
 I $Y>(BGPIOSL-5) D HEADERL^BGPMUPH Q:BGPQUIT
 W !,"PATIENT NAME",?23,"HRN",?30,"COMMUNITY",?42,"SEX",?46,"AGE",?50,"DENOMINATOR",?65,"NUMERATOR"
 S LINE="",$P(LINE,"-",81)="" W !,LINE
 I BGPLIST="D"!(BGPLIST="A") D
 .K BGPARR
 .D PTLSORT^BGPMUUTL(.BGPARR,"^TMP(""BGPMU0055"","_$J_",""PAT"",""C"",""NOT"")")
 .S PT=0 F  S PT=$O(BGPARR(PT)) Q:PT=""  D
 ..S PTCT=PTCT+1
 ..S NODE=$G(BGPARR(PT))
 ..D DATA(NODE)
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
 W !!,"Total # of patients on list: "_PTCT
 Q
DATA(NODE) ;GET DATA
 N NAME,HRN,DEN,NUM,AGE,DFN,SEX,COMM,NUM1,NUM2,DEN1,DEN2,DEN3,LINE
 S DFN=$P(NODE,U,1)
 S NAME=$E($$GET1^DIQ(2,$P(NODE,U,1),.01),1,22)
 S HRN=$$HRN^AUPNPAT(DFN,DUZ(2))
 S AGE=$$AGE^AUPNPAT(DFN,BGPED)
 S DEN=$P(NODE,U,2)
 S DEN1=$P(DEN,";",1),DEN2=$P(DEN,";",2),DEN3=$P(DEN,";",3)
 S NUM=$P(NODE,U,3)
 S NUM1=$P(NUM,";",1),NUM2=$$DATE^BGPMUAP3($P($P(NUM,";",2),".",1))
 S COMM=$E($$GET1^DIQ(9000001,DFN,1118),1,11)
 S SEX=$P(^DPT(DFN,0),U,2)
 I $Y>(BGPIOSL-2) D
 .D HEADERL^BGPMUPH Q:BGPQUIT
 .W !,"PATIENT NAME",?23,"HRN",?30,"COMMUNITY",?42,"SEX",?46,"AGE",?50,"DENOMINATOR",?65,"NUMERATOR"
 .S LINE="",$P(LINE,"-",81)="" W !,LINE
 W !,NAME,?23,HRN,?30,COMM,?43,SEX,?46,AGE,?50,DEN1,?65,NUM1
 I DEN2'="" D
 .W !,?50,DEN2,?65,NUM2
 I DEN3'="" D
 .W !,?50,DEN3
 Q
DATA2(NODE) ;GET DATA
 N NAME,HRN,DEN,SEX,COMM,DEN1,DEN2,DEN3,NUM1,NUM2,LINE
 S DFN=$P(NODE,U,1)
 S NAME=$E($$GET1^DIQ(2,$P(NODE,U,1),.01),1,18)
 S HRN=$$HRN^AUPNPAT(DFN,DUZ(2))
 S AGE=$$AGE^AUPNPAT(DFN,BGPED)
 S SEX=$P(^DPT(DFN,0),U,2)
 S COMM=$E($$GET1^DIQ(9000001,DFN,1118),1,11)
 S DEN=$P(NODE,U,2)
 S DEN1=$P(DEN,";",1),DEN2=$P(DEN,";",2),DEN3=$P(DEN,";",3)
 S NUM=$P(NODE,U,3)
 S NUM1=$P(NUM,";",1),NUM2=$$DATE^BGPMUAP3($P($P(NUM,";",2),".",1))
 I $Y>(BGPIOSL-2) D
 .D HEADERL^BGPMUPH Q:BGPQUIT
 .W !,"PATIENT NAME",?23,"HRN",?30,"COMMUNITY",?42,"SEX",?46,"AGE",?50,"DENOMINATOR",?65,"NUMERATOR"
 .S LINE="",$P(LINE,"-",81)="" W !,LINE
 W !,NAME,?23,HRN,?30,COMM,?43,SEX,?46,AGE,?50,DEN1,?65,NUM1
 I DEN2'="" D
 .W !,?50,DEN2,?65,NUM2
 I DEN3'="" D
 .W !,?50,DEN3
 Q
 ;Get printout for Diabetic foot measure
FOOT ;EP
 D FT1
 K ^TMP("BGPMU0056")
 Q
FT1 ;Write individual measure
 N X,Y,Z,DEN,NUM,PC,STRING1,STRING2,PRD,PRD2,PRN,PRN1,PRN2,PRD1,PRD3,PRN3
 S STRING1=$$FT56("C")
 S STRING2=$$FT56("P")
 S STRING3=$$FT56("B")
 D SUMMARY2(STRING1,STRING2,STRING3)
 S PRD=$P(STRING1,U,5)-$P(STRING2,U,5)
 S PRD1=$P(STRING1,U,6)-$P(STRING2,U,6)
 S PRD2=$P(STRING1,U,7)-$P(STRING2,U,7)
 S PRD3=$P(STRING1,U,9)-$P(STRING2,U,9)
 S PRN=$P(STRING1,U,5)-$P(STRING3,U,5)
 S PRN1=$P(STRING1,U,6)-$P(STRING3,U,6)
 S PRN2=$P(STRING1,U,7)-$P(STRING3,U,7)
 S PRN3=$P(STRING1,U,9)-$P(STRING3,U,9)
 D HEADER^BGPMUPH Q:BGPQUIT
 D HDRBLK^BGPMUPH
 I $Y>(BGPIOSL-3) D HEADER^BGPMUPH,HDRBLK^BGPMUPH Q:BGPQUIT
 W !,"Pts 18-75 w/diabetes",?33,$P(STRING1,U,1),?44,$P(STRING2,U,1),?64,$P(STRING3,U,1)
 I $Y>(BGPIOSL-3) D HEADER^BGPMUPH,HDRBLK^BGPMUPH Q:BGPQUIT
 W !,"# Excluded (Exc)",?33,$P(STRING1,U,4),?44,$P(STRING2,U,4),?64,$P(STRING3,U,4)
 I $Y>(BGPIOSL-3) D HEADER^BGPMUPH,HDRBLK^BGPMUPH Q:BGPQUIT
 W !,"Pts 18-75 w/diabetes less Exc",?33,$P(STRING1,U,3),?44,$P(STRING2,U,3),?64,$P(STRING3,U,3)
 I $Y>(BGPIOSL-3) D HEADER^BGPMUPH,HDRBLK^BGPMUPH Q:BGPQUIT
 W !!,"# w/foot exam",?33,$P(STRING1,U,2),?38,$J($P(STRING1,U,5),5,1),?44,$P(STRING2,U,2),?49,$J($P(STRING2,U,5),5,1),?56,$J($FN(PRD,",+",1),6),?64,$P(STRING3,U,2),?68,$J($P(STRING3,U,5),5,1),?74,$J($FN(PRN,",+",1),6)
 I $Y>(BGPIOSL-3) D HEADER^BGPMUPH,HDRBLK^BGPMUPH Q:BGPQUIT
 W !,"# w/o foot exam",?33,$P(STRING1,U,8),?38,$J($P(STRING1,U,9),5,1),?44,$P(STRING2,U,8),?49,$J($P(STRING2,U,9),5,1),?56,$J($FN(PRD3,",+",1),6),?64,$P(STRING3,U,8),?68,$J($P(STRING3,U,9),5,1),?74,$J($FN(PRN3,",+",1),6)
 I $Y>(BGPIOSL-3) D HEADER^BGPMUPH,HDRBLK^BGPMUPH Q:BGPQUIT
 I $D(BGPLIST(BGPIC)) D FT2
 Q
FT56(TF) ;Get the numbers for this measure
 N ARRAY,DEN,NUM,PC1,PC2,NOT,NNUM,EXC,PC3,PC4
 S DEN=+$G(^TMP("BGPMU0056",$J,TF,"DEN"))
 S NUM=+$G(^TMP("BGPMU0056",$J,TF,"NUM"))
 S NOT=+$G(^TMP("BGPMU0056",$J,TF,"NOT"))
 S EXC=+$G(^TMP("BGPMU0056",$J,TF,"EXC"))
 S NNUM=DEN-EXC
 I DEN=0 S (PC1,PC2,PC3,PC4)=0
 I DEN>0&(NNUM=0) D
 .S (PC1,PC11,PC14)=0
 .S PC13=$$ROUND^BGPMUA01((EXC/DEN),3)*100
 I DEN>0&(NNUM>0) D
 .S PC1=$$ROUND^BGPMUA01((NUM/DEN),3)*100
 .S PC2=$$ROUND^BGPMUA01((NNUM/DEN),3)*100
 .S PC3=$$ROUND^BGPMUA01((EXC/DEN),3)*100
 .S PC4=$$ROUND^BGPMUA01((NOT/NNUM),3)*100
 S ARRAY=DEN_U_NUM_U_NNUM_U_EXC_U_PC1_U_PC2_U_PC3_U_NOT_U_PC4
 Q ARRAY
FT2 ;Do the Details
 N PT,NODE,NAME,BP,PTCT,BGPARR,LINE
 D HEADERL^BGPMUPH
 S X="Diabetic patients 18-75 years of age with at least 1 or 2 encounters with the EP" D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="during the reporting period, who had a foot exam (visual inspection, sensory" D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="exam with monofilament, or pulse exam), if any." D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="" D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="Patients who do not meet the numerator criteria are listed first (NM:), followed" D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="by patients who do meet the numerator criteria (M:).  Excluded patients are" D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="listed last." D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="" D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="The following are the abbreviations used in the denominator and numerator" D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="columns:" D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="ICD=Diabetes Diagnosis" D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="MED=Medication Indicative of Diabetes"  D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X="EN=Encounter"  D W^BGPMUPP(X,0,1,BGPPTYPE)
 S X=""  D W^BGPMUPP(X,0,1,BGPPTYPE)
 S PTCT=0
 I $Y>(BGPIOSL-5) D HEADERL^BGPMUPH Q:BGPQUIT
 W !,"PATIENT NAME",?23,"HRN",?30,"COMMUNITY",?42,"SEX",?46,"AGE",?50,"DENOMINATOR",?70,"NUMERATOR"
 S LINE="",$P(LINE,"-",81)="" W !,LINE
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
 W !!,"Total # of patients on list: "_PTCT
 Q
SUMMARY1(STRING1,STRING2,STRING3,CT) ;Summmary setup
 N DESC,DESC2,LINE
 K ^TMP("BGPMU SUMMARY",$J,BGPIC)
 S ^TMP("BGPMU SUMMARY",$J,BGPIC)="0055^117"
 S DESC="18-75 # w/eye exam"
 S LINE=""
 S LINE="MU.EP.0055.1"_U_DESC_U_$P(STRING1,U,4)_U_$P(STRING1,U,1)_U_$P(STRING1,U,2)_U_$P(STRING1,U,5)_U_U_U_U_U
 S LINE=LINE_$P(STRING2,U,4)_U_$P(STRING2,U,1)_U_$P(STRING2,U,2)_U_$P(STRING2,U,5)_U_$P(STRING3,U,4)_U_$P(STRING3,U,1)_U_$P(STRING3,U,2)_U_$P(STRING3,U,5)
 S ^TMP("BGPMU SUMMARY",$J,BGPIC,1)=LINE
 Q
SUMMARY2(STRING1,STRING2,STRING3) ;Summary setup
 N DESC
 K ^TMP("BGPMU SUMMARY",$J,BGPIC)
 S ^TMP("BGPMU SUMMARY",$J,BGPIC)="0056^163"
 S DESC="18-75 # w/foot exam"
 S LINE="MU.EP.0056.1"_U_DESC_U_$P(STRING1,U,4)_U_$P(STRING1,U,1)_U_$P(STRING1,U,2)_U_$P(STRING1,U,5)_U_U_U_U_U
 S LINE=LINE_$P(STRING2,U,4)_U_$P(STRING2,U,1)_U_$P(STRING2,U,2)_U_$P(STRING2,U,5)_U_$P(STRING3,U,4)_U_$P(STRING3,U,1)_U_$P(STRING3,U,2)_U_$P(STRING3,U,5)
 S ^TMP("BGPMU SUMMARY",$J,BGPIC,1)=LINE
 Q
DATE(D) ;EP
 I D="" Q ""
 Q $E(D,4,5)_"/"_$E(D,6,7)_"/"_$E(D,2,3)
