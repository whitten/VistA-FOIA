APCLD205 ; IHS/CMI/LAB -IHS -CUMULATIVE REPORT ;
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
 ;
 ;
CUML ;EP
 K APCLCUML
 S APCLCUML(10)="Gender"
 S APCLCUML(20)="Age"
 S APCLCUML(25)="Diabetes Type"
 S APCLCUML(30)="Duration of Diabetes"
 S APCLCUML(40)="Weight Control (BMI) - does not add up to 100%"
 S APCLCUML(50)="Blood Sugar Control - uses last HGB A1C value"
 S APCLCUML(70)="Tuberculosis Status"
 S APCLCUML(60)="Blood Pressure Control - based on mean of last 3 bp's"
 S APCLCUML(80)="Tobacco use"
 S APCLCUML(90)="DIABETES TREATMENT"
 S APCLCUML(100)="CHRONIC ASPIRIN THERAPY"
 S APCLCUML(110)="ACE INHIBITOR (OR ARB) USE"
 S APCLCUML(115)="LIPID LOWERING AGENT USE"
 ;
PROCESS ;
 S APCLPD=0 F  S APCLPD=$O(^XTMP("APCLDM20",APCLJOB,APCLBTH,"AUDIT",APCLPD)) Q:APCLPD'=+APCLPD  D CUML1
 Q
 ;
CUML1 ;
GENDER ;
 ;gender APCLCUML(10)="Gender^total^females^males"
 S $P(APCLCUML(10),U,2)=$P(APCLCUML(10),U,2)+1
 S V=$G(^XTMP("APCLDM20",APCLJOB,APCLBTH,"AUDIT",APCLPD,20))
 S P=$S($E(V)="F":3,$E(V)="M":4,1:5)
 S $P(APCLCUML(10),U,P)=$P(APCLCUML(10),U,P)+1
AGE ;
 S V=$$AGE^AUPNPAT(APCLPD,APCLADAT)
 ;APCLCUML(20)="Age^total^<15^15-44^45-64^>65^unknown"
 S $P(APCLCUML(20),U,2)=$P(APCLCUML(20),U,2)+1
 S P=$S(V<15:3,V>14&(V<45):4,V>44&(V<65):5,V>64:6,1:7)
 S $P(APCLCUML(20),U,P)=$P(APCLCUML(20),U,P)+1
TYPE ;
 ;APCLCUML(25)="Total^Type 1^Type 2"
 S X=$$TYPE^APCLD206(APCLPD,APCLDMRG,APCLADAT)
 S $P(APCLCUML(25),U,2)=$P(APCLCUML(25),U,2)+1
 S P=$S(X="":5,X=1:3,X=2:4,1:5)
 S $P(APCLCUML(25),U,P)=$P(APCLCUML(25),U,P)+1
DURDMC ;
 ;APCLCUML(30)="Duration of Diabetes^total^<10^10 or more^do date of dx on problem list or cms register"
 S $P(APCLCUML(30),U,2)=$P(APCLCUML(30),U,2)+1
 S V=$$DURDM^APCLD204(APCLPD,APCLDMRG,APCLADAT)
 S P=$S(V="":5,V<10:3,V>9:4,1:5)
 S $P(APCLCUML(30),U,P)=$P(APCLCUML(30),U,P)+1
BMI ;
 ;APCLCUML(40)="Weight Control (BMI) - does not add up to 100%^total^total^overweight^obese^height or weight missing"
 S V=$G(^XTMP("APCLDM20",APCLJOB,APCLBTH,"AUDIT",APCLPD,112))
 S $P(APCLCUML(40),U,2)=$P(APCLCUML(40),U,2)+1
 D
 .I V="" S $P(APCLCUML(40),U,5)=$P(APCLCUML(40),U,5)+1 Q
 .I $$OW^APCLD204(APCLPD,V,APCLADAT) S $P(APCLCUML(40),U,3)=$P(APCLCUML(40),U,3)+1
 .I $$OB^APCLD204(APCLPD,V,APCLADAT) S $P(APCLCUML(40),U,4)=$P(APCLCUML(40),U,4)+1
HGB ;
 ;use last hgba1c value only
 ;APCLCUML(50)=
 S $P(APCLCUML(50),U,2)=$P(APCLCUML(50),U,2)+1
 S V=$P($G(^XTMP("APCLDM20",APCLJOB,APCLBTH,"AUDIT",APCLPD,78)),U)
 S P=$S(V="":9,V[">":8,$E(V)'=+$E(V):9,V<7.0:3,V>6.9&(V<8.0):4,V>7.9&(V<9.0):5,V>8.9&(V<10.0):6,V<11.0&(V>9.9):7,V>10.9:8,1:9)
 S $P(APCLCUML(50),U,P)=$P(APCLCUML(50),U,P)+1
BPC ;blood pressure control
 ;take last 3 bp's and get mean systolic and mean diastolic
 S $P(APCLCUML(60),U,2)=$P(APCLCUML(60),U,2)+1
 S S=$$SYSMEAN(APCLPD,APCLRBD,APCLRED)
 S D=$$DIAMEAN(APCLPD,APCLRBD,APCLRED)
 D
 .I S=""!(D="") S $P(APCLCUML(60),U,8)=$P(APCLCUML(60),U,8)+1 Q
 .I S<120&(D<80) S $P(APCLCUML(60),U,3)=$P(APCLCUML(60),U,3)+1 Q
 .I S<130&(D<85) S $P(APCLCUML(60),U,4)=$P(APCLCUML(60),U,4)+1 Q
 .I S<140&(D<90) S $P(APCLCUML(60),U,5)=$P(APCLCUML(60),U,5)+1 Q
 .I S<160&(D<95) S $P(APCLCUML(60),U,6)=$P(APCLCUML(60),U,6)+1 Q
 .S $P(APCLCUML(60),U,7)=$P(APCLCUML(60),U,7)+1
TBSTAT ;
 S $P(APCLCUML(70),U,2)=$P(APCLCUML(70),U,2)+1
 S V=$$TBCODE^APCLD206(APCLPD,APCLRED,APCLDMRG)
 S $P(APCLCUML(70),U,(V+2))=$P(APCLCUML(70),U,(V+2))+1
TOBACCO ;
 S $P(APCLCUML(80),U,2)=$P(APCLCUML(80),U,2)+1
 S V=$G(^XTMP("APCLDM20",APCLJOB,APCLBTH,"AUDIT",APCLPD,27))
 S V1=$G(^XTMP("APCLDM20",APCLJOB,APCLBTH,"AUDIT",APCLPD,28))
 I +V=1 S $P(APCLCUML(80),U,3)=$P(APCLCUML(80),U,3)+1 S P=$S($E(V1)="Y":4,$E(V1)="N":5,1:5) S $P(APCLCUML(80),U,P)=$P(APCLCUML(80),U,P)+1
 I +V=2 S $P(APCLCUML(80),U,6)=$P(APCLCUML(80),U,6)+1
 I +V=3 S $P(APCLCUML(80),U,7)=$P(APCLCUML(80),U,7)+1
DMTX ;diabetes treatment
 S APCL6MBD=$$FMADD^XLFDT(APCLADAT,-(6*31)),APCL6MBD=$$FMTE^XLFDT(APCL6MBD)
 S $P(APCLCUML(90),U,2)=$P(APCLCUML(90),U,2)+1
 S V=$$THERAPY^APCLD206(APCLPD,APCL6MBD,APCLRED)
 I V=1 S $P(APCLCUML(90),U,3)=$P(APCLCUML(90),U,3)+1
 I $L(V)=1 S P=$S(V=2:4,V=3:5,V=4:6,V=5:7,V=6:8,1:"") S $P(APCLCUML(90),U,P)=$P(APCLCUML(90),U,P)+1
 I $L(V)>1,V["2" S $P(APCLCUML(90),U,10)=$P(APCLCUML(90),U,10)+1
 I $L(V)>1,V'[2 S $P(APCLCUML(90),U,9)=$P(APCLCUML(90),U,9)+1
ASPIRIN ;
 S $P(APCLCUML(100),U,2)=$P(APCLCUML(100),U,2)+1
 S V=$G(^XTMP("APCLDM20",APCLJOB,APCLBTH,"AUDIT",APCLPD,62))
 S P=$S($E(V)="Y":3,1:4)
 S $P(APCLCUML(100),U,P)=$P(APCLCUML(100),U,P)+1
ACE ;110 title^total pts^total pts with protein^# of those on ace^# with htn^# of those on ace"
 S $P(APCLCUML(110),U,2)=$P(APCLCUML(110),U,2)+1
 ;set 3rd piece with # with proteinuria
 S P=$G(^XTMP("APCLDM20",APCLJOB,APCLBTH,"AUDIT",APCLPD,94))
 S H=$G(^XTMP("APCLDM20",APCLJOB,APCLBTH,"AUDIT",APCLPD,34))
 S A=$G(^XTMP("APCLDM20",APCLJOB,APCLBTH,"AUDIT",APCLPD,60))
 I $E(P)="Y" S $P(APCLCUML(110),U,3)=$P(APCLCUML(110),U,3)+1 I $E(A)="Y" S $P(APCLCUML(110),U,4)=$P(APCLCUML(110),U,4)+1
 I $E(H)="Y" S $P(APCLCUML(110),U,5)=$P(APCLCUML(110),U,5)+1 I $E(A)="Y" S $P(APCLCUML(110),U,6)=$P(APCLCUML(110),U,6)+1
LIPID ;115
 S V=$G(^XTMP("APCLDM20",APCLJOB,APCLBTH,"AUDIT",APCLPD,61))
 S L=$$LDL^APCLD208(APCLPD,APCLRBD,APCLRED,"I"),L=$P(L,U)
 S T=$$CHOL^APCLD208(APCLPD,APCLRBD,APCLRED,"I"),T=$P(T,U)
 I T]"",T'<240 S $P(APCLCUML(115),U,3)=$P(APCLCUML(115),U,3)+1 I $E(V)="Y" S $P(APCLCUML(115),U,4)=$P(APCLCUML(115),U,4)+1
 I L]"",L>160 S $P(APCLCUML(115),U,5)=$P(APCLCUML(115),U,5)+1 I $E(V)="Y" S $P(APCLCUML(115),U,6)=$P(APCLCUML(115),U,6)+1
EXAMS ;
 S:'$D(APCLCUML(120)) APCLCUML(120)="EXAMS - Yearly"
 S $P(APCLCUML(120),U,2)=$P(APCLCUML(120),U,2)+1
 S V=$G(^XTMP("APCLDM20",APCLJOB,APCLBTH,"AUDIT",APCLPD,38))
 I $E(V)="Y" S $P(APCLCUML(120),U,3)=$P(APCLCUML(120),U,3)+1
 I $E(V)="R" S $P(APCLCUML(120),U,6)=$P(APCLCUML(120),U,6)+1
 S V=$G(^XTMP("APCLDM20",APCLJOB,APCLBTH,"AUDIT",APCLPD,40))
 I $E(V)="Y" S $P(APCLCUML(120),U,4)=$P(APCLCUML(120),U,4)+1
 I $E(V)="R" S $P(APCLCUML(120),U,7)=$P(APCLCUML(120),U,7)+1
 S V=$G(^XTMP("APCLDM20",APCLJOB,APCLBTH,"AUDIT",APCLPD,42))
 I $E(V)="Y" S $P(APCLCUML(120),U,5)=$P(APCLCUML(120),U,5)+1
 I $E(V)="R" S $P(APCLCUML(120),U,8)=$P(APCLCUML(120),U,8)+1
 I $P(^DPT(APCLPD,0),U,2)="F" S $P(APCLCUML(120),U,9)=$P(APCLCUML(120),U,9)+1
 S V=$G(^XTMP("APCLDM20",APCLJOB,APCLBTH,"AUDIT",APCLPD,108))
 I $E(V)="Y" S $P(APCLCUML(120),U,10)=$P(APCLCUML(120),U,10)+1
 I $E(V)="R" S $P(APCLCUML(120),U,11)=$P(APCLCUML(120),U,11)+1
EDUC ;
 S:'$D(APCLCUML(130)) APCLCUML(130)="DIABETES-RELATED EDUCATION - Yearly"
 S $P(APCLCUML(130),U,2)=$P(APCLCUML(130),U,2)+1
 S G=0,V=$G(^XTMP("APCLDM20",APCLJOB,APCLBTH,"AUDIT",APCLPD,44))
 I $E(V)="Y" S $P(APCLCUML(130),U,3)=$P(APCLCUML(130),U,3)+1 S G=1
 I $E(V)="R" S $P(APCLCUML(130),U,7)=$P(APCLCUML(130),U,7)+1
 S V=$G(^XTMP("APCLDM20",APCLJOB,APCLBTH,"AUDIT",APCLPD,46))
 I $E(V)="Y" S $P(APCLCUML(130),U,4)=$P(APCLCUML(130),U,4)+1 S G=1
 I $E(V)="R" S $P(APCLCUML(130),U,8)=$P(APCLCUML(130),U,8)+1
 S V=$G(^XTMP("APCLDM20",APCLJOB,APCLBTH,"AUDIT",APCLPD,48))
 I $E(V)="R" S $P(APCLCUML(130),U,9)=$P(APCLCUML(130),U,9)+1
 I $E(V)="Y" S $P(APCLCUML(130),U,5)=$P(APCLCUML(130),U,5)+1 S G=1
 I G S $P(APCLCUML(130),U,6)=$P(APCLCUML(130),U,6)+1
 D ^APCLD201
 Q
SYSMEAN(P,BDATE,EDATE) ;EP
 NEW X S X=$$BPS^APCLD207(P,BDATE,EDATE,"I")
 I X="" Q ""
 NEW Y,C S C=0 F Y=1:1:3 I $P(X,";",Y)]"" S C=C+1
 I C'=3 Q ""
 S C=0 F Y=1:1:3 S C=$P($P(X,";",Y),"/")+C
 Q C\3
 Q ""
DIAMEAN(P,BDATE,EDATE) ;EP
 NEW X S X=$$BPS^APCLD207(P,BDATE,EDATE,"I")
 I X="" Q ""
 NEW Y,C S C=0 F Y=1:1:3 I $P(X,";",Y)]"" S C=C+1
 I C'=3 Q ""
 S C=0 F Y=1:1:3 S C=$P($P(X,";",Y),"/",2)+C
 Q C\3
