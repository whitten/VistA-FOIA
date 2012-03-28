BGP5D41 ; IHS/CMI/LAB - indicator 3 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;
 ;IHS/CMI/LAB - patch 1 7-7-03 added HDL and Triglyceride check to 30-1
 ;  - fixed IMM refusal check for null reason
I29 ;EP
 S BGPHTN="",BGPIHD=""
 S (BGPN1,BGPN2,BGPN3,BGPN4,BGPN5,BGPN6,BGPN7,BGPD1,BGPD2,BGPD3,BGPD4)=0
 I BGPACTCL,'BGPDM1,BGPAGEB>45 S BGPD1=1
 I BGPACTUP,'BGPDM1,BGPAGEB>45 S BGPD2=1
 I BGPDMD2,BGPAGEB>45 S BGPD3=1
 I BGPACTCL,$$V2IHD^BGP5D9(DFN,BGP365,BGPEDATE),$$FIRSTIHD^BGP5D9(DFN,BGPEDATE) S BGPIHD=1,BGPD4=1
 I '(BGPD1+BGPD2+BGPD3+BGPD4) S BGPSTOP=1 Q
 S BGPBP=$$MEANBP(DFN,$$FMADD^XLFDT(BGPEDATE,-(2*365)),BGPEDATE)
 S BGPN1=$S($P(BGPBP,U,2):1,1:0)
 S BGPLDL=$$LDL^BGP5D2(DFN,$$FMADD^XLFDT(BGPEDATE,-(5*365)),BGPEDATE) ;date^value
 I $P(BGPLDL,U) S BGPN2=1  ;had ldl done
 S BGPTOB=$$TOBACCO(DFN,BGP365,BGPEDATE)
 I $P(BGPTOB,U) S BGPN3=1
 S BGPBMI=$$BMI^BGP5D6(DFN,BGPEDATE,BGPAGEE),BGPN4=$S(BGPBMI]"":1,1:0)
 S BGPLIFE=$$LIFE(DFN,BGP365,BGPEDATE),BGPN5=$S($P(BGPLIFE,U):1,1:0)
 S BGPDEP=$$DEP(DFN,BGP365,BGPEDATE),BGPN6=$S($P(BGPDEP,U):1,1:0)
 I BGPN1,BGPN2,BGPN3,BGPN4,BGPN5,BGPN6 S BGPN7=1
 S BGPDV=$S(BGPD2:"UP;",1:"")_$S(BGPD1:"AC;",1:"")_$S(BGPD3:"AD;",1:"")_$S(BGPD4:"IHD",1:"")
 S BGPVALUE=BGPDV_"|||"_$S(BGPN7:"ALL  ",1:"")_$S(BGPN1:"BP: "_$P(BGPBP,U),1:"")
 S %=$S($P(BGPLDL,U,2):"LDL: "_$$DATE^BGP5UTL($P(BGPLDL,U,2)),1:"")
 I %]"" S BGPVALUE=BGPVALUE_" "_%
 S %=$S($P(BGPTOB,U):"TOB: "_$$DATE^BGP5UTL($P(BGPTOB,U,3))_":"_$P(BGPTOB,U,2),1:"")
 I %]"" S BGPVALUE=BGPVALUE_" "_%
 S %=$S($P(BGPBMI,U)]"":"BMI: "_$J(BGPBMI,5,1),1:"")
 I %]"" S BGPVALUE=BGPVALUE_" "_%
 S %=$S($P(BGPLIFE,U):"LIFE: "_$$DATE^BGP5UTL($P(BGPLIFE,U,2))_":"_$P(BGPLIFE,U,3),1:"")
 I %]"" S BGPVALUE=BGPVALUE_" "_%
 S %=$S($P(BGPDEP,U):"DEP: "_$P(BGPDEP,U,2),1:"")
 I %]"" S BGPVALUE=BGPVALUE_" "_%
 K BGPBP,BGPLDL,BGPTOB,BGPBMI,BGPLIFE,BGPDEP,BGPDV
 Q
I27 ;EP
 S (BGPN1,BGPN2,BGPN3,BGPN4,BGPN5,BGPD1,BGPD2,BGPD3,BGPD4,BGPD5)=0
 I BGPAGEB<20 S BGPSTOP=1 Q  ;no need to process
 I BGPACTUP S BGPD1=1
 I BGPACTCL S BGPD2=1
 I BGPACTCL,$$V2IHD^BGP5D9(DFN,BGP365,BGPEDATE),$$FIRSTIHD^BGP5D9(DFN,BGPEDATE) S BGPIHD=1,BGPD3=1
 S BGPVALUE=$$MEANBP(DFN,$$FMADD^XLFDT(BGPEDATE,-(2*365)),BGPEDATE)
 S BGPN1=$S($P(BGPVALUE,U,2):1,1:0)  ;any value 2-6
 S BGPN2=$S($P(BGPVALUE,U,2)=2:1,1:0)
 S BGPN3=$S($P(BGPVALUE,U,2)=3:1,1:0)
 S BGPN4=$S($P(BGPVALUE,U,2)=4:1,1:0)
 S BGPN5=$S($P(BGPVALUE,U,2)=5:1,1:0)
 S BGPN6=$S($P(BGPVALUE,U,2)=6:1,1:0)
 S BGPVALUE=$S(BGPD1:"UP;",1:"")_$S(BGPD2:"AC;",1:"")_$S(BGPD3:"IHD",1:"")_"|||"_$P(BGPVALUE,U)
 Q
DEP(P,BDATE,EDATE) ;EP
 S X=$$DEP^BGP5D21(P,BDATE,EDATE) I $P(X,U) Q X
 S X=$$DEPSCR^BGP5D21(P,BDATE,EDATE) I $P(X,U) Q X
 Q ""
LIFE(P,BDATE,EDATE) ;EP
 S X=$$MEDNUTR^BGP5D71(P,BDATE,EDATE) I X]"" Q 1_U_X
 S X=$$SPECNUTR^BGP5D71(P,BDATE,EDATE) I X]"" Q 1_U_X
 S X=$$SPECEX^BGP5D71(P,BDATE,EDATE) I X]"" Q 1_U_X
 S X=$$OTHREL^BGP5D71(P,BDATE,EDATE) I X]"" Q 1_U_X
 Q ""
TOBACCO(P,BDATE,EDATE) ;EP
 S X=$$TOBACCO^BGP5D7(P,BDATE,EDATE) I $P(X,U) Q 1_U_X
 S X=$$TOBDX(P,BDATE,EDATE) I X]"" Q 1_U_X
 S X=$$PED^BGP5D7(P,BDATE,EDATE) I X]"" Q 1_U_X
 S X=$$DENT^BGP5D7(P,BDATE,EDATE) I X]"" Q 1_U_X
 Q ""
MEANBP(P,BDATE,EDATE) ;EP
 S X=$$BPS(P,BDATE,EDATE,"I")
 S S=$$SYSMEAN(X) I S="" Q "unknown^^"
 S DS=$$DIAMEAN(X) I DS="" Q "unknown^^"
 I S>159!(DS>99) Q S_"/"_DS_" STG 2 HTN"_U_6
 I S>139&(S<160)!(DS>89&(DS<100)) Q S_"/"_DS_" STG 1 HTN"_U_5
 I S>129&(S<140)!(DS>79&(DS<90)) Q S_"/"_DS_" PRE STG II"_U_4
 I S>119&(S<130)!(DS=80) Q S_"/"_DS_" PRE STG 1"_U_3
 I S<120&(DS<80) Q S_"/"_DS_" NORMAL"_U_2
 Q ""
 ;
SYSMEAN(X) ;EP
 I X="" Q ""
 S C=0 F Y=1:1:2 I $P(X,";",Y)]"" S C=C+1
 I C'=2 Q ""
 S C=0 F Y=1:1:2 S C=$P($P(X,";",Y),"/")+C
 Q C\2
 ;
DIAMEAN(X) ;EP
 I X="" Q ""
 S C=0 F Y=1:1:2 I $P(X,";",Y)]"" S C=C+1
 I C'=2 Q ""
 S C=0 F Y=1:1:2 S C=$P($P(X,";",Y),"/",2)+C
 Q C\2
 ;
BPS(P,BDATE,EDATE,F) ;EP ;
 I $G(F)="" S F="E"
 S BGPGLL=0,BGPGV=""
 K BGPG
 S X=P_"^LAST 40 MEAS BP;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE) S E=$$START1^APCLDF(X,"BGPG(")
 S BGPGL=0 F  S BGPGL=$O(BGPG(BGPGL)) Q:BGPGL'=+BGPGL!(BGPGLL=2)  S BGPGBP=$P($G(BGPG(BGPGL)),U,2) D
 .Q:$$CLINIC^APCLV($P(BGPG(BGPGL),U,5),"C")=30
 .S BGPGLL=BGPGLL+1
 .I F="E" S $P(BGPGV,";",BGPGLL)=BGPGBP_"  "_$$FMTE^XLFDT($P(BGPG(BGPGL),U))
 .I F="I" S $P(BGPGV,";",BGPGLL)=$P(BGPGBP," ")
 Q BGPGV
TOBDX(P,BDATE,EDATE) ;EP
 K BGPG
 S X=P_"^LAST DX 305.1;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE) S E=$$START1^APCLDF(X,"BGPG(")
 I $D(BGPG(1)) Q $P(BGPG(1),U,2)_U_$P(BGPG(1),U)
 S X=P_"^LAST DX V15.82;DURING "_$$FMTE^XLFDT(BDATE)_"-"_$$FMTE^XLFDT(EDATE) S E=$$START1^APCLDF(X,"BGPG(")
 I $D(BGPG(1)) Q $P(BGPG(1),U,2)_U_$P(BGPG(1),U)
 S X=0,G="" F  S X=$O(^AUPNPROB("AC",P,X)) Q:X'=+X!(G]"")  D
 .Q:$P(^AUPNPROB(X,0),U,12)'="A"
 .Q:$P(^AUPNPROB(X,0),U,3)>EDATE
 .Q:$P(^AUPNPROB(X,0),U,3)<BDATE
 .S Y=$P(^AUPNPROB(X,0),U)
 .I $P($$ICDDX^ICDCODE(Y),U,2)'="305.1",$P($$ICDDX^ICDCODE(Y),U,2)'="V15.82" Q
 .S G=$P($$ICDDX^ICDCODE(Y),U,2)_" PL"
 .Q
 Q G
