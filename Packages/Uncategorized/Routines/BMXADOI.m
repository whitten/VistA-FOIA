BMXADOI ; IHS/CIHA/GIS - RPC CALL: GENERATE DATA FOR AN ADO DATASET ;
 ;;4.0;BMX;**1**;JUN 28, 2010;Build 12
 ; CUSTOM IDENTIFIERS
 ;
 ;
 ; 
DEMOID(DA) ; EP-RETURN RPMS DEMOGRAPHIC INFO FOR IDENTIFIER FIELD
 N SEX,DOB,CHART,AGE,TRIBE,CC,X,Y,%,STG,FMDOB,NAME,S,SSN,CSTG,LOC,ABB
 I '$D(^DPT(+$G(DA),0)) Q ""
 S S="  "
 S X=$G(^DPT(DA,0)),SEX=$P(X,U,2),Y=$P(X,U,3),NAME=$P(X,U),SSN=$P(X,U,9)
 I '$L(NAME) Q ""
 I Y,$G(DT) S AGE=(DT-Y)\10000
 I Y X ^DD("DD") S DOB=Y
 S LOC=0,CSTG=""
 F  S LOC=$O(^AUPNPAT(DA,41,LOC)) Q:'LOC  D  ; GET ALL THE CHART NUMBERS
 . S CHART=$P($G(^AUPNPAT(DA,41,LOC,0)),U,2) I '$L(CHART) Q
 . S ABB=$P($G(^AUTTLOC(LOC,0)),U,7) I '$L(ABB) Q
 . I $L(CSTG) S CSTG=CSTG_", "
 . S CSTG=CSTG_ABB_" #"_CHART
 . Q
 ;I $G(DUZ(2)) S CHART=$P($G(^AUPNPAT(DA,41,DUZ(2),0)),U,2)
 S %=$P($G(^AUPNPAT(DA,11)),U,8) I % S TRIBE=$P($G(^AUTTTRI(%,0)),U)
 S CC=$P($G(^AUPNPAT(DA,11)),U,18)
 S STG=NAME_" "
 I $L(CSTG) S STG=STG_CSTG_"  --"
 I $G(AGE),$L(SEX) S STG=STG_S_AGE_" y/o "_SEX
 I '$G(AGE),$L(SEX) S STG=STG_S_SEX
 I $L($G(DOB)) S STG=STG_S_DOB
 I $L($G(SSN)) S STG=STG_S_$E(SSN,1,3)_"-"_$E(SSN,4,5)_"-"_$E(SSN,6,9)
 I $L($G(TRIBE)) S STG=STG_S_TRIBE
 I $L($G(CC)) S STG=STG_S_CC
 Q STG
 ; 
DATE(DATE) ; TEST TRIGGER
 Q DATE
 ; 
NAME(VIEN) ; RETURN THE PATIENT'S NAME
 I '$G(VIEN) Q ""
 N DFN
 S DFN=$P($G(^AUPNVSIT(VIEN,0)),U,5) I 'DFN Q ""
 Q $$GET1^DIQ(2,DFN_",",.01)
 ; 
SEX(VIEN) ; RETURN THE PATIENT'S SEX
 I '$G(VIEN) Q ""
 N DFN
 S DFN=$P($G(^AUPNVSIT(VIEN,0)),U,5) I 'DFN Q ""
 Q $$GET1^DIQ(2,DFN_",",.02)
 ; 
HRN(VIEN) ; RETURN THE CHART NUMBER FOR VISIT TRIGGER
 I '$G(VIEN) Q ""
 N DFN,LOC
 S DFN=$P($G(^AUPNVSIT(VIEN,0)),U,5) I 'DFN Q ""
 S LOC=$P($G(^AUPNVSIT(VIEN,0)),U,6) I 'LOC Q ""
 Q $$HRN^AUPNPAT(DFN,LOC,2)
 ;
DOB(VIEN) ; RETURN THE PATIENT'S DOB
 I '$G(VIEN) Q ""
 N DFN,LOC
 S DFN=$P($G(^AUPNVSIT(VIEN,0)),U,5) I 'DFN Q ""
 Q $$DOB^AUPNPAT(DFN,"E")
 ;
SSN(VIEN) ; RETURN THE PATIENTS DOB
 I '$G(VIEN) Q ""
 N DFN,LOC
 S DFN=$P($G(^AUPNVSIT(VIEN,0)),U,5) I 'DFN Q ""
 Q $$SSN^AUPNPAT(DFN)
 ; 
VISDATE(VIEN) ; RETURN THE DATE OF THE VISIT
 I '$G(VIEN) Q ""
 N FMDT
 S FMDT=+$G(^AUPNVSIT(VIEN,0))\1 I 'FMDT Q ""
 S %=$$FMTE^XLFDT(FMDT,1)
 G TD1
 ;
TODAY(VIEN) ; RETURN TODAY'S DATE
 I '$G(DT) Q ""
 S %=$$FMTE^XLFDT(DT,1)
TD1 S %=$$UP^XLFSTR(%)
 S %=$P(%," ",1,2)_$P(%," ",3)
 Q %
 ;
