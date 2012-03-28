APCDR08 ; IHS/CMI/LAB - V PROCEDURE REVIEW ;
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
 ;
 S APCDEREC=^AUPNVPRC(APCDEDFN,0)
OPCODE ; Operation Code-CP 94-97
 S APCDOPTR=$P(APCDEREC,U),APCDNPTR=$P(APCDEREC,U,4)
 I APCDOPTR="" S APCDE="E007" D ERR G XIT
 I '$D(^ICD0(APCDOPTR,0)) S APCDE="E007" D ERR G XIT
 S APCDOP=$P($$ICDOP^ICDCODE(APCDOPTR),U,2)
 I $P(^ICD0(APCDOPTR,0),U,9)]"" S APCDE="E045" D ERR G XIT
 I APCDOP=.9999 S APCDE="E032" D ERR G XIT
 I $L($P(APCDOP,".",2))>2 S APCDE="E003" D ERR G XIT
 ;I $P(^ICD0(APCDOPTR,0),U,10)]"",AUPNSEX'=$P(^ICD0(APCDOPTR,0),U,10) S APCDE="E043" D ERR G XIT
 I $P($$ICDOP^ICDCODE(APCDOPTR),U,11)]"",AUPNSEX'=$P($$ICDOP^ICDCODE(APCDOPTR),U,11) S APCDE="E043" D ERR G XIT
 I $$VERSION^XPDUTL("BCSV")]"" G OPNARR  ;no age edits in csv
 G:'$D(^ICD0($P(APCDEREC,U),9999999)) OPNARR
 I $P(^ICD0($P(APCDEREC,U),9999999),U,2)]"",($P(^ICD0($P(APCDEREC,U),9999999),U,2)<AUPNDAYS),'$D(APCDACC) S APCDE="E036" D ERR G XIT
 I $P(^ICD0($P(APCDEREC,U),9999999),U)]"",($P(^ICD0($P(APCDEREC,U),9999999),U)>AUPNDAYS),'$D(APCDACC) S APCDE="E036" D ERR G XIT
 ;
OPNARR ; Operation Narrative-CP 50-93
 I APCDNPTR="" S APCDE="E006" D ERR G XIT
 I '$D(^AUTNPOV(APCDNPTR,0)) S APCDE="E006" D ERR G XIT
 ;
DXPRFM ; Diagnosis for which Operation Performed. Char Pos 98-102.
 G:$P(APCDVREC,U,7)'="H" XIT
 S APCDICD="",APCDICDP=$P(APCDEREC,U,5) I APCDICDP="" S APCDE="E044" D ERR G XIT
 I '$D(^ICD9(APCDICDP,0)) S APCDE="E044" D ERR G XIT
 S APCDICD=$P($$ICDDX^ICDCODE(APCDICDP),U,2)
 K APCDE,APCDAGEE
 D ^APCDRICD
 I $D(APCDE) D ERR G XIT
 ;
XIT ; Clean up and exit
 K APCDEREC,APCDNPTR,APCDOPTR,APCDICD,APCDAGEE,APCDICDP,APCDE,APCDOP
 Q
ERR ;
 S APCDE("FILE")=9000010.08,APCDE("ENTRY")=APCDEDFN
 D ERR^APCDRV
 Q
