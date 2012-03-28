BGOICDP2 ; IHS/BAO/TMD - ICD PREFERENCES MANAGER 2 ;20-Mar-2007 13:52;DKM
 ;;1.1;BGO COMPONENTS;**1,3**;Mar 20, 2007
 ; Clone ICD preferences from PCC
 ;  INP = Provider IEN ^ ICD Preference IEN
CLONEOTH(RET,INP) ;EP
 N PRV,CAT,PIEN,ICD,TXT
 S RET=""
 S PRV=+INP
 I 'PRV S RET=$$ERR^BGOUTL(1027) Q
 I '$D(^VEN(7.1,"B",PRV)) S RET=$$ERR^BGOUTL(1028) Q
 S CAT=$P(INP,U,2)
 I 'CAT S RET=$$ERR^BGOUTL(1018) Q
 I '$D(^BGOICDPR(CAT,0)) S RET=$$ERR^BGOUTL(1019) Q
 S PIEN=0
 F  S PIEN=$O(^VEN(7.1,"B",PRV,PIEN)) Q:'PIEN  D  Q:RET
 .S ICD=$P(^VEN(7.1,PIEN,0),U,2)
 .Q:ICD=""
 .S ICD=$O(^ICD9("AB",ICD,0))
 .Q:'ICD
 .Q:$O(^BGOICDPR(CAT,1,"B",ICD,0))
 .S TXT=$P(^VEN(7.1,PIEN,0),U,3)
 .S RET=$$UPDITEM^BGOPFUTL(90362.35,CAT,ICD,0,TXT)
 Q
 ; Clone a preference
 ;  INP = Pref IEN (from) ^ Pref IEN (to)
CLONE(RET,INP) ;EP
 D CLONE^BGOPFUTL(.RET,INP,90362.35)
 Q
 ; Return list of PCC+ provider names
 ; Returned as a list of records of the format:
 ;  Provider Name ^ Provider IEN
OTHCATS(RET,DUMMY) ;EP
 N PRV,CNT
 S (PRV,CNT)=0
 F  S PRV=$O(^VEN(7.1,"B",PRV)) Q:'PRV  D
 .S:$D(^VA(200,PRV,0)) CNT=CNT+1,RET(CNT)=$P(^(0),U)_U_PRV
 Q
 ; Execute query
 ;  INP = Category IEN [1] ^ Provider IEN [2] ^ Clinic IEN [3] ^ Provider Class [4] ^ Hospital Location [5] ^
 ;        Start Date [6] ^ End Date [7] ^ Max Hits [8]
QUERY(RET,INP) ;EP
 N CAT,PRV,CLN,CLS,HL,BEGDT,ENDDT,VD,VIEN,VIS,PIEN,DX,ICD,REC,ICD,TXT,CNT,MAX,X
 S RET=""
 S CAT=$P(INP,U)
 S PRV=$P(INP,U,2)
 S CLN=$P(INP,U,3)
 S CLS=$P(INP,U,4)
 S HL=$P(INP,U,5)
 S BEGDT=$P(INP,U,6)
 S ENDDT=$P(INP,U,7)
 S MAX=+$P(INP,U,8)
 I CLN="",CLS="",PRV="",HL="" S RET=$$ERR^BGOUTL(1022) Q
 S RET=$$QRYINIT^BGOPFUTL(90362.35,CAT)
 Q:RET
 S VD=$S(BEGDT:BEGDT,1:DT-20000)
 S:'ENDDT ENDDT=DT
 S CNT=0
 F  S VD=$O(^AUPNVSIT("B",VD)) Q:'VD!RET!(VD>ENDDT)  D
 .S VIEN=0
 .F  S VIEN=$O(^AUPNVSIT("B",VD,VIEN)) Q:'VIEN!RET  D
 ..S VIS=$G(^AUPNVSIT(VIEN,0))
 ..Q:VIS=""
 ..I CLN,$P(VIS,U,8)'=CLN Q
 ..I HL,$P(VIS,U,22)'=HL Q
 ..I PRV!CLS,'$$VISPRCL^BGOPFUTL(VIEN,PRV,CLS) Q
 ..S DX=0
 ..F  S DX=$O(^AUPNVPOV("AD",VIEN,DX)) Q:'DX!RET  D
 ...S CNT=CNT+1
 ...S:CNT=MAX RET=CNT
 ...S REC=^AUPNVPOV(DX,0)
 ...S ICD=+REC
 ...S TXT=$P($G(^AUTNPOV(+$P(REC,U,4),0)),U)
 ...D QRYADD^BGOPFUTL(90362.35,CAT,ICD,TXT)
 S RET=$$QRYDONE^BGOPFUTL(90362.35,CAT)
 Q
