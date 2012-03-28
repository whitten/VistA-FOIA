BGOVTM ; IHS/BAO/TMD - Manage V ACTIVITY TIME ;20-Mar-2007 13:52;DKM
 ;;1.1;BGO COMPONENTS;**1,3**;Mar 20, 2007
 ; Get visit activity time
 ;  INP = Visit IEN ^ User IEN
 ; .RET = Activity Time IEN ^ Activity Time ^ Travel minutes
GET(RET,INP) ;EP
 N VIEN,UIEN,ACTTM,TRVTM,X
 S VIEN=+$G(INP)
 I 'VIEN S RET=$$ERR^BGOUTL(1002) Q
 S UIEN=$P(INP,U,2)
 I 'UIEN S RET=$$ERR^BGOUTL(1102) Q
 I '$D(^VA(200,UIEN,0)) S RET=$$ERR^BGOUTL(1103) Q
 S (ACTTM,TRVTM)=0
 S X=0
 F  S X=$O(^AUPNVTM("AD",VIEN,X)) Q:'X  I $P($G(^AUPNVTM(X,12)),U,4)=UIEN D  Q
 .S ACTTM=$P($G(^AUPNVTM(X,0)),U),TRVTM=$P($G(^(0)),U,4)
 S RET=+X_U_ACTTM_U_TRVTM
 Q
 ; Add/edit V ACTIVITY file
 ;  INP = V File IEN [1] ^ Visit IEN [2] ^ Provider IEN [3] ^ Activity Time [4] ^ Travel Minutes [5]
SET(RET,INP) ;EP
 N VIEN,PRVIEN,VFIEN,VFNEW,FNUM,TYPE,TRVTM,FDA
 S RET="",FNUM=$$FNUM
 S VFIEN=+INP
 S VFNEW='VFIEN
 I VFIEN,'$D(^AUPNVTM(VFIEN,0)) S RET=$$ERR^BGOUTL(1104) Q
 S VIEN=$P(INP,U,2)
 S RET=$$CHKVISIT^BGOUTL(VIEN)
 Q:RET
 S PRVIEN=+$P(INP,U,3)
 I 'PRVIEN S RET=$$ERR^BGOUTL(1027) Q
 I VFIEN,$P($G(^AUPNVTM(VFIEN,12)),U,4)'=PRVIEN S RET=$$ERR^BGOUTL(1105) Q
 S TYPE=+$P(INP,U,4)
 S TRVTM=$P(INP,U,5)
 I 'VFIEN D  Q:'VFIEN
 .D VFNEW^BGOUTL2(.RET,FNUM,TYPE,VIEN)
 .S:RET>0 VFIEN=RET,RET=""
 S FDA=$NA(FDA(FNUM,VFIEN_","))
 S @FDA@(.01)=TYPE
 S @FDA@(.04)=TRVTM
 S @FDA@(1201)="N"
 S @FDA@(1204)="`"_PRVIEN
 S RET=$$UPDATE^BGOUTL(.FDA,"E")
 I RET,VFNEW,$$DELETE^BGOUTL(FNUM,VFIEN)
 D:'RET VFEVT^BGOUTL2(FNUM,VFIEN,'VFNEW)
 S:'RET RET=VFIEN
 Q
 ; Delete an activity time entry
DEL(RET,VFIEN) ;EP
 D VFDEL^BGOUTL2(.RET,$$FNUM,VFIEN)
 Q
 ; Return V File #
FNUM() Q 9000010.19
