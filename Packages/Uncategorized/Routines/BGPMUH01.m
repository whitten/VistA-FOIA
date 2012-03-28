BGPMUH01 ; IHS/MSC/MGH - MI measure NQF0495&NQF0497 ED-1 ;13-May-2011 16:00;MGH
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;ED meaningful use reports
 ;Uses entries in the emergency room package
ENTRY(RPT) ;PEP  0495 Median time admission to discharge from ED
 N START,END,ERADMIT,IEN,ERDIS,DFN,ERVST,ERPRV,ERDX,EREND,ERPRIDX,DXCHK,POP,ADMIT,LIST,ADMOR
 S START=BGPBDATE
 S END=BGPEDATE_".2359"
 ;Collect the data from the ER visit
 F  S START=$O(^AMERVSIT("B",START)) Q:START=""!(START>END)  D
 .S IEN="" F  S IEN=$O(^AMERVSIT("B",START,IEN)) Q:IEN=""  D
 ..S ERADMIT=$P($G(^AMERVSIT(IEN,0)),U,1)
 ..Q:'ERADMIT
 ..S DFN=$P($G(^AMERVSIT(IEN,0)),U,2),ERVST=$P($G(^AMERVSIT(IEN,0)),U,3)
 ..Q:'DFN
 ..S BGPACTUP=$$ACTUPAP^BGPMUEPD(DFN,BGPBDATE,BGPEDATE,BGPBEN)
 ..I 'BGPACTUP,'$G(BGPXPXPX),'$G(BGPIISO) Q
 ..S ERPRV=$P($G(^AMERVSIT(IEN,0)),U,6)
 ..S ERPRIDX=$P($G(^AMERVSIT(IEN,5.1)),U,2)
 ..Q:'ERPRIDX
 ..S ERDIS=$P($G(^AMERVSIT(IEN,6)),U,1),EREND=$P($G(^AMERVSIT(IEN,6)),U,2)
 ..Q:'EREND
 ..;Set the default population for count
 ..S POP=1
 ..;Find admission and see if it was for OBS
 ..S ADMIT=$$FINDADM(DFN,ERADMIT,EREND)
 ..I ADMIT D
 ...I ADMIT=2 D
 ....S POP=2
 ....D RECSAVE(RPT,POP,.LIST,ERADMIT)
 ...;Check to see if primary dx was for behavioral health
 ...S DXCHK=$$CKDX(ERPRIDX)
 ...I +DXCHK D
 ....S POP=3    ;visits of behavioral health
 ....D RECSAVE(RPT,POP,.LIST,ERADMIT)
 ...;Add to default population if not behavioral health or OBS
 ...I POP=1 D
 ....D RECSAVE(RPT,POP,.LIST,ERADMIT)
 Q
FINDADM(DFN,ERADMIT,EREND) ;FIND AN ADMISSION
 N ADMIT,MGVT,MVTDT,MTVED,MVTIEN,X,X1,X2,TRANS,OBS,WARD,MVTFAC,TRANS2,FAC,SPEC
 N MVTED,SRC,UB
 S ADMIT=0
 S X1=ERADMIT,X2=1 D C^%DTC S MVTED=X
 S MVTIEN="" F  S MVTIEN=$O(^DGPM("C",DFN,MVTIEN)) Q:MVTIEN=""  D
 .;The movement must be an admission movement
 .S MVTDT=$P($G(^DGPM(MVTIEN,0)),U,1)
 .Q:MVTDT<ERADMIT!(MVTDT>MVTED)
 .S TRANS=$P($G(^DGPM(MVTIEN,0)),U,2)
 .I TRANS=1 D
 ..S SRC=$P($G(^DGPM(MVTIEN,"IHS")),U,6)
 ..Q:'SRC
 ..S UB=$P($G(^AUTTASRC(SRC,0)),U,2)
 ..Q:UB'=7
 ..S WARD=$P($G(^DGPM(MVTIEN,0)),U,6)
 ..;Quit if the ward itself is an observation ward
 ..I WARD'="" D
 ...S OBS=$$WARDCK(WARD)
 ...S:+OBS ADMIT=2
 ..;Next check for the specialty on the admission
 ..S SPEC=0
 ..S MVTFAC="" F  S MVTFAC=$O(^DGPM("CA",MVTIEN,MVTFAC)) Q:MVTFAC=""!(+SPEC)  D
 ...S TRANS2=$P($G(^DGPM(MVTFAC,0)),U,2)
 ...I TRANS2=6 D
 ....S FAC=$P($G(^DGPM(MVTFAC,0)),U,9)
 ....I FAC'="" D
 .....S SPEC=$$SPEC(FAC)
 .....S:+SPEC ADMIT=2
 ..S:'ADMIT ADMIT=1
 Q ADMIT
CKDX(DX) ;Check for primary diagnosis of behavioral health issue
 N RESULT,BGPTX,TAX
 S RESULT=0
 S TAX="BGPMU ED MENTAL DISORDERS"
 S BGPTX=$O(^ATXAX("B",TAX,0))  ;get taxonomy ien
 I BGPTX="" Q 0  ;not a valid taxonomy
 S RESULT=$$ICD^ATXCHK(DX,BGPTX,9)
 Q RESULT
 ;Observation specialties from the Specialty (#42.4) file are:
 ;
 ;     18 - Neurology Observation
 ;     23 - Spinal Cord Injury Observation
 ;     24 - Medical Observation
 ;     36 - Blind Rehab Observation
 ;     41 - Rehab Medicine Observation
 ;     65 - Surgical Observation
 ;     94 - Psychiatric Observation
WARDCK(WARD) ;ward check
 N SPIFN,WSPEC
 S WSPEC=0
 Q:WARD="" 0
 S SPIFN=$P($G(^DIC(42,WARD,0)),U,12)
 Q:SPIFN="" 0
 S WSPEC=$$SPEC^DGPMOBS(SPIFN)
 I +WSPEC=-1 S WSPEC=0
 Q WSPEC
SPEC(FAC) ;specialty check
 N SPIFN,FSPEC
 Q:FAC="" 0
 S SPIFN=$P($G(^DIC(45.7,FAC,0)),U,2)
 S FSPEC=$$SPEC^DGPMOBS(SPIFN)
 I +FSPEC=-1 S FSPEC=0
 Q FSPEC
RECSAVE(RPT,POP,LIST,ER) ;save record
 I RPT=1 D
 .D SAVE(.LIST,POP,ER)
 I RPT=2 D
 .S ADMOR=$$ORDER(DFN,ERADMIT,EREND)
 .I +ADMOR D SAVE2(.LIST,POP,ER)
 D TOTAL(.LIST,RPT,POP)
 Q
SAVE(LIST,POP,ER) ;Save the result
 N TIME,X1,X2,X3,CNT,RESULT
 S CNT=$G(^TMP("BGPMU0495",$J,BGPMUTF,"POP",POP,"PAT","CNT"))
 S CNT=CNT+1
 S X1=EREND,X2=ERADMIT,X3=2
 S TIME=$$FMDIFF^XLFDT(X1,X2,X3)
 S LIST("ORDERED",POP,TIME,CNT)=TIME
 S LIST("POP",POP,CNT)=TIME
 I POP=1 S RESULT="ED:"_(TIME/60)_" "_$$DATE^BGPMUUTL(ER) S BGPICARE("MU.ED.0495.1",BGPMUTF,CNT)=DFN_U_TIME_U_POP_U_U_RESULT
 I POP=2 S RESULT="OS:"_(TIME/60)_" "_$$DATE^BGPMUUTL(ER) S BGPICARE("MU.ED.0495.2",BGPMUTF,CNT)=DFN_U_TIME_U_POP_U_U_RESULT
 I POP=3 S RESULT="MD:"_(TIME/60)_" "_$$DATE^BGPMUUTL(ER) S BGPICARE("MU.ED.0495.3",BGPMUTF,CNT)=DFN_U_TIME_U_POP_U_U_RESULT
 S ^TMP("BGPMU0495",$J,BGPMUTF,"POP",POP,"PAT","CNT")=CNT
 S ^TMP("BGPMU0495",$J,BGPMUTF,"POP",POP,"PAT",CNT)=DFN_U_RESULT
 Q
SAVE2(LIST,POP,ER) ;Save the result
 N TIME,X1,X2,X3,CNT,RESULT
 S CNT=$G(^TMP("BGPMU0497",$J,BGPMUTF,"POP",POP,"PAT","CNT"))
 S CNT=CNT+1
 S X1=EREND,X2=ADMOR,X3=2
 S TIME=$$FMDIFF^XLFDT(X1,X2,X3)
 S LIST("ORDERED",POP,TIME,CNT)=TIME
 S LIST("POP",POP,CNT)=TIME
 I POP=1 S RESULT="ED:"_(TIME/60)_" "_$$DATE^BGPMUUTL(ER) S BGPICARE("MU.ED.0497.1",BGPMUTF,CNT)=DFN_U_TIME_U_POP_U_U_RESULT
 I POP=2 S RESULT="MD:"_(TIME/60)_" "_$$DATE^BGPMUUTL(ER) S BGPICARE("MU.ED.0497.2",BGPMUTF,CNT)=DFN_U_TIME_U_POP_U_U_RESULT
 I POP=3 S RESULT="OS:"_(TIME/60)_" "_$$DATE^BGPMUUTL(ER) S BGPICARE("MU.ED.0497.3",BGPMUTF,CNT)=DFN_U_TIME_U_POP_U_U_RESULT
 S ^TMP("BGPMU0497",$J,BGPMUTF,"POP",POP,"PAT","CNT")=CNT
 S ^TMP("BGPMU0497",$J,BGPMUTF,"POP",POP,"PAT",CNT)=DFN_U_RESULT
 Q
ORDER(DFN,START,END) ;Find the admission order
 ;Admission order should be after the ER admit time and before the ER end time
 N ADMIT,DISCH,PT,ORD,ORIEN,ORDIA,ORDLK,TXT,RET
 S RET=""
 S ADMIT=9999999-START,DISCH=9999999-END
 S PT=DFN_";DPT("
 S ORD=DISCH F  S ORD=$O(^OR(100,"AR",PT,ORD)) Q:ORD=""!(ORD>ADMIT)!(+RET)  D
 .S ORIEN="" F  S ORIEN=$O(^OR(100,"AR",PT,ORD,ORIEN)) Q:ORIEN=""  D
 ..S ORDIA=$P($G(^OR(100,ORIEN,0)),U,5)
 ..S ORDLK=$P(ORDIA,";",1)
 ..S TXT=$P($G(^ORD(101.41,ORDLK,0)),U,1)
 ..I TXT["ADMIT" S RET=$P($G(^OR(100,ORIEN,0)),U,7)
 Q RET
TOTAL(LIST,RPT,POP) ;Find the median
 N CNT2,MIDDLE,MID1,MID2,WHOLE,CNT,MEDIAN,OFFSET,OFFSET2,I
 S MIDDLE=""
 I RPT=1 D
 .S CNT=$G(^TMP("BGPMU0495",$J,BGPMUTF,"POP",POP,"PAT","CNT"))
 I RPT=2 D
 .S CNT=$G(^TMP("BGPMU0497",$J,BGPMUTF,"POP",POP,"PAT","CNT"))
 S CNT2=CNT/2
 I $P(CNT2,".",2)="" D
 .;EVEN number of patients - average the middle two
 .S WHOLE=$P(CNT2,".",1)
 .S OFFSET=0
 .;;;;;;I WHOLE="" S WHOLE=0,OFFSET=$O(LIST("ORDERED",POP,OFFSET))
 .I WHOLE="" S WHOLE=1
 .S TIME="" F  S TIME=$O(LIST("ORDERED",POP,TIME)) Q:TIME=""!(+MIDDLE)  D
 ..S PTCNT="" F  S PTCNT=$O(LIST("ORDERED",POP,TIME,PTCNT)) Q:PTCNT=""!(+MIDDLE)  D
 ...S OFFSET=OFFSET+1
 ...I OFFSET=WHOLE  D
 ....S NXTPT=$O(LIST("ORDERED",POP,TIME,PTCNT))
 ....I NXTPT'="" S (MID1,MID2)=TIME ;Two middle patients had the same time
 ....E  S MID1=TIME,MID2=$O(LIST("ORDERED",POP,TIME)) ;get next time for 2nd patient
 ....S MIDDLE=(MID1+MID2)/2
 E  D
 .;ODD number of patients - use the middle patient's time
 .S WHOLE=$P(CNT2,".",1)
 .S OFFSET=0
 .S TIME="" F  S TIME=$O(LIST("ORDERED",POP,TIME)) Q:TIME=""!(+MIDDLE)  D
 ..S PTCNT="" F  S PTCNT=$O(LIST("ORDERED",POP,TIME,PTCNT)) Q:PTCNT=""!(+MIDDLE)  D
 ...S OFFSET=OFFSET+1
 ...I OFFSET>WHOLE S MIDDLE=TIME
 S MEDIAN=MIDDLE/60   ;get answer in minutes
 I RPT=1 D
 .S ^TMP("BGPMU0495",$J,BGPMUTF,"POP",POP)=MEDIAN_U_CNT
 I RPT=2 D
 .S ^TMP("BGPMU0497",$J,BGPMUTF,"POP",POP)=MEDIAN_U_CNT
 Q
ENTRY2 ;PEP  0497 Time from provider order to discharge
