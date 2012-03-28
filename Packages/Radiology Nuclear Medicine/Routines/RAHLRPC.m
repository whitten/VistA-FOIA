RAHLRPC ;HIRMFO/BNT-Rad/NM HL7 Protocol calls ;05/21/99   14:50
 ;;5.0;Radiology/Nuclear Medicine;**12,25**;Mar 16, 1998
REG ; register exam
 ;ihs/cmi/sitka/maw this is the trigger point for new exams
 S INA("RADPT")=$G(RADFN)
 S INA("RADTI")=$G(RADTI)
 S INA("RACNI")=$G(RACNI)
 S INDA=$G(RADFN)
 D ^INHF("HL IHS O01 GE OUT PARENT",.INDA,.INA)
 K INDA,INA
 N X,RAPID,RAEID
 S RAPID="RA REF" ; get all protocols beginning RA REG
 F  S RAPID=$O(^ORD(101,"B",RAPID)) Q:RAPID'["RA REG"  D
 .S RAEID=$O(^ORD(101,"B",RAPID,0))
 .D:RAEID EN^RAHLR
 Q
CANCEL ; cancel exam
 ;ihs/cmi/sitka/maw this is the trigger point for cancelled exams
 S INA("RADPT")=$G(RADFN)
 S INA("RADTI")=$G(RADTI)
 S INA("RACNI")=$G(RACNI)
 S INDA=$G(RADFN)
 S INA("CANC")=1
 D ^INHF("HL IHS O01 GE OUT PARENT",.INDA,.INA)
 K INDA,INA
 N X,RAPID,RAEID
 S RAPID="RA CANCEK" ; get all protocols beginning RA CANCEL
 F  S RAPID=$O(^ORD(101,"B",RAPID)) Q:RAPID'["RA CANCEL"  D
 .S RAEID=$O(^ORD(101,"B",RAPID,0))
 .D:RAEID EN^RAHLR
 Q
RPT ; report verified or released/not verified
 ;ihs/cmi/sitka/maw this is the trigger point for reporting
 S INA("RADPT")=$G(RADFN)
 S INA("RADTI")=$G(RADTI)
 S INA("RACNI")=$G(RACNI)
 S INA("RARPT")=$G(RARPT)
 S INA("RESULT")=1
 S INDA=$G(RADFN),INDA(74,1)=$G(RARPT)
 D ^INHF("HL IHS R01 GE OUT PARENT",.INDA,.INA)
 K INDA,INA
 N X,RAPID,RAEID
 S RAPID="RA RPS" ; get all protocols beginning RA RPT
 F  S RAPID=$O(^ORD(101,"B",RAPID)) Q:RAPID'["RA RPT"  D
 .S RAEID=$O(^ORD(101,"B",RAPID,0))
 .D:RAEID EN^RAHLRPT
 Q
EXM ; examined case
 ;Called from RAUTL1 and RASTED after a case's status is upgraded
 ; and case's 30th piece is null
 ;
 ;If this new status is :
 ; at a status (or higher than a status) where
 ; GENERATE EXAMINED HL7 MSG = Y,
 ; then :
 ; 1. send an HL7 msg re this case having reached EXAMINED status
 ; 2. set subfile 70.03's HL7 EXAMINED MSG SENT  to  Y
 ;
 ; RALOWER = next lower status
 ; RANEWST = new status ien
 ; 
 N RAIMGTYI,RAIMGTYJ,RALOWER,RANEWST,RAEXMDUN,RAGENHL7
 S RAIMGTYI=$P($G(^RADPT(RADFN,"DT",RADTI,0)),U,2),RAIMGTYJ=$P(^RA(79.2,RAIMGTYI,0),U),RANEWST=$P($G(^RADPT(RADFN,"DT",RADTI,"P",RACNI,0)),U,3)
 Q:$P(^RADPT(RADFN,"DT",RADTI,"P",RACNI,0),U,30)="Y"  ;already sent
 G:$P(^RA(72,RANEWST,0),U,8)="Y" 1 ;this status has GEN HL7 marked Y
 ; look thru lower statuses for GEN HL7 marked Y
DOWN S RALOWER=$P($G(^RA(72,+RANEWST,0)),U,3)
 F  S RALOWER=$O(^RA(72,"AA",RAIMGTYJ,RALOWER),-1) Q:RALOWER<1  S:$P(^RA(72,+$O(^RA(72,"AA",RAIMGTYJ,RALOWER,0)),0),U,8)="Y" RAGENHL7=1
 ;
 Q:'$G(RAGENHL7)  ;none of the lower status levels have GEN HL7 marked Y
1 S RAEXMDUN=1
 ;ihs/cmi/sitka/maw this is the trigger point for new exams
 S INA("RADPT")=$G(RADFN)
 S INA("RADTI")=$G(RADTI)
 S INA("RACNI")=$G(RACNI)
 S INA("XAM")=1
 S INDA=$G(RADFN)
 D ^INHF("HL IHS O01 GE OUT PARENT",.INDA,.INA)
 K INDA,INA
 N X,RAPID,RAEID
 S RAPID="RA EXAMINEC" ; get all protocols beginning RA EXAMINED
 F  S RAPID=$O(^ORD(101,"B",RAPID)) Q:RAPID'["RA EXAMINED"  D
 .S RAEID=$O(^ORD(101,"B",RAPID,0))
 .D:RAEID EN^RAHLR
2 S $P(^RADPT(RADFN,"DT",RADTI,"P",RACNI,0),U,30)="Y"
 Q
