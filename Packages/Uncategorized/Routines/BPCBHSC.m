BPCBHSC ; IHS/OIT/MJL - BEHAVIORAL HEALTH SPECIAL CROSSREFERENCE FOR GUI ; [ 12/31/2007  10:16 AM ]
 ;;1.5;BPC;**1,4**;OCT 04, 2005
 ;
 ; BPCSETLV - Setting Primary List view - Assumes MH Encounter is central
 ; BPCCTYPE - Screen to Admin Type
 ; BPCGREF  - Primary Global Reference
 ; BPCSBJGR - Subject Global Reference - Used if all of the entries are to be used under this node -- don't need to check a date
 ; BPCDIVAL - Look at all divisions - Not used here, used to screen patients selected prior to calling this routine
 ; BPCBHALL - Look at all users' entries - Only relates to MH REC
 ;
SETUP ;
 S BGUCRFS="",BPCVTYPE=$G(BPCVTYPE)
 S:'$D(BPCGREF) BPCVTYPE=1,BPCGREF="^AMHREC(""AF"",BPCPIEN)"
 S:BGUMAX=25 BGUMAX=0 S BPCLIM=BGUMAX,BGUMAX=999999999 S:'BPCLIM BPCLIM=BGUMAX
 S BPCDIR=1 S:BGUDIR="B" BPCDIR=-1 S BGUDIR=""
 S (BGUBEGIN,BGUEND)="",BPCC=0,BPCGROUP=$G(BPCGROUP),BPCSETLV=$G(BPCSETLV,1),BPCDIVAL=1
 Q
 ;
MHRC ; Get records for a patient by date range
 ;S BPCGREF=$G(BPCGREF,"^AMHREC(""AF"",BPCPIEN)")
 S BPCPIEN=$P(BGUBEGIN,"`"),BGUBEGIN=$P(BGUBEGIN,"`",2)
 D MHREC
 Q
 ;
MHENC ; Get records for an encounter
 I '$D(BGUDRIVR) S BGUDRIVR="MHENC^BPCBHSC",BPCVIEN=BGUBEGIN  D SETUP Q
 D:BPCVIEN'="" MHENCTR
 D KILL
 Q
 ;
MHREC ;
 I '$D(BGUDRIVR) S BGUDRIVR="MHREC^BPCBHSC",BPCSDATE=$P(BGUBEGIN,"`"),BPCVIEN=$P(BGUBEGIN,"`",2),BPCEDATE=$P(BGUEND,"`",1) D SETUP Q
 S BPCVWOPT="2",BPCSBJGR=$G(BPCSBJGR),BPCCTYPE=$G(BPCCTYPE),BPCDTCK=$G(BPCDTCK)
 D MHREC1,KILL
 Q
MHREC1 ;
 D
 .I BPCSDATE'=+BPCSDATE D  Q
 ..S:BPCSDATE="" BPCSDATE="1/1/1980"
 ..S:BPCEDATE="" BPCEDATE="T"
 ..D DT^DILF("",BPCSDATE,.BPCSDAT)
 ..I BPCSDAT=-1 S BPCSDATE="1/1/1980" D DT^DILF("",BPCSDATE,.BPCSDAT)
 ..D DT^DILF("",BPCEDATE,.BPCEDAT)
 ..I BPCEDAT=-1 S BPCEDATE="T" D DT^DILF("",BPCEDATE,.BPCEDAT)
 ..S:BPCDIR<0 BPCEDAT=BPCEDAT+.9999
 .S BPCSDAT=BPCSDATE,BPCEDAT=BPCEDATE
 S BPCCTYPE=$G(BPCCTYPE)
 S BPCC=0,BPCX=0 ;,BPCLIM=$S(BPCVWOPT="0":BPCVWNO,1:999999)
 I BPCDTCK'="" D @BPCDTCK Q
 S BPCX=$S(BPCDIR>0:$O(@BPCGREF@(BPCSDAT),-1),1:$O(@BPCGREF@(BPCEDAT))) F  S BPCX=$O(@BPCGREF@(BPCX),BPCDIR) Q:'BPCX  Q:$S(BPCDIR>0:BPCX\1>BPCEDAT,1:BPCX\1<BPCSDAT)  D  Q:BPCC=BPCLIM
 .F  S BPCVIEN=$O(@BPCGREF@(BPCX,BPCVIEN)) Q:'BPCVIEN  Q:BPCC=BPCLIM  D MHENCTR
 Q
 ;
MHENCTR ;
 ;IHS/CMI/LAB begin mods for UU logic
 I BPCGREF["AMHPSUIC",'$$SFSDE(DUZ,BPCVIEN) Q  ;APPLY SDE LOGIC TO SF FORMS
 ;NEXT LINE CHECKS LOCATION OF ENCOUNTER SCREEN (UU) for group and visits
 I BPCVTYPE S BPCXX=$S(BPCGROUP:$P(^AMHGROUP(BPCVIEN,0),U,5),1:$P(^AMHREC(BPCVIEN,0),U,4)) I $T(ALLOWV^AMHUTIL)]"",'$$ALLOWV^AMHUTIL(DUZ,BPCXX) Q  ;DON'T DISPLAY THIS VISIT
 ;IHS/CMI/LAB end mods for UU logic
 I BPCCTYPE'="",$P(^AMHREC(BPCVIEN,0),U,7)'=BPCCTYPE Q
 S BPCBHALL=1 I BPCVTYPE S BPCBHALL=0,BPCXX=$S(BPCGROUP:$P(^AMHGROUP(BPCVIEN,0),U,5),1:$P(^AMHREC(BPCVIEN,0),U,4)) I BPCXX,$D(^AMHSITE(BPCXX,16,DUZ))!($D(^AMHSITE(DUZ(2),16,DUZ))) S BPCBHALL=1
 ;IHS/CMI/LAB - modified line below to use $$ALLOW instead of checking 19th piece
 I 'BPCBHALL,$S(BPCGROUP:$P(^AMHGROUP(BPCVIEN,0),U,15)'=DUZ,1:'$D(^AMHSITE(DUZ(2),16,DUZ))) S BGUV(BGUFILE,88888)=0 D  Q:'BGUV(BGUFILE,88888)
 .I BPCGROUP D  Q
 ..F  S BGUV(BGUFILE,88888)=$O(^AMHGROUP(BPCVIEN,11,BGUV(BGUFILE,88888))) Q:'BGUV(BGUFILE,88888)  Q:$P(^AMHGROUP(BPCVIEN,11,BGUV(BGUFILE,88888),0),U)=DUZ
 .F  S BGUV(BGUFILE,88888)=$O(^AMHRPROV("AD",BPCVIEN,BGUV(BGUFILE,88888))) Q:'BGUV(BGUFILE,88888)  Q:$P(^AMHRPROV(BGUV(BGUFILE,88888),0),U)=DUZ
 D
 .I 'BPCGROUP D:BPCSETLV  Q  ;PICKUP ALL THESE FIELDS IF BPCSETLV IS SET, INSTEAD OF REQUIRING THEIR OWN FLAGS
 ..S BGUV(BGUFILE,666666)=$O(^AMHREDU("AD",BPCVIEN,0))
 ..S BGUV(BGUFILE,777777)=$O(^AMHRHF("AD",BPCVIEN,0))
 ..S BGUV(BGUFILE,99999)=$O(^AMHRPRO("AD",BPCVIEN,0))
 ..S BGUV(BGUFILE,88888)=0 D
 ...F  S BGUV(BGUFILE,88888)=$O(^AMHRPROV("AD",BPCVIEN,BGUV(BGUFILE,88888))) Q:BGUV(BGUFILE,88888)=""  Q:$P(^AMHRPROV(BGUV(BGUFILE,88888),0),U,4)="P"
 .I BPCSETLV D
 ..S BGUV(BGUFILE,99999)=$O(^AMHGROUP(BPCVIEN,21,0)) S:BGUV(BGUFILE,99999) BGUV(BGUFILE,99999)=$P(^(BGUV(BGUFILE,99999),0),U)
 ..S BGUV(BGUFILE,88888)=0 D
 ..F  S BGUV(BGUFILE,88888)=$O(^AMHGROUP(BPCVIEN,11,BGUV(BGUFILE,88888))) Q:'BGUV(BGUFILE,88888)  I $P(^AMHGROUP(BPCVIEN,11,BGUV(BGUFILE,88888),0),U,2)="P" S BGUV(BGUFILE,88888)=$P(^(0),U) Q
 I BPCSBJGR="" S BGUSUB(1)=BPCVIEN,BGUV(BGUFILE,.0001)=BPCX D FIELDS^BGULIST S BPCC=BGUICNT Q
 S BPCSIEN=0 F  S BPCSIEN=$O(@BPCSBJGR@(BPCSIEN)) Q:'BPCSIEN  S BPCC=BPCC+1,BGUSUB(1)=BPCSIEN,BGUV(BGUFILE,.0001)=BPCX D FIELDS^BGULIST S BPCC=BGUICNT
 Q
 ;
ADM ; Get Administration Entries
 S BPCCTYPE=$O(^AMHTSET("B","ADMINISTRATIVE",""))
AD ; Get AD xref entries
 S BPCBHALL=1,BPCGREF="^AMHREC(""B"")",BPCVTYPE=1
 D MHREC
 Q
 ;
MHCASE ;
 S BPCGREF="^AMHPCASE(""C"",BPCPIEN)",BPCDTCK="MHCASE1"
 D MHRC
 Q
 ;
MHCASE1 ;
 ;IHS/CMI/LAB - added $$CDSDE check
 S BPCVIEN=0 F  S BPCVIEN=$O(@BPCGREF@(BPCVIEN)) Q:'BPCVIEN  Q:BPCC=BPCLIM  S BPCX=$P(^AMHPCASE(BPCVIEN,0),U,1) I BPCX'<BPCSDAT,BPCX'>BPCEDAT,$$CDSDE(DUZ,BPCVIEN) D MHENCTR
 Q
 ;
MHTREAT ;
 S BPCGREF="^AMHPTXP(""AC"",BPCPIEN)",BPCDTCK="MHTREAT1",BPCSETLV=""
 D MHRC
 Q
 ;
MHTREAT1 ;
 ;IHS/CMI/LAB - added $$TPSDE check
 S BPCVIEN=0 F  S BPCVIEN=$O(@BPCGREF@(BPCVIEN)) Q:'BPCVIEN  Q:BPCC=BPCLIM  S BPCX=$P(^AMHPTXP(BPCVIEN,0),U,1) I BPCX'<BPCSDAT,BPCX'>BPCEDAT,$$TPSDE(DUZ,BPCVIEN) D MHENCTR
 Q
 ;
MHSUIV ;
 S BPCGREF="^AMHPSUIC(""AD"")",BPCSETLV=""
 D MHREC
 Q
 ;
MHSUI ;
 S BPCGREF="^AMHPSUIC(""AC"",BPCPIEN)",BPCDTCK="MHSUI1",BPCSETLV=""
 D MHRC
 Q
 ;
MHSUI1 ;
 ;IHS/CMI/LAB - added $$SFSDE check
 S BPCVIEN=0 F  S BPCVIEN=$O(@BPCGREF@(BPCVIEN)) Q:'BPCVIEN  Q:BPCC=BPCLIM  S BPCX=$P(^AMHPSUIC(BPCVIEN,0),U,6) I BPCX'<BPCSDAT,BPCX'>BPCEDAT,$$SFSDE(DUZ,BPCVIEN) D MHENCTR
 Q
 ;
HF ; Health Factors
 S BPCSBJGR="^AMHRHF(""AD"",BPCVIEN)",BPCSETLV=0,BPCCTYPE=""
 D MHENC
 Q
 ;
EDU ; Patient Education
 S BPCSBJGR="^AMHREDU(""AD"",BPCVIEN)",BPCSETLV=0,BPCCTYPE=""
 D MHENC
 Q
 ;
MHGRP ;
 S BPCGREF="^AMHGROUP(""B"")",BPCGROUP=1,BPCVTYPE=1
 D MHREC
 Q
 ;
KILL ;
 K BGUDRIVR,BPCC,BPCCTYPE,BPCDIVAL,BPCDTCK,BPCEDATE,BPCGREF,BPCGROUP,BPCBHALL,BPCLBONL,BPCLIM,BPCPIEN,BPCSBJGR,BPCSDATE,BPCSETLV,BPCSIEN,BPCVIEN,BPCVTYPE,BPCVWNO,BPCVWOPT,BPCX,BPCXX
 Q
CDSDE(P,I) ;can user P see this case status record per SDE parameter?
 I $D(^AMHSITE(DUZ(2),16,P)) Q 1  ;allow all with access
 I $P(^AMHPCASE(I,0),U,8)=P Q 1
 Q 0
 ;
TPSDE(P,I) ;can user P see this treatment plan per SDE parameters?
 I $D(^AMHSITE(DUZ(2),16,P)) Q 1  ;allow all with access
 I $P(^AMHPTXP(I,0),U,4)=P Q 1
 Q 0
 ;
SFSDE(P,I) ;
 I $D(^AMHSITE(DUZ(2),16,P)) Q 1  ;allow all with access
 I $P(^AMHPSUIC(I,0),U,3)=P Q 1  ;allow your own
 Q 0
 ;
ALLOW(R) ;
 I $D(^AMHSITE(DUZ(2),16,DUZ)) Q 1  ;allow all with access
 NEW X,G S G=0 S X=0 F  S X=$O(^AMHRPROV("AD",R,X)) Q:X'=+X  I $P(^AMHRPROV(X,0),U)=DUZ S G=1
 I G Q 1
 I $P(^AMHREC(R,0),U,19)=DUZ Q 1
 Q 0
