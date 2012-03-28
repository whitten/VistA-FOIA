LRSORA3 ;VA/SLC/KCM - SEARCH LAB DATA AND PRINT REPORT ;JUL 06, 2010 3:14 PM;
 ;;5.2;LAB SERVICE;**1,344,1027**;NOV 01, 1997
BUILD ;
 S LRLOG="I "
 F %=1:1:$L(LRTST(0)) D
 . S LRLOG=LRLOG_$S($E(LRTST(0),%)?1A:"T("_($A(LRTST(0),%)-64)_")",1:$E(LRTST(0),%))
 S LRDFN=0,LRLDFN=0,LREND=0
 D SHORT:'LRLONG,LONG:LRLONG
 Q
SHORT ;
 S LRVDT=$P(LREDT,".",1)-.01
 F  S LRVDT=$O(^LRO(69,LRVDT)) Q:LRVDT=""!(LRVDT>LRSDT)  D
 . S LRLLOC=""
 . F  S LRLLOC=$O(^LRO(69,LRVDT,1,"AN",LRLLOC)) Q:LRLLOC=""  D
 .. S LRDFN=0
 .. F  S LRDFN=$O(^LRO(69,LRVDT,1,"AN",LRLLOC,LRDFN)) Q:LRDFN<1  D GIDT
 Q
GIDT ;
 S LRIDT=0
 F  S LRIDT=$O(^LRO(69,LRVDT,1,"AN",LRLLOC,LRDFN,LRIDT)) Q:LRIDT<1  D EVTW
 Q
LONG ;
 S LRSDT=9999998-LRSDT,LREDT=9999999-LREDT
 S LRDFN=0 F  S LRDFN=$O(^LR(LRDFN)) Q:LRDFN<1  D GDATA
 Q
GDATA ;
 S LRIDT=LRSDT
 F  S LRIDT=$O(^LR(LRDFN,"CH",LRIDT)) Q:LRIDT=""!(LRIDT>LREDT)  D
 . D:$L($P(^LR(LRDFN,"CH",LRIDT,0),U,3)) EVTW
 Q
EVTW ;
 S %=$G(^LR(LRDFN,"CH",LRIDT,0)) Q:'$L(%)
 I LRAA S LRAAA=$P($P(%,U,6)," ") Q:'$D(LRAA(LRAAA))#2
 K V F J=1:1:LRTST S T(J)=0
 D EVAL Q:$G(LRNOP)  X LRLOG
 I $T S LRIDT1=0 F  S LRIDT1=$O(V(LRIDT1)) Q:LRIDT1<1  D
 . S LRSUB=0 F  S LRSUB=$O(V(LRIDT1,LRSUB)) Q:LRSUB<1  D SET
 Q
EVAL ;
 F J=1:1:LRTST X LRTST(J,1) D
 . I $T S T(J)=1
 . I  S X=$P(LRTST(J,3),U,1)
 . I  S $P(V(LRIDT,X),U,1)=$P(^LR(LRDFN,"CH",LRIDT,X),U,1)
 . I  S $P(V(LRIDT,X),U,2)=$P(^LR(LRDFN,"CH",LRIDT,X),U,2)
 . I  S $P(V(LRIDT,X),U,3)=$P(LRTST(J,2),U,1)
 Q
SET ;
 K LRWRD
 S LRDPF=$P(^LR(LRDFN,0),U,2),DFN=$P(^(0),U,3) Q:LRDPF<1!(LRDPF=62.3)  D PT^LRX
 I LRPTS Q:'$D(LRPTS(DFN))
 S %=^LR(LRDFN,"CH",LRIDT1,0),LRAN=$P(^(0),U,6),LRLOC=$P(^(0),U,11)
 Q:LRLOC=""  I LRLCS Q:'$D(LRLCS(LRLOC))
 ;S LRWRD="" S:LRDPF=2 LRWRD=$S($D(^DPT(DFN,.1)):^(.1),1:"")
 S LRWRD=$G(^DPT(DFN,.1))
 S (Y1,LRDAT)=$P(^LR(LRDFN,"CH",LRIDT1,0),U,1),Y2=1
 S LRCDT=$$DDDATE^LRAFUNC1(Y1,Y2) K Y1,Y2
 ; S LRSUB1=$S(LRSRT="P":PNM_SSN,1:LRLOC)
 S LRSUB1=$S(LRSRT="P":PNM_HRCN,1:LRLOC)  ;IHS/ANMC/CLS 08/18/96
 ; S LRSUB2=$S(LRSRT="P":LRDAT,1:PNM_SSN)
 S LRSUB2=$S(LRSRT="P":LRDAT,1:PNM_HRCN)  ;IHS/ANMC/CLS 08/18/96
 S LRSUB3=$S(LRSRT="P":LRLOC,1:LRDAT)
 S LRSPEC=$P(^LR(LRDFN,"CH",LRIDT1,0),U,5) D RRNG
 S LRSPEC=$P($G(^LAB(61,LRSPEC,0)),U)
 S LRTEST=LRTSTX
 S LRVAL=$P(V(LRIDT1,LRSUB),U)
 S LRMRK=$P(V(LRIDT1,LRSUB),U,2)
 ; I LRSRT="P" S ^TMP("LR",$J,LRSUB1,LRSPEC,LRTEST,LRIDT)=PNM_U_SSN_U_LRLOC_U_U_LRSPEC_U_U_LRLO_U_LRHI_U_LRVAL_U_LRMRK_U_LRTHER_U_LRWRD_U_LRAN_U_LRDAT_U_LRTEST_U_LRUNITS
 I LRSRT="P" S ^TMP("LR",$J,LRSUB1,LRSPEC,LRTEST,LRIDT)=PNM_U_HRCN_U_LRLOC_U_U_LRSPEC_U_U_LRLO_U_LRHI_U_LRVAL_U_LRMRK_U_LRTHER_U_LRWRD_U_LRAN_U_LRDAT_U_LRTEST_U_LRUNITS  ;IHS/ANMC/CLS 08/18/96
 ; I LRSRT'="P" S ^TMP("LR",$J,LRLOC,LRSUB2,LRSPEC,LRTEST,LRIDT)=PNM_U_SSN_U_LRLOC_U_U_LRSPEC_U_U_LRLO_U_LRHI_U_LRVAL_U_LRMRK_U_LRTHER_U_LRWRD_U_LRAN_U_LRDAT_U_LRTEST_U_LRUNITS
 I LRSRT'="P" S ^TMP("LR",$J,LRLOC,LRSUB2,LRSPEC,LRTEST,LRIDT)=PNM_U_HRCN_U_LRLOC_U_U_LRSPEC_U_U_LRLO_U_LRHI_U_LRVAL_U_LRMRK_U_LRTHER_U_LRWRD_U_LRAN_U_LRDAT_U_LRTEST_U_LRUNITS  ;IHS/ANMC/CLS 08/18/96
 S C=0
 F  S C=$O(^LR(LRDFN,"CH",LRIDT,1,C)) Q:'C  D
 . I LRSRT="P" S ^TMP("LR",$J,LRSUB1,LRSPEC,LRTEST,LRIDT,"COM",C)=^LR(LRDFN,"CH",LRIDT,1,C,0)
 . I LRSRT'="P" S ^TMP("LR",$J,LRLOC,LRSUB2,LRSPEC,LRTEST,LRIDT,"COM",C)=^LR(LRDFN,"CH",LRIDT,1,C,0)
 Q
RRNG ;
 N LRFLAG
 S (LRHI,LRLO,LRTHER,LRFLAG)="",X="CH;"_LRSUB_";1",X=$O(^LAB(60,"C",X,0))
 S LRTSTX=$P(^LAB(60,X,.1),U)
 S LRUNITS=$P($G(^LAB(60,X,1,LRSPEC,0)),"^",7)
 S:$L(X)&$L(LRSPEC) X=$S($D(^LAB(60,X,1,LRSPEC,0)):^(0),1:"") Q:X=""
 ;
 ; check for ranges in file 63
 D CHK63
 ;
 S LRTHER=$P(X,U,11)'=""&($P(X,U,12)'="")
 S LRLO=$S('LRTHER:$P(X,U,2),1:$P(X,U,11))
 S LRHI=$S('LRTHER:$P(X,U,3),1:$P(X,U,12))
 I 'LRFLAG D
 . S @("LRLO="_$S($L(LRLO):LRLO,1:""""""))
 . S @("LRHI="_$S($L(LRHI):LRHI,1:""""""))
 Q
 ;
CHK63 ;
 N LR63DAT,PC5
 S LR63DAT=$G(^LR(LRDFN,"CH",LRIDT,LRSUB))
 I $G(^LR(LRDFN,"CH",LRIDT,"NPC"))>1,$P(LR63DAT,U,5,12)'="" S LRFLAG=1
 I LRFLAG D
 .S PC5=$P(LR63DAT,U,5)
 .S PC5=$TR(PC5,"!","^")
 .S X=PC5
 Q
