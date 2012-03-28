BRNRU11 ; IHS/OIT/LJF - PROCESS VISIT LIST
 ;;2.0;RELEASE OF INFO SYSTEM;*1*;APR 10, 2003
 ;IHS/OIT/LJF 10/25/2007 PATCH 1 Added this routine
 ;
 ;
START ;
 ; Known variables:  BRNRPT=ien of temp report, BRNBD & BRNED = date range
 S (BRNBT,BRNBTH)=$H,BRNJOB=$J,BRNRCNT=0
 D XTMP^BRNU("BRNVL",BRNJOB,"ROI REPORTING UTILITY")
 D RUN,END
 Q
 ;
RUN ; Run by date request initiated
 S X1=BRNBD,X2=-1 D C^%DTC S BRNSD=X
 S BRNODAT=BRNSD_".9999" F  S BRNODAT=$O(^BRNREC("B",BRNODAT)) Q:BRNODAT=""!((BRNODAT\1)>BRNED)  D V1
 Q
END ;
 S BRNET=$H
 Q
 ;
V1 ; Within each date, find all disclosure requests
 S BRNVIEN="" F  S BRNVIEN=$O(^BRNREC("B",BRNODAT,BRNVIEN)) Q:BRNVIEN'=+BRNVIEN  I $D(^BRNREC(BRNVIEN,0)) D PROC
 Q
 ;
PROC ; For each disclosure request, does it pass the selection criteria?
 K BRNSPEC
 S BRNVREC=^BRNREC(BRNVIEN,0),DFN=$P(BRNVREC,U,3)
 Q:'$D(^DPT(DFN,0))
 Q:'$D(^AUPNPAT(DFN,0))
 D SCREENS       ; run selection criteria
 Q:$D(BRNSKIP)   ; if it doesn't pass, skip it
 ;
 ; set sort (in printable format) for this entry
 K BRNSRT S BRNCRIT=BRNSORT,BRNX=0
 X:$D(^BRNSORT(BRNSORT,4)) ^BRNSORT(BRNSORT,4)
 I '$D(BRNSRT) S BRNSRT=$P(^DPT(DFN,0),U)
 ;
 ; do subcounts
 D SUBPAT
 ;
 ; set entries into arrays for printing
 S ^XTMP("BRNVL",BRNJOB,BRNBTH,"DATA HITS",BRNSRT,BRNVIEN)="",BRNRCNT=BRNRCNT+1
 Q:$D(^XTMP("BRNVL",BRNJOB,BRNBTH,"PATIENTS",DFN))
 S ^XTMP("BRNVL",BRNJOB,BRNBTH,"PATIENTS",DFN)="",BRNPTCT=BRNPTCT+1
 Q
 ;
SUBPAT ; tally # of patients by sort value on detailed/subtotal
 Q:BRNCTYP="C"
 Q:BRNCTYP="P"
 Q:BRNCTYP="F"
 Q:BRNCTYP="T"
 Q:BRNCTYP="L"
 S:$G(BRNSRT)="" BRNSRT="????"
 Q:$D(^XTMP("BRNVL",BRNJOB,BRNBTH,"SUB PATIENT HIT",BRNSRT,DFN))
 S:'$D(^XTMP("BRNVL",BRNJOB,BRNBTH,"SUB PAT COUNT",BRNSRT)) ^XTMP("BRNVL",BRNJOB,BRNBTH,"SUB PAT COUNT",BRNSRT)=0
 S ^XTMP("BRNVL",BRNJOB,BRNBTH,"SUB PAT COUNT",BRNSRT)=^XTMP("BRNVL",BRNJOB,BRNBTH,"SUB PAT COUNT",BRNSRT)+1
 Q:$D(^XTMP("BRNVL",BRNJOB,BRNBTH,"SUB PATIENT HIT",BRNSRT,DFN))
 S ^XTMP("BRNVL",BRNJOB,BRNBTH,"SUB PATIENT HIT",BRNSRT,DFN)=""
 Q
 ;
SCREENS ; Check screen logic for this disclosure request
 ; loop through all selection criteria; BRNSKIP means does not meet criteria
 K BRNSKIP
 S BRNI=0 F  S BRNI=$O(^BRNRPT(BRNRPT,11,BRNI)) Q:BRNI'=+BRNI!($D(BRNSKIP))  D
 . I '$P(^BRNSORT(BRNI,0),U,8) D SINGLE Q
 . D MULT
 Q
 ;
SINGLE ; Logic sets X if this record (BRNVIEN) meets this criteria (BRNI)
 ; BRNRANG can be set by screen logic if screen is a range and not
 ;         stored individually in the Temp Report global (ex. Age Range)
 K X,BRNRANG S X="",BRNX=0
 X:$D(^BRNSORT(BRNI,1)) ^(1)
 I X="" S BRNSKIP="" Q
 I '$D(BRNRANG),'$D(^BRNRPT(BRNRPT,11,BRNI,11,"B",X)) S BRNSKIP="" Q
 I $G(BRNRANG) I ($P(^BRNRPT(BRNRPT,11,BRNI,11,1,0),U)>X)!(X>$P(^BRNRPT(BRNRPT,11,BRNI,11,1,0),U,2)) S BRNSKIP="" Q
 Q
 ;
MULT ; Logic creates array of possible matches then loops thru multiple in temp report to see if any do
 NEW FOUND,Y,X K BRNSKIP S X=""
 X:$D(^BRNSORT(BRNI,1)) ^(1)
 I $O(X(""))="" S BRNSKIP="" Q
 S Y="" F  S Y=$O(X(Y)) Q:Y=""  I $D(^BRNRPT(BRNRPT,11,BRNI,11,"B",Y)) S FOUND="" Q
 S:'$D(FOUND) BRNSKIP=""
 Q
 ;
XIT ;EP - CALLED FROM BRNVL
 K BRNBD,BRNBDD,BRNED,BRNEDD,BRNSD,BRNSORT,BRNSORV,BRNTCW,BRNRPT,BRNLHDR,BRNDISP,%H,BRNET,BRNLINE,BRNPRNM,BRNPRNT,BRNSKIP,BRNTYPE,BRNSPAG,BRNEN1,BRNSEAT,BRN,BRNCAND,BRNHDR,BRNHEAD,BRNSPEC,BRNOPT
 K BRNCTYP,BRNFLG,BRNG,BRNNAME,BRNNIFN,BRNSAVE,BRNTITL,BRNQUIT,BRNPCNT,BRNQFLG,BRNPTCT,BRNTL,BRNSRTR,BRNSRTV,BRNFILE,BRNJD,BRNFCNT,BRNX1,BRNX2,BRNSDAT
 K C,D,D0,DA,DIC,DD,DFN,DIADD,DLAYGO,DICR,DIE,DIK,DINUM,DIQ,DIR,DIRUT,DUOUT,DTOUT,DR,J,I,J,K,M,S,TS,X,Y,DIG,DIH,DIV,DQ,DDH,AMQQEN3,AMQQLX
XIT1 ;EP
 K BRNANS,BRNBTH,BRNC,BRNCNT,BRNCRIT,BRNCUT,BRND,BRNDISP,BRNDONE,BRNHIGH,BRNI,BRNJOB,BRNQMAN,BRNSEL,BRNTEXT,BRNVAR,BRNSKIP,BRNPRNT,BRNPRNM,BRNLINE,BRNRCNT,BRNSCNT,BRNDFET,BRNY,DFN
 K X,X1,X2,IO("Q"),%,Y,POP,DIRUT,H,S,TS,M,DUOUT,DIR,DTOUT,V,Z,I,DIC,DIK,DIADD,DLAYGO,DA,DR,DIE,DIU,AMQQTAX,DINUM,BRNPACK,BRNEP1,BRNEP2,D,BRNLENG,BRNLHDR,BRNSAVE,AMQQND
 Q
