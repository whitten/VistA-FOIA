AIBCVT ;IHS/DDPS/DFM - IBM STAT RECORDS CREATION ROUTINE [ 09/23/88  11:51 AM ]
 ;1.3; 9/23/88 RESTRUCTURE, ALLOW FOR REGISTRATION ELIGIBILITY FILE
 ;1.1; 5/30/88 ADD DSM DEVICE LOGIC
 ;1.0; 3/28/88
 D INIT^AIBCVT4 G:$L(AIBA)=0 CTL G:AIBA="A" ABEND G CLOSE
CTL ;PERFORM GET CONTROL INFORMATION ROUTINE
 D CONTROL^AIBCVT0 G:$L(AIBA)>0 ABEND U AIBCDV D GBLHEAD^AIBCVT2 G:$L(AIBA)>0 ABEND
 S:AIBOS="M" (AIBT1,AIBFN)=AIBDIR_AIBCF_"."_AIBFILE
 I AIBT2="A" I AIBOS="D" G PROCESS
 U AIBCDV D OUT^AIBSDEV G:AIBA="A" ABEND G:AIBA="C" OPCANCL S AIBTDV=AIBDEV,AIBFFN=2 G:AIBOS="D" PROCESS
 I AIBFILE="keytap" G TAPE
 I AIBT2="W" D BJCL^AIBCVT0 G PROCESS
 G PROCESS
TAPE ;DISPLAY VOL SER TO CREATE
 I AIBT2="A" G PROCESS
 I AIBGBLP="AGEL" G PROCESS
 U AIBCDV R !!,"Enter Volume Serial Number Of Keytape To Be Created ",AIBV:DTIME W !!
PROCESS ;PROCESS RECORDS
 D PROCESS^AIBCVT1 G:AIBA="A" ABEND G:AIBA="R" MTERR G:AIBA="C" OPCANCL G CKEOJ
MTERR ;ABEND ON BAD TAPE WRITE
 D MTERR^AIBCVT6 G CLOSE
OPCANCL ;ABEND BECAUSE OPERATOR CANCLED JOB
 D OPCANCL^AIBCVT6 G CLOSE
ABEND ;ABNORMAL END OF JOB
 D ABEND^AIBCVT6 G CLOSE
CKEOJ ;CHECK IF DONE
 I $L(AIB2ND)=0 G CKRJE
 D EOJ^AIBCVT2
 S AIBGBLP=AIBTGBL,AIBA="",AIBT2="A" K:'$D(AIBOK) @AIBGBL D CKGLOB^AIBCVT4
 U AIBTDV S (IO,AIBDEV)=AIBTDV D:AIBOS="M" CLOSE^AIBSDEV
 G CTL
CKRJE ;CHECK IF RJE ENDING JCL NEEDS TO BE WRITTEN
 D:AIBFILE="ibmjob" EJCL^AIBCVT0
 D EOJ^AIBCVT2
CLOSE ;CLOSE FILES
 D CLOSEND^AIBCVT6 Q
